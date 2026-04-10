---
title: "Haskell data visualization part 3: Bonjour, plots (fancy plotting)"
date: 2026-04-09
description: It's time for fancy plots -- facets, interactivity, annotation,
  and drawing shapes wherever you want. This post only covers fancy plotting in `hvega`.
readingTime: 6
---

<head>
  <script src="https://cdn.jsdelivr.net/npm/vega@5"></script>
  <script src="https://cdn.jsdelivr.net/npm/vega-lite@4"></script>
  <script src="https://cdn.jsdelivr.net/npm/vega-embed"></script>
</head>

This post covers fancier plotting than the two previous posts. The [Hello, plots] post
covered simple scatter plots. The configuration post in [part two] covered how to change
a bunch of plot display attributes like scales and titles. This post will cover interactivity,
small multiples, and labeling specific interesting points on your plots.

These examples will be more complicated than either of the previous post's examples, so I'll explain more of the
plotting code than in previous posts. Because this post would be really
long if I included more than one library's worth of examples, I'll only focus on `hvega` here.

Both examples below can be found in my [`goofing-off` repo].

## Interactivity and annotation

This plot shows an estimate of pi based on the proportion of random
points that fall inside a quarter of the unit circle.
It's based on this example from [Vega's tutorials].

<div class="flex-container">
![](./gen/pi-monte-carlo.html)
</div>

```haskell
hvegaPiMonteCarlo :: IO ()
hvegaPiMonteCarlo =
  let nRows = 10000
      points = samplePointsDf nRows
      rows =
        -- map the columns from the dataframe into objects hvega expects
        V.dataFromColumns []
          . V.dataColumn "idx" (V.Numbers (fromIntegral @Int <$> DT.columnAsList @"idx" points))
          . V.dataColumn "x" (V.Numbers (DT.columnAsList @"x" points))
          . V.dataColumn "y" (V.Numbers (DT.columnAsList @"y" points))

      -- add Vega data to each row for whether the point is inside
      -- or outside the unit circle, a rolling count of how many
      -- points are inside the unit circle, and an estimate of pi
      -- based on that count.
      randomPointsTransform =
        V.transform $
          ( V.filter (V.FCompose (V.Expr "num_points_idx >= datum.idx"))
              . V.calculateAs "datum.x * datum.x + datum.y * datum.y < 1 ? 1 : 0" "inside"
              . V.window [([V.WAggregateOp V.Sum, V.WField "inside"], "insideCount")] []
              . V.calculateAs "datum.insideCount * 4 / datum.idx" "piEstimate"
          )
            []
      -- color the points based on whether they're inside or outside the unit circle
      enc =
        V.encoding
          . V.position V.X [V.PName "x", V.PmType V.Quantitative]
          . V.position V.Y [V.PName "y", V.PmType V.Quantitative]
          . V.color [V.MName "inside", V.MmType V.Nominal]
          . V.opacity [V.MNumber 0.15]
      -- slider to choose a value bound to "num_points_idx" (combination of the
      -- selection field name and slider field name, for reasons I don't really
      -- understand); used to filter for rows with idx < slider value, i.e. to choose
      -- how many points are used in the Monte Carlo estimation of pi
      slider = V.IRange "idx" [V.InMin 100, V.InMax (fromIntegral nRows), V.InStep 10]
      selection =
        V.selection
          . V.select
            "num_points"
            V.Single
            [V.Fields ["idx"], V.SInit [("idx", V.Number 1000)], V.Bind [slider]]
      pi_ = V.transform . V.filter (V.FCompose (V.Expr "num_points_idx == datum.idx"))
   in V.toHtmlFile "plots/vegaCalculatePi.html" $
        V.toVegaLite
          [ rows [],
            randomPointsTransform,
            V.layer
              [ V.asSpec
                  [ V.mark V.Point [V.MFilled True],
                    enc [],
                    selection []
                  ],
                V.asSpec
                  [ pi_ [],
                    V.mark V.Text [V.MFontSize 18, V.MFontWeight V.Bold],
                    V.encoding
                      . V.position V.X [V.PmType V.Quantitative, V.PDatum (V.Number 1)]
                      . V.position V.Y [V.PmType V.Quantitative, V.PDatum (V.Number 0.5)]
                      . V.text
                        [ V.TName "piEstimate",
                          V.TFormatAsNum,
                          V.TFormat ".3f"
                        ]
                      $ []
                  ]
              ]
          ]
```

The biggest difference between this plot and previous plots is that this plot responds to user input
with a slider for choosing how many points you want to use to calculate the value of pi. That happens via the
`IRange` and `selection` functions. Additionally, I deviated from the example in [Vega's tutorials] by just writing
the current estimate of pi onto the main plot instead of adding it to a different plot. I made this choice to show
adding text at a particular location in the plot and to keep the example from taking on all the complexity of the
version in the tutorial.

Putting the annotation on the same plot works with _layers_ (the two `asSpec` values passed to `V.layer`). The first
layer shows only the points colored by whether they're inside or outside the unit circle. The second layer shows
only the current estimate of pi. `pi_` in the second layer is a filtered view of the data from the first layer
containing only the last row of the input for the number of points you choose.

If you pick a low value for the number of points then toggle back and forth, you can see that the data used to estimate
pi are fixed -- each increment/decrement adds and removes the same 10 points to the plot. The reason for that is that
I started with a `DataFrame` instead of letting Vega do all of the work of generating the data, then calculated the
data to estimate pi from the random points in Vega.[^1]

## Faceting and freeform drawing

This plot shows another scatter plot, but with two modifications. First, instead of all the points for every tag in
the same plot, I've separated the plots for each tag. Second, I've drawn a weird little chevron-ish shape on top of the
mean position for each tag.

<div class="flex-container">
![](./gen/hvega-simple-facet.html)
</div>

```haskell
hvegaFacetAnnotatedMeans :: DT.TypedDataFrame LabeledDfSchema -> IO ()
hvegaFacetAnnotatedMeans df =
  let x = DT.columnAsList @"x" df
      y = DT.columnAsList @"y" df
      tag = DT.columnAsList @"tag" df
      vegaData =
        V.dataFromColumns []
          . V.dataColumn "x" (V.Numbers x)
          . V.dataColumn "y" (V.Numbers y)
          . V.dataColumn "tag" (V.Strings . (Text.pack . pure <$>) $ tag)
      means =
        V.transform
          . V.aggregate
            [ V.opAs V.Mean "x" "xBar",
              V.opAs V.Mean "y" "yBar"
            ]
            ["tag"]
      colorMarkProperties =
        [ V.MName "tag",
          V.MmType V.Nominal,
          V.MScale [V.SScheme "blues" [5]]
        ]
      enc =
        V.encoding
          . V.position V.X [V.PName "x", V.PmType V.Quantitative]
          . V.position V.Y [V.PName "y", V.PmType V.Quantitative]
          . V.color colorMarkProperties

      meansEnc =
        V.encoding
          . V.position V.X [V.PName "xBar", V.PmType V.Quantitative]
          . V.position V.Y [V.PName "yBar", V.PmType V.Quantitative]
          -- use the svgText shape defined above to mark the means
          . V.shape [V.MSymbol $ V.SymPath svgText]
   in V.toHtmlFile "plots/hvegaSimpleFacet.html" $
        V.toVegaLite
          [ -- facet by tag values
            V.facetFlow [V.FName "tag", V.FmType V.Nominal],
            -- define the facet layout; 4 columns, as many rows as necessary
            V.columns 4,
            vegaData [],
            V.specification . V.asSpec $
              [ V.layer
                  [ V.asSpec
                      [ V.mark V.Point [V.MFilled True],
                        enc []
                      ],
                    V.asSpec
                      [ means [],
                        meansEnc $ [],
                        (V.mark V.Point [V.MColor "firebrick"])
                      ]
                  ]
              ]
          ]
```

To create a custom marker shape, I used `SymPath`. `SymPath` takes a set of [SVG path instructions] to use
as a marker.[^2] In principle I think this means you can draw anything you want for a marker, but I didn't
try anything other than the little chevron.[^3] A custom marker shape isn't quite drawing whatever you
want wherever you want, but it's something.

Faceting turns out to be pretty simple.

```haskell
[
  V.facetFlow [V.FName "tag", V.FmType V.Nominal], -- 1
  V.columns 4, -- 2
  V.specification . V.asSpec $ [ ... ] -- 3
]
```

1. I declared I wanted to facet by the `tag` value, which holds nominal data.
2. I laid out the facets with four columns, letting Vega figure out how many rows that requires.
3. I provided additional plotting instructions that declare how each faceted plot should be drawn.

At 3, I didn't have to provide data to each of the subplots. I don't know exactly how it works in Vega's javascript,
but as an approximation, the faceted plots get their data from the root of the specification, filtered only to the part
of the data that's relevant for each subplot.

## Other posts

This post is the third post in a series:

* [Hello, plots]
* [Plot configuration]
* Fancy plotting in `hvega` (this post)
* Fancy plotting in `chart-svg`

[`goofing-off` repo]: https://github.com/jisantuc/goofing-off/blob/main/src/plotting-survey/PlotSurvey/BonjourHVega.hs
[Hello, plots]: ./2026-03-05-Haskell-data-visualization.html
[part two]: ./2026-03-21-Haskell-data-visualization-part-2.html
[Plot configuration]: ./2026-03-21-Haskell-data-visualization-part-2.html
[Vega's tutorials]: https://vega.github.io/vega/examples/pi-monte-carlo/
[SVG path instructions]: https://developer.mozilla.org/en-US/docs/Web/SVG/Tutorials/SVG_from_scratch/Paths

[^1]: An idea I've gotten a little stuck on is that there are striking similarities between accessing columns from
dataframes and accessing columns from the Vega data definition. In each, you register something with a string, then
later you refer to it by a string, and you really hope you've picked the same string both times. The similarity extends
to transformations over the input data. `dataframe`'s typed API also models
[derivations](https://hackage-content.haskell.org/package/dataframe-1.0.0.1/docs/DataFrame-Typed-Operations.html#v:derive)
of new values from the initial schema, which would have been really useful in this plotting code as well. I don't want
to say how many times I mixed up what I was calling `insideCount` vs. `countInside`.
[^2]: Figuring this out took me way longer than I wish it had. I tried a few different `<path d="..."/>` tags, with
Vega yelling at me about "Invalid SVG path, incorrect parameter type". I didn't know what to do with that and googling
the error didn't help. I don't know what caused me to try the path instructions directly. Also, I created the little
chevron shape by hand. There are a few SVG builder libraries for Haskell like `lucid-svg`, `diagrams-svg`, and
`blaze-svg`. They all look nice but they seemed like overkill here. If you want to create a more interesting shape than
the one I used, one of those libraries could be useful
(see [svg on hackage](https://hackage.haskell.org/packages/search?terms=svg)).
[^3]: If you wanted, I bet you could use the [Haskell logo](https://commons.wikimedia.org/wiki/File:Haskell-Logo.svg)
as a marker if you
[restricted it to the right coordinate range](https://hackage.haskell.org/package/hvega-0.12.0.7/docs/Graphics-Vega-VegaLite.html#v:SymPath).
