---
title: "Haskell data visualization part 3: Bonjour, plots (complicated plotting)"
date: 2026-03-28
description: It's time for fancy plots -- facets, interactivity, annotation,
  and drawing shapes wherever you want. This post only covers fancy plotting in `hvega`.
readingTime: 15
---

<!-- TODO: tighten up -->

In [part two], I changed plots from their defaults with some goofy configuration
options, and all of the plotting libraries I'd tried so far had some knobs to turn.
In this post, I'll cover several more complex plotting features.

* interactivity -- mouse clicks, dropdown menus -- other features that let a user's
  actions influence a plot
* annotation -- adding text in specific locations
* faceting -- subplots for different values of an enum field
* freeform drawing -- sometimes you really need a circle somewhere in your plot

I'll only focus on `hvega` here. These examples will be more complicated than
either of the previous post's examples, so I'll explain more of the plotting
code than in previous posts. As a consequence, this post would be really
long if I included more than one library's worth of examples.

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

There's a lot more going on here than in previous examples. The biggest change is that this plot responds to user input
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

## Faceting

## Freeform drawing

## Other posts

This post is the third post in a series
* [Hello, plots]
* [Plot configuration]
* Fancy plotting in `hvega` (this post)
* Fancy plotting in `chart-svg`

[Hello, plots]: ./2026-03-05-Haskell-data-visualization.md
[part two]: ./2026-03-21-Haskell-data-visualization-part-2.html
[Plot configuration]: ./2026-03-21-Haskell-data-visualization-part-2.html
[Vega's tutorials]: https://vega.github.io/vega/examples/pi-monte-carlo/
[^1]: An idea I've gotten a little stuck on is that there are striking similarities between accessing columns from
dataframes and accessing columns from the Vega data definition. In each, you register something with a string, then
later you refer to it by a string, and you really hope you've picked the same string both times. The similarity extends
to transformations over the input data. `dataframe`'s typed API also models
[derivations](https://hackage-content.haskell.org/package/dataframe-1.0.0.1/docs/DataFrame-Typed-Operations.html#v:derive)
of new values from the initial schema, which would have been really useful in this plotting code as well. I don't want
to say how many times I mixed up what I was calling `insideCount` vs. `countInside`.
