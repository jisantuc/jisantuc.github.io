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
          . V.fill [V.MName "inside", V.MmType V.Nominal]
          . V.color [V.MName "inside", V.MmType V.Nominal]
          . V.opacity [V.MNumber 0.15]
      -- TODO: include comments from goofing-off
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
                  [ V.mark V.Point [],
                    enc [],
                    selection []
                  ],
                V.asSpec
                  [ pi_ [],
                    V.mark V.Text [],
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
