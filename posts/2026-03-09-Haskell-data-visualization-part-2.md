---
title: "Haskell data visualization part 2: Hello, slightly different plots"
date: 2026-03-09
description: |
  Part 2 of a short series on making plots in Haskell. Part 1 covered making some 
  simple scatter plots. This part focuses on changing attributes of your plots.
  Part 3 will cover more complex plotting.
---

In [part one], I made a few scatter plots with default settings (or examples from docs if
"default settings" wasn't a meaningful category). In part two, I'll look at how plot configuration works in each of the five libraries I used in part one. Those libraries are:

* [`dataframe`]
* [`granite`]
* [`hvega`]
* [`Chart`]
* [`chart-svg`]

I'm interested in a few kinds of plot configuration, specifically:

* Axis control:
    * How do you change an axis's scale and limits?
* Labeling:
    * How do you re-title a plot?
    * How do you control axis labels?
* Appearance:
    * How do you change the color cycle / color map used for symbols on a plot?
    * How do you change the font used in a plot's text?

[part one]: ./2026-03-05-Haskell-data-visualization.html
