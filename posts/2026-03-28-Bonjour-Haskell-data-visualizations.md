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

<!-- TODO: add pi monte carlo plot -->

<!-- TODO: add pi monte carlo code sample (with more comments) -->

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
