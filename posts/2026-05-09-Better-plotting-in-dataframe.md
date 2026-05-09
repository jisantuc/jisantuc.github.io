---
title: Better plotting in dataframe
date: 2026-05-09
description: |
  dataframe's current plotting interface is simple but low powered. Off-the-shelf options can add a lot of power at the
  cost of simplicity. Maybe there's a way to have both.
---

Now that I'm the foremost amateur[^1] in Haskell plotting libraries, `rest of intro`

1. Learn from other plotting libraries.
2. Maximize power (aka [make hard things possible])

### Learning from other plotting libraries

* good stuff to borrow:
  * attitude of "easy thing easy / [make hard things possible]" -- simple plots should be simple to express (`matplotlib`,
    `dataframe`, [`granite`], `pandas`, ...), you shouldn't have to care about power unless you need it
  * model plot data ([`chart-svg`], [`hvega`] / `Vega` even if it's basically JSON) instead of text (`granite`)
  * optics for setting attributes on whatever the data representation of the plot is (`chart-svg`) because the data
    representation is probably huge / complex

### Maximizing power

* need a low-level API -- sometimes you want to do something weird and custom, or maybe you're inventing a plot type
  that doesn't even exist yet 🤷🏻, or if you're building on an existing plotting ecosystem, need an escape hatch to
  stuff that might not be available

## A layered API with `hvega`

In the previous few posts, I learned that [`hvega`] was the only one of
the available plotting libraries that supported all of the fancy plot types and interactivity. Interactivity
is also on the [roadmap] and is really valuable in notebook environments, and my `hvega` $\pi$ estimator's [slider]
worked great in a [Sabela] notebook.

At the same time, it's _really important_ from the perspective of "making easy things easy" that people don't have to
learn `hvega` / `Vega` / `VegaLite` to make a simple scatter plot. Learning `hvega` and Vega at the same time
was _hard_. Minimizing "time to first plot" is a goal I've heard for `dataframe`'s onboarding documentation. Detouring
through documentation of two other large libraries takes time. 

### Highest level API

`dataframe`'s simplicity is a great starting point. There's not a ton of power in how you can plot with `dataframe`,
but sometimes you just want a picture with a scatter plot in it. Methods for "just give me a picture" should live in 
`DataFrame.Display.IO` and should include e.g.

* why's display separate
  * displaying a plot is a different concern from constructing a plot -- want something like MPL's interactivity from
    `plt.show()`, also want display to be easy in notebook settings, these are probably different things
  * notion of backends? or just different functions? probably no backends, unless maybe there's a `Plottable`
    typeclass? I can't come up with laws for it, and `a -> IO ()` is not a super informative interface for `showPlot`,
    so I don't think so.

```haskell
showScatter :: Text -> Text -> DataFrame -> IO ()

htmlScatter :: Text -> Text -> DataFrame -> HtmlText

showVegaLite :: VegaLite -> IO ()

htmlVegaLite :: VegaLite -> HtmlText
```

where `showScatter` is like calling `plt.show()` after some plotting calls in `matplotlib`, and `HtmlText` is a
`newytpe` I'm pretending exists that is compatible with what `Sabela` needs for [`displayHtml`]. In practice, right
now, Sabela just wants a `String`, but it could be some other constrained type in the future, and `htmlScatter` should
return something compatible with it.

Mostly functions in this module have nothing to do with each other, dispatching instead to calls to the high level
API. The exceptions are `showVegaLite` and `htmlVegaLite`, which handle conversion from `VegaLite` to the convenient
display formats required.

This level of the API almost entirely hides `hvega`'s involvement other than the two conversion functions.

### High level API

* functions producing `VegaLite` type from `hvega`
* chart types, but not what you do with them
* (up) display API doesn't have to do any chart config, just calls functions from high level API,
  (down) high level API produces values that can be conveniently configured with optics

example:

```haskell
-- untyped API
df |> scatter "x" "y" |> showVegaLite

-- typed API
df |> scatter @"x" @"y" |> showVegaLite

-- in highest level
showScatter xCol yCol = showVegaLite . scatter xCol yCol
```

### Mid level API

* optics for modifying `VegaLite` encodings, e.g. `colorBy`, `facetBy`, `alphaBy`, `sizeBy` (do those all work with the
  data nesting in Vega? not sure)
* optics for modifying chart config (`title`, `xLabel`, `yLabel`, `legendPlacement`, etc.)
* use optics to get to `VegaLite -> VegaLite` functions
* `VegaLite` only shows up in the `VegaLite -> VegaLite` tail of the signature
* contracts: (up) signatures all end `VegaLite -> VegaLite` so they can be pipelined,
  (down) mid level API assembles values to set with low level API

example:

```haskell
df -- dataframe
  |> scatter @"x" @"y"
  |> setTitle "good plot" -- VegaLite 
```

### Low level API

* `dataframe` domain -> encodings, etc.
* why use low level?
  * maybe you're doing `hvega` with _not_ `dataframe` and want values to combine
  * maybe you're doing `hvega` with things `dataframe` doesn't directly integrate with
  * maybe you want stuff that `dataframe-hvega` didn't provide like geojson plotting

## Other posts in this series

This post is part of a series on data visualization in Haskell. You can find other posts below:

* [Hello, plots]
* [Plot configuration]
* [Fancy plotting in `hvega`]
* Proposing a plotting API on top of `dataframe`

[`displayHtml`]: https://github.com/DataHaskell/sabela/blob/8d8936077d6b4d21a3402be20e6dc4601197f06c/display/Sabela/Display.hs#L55-L56
[`hvega`]: https://hackage.haskell.org/package/hvega
[`chart-svg`]: https://hackage.haskell.org/package/chart-svg
[`granite`]: https://hackage.haskell.org/package/granite
[make hard things possible]: https://matplotlib.org/
[Plot configuration]: ./2026-03-21-Haskell-data-visualization-part-2.html
[Hello, plots]: ./2026-03-05-Haskell-data-visualization.html
[Fancy plotting in `hvega`]: ./2026-04-09-Bonjour-Haskell-data-visualizations.html
[Sabela]: https://github.com/DataHaskell/sabela
[slider]: ./2026-04-09-Bonjour-Haskell-data-visualizations.html#interactivity-and-annotation
[Vega specification]: https://vega.github.io/
[roadmap]: https://www.datahaskell.org/docs/community/roadmap.html
[^1]: Not a real certification
[^2]: No shade here -- built-in plotting in `dataframe` chose simple plotting a while ago. That makes sense for the
goal of having the ability to plot anything vs. not having the ability to plot anything. Given the breadth of the `dataframe` library, it's unreasonable to expect every piece of the API to be alive in its final form already.
[^3]: I never encounter iron triangles in the wild! This is great for me.
