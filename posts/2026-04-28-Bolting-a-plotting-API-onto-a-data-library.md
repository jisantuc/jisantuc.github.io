---
title: Bolting a plotting API onto a data library
date: 2026-04-28
description: Bolting a plotting API onto a data library
---

* Now that I'm the foremost amateur[^1] in Haskell plotting libraries, tktktk

Adding more plotting to something that currently kind of has plotting[^2] is tough. 

1. Learn from other plotting libraries.
2. Maximize power (aka [make hard things possible])
3. Don't make someone learn the [Vega specification] to make a scatter plot (aka make easy things easy).

The second and third goals are also at odds. In the previous few posts, I learned that [`hvega`] was the only one of
the available plotting libraries that supported all of the fancy plot types and interactivity. Interactivity
is also on the roadmap and is really valuable in notebook environments. My `hvega` $\pi$ estimator's [slider] worked
great in a [Sabela] notebook, but learning `hvega` and Vega at the same time was _hard_.

Since I think building on `hvega` is the way forward, the first and third goals also end up at odds. Being careful
about the API can mitigate some of the difficulty of building on Vega, and many people probably know Vega and VegaLite
better than I do anyway.

## A layered API

There's a lot to learn from other plotting libraries that `dataframe`'s plotting should take advantage of.
The choices other libraries have made around simple specifications for simple plots, optics for modifying parts of
complex plot objects, and escape hatches to drop to the lowest plotting level can all be integrated into a layered
API built off of `hvega` plotting.

### Highest level API

`dataframe`'s simplicity is a great starting point. There's not a ton of power in how you can plot with `dataframe`,
but sometimes you just want a picture with a scatter plot in it. Methods for "just give me a picture" should live in 
`DataFrame.Display.IO` and should include e.g.

```haskell
showScatter :: Text -> Text -> IO ()

htmlScatter :: Text -> Text -> HtmlText

showVegaLite :: VegaLite -> IO ()

htmlVegaLite :: VegaLite -> IO ()
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
* contracts: (up) display API doesn't have to do any chart config, just calls functions from high level API,
  (down) high level API produces values that can be conveniently configured with optics

example:

```haskell
df |> scatter @"x" @"y"
```

### Mid level API

* optics for modifying `VegaLite` encodings, e.g. `colorBy`, `facetBy`, `alphaBy`, `sizeBy` (do those all work?)
* optics for modifying chart config (`title`, `xLabel`, `yLabel`, `legendPlacement`, etc.)
* not really optics I guess, just functions
* `VegaLite` only shows up in the `VegaLite -> VegaLite` tail of the signature
* contracts: (up) signatures all end `VegaLite -> VegaLite` so they can be pipelined,
  (down) mid level API assembles values to set with low level API

example:

```haskell
df -- dataframe
  |> scatter @"x" @"y" -- VegaLite
  |> setTitle "good plot" -- VegaLite 
```

### Low level API

* `dataframe` domain -> encodings, etc.
* why use low level? maybe you're doing `hvega` with _not_ `dataframe` and want values to combine, maybe you're doing
  `hvega` with things `dataframe` doesn't directly integrate with

### Lowest level API

* just use `hvega`

-----

Regrets:

The current plotting interface in `DataFrame` uses [`granite`]. `granite` is
[neat for its niche], but to support the rest of the visualization DataHaskell [roadmap], 
it would need a lot of capabilities that already lives elsewhere in the Haskell visualization ecosystem in libraries
like [`chart-svg`] and [`hvega`].


`chart-svg` and `Chart` both 

Accepting that there's no perfect option, the task is picking good tradeoffs. Based on the tour of plotting libraries, that means
`hvega` with a friendlier API.
* Plotting data and querying data are related but not identical concerns
* Some API overlap -- don't want to ask for columns that don't make sense when just looking at a list of values,
  don't want to ask for columns that don't make sense to plot stuff
    * or in `hvega` case, don't want to try to do transformations of invalid types (`x * 3` on a string column)
      or with data that don't exist
    * as soon as anything's wrong in `hvega` you're debugging JS library errors instead of Haskell errors, and if that
      sounded fun to you you'd have given up on this series of posts a while ago

* levels:
  * low: construct hvega primitives almost directly
  * mid: modify hvega-ish plots with convenient lenses (e.g. `colorBy :: VegaLite -> VegaLite`)
  * high: named plot types, like `scatter`
  * display
* why's display separate
  * displaying a plot is a different concern from constructing a plot -- want something like MPL's interactivity from
    `plt.show()`, also want display to be easy in notebook settings, these are probably different things
  * notion of backends? or just different functions?

## Other posts in this series

This post is part of a series on data visualization in Haskell. You can find other posts below:

* [Hello, plots]
* [Plot configuration]
* [Fancy plotting in `hvega`]
* Proposing a plotting API on top of `dataframe`

[`displayHtml`]: https://github.com/DataHaskell/sabela/blob/8d8936077d6b4d21a3402be20e6dc4601197f06c/display/Sabela/Display.hs#L55-L56
[`granite`]: https://hackage.haskell.org/package/granite
[`hvega`]: https://hackage.haskell.org/package/hvega
[`chart-svg`]: https://hackage.haskell.org/package/chart-svg
[make hard things possible]: https://matplotlib.org/
[neat for its niche]: ./2026-03-05-Haskell-data-visualization#granite
[Plot configuration]: ./2026-03-21-Haskell-data-visualization-part-2.html
[Hello, plots]: ./2026-03-05-Haskell-data-visualization.html
[Fancy plotting in `hvega`]: ./2026-04-09-Bonjour-Haskell-data-visualizations.html
[Sabela]: https://github.com/DataHaskell/sabela
[slider]: ./2026-04-09-Bonjour-Haskell-data-visualizations.html#interactivity-and-annotation
[Vega specification]: https://vega.github.io/
[roadmap]: https://www.datahaskell.org/docs/community/roadmap.html
[^1]: Citation needed
[^2]: No shade here -- built-in plotting in `dataframe` chose simple plotting a while ago. That makes sense for the
goal of having the ability to plot anything vs. not having the ability to plot anything. Given the breadth of the `dataframe` library, it's unreasonable to expect every piece of the API to be alive in its final form already.
[^3]: I never encounter iron triangles in the wild! This is great for me.
