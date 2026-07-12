---
title: Still more plotting with dataframe
date: 2026-07-11
description: |
  dataframe's current plotting interface is simple but low powered. Off-the-shelf options can add a lot of power at the
  cost of simplicity. Maybe there's a way to have both.
---

A few months ago I worked on a few posts about the state of plotting in Haskell.
Those posts took me through [`hvega`], [`granite`], [`chart-svg`], and [`Chart`].[^1]
Since then, `@frenzieddoll` started work on a new plotting library that speaks (un-typed) `dataframe` called
[`hgg`].
As the foremost scrub[^2] in Haskell plotting libraries, I tried it out while I was also trying to write a nice interop
layer between `hvega` and `dataframe`. The latter didn't go great, but I feel optimistic that `hgg` could serve as an
ergonomic plotting backend that provides a lot of power while also not baffling data scientists who find themselves
writing Haskell who are used to `ggplot`. That's neat!

This post will discuss what was hard about the `hvega` work and where I think building on `hgg` can go better.

## Setting out in `hvega`

I wanted to learn from what I liked about using the plotting libraries I tried out in building a nicer API to use
`hvega` with `dataframe`. I thought the way that'd work would be:

* a low-level API that does nothing but construct `hvega` data from dataframes,
* a mid-level API for combining those into more meaningful components, and
* a high-level API that provides complete plots, returning some datatype that conforms to what the mid-level API
  expects for e.g. changing the title, colors, legend placements, or whatever afterward.

My first effort focused on building up a scatter plot that way.

<div class="flex-container">
![](./gen/dataframe-plot-scatter.html)
</div>

That gave me a bunch of the typed machinery for converting between `TypedDataFrame` values and `hvega` data. Low-level
functions produced `hvega` types, the mid-level API produced `VegaLite` values, and the high level API produced
displayable plots. You can browse the repo at that [checkpoint] to see how the pieces fit together.

The next step was to be able to set the plot title, and it got awkward. `VegaLite` is a newtype over an `aeson`
`Value`, meaning the only structure preserved in the type system is that it's JSON of some sort. I thought it'd be fine
just to be _really careful_ in modifying that JSON, but `hvega` pretty sensibly makes that difficult -- there's a
[`fromVL`] function to get the `Value` out of the `VegaLite` newtype, with a pretty big caveat:

> Note that there is no validation done to ensure that the output matches the Vega Lite schema. That is, it is possible
> to create an invalid visualization with this module (e.g. missing a data source or referring to an undefined field).

There's no handy constructor to go the other way.[^3] Given that a `VegaLite` value isn't known valid, I think that's
reasonable -- what kinds of guarantees is it possible to make for a workflow that starts with an invalid value,
modifies that value without constraints, then puts the modified value back into the wrapper? Maybe none, certainly not
strong ones, and facilitating that workflow seems like a way to get a bunch of bug reports that are hard to pin on
the library vs. the open-ended modification (even though, and I can't emphasize this enough, I was going to be
_really, really careful_).

That made it pretty awkward to implement `setTitle`. In the mid-level, I wanted its signature to be
`Text -> VegaLite -> VegaLite`. The `VegaLite -> VegaLite` trailer was supposed to support easy plot construction and
modification along the lines of `somePlot & setTitle "foo" & setTheme something & ...`. That was never really going to
work at the `VegaLite` level though.

There's a level below that -- `[PropertySpec]`, which, if you expand the aliases and newtypes, is a
`[(VLProperty, Value)]`, where `VLProperty` is a union type holding all of the unique properties a Vega specification
might contain. That's... fine. I mean, it was workable. It's not hard to build another `(VLProperty, Value)`, since
that's what a lot of the functions in `hvega` return (e.g. [`title`]). But if I'm just building a list of tuples where
all I know about the second element is that it's a JSON value, then I'm pretty much building a `Map String Value`,
which didn't feel like it was going to help the type system keep me from doing something dumb.

On top of the JSON-flavored data modeling, I had trouble getting [`vl-convert`](https://github.com/vega/vl-convert)
into a `nix` flake[^4] and I didn't have another plan for how I was going to create PNGs. The yak shave was starting
to feel yak shave-y enough that I was losing confidence in the low/mid/high APIs I'd imagined on top of `hvega`.

## Enter `hgg`

tktktk

# cut below

### Goals

1. Learn from other plotting libraries.
2. Maximize power (aka [make hard things possible])

`dataframe-plot` -- distinct plotting package that just makes plots (like `dataframe-fastcsv`, `dataframe-hasktorch`,
etc.)

### Learning from other plotting libraries

* good stuff to borrow:
  * attitude of "easy thing easy / [make hard things possible]" -- simple plots should be simple to express (`matplotlib`,
    `dataframe`, [`granite`], `pandas`, ...), you shouldn't have to care about power unless you need it
  * model plot data ([`chart-svg`], [`hvega`] / `Vega` even if it's basically JSON) instead of text (`granite`)
* what I thought I'd borrow:
  * optics for setting attributes on whatever the data representation of the plot is (`chart-svg`) because the data
    representation is probably huge / complex -- not actually though, structure in `hvega` was rough

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
[`matplotlib`]: https://hackage.haskell.org/package/matplotlib
[`hgg`]: https://github.com/frenzieddoll/hgg/tree/provisional/docs-wip-20260703
[make hard things possible]: https://matplotlib.org/
[Plot configuration]: ./2026-03-21-Haskell-data-visualization-part-2.html
[Hello, plots]: ./2026-03-05-Haskell-data-visualization.html
[Fancy plotting in `hvega`]: ./2026-04-09-Bonjour-Haskell-data-visualizations.html
[Sabela]: https://github.com/DataHaskell/sabela
[slider]: ./2026-04-09-Bonjour-Haskell-data-visualizations.html#interactivity-and-annotation
[Vega specification]: https://vega.github.io/
[roadmap]: https://www.datahaskell.org/docs/community/roadmap.html
[checkpoint]: https://github.com/jisantuc/dataframe-plot/tree/ba2f6536aa691d6d3d736014ed6b8c84844ca7e8/src/DataFrame/Plot/Typed
[`fromVL`]: https://hackage.haskell.org/package/hvega-0.12.0.7/docs/Graphics-Vega-VegaLite.html#v:fromVL
[`title`]: https://hackage.haskell.org/package/hvega-0.12.0.7/docs/Graphics-Vega-VegaLite.html#v:title
[^1]: and nearly a Haskell... frontend? client? of [`matplotlib`], but it's difficult to figure out even what to call it.
[^2]: Not a real certification
[^3]: Also, [`coerce`](https://hackage-content.haskell.org/package/base-4.22.0.0/docs/Data-Coerce.html#v:coerce) isn't
  an option because the `VL` constructor isn't exported.
[^4]: Skill issue for sure, but anyway.
