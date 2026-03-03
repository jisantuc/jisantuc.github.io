---
title: "Haskell data visualization part 1: Hello, plots"
date: 2026-03-03
description: Haskell data visualization
---

I learned Python a decade ago because I visited my partner's lab when she was working on [SPIDER] and saw plots in
[`kst`] that told her whether one of their machines would explode.[^1] I spent all of my
formative just-enough-Python-to-be-dangerous time at my first job with [`pandas`],
[`matplotlib`], and an assortment of other Python plotting libraries. I hand-rolled
tables with shaded cells and abused the annotations and shapes APIs while trying to
convince any coworker of mine who hadn't yet lost patience for this discussion that
we should get rid of all of our Stata and SAS and R and Excel[^2] and whatever else people were
using and do everything in Python. When I want to plot something, I've often finished
typing `fix, ax = plt.subplots()` before I've even figured out what kind of plot I want.

I'm explaining all this because a week ago I got around to posting [Don't fire Styer]. One of the challenges I set
myself while working on that was that I wasn't allowed to use Python to produce the plots -- I had to produce them in
Haskell, the same language I used for the [simulation code](https://github.com/jisantuc/goofing-off/blob/main/src/mosconi-sim/Sim.hs).

I wound up using the built-in plotting utilities of the new-ish Haskell [`dataframe`] library, which went fine, but a
lot of time has passed since the last time I went looking for Haskell visualization libraries, and I wanted to know
what else is out there.

This post is the first of, I don't know, probably three or four posts on data visualization
in Haskell in early 2026.

## Libraries

I found a few libraries, with some help from the [DataHaskell Discord].

* [`dataframe`]
* [`granite`]
* [`hvega`]
* [`Chart`]
* [`chart-svg`]

I wanted to compare them on a few points, starting with what it looks like to take
a dataframe and produce a scatter plot.

Each library has its own idea of what kind of data is plottable. I'm starting with
a dataframe in every case to hand-wave away some of that difference and to focus instead
on getting from some kind of data to some kind of graphic.

I'm not going to pick winners and losers, just comment on what's easy and difficult
at each stage along the way, especially at this early stage -- "it's easy to do something easy" and "it's easy to do
something hard" are basically uncorrelated.

Anyway, without any further ado, here are five scatter plots, all produced in Haskell, all unmodified.

### `dataframe`
### `granite`
### `hvega`
### `chart`
### `chart-svg`

[Don't fire Styer]: ./2026-02-11-Don't-fire-Styer.md
[SPIDER]: https://spider.princeton.edu/
[`kst`]: https://kst-plot.kde.org/
[`pandas`]: https://pandas.pydata.org/
[`matplotlib`]: https://matplotlib.org/
[`dataframe`]: https://hackage.haskell.org/package/dataframe
[`granite`]: https://hackage.haskell.org/package/granite
[`hvega`]: https://hackage.haskell.org/package/hvega
[`Chart`]: https://hackage.haskell.org/package/Chart
[`chart-svg`]: https://hackage.haskell.org/package/chart-svg
[DataHaskell Discord]: https://discord.gg/8u8SCWfrNC
[^1]: I took a Java course in high school that was probably important for being able to
learn Python, but this was when I became Serious™️.
[^2]: I was also on team "we should get really good at Excel," though I don't think I had
the imagination for what "really good" meant -- mainly I just wanted everyone to understand
`vlookup` and conditionals.
