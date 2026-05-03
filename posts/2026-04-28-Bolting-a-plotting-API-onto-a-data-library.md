---
title: Bolting a plotting API onto a data library
date: 2026-04-28
description: Bolting a plotting API onto a data library
---

* Now that I'm the foremost amateur[^1] in Haskell plotting libraries, tktktk

Adding more plotting to something that currently kind of has plotting[^2] is tough. I don't want to throw away existing
work, I don't want to reinvent the wheel, and I don't want to give up power. Those three things are collectively
at odds.[^3]

1. Not throwing away existing work + not giving up power means building a lot more capability onto the current
  plotting API, which probably means reinventing the wheel.
2. Not reinventing the wheel + not giving up power means swapping in a more powerful library, which means throwing away existing work.
3. Not throwing away existing work + not reinventing the wheel means shoe-horning some existing more powerful lbirary
  into the current plotting API, which probably means giving up power in the assisting library.

Accepting that there's no perfect option, the task is picking good tradeoffs. I think not keeping existing work is
the least important, so I pick power + not reinventing the wheel. Based on the tour of plotting libraries, that means
`hvega` with a friendlier API.
* Plotting data and querying data are related but not identical concerns
* Some API overlap -- don't want to ask for columns that don't make sense when just looking at a list of values,
  don't want to ask for columns that don't make sense to plot stuff
    * or in `hvega` case, don't want to try to do transformations of invalid types (`x * 3` on a string column)
      or with data that don't exist
    * as soon as anything's wrong in `hvega` you're debugging JS library errors instead of Haskell errors, and if that
      sounded fun to you you'd have given up on this series of posts a while ago

[^1]: Citation needed
[^2]: No shade here -- built-in plotting in `dataframe` chose simple plotting a while ago. That makes sense for the
goal of having the ability to plot anything vs. not having the ability to plot anything. Given the breadth of the `dataframe` library, it's unreasonable to expect every piece of the API to be alive in its final form already.
[^3]: I never encounter iron triangles in the wild! This is great for me.
