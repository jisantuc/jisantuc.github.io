---
title: Bolting a plotting API onto a data library
date: 2026-04-28
description: Bolting a plotting API onto a data library
---

* Now that I'm the foremost amateur[^1] in Haskell plotting libraries, tktktk

* Plotting data and querying data are related but not identical concerns
* Some API overlap -- don't want to ask for columns that don't make sense when just looking at a list of values,
  don't want to ask for columns that don't make sense to plot stuff
    * or in `hvega` case, don't want to try to do transformations of invalid types (`x * 3` on a string column)
      or with data that don't exist
    * as soon as anything's wrong in `hvega` you're debugging JS library errors instead of Haskell errors, and if that
      sounded fun to you you'd have given up on this series of posts a while ago

[^1]: Citation needed
