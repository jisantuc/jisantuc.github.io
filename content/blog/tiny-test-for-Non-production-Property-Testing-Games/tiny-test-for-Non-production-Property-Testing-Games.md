---
title: `tiny-test` , for Non-production Property Testing Games
date: 2021-03-29T21:52:01+00:00
description: tiny-test, for Non-production Property Testing Games
---

Property testing is a testing strategy where the programmer defers the
responsibility of writing example cases to the computer. That's most of the
game, but isn't super useful for practice with property testing outside
of a production setting.

Property
testing is an area of software development dear to my heart -- one of the first
software talks I ever saw where I thought I'd like to be a developer was on
[ `hypothesis` ](https://hypothesis.readthedocs.io/en/latest/)
by [Matt Bachmann](https://www.youtube.com/watch?v=jvwfDdgg93E). However, I've often
had trouble explaining the mechanics to people without hand-waving. To that end, 
I created [ `tiny-test` ](https://github.com/jisantuc/tiny-test/), a testing framework
you _absolutely should not use_ in any production setting, but which is fine for playing
around.
