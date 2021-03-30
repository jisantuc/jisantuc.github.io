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
[`hypothesis`] by [Matt Bachmann]. However, I've often
had trouble explaining the mechanics to people without hand-waving. To that end, 
I created [`tiny-test`], a testing framework
you _absolutely should not use_ in any production setting, but which is fine for playing
around.

What's the goal with `tiny-test`? Write a "test framework" that illustrates the
[stylized facts] of property testing.

## What are the stylized facts of property testing frameworks?

Property testing goes by a lot of names -- [fuzz testing], [generative testing],
[monkey testing]... there are a lot. These names collectively emphasize that the test cases
will be driven by _randomness_, so that will be the first stylized fact:
test cases in `tiny-test` should be driven by randomness.

Second, since `tiny-test` is a testing framework, it should have opinions about what
a test is. The second stylized fact is that tests are things that produce test results.
What are test results? Test results are checked expectations -- e.g., "I think this
should run without crashing," or "I think the result should be less than six."

With these two stylized facts, there's a pretty straightforward main requirement:
`tiny-test` needs to provide APIs for transforming randomness into test results. Cool! Let's
work on that.

## Useful randomness

The hello world of property testing is a custom `add` function that acts like the built-in
`+` operator.

In Haskell, that would look like this:

```
add :: Int -> Int -> Int
add = (+)
```

However, that's not how `add` is [implemented in `tiny-test`]. How do
we explore the possible integer inputs such that we can find the values where the `add` implementation
is wrong?

-- in python

-- with _purity_ -- RandomIO

-- is it special to _ints_? no! let's make a typeclass out of this -- typeclass for "I can summon
-- values of this type if I declare IO"

## Functions over Arbitrary types

Most basic

-- `Arbitrary a => (a -> Result) -> IO Result`

Functions of _two_ parameters?

-- `Arbitrary a => Arbitrary b => (a -> b -> Result) -> IO Result`

How high can we go? Obviously to infinity. But `tiny-test` stops at 3 because that's not the point.

[`hypothesis`]: https://hypothesis.readthedocs.io/en/latest/
[Matt Bachmann]: https://www.youtube.com/watch?v=jvwfDdgg93E
[`tiny-test`]: https://github.com/jisantuc/tiny-test/
[stylized facts]: https://en.wikipedia.org/wiki/Stylized_fact
[fuzz testing]: https://en.wikipedia.org/wiki/Fuzzing
[generative testing]: https://medium.com/geckoboard-under-the-hood/how-generative-testing-changed-the-way-we-qa-geckoboard-b4a48a193449
[monkey testing]: https://en.wikipedia.org/wiki/Monkey_testing
[typeclasses]: http://learnyouahaskell.com/types-and-typeclasses
[implemented in `tiny-test`]: https://github.com/jisantuc/tiny-test/blob/master/src/Lib.hs