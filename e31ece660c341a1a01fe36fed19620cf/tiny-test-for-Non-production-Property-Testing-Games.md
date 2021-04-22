---
title: tiny-test, for non-production property testing games
date: 2021-03-29T21:52:01+00:00
description: I wrote a small property testing framework called tiny-test to explore / verify my understanding of / teach property testing.
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

```haskell
add :: Int -> Int -> Int
add = (+)
```

However, that's not how `add` is [implemented in `tiny-test`]. How do
we explore the possible integer inputs such that we can find the values where the `add` implementation
is wrong?

### In Python

Let's suppose we're working in a language where we can freely mix effectful and non-effectful code,
like python. In that case, our test might look something like:

```python
import random

def test_add():
    a = random.randint(0, 1000)
    b = random.randint(0, 1000)
    assert add(a, b) == a + b
```

Cool, now we can run _one test_ with two random inputs. What if we want to run _lots_ of tests?
Let's try that instead:

```python
def test_add_a_lot():
    some_as = [random.randint(0, 1000) for _ in range(1000)]
    some_bs = [random.randint(0, 1000) for _ in range(1000)]
    for a, b in zip(some_as, some_bs):
        assert add(a, b) == a + b
```

Nice, now we've run _lots of tests_. This is great because we didn't have to do very much work and
we got to make sure that our `add` function works for lots of different int combinations, instead
of the six combinations we might come up with in a unit test.

What if we want to test some property about strings?

```python
def some_rand_string_fn():
    # This returns a random string, I swear.
    ...

def test_append():
    some_strings = [some_rand_string_fn() for _ in range(1000)]
    for st in some_strings:
        assert st in (st + st)
```

This pattern looks familiar -- generate a bunch of data of the type that we need,
call some function on the generated data, and assert some expectation about the result.

## Functions over Arbitrary types

We saw in the Python example that it's useful to be able to generate data of different types.
That example included only `ints` and `strings`, but we could conceivably want to test functions
with _any_ input type. In that case, we'd want some way to express the requirements that:

* we can generate data of the type needed
* We have some function on the generated data that gives us a test result

In languages with typeclasses, we can represent some capability that we want to express about
a type with a _typeclass_. In property testing libraries, this capability is frequently
([scalacheck], [quickcheck], [hedgehog]) wrapped up in a `Gen` type. A value of type `Gen a`
or `Gen[A]` says "I can generate values of type `a`/`A`." Often, access to that generator
is hidden in an `Arbitrary` typeclass. In `tiny-test`, we'll make things a little simpler,
and just say that we have a `sample` method that effectfully procures us a list of values
of the type we want:

```haskell
class Arbitrary a where
  sample :: IO [a]
```

The second requirement is simpler -- we said tests are things that produce test results, so let's
summon a type called `Result` (the details aren't important here) and say that we need a function
from something we can summon to a `Result`. In Haskell, that looks like:

```haskell
prop :: Arbitrary a => (a -> Result) -> IO Result
```

Now we have a function that, provided you know how to summon values of type `a`, we can use to
run lots of tests. Let's assume that `Results` can be combined -- in that case, we can smash
all of our results in the implementation with:

```haskell
prop :: Arbitrary a => (a -> Result) -> IO Result
prop f = do
  someAs <- sample
  pure $ mconcat (f <$> someAs)
```

`do` takes us into something that looks a little imperative -- you can read each line as "this, then
that, then the other thing," similar to languages like Python and JavaScript.
The first line says "get me the list of `a`-s that you promised," and the second says "test them all,
smash the results together, and give me the result." The `pure $` at the beginning is just there to
make the types cooperate.

Similarly, for functions of two parameters that we know how to summon from nothing, we can implement
`prop2`:

```haskell
prop2 :: (Arbitrary a, Arbitrary b) => (a -> b -> Result) -> IO Result
prop2 f =
  do
    as <- sample
    bs <- sample
    pure . fold $ zipWith f as bs
```

How high can we go with this? Obviously all the way to infinity.

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
[hedgehog]: https://hackage.haskell.org/package/hedgehog-1.0.5/docs/Hedgehog-Internal-Gen.html
[quickcheck]: https://hackage.haskell.org/package/QuickCheck-2.14.2/docs/Test-QuickCheck-Gen.html
[scalacheck]: https://github.com/typelevel/scalacheck/blob/main/src/main/scala/org/scalacheck/Gen.scala