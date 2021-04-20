---
title: Variable roles and functional programming concepts
date: 2021-04-16T16:51:08+00:00
description: Variable roles and functional programming concepts
---

Reading [*The Programmer's Brain*], it's great, whatever

Variable roles and FP concepts. "Roles" correspond to things that variables can
represent in your code. Here are the roles listed in *The Programmer's Brain*:

- list
- of
- roles
- here

### Mapping roles to FP concepts

Something I've believed for a while is that language features that are common
in FP languages reduce complexity introduced by the language.

- fixed value -- no ambiguity, everything is immutable so every value is fixed
- stepper -- without mutability these also don't exist. two roles down! way closer to
  irreducible complexity already. if you want to step, recurse
- walker -- just pattern matching and recursion
- flag -- with ADTs, not always necessary. model the data we care about, not information
  about the data we care about. e.g., instead of `if isEmpty(someList): ...`, model non-empty
  lists
- most-recent holder / most-wanted holder -- two versions of "the best one." more on this soon
  with `Foldable` and role combinations
- gatherer -- can be smashed into. represented as things with a `semigroup`, and if I can get a
  sensible default, `monoid`. 
- container -- *can hold more stuff*. incidentally, `gatherer` and `container` are pretty much the
  same -- very little difference between `+` and `append`
- follower -- state
- organizers / temporary -- purely local scope

### Combinations of roles

"A program with a stepper and a most-wanted holder value is a search program."

Let's talk about `Foldable` -- we have an array of values, and we want to find the value that's closest
to a power of two... let's say logarithmically. That is, we have a function for some numeric type `a`,
and it picks whichever of two values which, log base 2, is closest to a whole number:

```purescript
bestNum :: Int -> Int -> Int
bestNum = ...
```

Here we have a search program, which we'll implement using a `fold`, specifically a left `fold` but it doesn't
really matter in this case.

Here's the signature for `fold`:

```purescript
foldl :: forall f a b. Foldable f => (b -> a -> b) -> b -> f a -> b
```

And here's what this signature is telling us:

- `Foldable f =>`: whatever context we're working in, it can be folded. We'll fix this to an array.
- `(b -> a -> b)`: give me a function that takes a `b` and an `a` and gives me a `b`. `a` and `b` can be the same or different.
- `b`: give me an initial value of type `b`
- `f a`: give me some values of type `a` in your foldable context -- again, for us, arrays
- `-> b`: I'll give you a `b` back

What are the roles here:

- `(b -> a -> b)` -- this does our gathering or searching or whatever. It depends on the shape of the function. For `bestNum`, searching for
  the best one.
- `b` -- this is the thing that values get gathered into. We start with an initial value because we can't promise that the function will ever
  be called, for example, if we have an empty array. This value is, for each function call, the "most recent best"
- `f a`: saying that `f` is `Foldable` effectively means `f` can be stepped over, so this is our stepper
- `-> b`: this is just a promise about what comes out


What other kinds of programs? Depend on type of `b`.

- With `b` as some kind of container type (array, `Maybe`, etc.), we have searches, but also different kinds of searches. Arrays indicate we can find all the values, while `Maybe` says we're going to find 0 or 1 values
- With `b` as some kind of `gatherer` type, we can get a generic notion of sums, e.g., whatever `b` is, we can keep smashing more `a` values into it somehow, e.g., in a simple wordcount example