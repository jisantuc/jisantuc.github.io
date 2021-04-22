---
title: Variable roles, types, and code skimming
date: 2021-04-22T15:00:00+00:00
description: I've been reading *The Programmer's Brain*. One of the techniques for reading unfamiliar code is using the *roles of variables* framework. This post walks through combining information from roles and types to skim unfamiliar code written with folds faster.
---

I've been reading [*The Programmer's Brain*], since I want to be better at explaining / teaching
unfamiliar programming concepts to people who might want me to go away so they can get some
real work done. As someone who learned to program mostly on my own (not to program _well_, but to
program), I was curious whether I'd recognize any of the cognitive patterns, since it's still
pretty easy for me to remember when it was hard to open files in Python. I haven't been disappointed
and the exercises have already re-shaped how I think about digging into unfamiliar code. I just
finished chapter 5 a few days ago.

One of many concepts that was new to me was [_roles of variables_]. "Roles" describe what variables are _for_(unlike simple types, which describe what variables are).
Here are some of the roles listed in *The Programmer's Brain*:

- most-recent holder: holds the most recent value you've seen, e.g., while stepping through a list by index, you would put the most recent value you've seen in one of these.
- most-wanted holder: holds the "best" value, whatever "best" means for you
- gatherer: "a variable that collects data and aggregates it into one value"
- container: "any data structure that holds multiple elements"

I've picked these four out in particular because of an example Felienne Hermans gives at the end of the section introducing roles:

> a program with a stepper and a most-wanted holder value is a search program

### Mapping roles to FP concepts

Felienne Hermans mentions annotating roles of variables on printed code using some specific symbols, translated loosely into emojis here:

- most-recent holder: 📆
- most-wanted holder: 💎
- gatherer: 🧺
- container: 🔳

This makes sense -- if you can pictorially represent the type of program you have, you can
dramatically reduce the amount of code someone has to read to get the big picture. One of my favorite PureScript
projects, [`prelewd`], plays a similar game, asking:

> Is `f <$> a <*> b` scarier than `f 🚂(a)🚋(b)🚋(c)`? If so, why?

Let's write some tiny search functions in PureScript with these emoji annotations!

### A search program in PureScript

PureScript is a statically typed pure functional programming language with a Haskell-y syntax.

For our search program, we're going to find the tallest building from a list of buildings. We're also not going
to use [`maximumBy`], because that's boring.

Here's out list of buildings:

```purescript
buildings = [
  { "name": "Sears Tower", "height": 1000 }
  , { "name": "Burj Khalifa", "height": 1200 }
  , { "name": "Small building", "height": 20 }
  , { "name": "Medium building", "height": 50 }
  , { "name": "Non-notably tall buidling", "height": 200 }
  , { "name": "Unreasonably tall building", "height": 10000 }
]
```

To find the tallest building, we'll use the [`foldl`] function. The `foldl` function's type signature looks like this:

```purescript
forall f a b. Foldable f => (b -> a -> b) -> b -> f a -> b
```

With roles of variables, we could annotate it as:

```purescript
--                  🔳   =>      🧺      ->  💎/📆 -> 🔳 -> 💎
forall f a b. Foldable f => (b -> a -> b) ->   b    -> f a -> b
```

Or: it's a function from some stuff in a container, with a gathering function, that eventually finds a most wanted value.

Let's actually find the tallest building now:

```purescript
type Building = { name: String, height: Int }

compareBuilding :: Building -> Building -> Building
compareBuilding b1 b2 = if (b1.height > b2.height) then b1 else b2

--                    💎
tallestBuilding :: Building
tallestBuilding = foldl
  compareBuilding -- 🧺
  { name: "impossible building", height: 0 } -- 💎/📆
  buildings -- 🔳
```

The only weird part here is the 💎/📆 value -- since we're traversing the whole list, and comparing with the best value we know
at each point, our initial value starts as the "most recent best" value. Starting with a bogus value for our most recent best building guarantees that we can
find a tallest building even in empty lists.

#### Type hints for variables roles

The function above finds exactly one value, but we can vary the type of our most recent best container to get different sorts of functions.
Maybe  instead of the tallest building, we want to find the first building with a specific name. We can also write this search with `foldl`, but
the gathering function and initial most recent best value will be different:

```purescript
isItMedium :: Maybe Building -> Building -> Maybe Building
isItMedium (Just building) _ = Just building
isItMedium _ building = if (building.name == "Medium building") then Just building else Nothing

--                 🔳      💎
mediumBuilding :: Maybe Building
mediumBuilding = foldl
  isItMedium -- 🧺
  Nothing -- 🔳💎
  buildings -- 🔳
```

In this case, we're searching for something in the list that might not be there. As a result, our most wanted value
starts out with an empty container, in this case, an empty [`Maybe`] value. `Maybes` can hold _zero or one_ values.
Once `isItMedium` finds a building that matches, it puts that building into the `Maybe` container with the `Just`
constructor. Functions such as this one, where we model the possibility of not finding what we're looking for are also searches.

Finally, let's assume the buildings are sorted by year of construction. We could search for buildings that were the tallest
building in the world when they were constructed:

```purescript
tallestBuildings :: Array Building -> Building -> Array Building
tallestBuildings buildings building =
  case last buildings of
    Just b -> if (b.height < building.height) then buildings `append` [building] else buildings
    Nothing -> [building]

--                 🔳     💎
recordSetters :: Array Building
recordSetters = foldl
  tallestBuildings -- 🧺
  [] -- 🔳💎
  buildings -- 🔳
```

This example is similar to the preveious one. However, the container for our most wanted value is an array, which holds
zero or any number of values, instead of zero or one like `Maybe`. Functions such as this one, where the best values are gathered into an unbounded container, are filters.

Note that in the last example, there's no rule that says that each building can't expand the array -- it's only because we know we're looking for a kind of "best" value that this is a filter.

### The end

These three examples showed how the roles of variables framework, in conjunction with information represented in types,
can give you a lot of clues about how a function or program works without having to dig into the implementation. While some
of the syntax might be strange if you're not familiar with PureScript, I hope the annotated and emojified variable roles help make clear what's going on.

Also, if you're also interested in turning the roles of variables annotations into a VSCode extension, please tweet at me
`@james_santucci` -- I don't know anything about VSCode extensions at this point, but it sounds fun.

[*The Programmer's Brain*]: https://www.manning.com/books/the-programmers-brain
[_roles of variables_]: https://www.tandfonline.com/doi/full/10.1080/08993400500056563
[`prelewd`]: https://pursuit.purescript.org/packages/purescript-prelewd/0.1.0
[`maximumBy`]: https://pursuit.purescript.org/packages/purescript-foldable-traversable/5.0.1/docs/Data.Semigroup.Foldable#v:maximumBy
[`Maybe`]: https://pursuit.purescript.org/packages/purescript-maybe/5.0.0/docs/Data.Maybe#t:Maybe
[`foldl`]: https://pursuit.purescript.org/packages/purescript-foldable-traversable/5.0.1/docs/Data.Foldable#v:foldl