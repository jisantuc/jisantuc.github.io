---
title: cliffs, a small CLI for remembering what your scripts to rule them all are for
date: 2021-11-16T04:11:52+00:00
description: cliffs, a small CLI for remembering what your scripts to rule them all are for
---

Project I've worked on at Azavea really frequently use the [Scripts to Rule Them
All] (STRTA) pattern. Using STRTA is great once everyone is really used to what
all of the scripts are for -- provided no one has done anything weird, it's much
easier to get spun up in a new project with `scripts/setup` and `scripts/update`
than with a bunch of bespoke, one-off commands relying on whatever each project
is calling a "build tool."

However, a few weeks ago, someone pointed out that while that's nice for
everyone who is used to STRTA, if the scripts reach a point where no one can
remember well enough the differences between `setup`, `bootstrap`, and `update`,
the scripts can get in the way. You can of course run `./scripts/setup --help`,
`./scripts/bootstrap --help`, and `./scripts/update --help` provided your
friendly colleagues remembered to include a `--help` option, but that ends up
being a lot of typing to do every time.

Also, over time, the list of available scripts tends to grow much more often
than it shrinks. That's how in Raster Foundry we wound up with [sixteen
scripts], most of them non-standard, and commonly only call a few.

Something I noticed a bit ago is that some of our projects, like [Raster
Foundry], also have nice tables describing the scripts. I thought: can you use
semi-structured READMEs (a bunch of markdown text with a table with special
column names) and semi-structured scripts (executable text files that 🤞🏻 print
helpful information when called with `--help`) to show information about what
scripts are for in a single interactive location?

It turns out you can!

Then an attentive and conscientious human could use that information never to be
confused about what scripts are for and to have an up-to-date view of all the
scripts that are still around.

I think I probably poisoned my brain and I'm now seeing every problem as a
terminal UI problem, but I think it was worth it to learn a fun new tool.

## `cliffs`

[`cliffs`] is a small CLI that does very little, but the little it does is
pretty useful. If you have a README with specially named columns and a scripts
directory named `scripts`, running `cliffs` will get you a nice little
interactive session for exploring what each thing does:

<!-- TODO: video embedding not working, try out more plugins while less sleepy -->
VIDEO GOES HERE

In short:

- run `cliffs` in a directory containing a README.md file and a `scripts/`
  directory
- ⬆️/⬇️ to preview the `--help` text for a script
- or you can just look at descriptions if they're sufficient

I think you could use `cliffs` either to help you find out what scripts are for
if you've forgotten or to audit your scripts to make sure they all understand
what the `--help` flag does. While frameworks like Python's `click` or `oclif`
in Node.js will handle `--help` for you, if you're hand-rolling bash scripts,
it's much easier to forget. 

You can install cliffs with [`nix`] as:

```bash
nix-env -i -f https://github.com/jisantuc/cliffs/archive/refs/tags/v0.0.1.zip
```

`cliffs` is built on...

### [`brick`]

[`brick`] handles the terminal UI interactions. `brick` was a delight to get
started with and felt like writing [The Elm Architecture] for a terminal
program. As someone whose brain breaks every time I have to think about frontend
layout, getting live in a land of pure functions from state to UI was really
nice. Also, the [examples directory] was really effective as documentation.

### [`cmark-gfm`]

[`cmark-gfm`] stands for "CommonMark GitHub-flavored markdown". It's the library
responsible for turning raw text into something with some structure. It's
oriented around nodes and their children, which means for any sort of search for
a specific kind of element, we just have to walk a tree and find a node matching
some predicate. It claims to be _very fast_, which is nice, but didn't really
come up for the READMEs I tested on. If you can describe in words the
relationships between the things you're looking for -- e.g. "the text in the
second column of the row" -- it's not super difficult to guess the relationships
between nodes that you end up with after parsing with `cmark-gfm`.

A surprising bonus is that parsing [_can't fail_] -- note that the return type
for `commonmarkToNode` doesn't return an `Either` or `Option` or `IO` of `Node`,
just a `Node`. I hadn't considered this before, but the reason for this is that
there's no such thing as an invalid CommonMark document -- the [CommonMark spec]
even notes that

> nothing in Markdown counts as a “syntax error”

I hadn't considered this previously and it was cool to learn something about
something I use almost _every day_ from the return type of a parser.

[Scripts to Rule Them All]: https://github.blog/2015-06-30-scripts-to-rule-them-all/
[Raster Foundry]: https://github.com/raster-foundry/raster-foundry/#scripts
[sixteen scripts]: https://github.com/raster-foundry/raster-foundry/tree/develop/scripts
[`cliffs`]: https://github.com/jisantuc/cliffs
[`brick`]: https://github.com/jtdaugherty/brick
[The Elm Architecture]: https://guide.elm-lang.org/architecture/
[`cmark-gfm`]: https://hackage.haskell.org/package/cmark-gfm-0.2.2
[_can't fail_]: https://hackage.haskell.org/package/cmark-gfm-0.2.2/docs/CMarkGFM.html#v:commonmarkToNode
[CommonMark spec]: https://spec.commonmark.org/0.30/#why-is-a-spec-needed-
[`nix`]: https://nixos.org/download.html
[examples directory]: https://github.com/jtdaugherty/brick/tree/master/programs