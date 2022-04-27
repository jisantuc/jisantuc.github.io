---
title: anti-development platforms
date: 2022-04-07T02:25:10+00:00
description: anti-development platforms
---

One idea that seems to have gained traction in *gestures around* the industry is
that it should be easy to develop and deploy software. To facilitate this, we're
are now drowning in _platforms_. We have [development
platforms](https://humanitec.com/blog/what-is-an-internal-developer-platform)
and [continuous integration
platforms](https://harness.io/blog/devops/what-is-cicd-platform-why-should-i-care/)
and [machine learning
platforms](https://www.snowflake.com/guides/machine-learning-platforms), even
[catalog platforms (???)](https://backstage.io/), and lots of things that exist
to make *gestures vaguely at whatever it is you're doing* whatever it is you're
doing _easy_. One entrant missing from the platform space unfortunately is
anti-development platforms.

## Too much software

I'm not going to bother claiming that it should be hard to make useful software
-- obviously, if you're making useful software, it's better if that's easy. This
is not one of those screeds! If you want one of those screeds, feel free to
spend your time on this bizarre thread about how Neovim users are bad actually
because they like something that... makes it easier for them to do a good job at
whatever they're actually working on. I won't link it here, it was the Twitter
outrage flavor of the week a few weeks ago I think.

Anyway! Facilitating useful software isn't what platforms of different sorts
_do_. There's nothing that says the software you produce easily has to be useful
or good or even not actively harmful.

As a result, there's too much software. Too much how?

- maintenance -- Another side of the energy consumption is the general
  maintenance burden. It's probably true that no one is going to pay you to
  spend the time to keep whatever new open source thing you're working on
  up-to-date. Most of the time that's fine, since most software hardly runs
  anywhere, and most updates aren't security updates. But sometimes software
  runs _everywhere_ and updates _are_ security updates, and then the total
  effort required to remediate a vulnerability gets pretty big! So big that
  months after a patch is released to fix a horrible vulnerability that everyone
  agrees is very bad, a third of the exposed software [still hasn't been
  upgraded](https://techcrunch.com/2022/03/18/study-30-of-log4shell-instances-remain-vulnerable/).
  If you write software that tends to depend on a widely used library with a
  vulnerability, it's easier to upgrade a few things than many things.

- energy -- Another side of the maintenance burden is energy consumption. A
  common combination in modern development is continuous integration and
  automatic dependency updates. But also, maintenance is hard to prioritize.
  That leads to lots of open PRs, e.g. https://github.com/azavea/franklin/pulls,
  each of which runs a continous integration job that takes some time, which
  takes energy, which probably isn't important. It's not super easy to find out
  how much energy -- I worked on
  [`sbt-energymonitor`](https://www.47deg.com/blog/sbt-energymonitor-plugin/) at
  work, but the conditions under which you can use it are narrow enough that I
  learned even if you want to measure how much energy the fleet of computers
  that takes care of things you kind of care about while you're not paying
  attention consumes... it's pretty hard to measure! Anyway, more software means
  more CI runs, which means more energy consumption on kind of frivolous things.

- discoverability -- in addition to maintenance being a challenge for the people
  who have to do the maintenance, it's also a problem for people who want to
  consume seemingly unmaintained packages. For example, I filled in random names
  trying to find weird npm packages
  https://twitter.com/james_santucci/status/1511901722066620420. If you're
  looking for a package that does A Thing, it's _way harder_ to pick one if
  there are a lot of libraries that do the thing than if there is one (or a
  small number). Consider, for instance, if you've chosen to use React and want
  to choose a state management library -- here are [six you could choose
  from](https://blog.openreplay.com/top-6-react-state-management-libraries-for-2022),
  and the article notes

  > Naturally, there are many more libraries like these out there, with *more
  > coming out seemingly every day*. (emphasis mine)

My guess is what's going on is basically a technical Fourier transform
([SMBC](https://www.smbc-comics.com/comic/the-ethical-fourier-transform)).

![](https://www.smbc-comics.com/comics/1439737725-20150816.png)

As in the comic, we want to take some problem that's actually hard and convert
it into a technical problem. Let's take "estimation" as our hard problem. If you
want to know why estimates tend to be off in certain ways, how risk goes
unaccounted for when we reduce a task to a difficulty or time or complexity
number, where other kinds of missing work go... that's hard! Those are
organizational problems. But a system for tracking estimation errors with some
metadata that produces a probability of meeting a sprint deadline? That's not
hard! It's at least tractable enough that I can't even type the sentence without
starting to imagine how I'd build it.

## How does an anti-development platform prevent any of this?

The purpose of an anti-development platform is to challenge the default of
developing something new to solve the problem, especially if it's a problem you
heard about secondhand five minutes ago. In any organization that employs
software engineers, a risk of speaking a problem out loud is that a software
engineer within earshot will immediately start imagining how to solve that
problem with software. In an era in which you can start a new project and
publish version 0.0.1 to a public package repository in under 30 minutes,
_that's dangerous_. The purpose of the anti-development platform is to add
friction between "I could program something kind of like what you're talking
about" and "I made this thing that will now slowly accrete features for the next
15 months before it's quietly abandoned."

Anti-development platforms do this by asking lots of annoying questions. In the
next post, I'll talk about two specific workflows -- one for libraries, and one
for applications -- that an anti-development platform should facilitate.

### Libraries anti-workflow

Suppose, for example, that you work at a company with TKTKTKTKTK. In an all
hands, someone mentions off-hand that it would really help the executive team if
they could TKTKTK. You, a developer, think "I could write a Slackbot for that,"
because you're a developer and it's your first thought when you hear most
non-technical problems. You also, while you're developing that Slackbot, notice
that parts of the Slackbot aren't really specific to the bot, but "might make a
decent open source library". Because it's the future and we are in hell,
twenty-eight minutes later you've published a 10% complete version of your
library to npm.

An anti-development platform would intercede before you're able to publish. The
workflow would start by asking what kind of project you're working on. You'd
indicate an open source library, and be treated to a series of questions, each
of which would require at least a 50 word response. If the 50 word response to a
question was below a seventh grade reading level, you'd have to revise until
you'd written better prose describing why your project needs to exist. A
potential series of questions for an open source library might be:

- Does anyone care that this library doesn't exist?
- Does it need to be open source?
- But really, _does it_?
- If you said yes because "someone" might find it useful someday, how would they
  find it? Who are they?
- Have you met anyone like that?
- Did you tell them about this thing?
- Would you describe their response as "enthusiastic"?
- Do you think they were being enthusiastic just to be nice? (Note: you *must*
  answer yes to this question if you have any power over the person you asked
  about it)

### Applications anti-workflow