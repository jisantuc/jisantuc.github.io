---
title: On refusing to use LLMs
date: 2026-11-10
description: Doing my best to ignore LLMs
readingTime: 13
---

Hi, I'm a professional software engineer, and I don't use LLMs personally or in my day job. I [don't think I'm alone]
in my suspicion of the value of LLMs.[^1] But I'm pretty alone in my own professional circle, so I wanted to record my
rejection and its justification in a single place so I can stop stammering over the answer every time this comes up.

I'm a senior software engineer at [Umbra]. I have not yet been compelled to integrate AI tooling into my daily work,
and to the best of my knowledge token consumption was not part of my performance review this year. At this point, I
think my refusal is being treated as a curiosity. I have a burrito bet with my manager about whether I'll have
integrated any generative AI tooling into my daily work by June 2027.[^2] I'm also my team's tech lead[^3], so
the proportion of my responsibility that production of code takes up has shrunk since January 2025. That shift might
matter for how useful LLMs are likely to be for me. It also might not -- writing prose ate up most of that shift in
responsibility, and that's also something people are turning to LLMs for.

I'm not going to tell you you shouldn't use LLMs because of the environmental impact of AI and data centers. Those
impacts are _bad_, but I don't make all of my choices around minimizing CO2 emissions[^1234] and I don't want to
have an eco-consciousness purity test. While the energy mix in the US today is abysmal, there's probably a set of die
rolls where an AI crash happens after a bunch of relatively clean power gets added to the grid for data center mania,
and like with the railroad and dot-com bubbles, maybe we get something useful for very dumb reasons. Or the US could
stumble into enough wars of choice with fossil fuel exporters that the renewable energy revolution gets forced on us
and everyone says "obviously I supported clean energy independence the whole time." I don't know man, the future is
very long and far away and probably too weird to predict with much specificity. While I think the short-term and most
likely medium- and long-term outlooks for LLMs and energy/land use are alarming, I do not want to use LLMs regardless
of whether they're terrible for the planet.

I'm also not going to try to convince you that AI and LLMs are going to send us back to the Dark Ages by eroding
everyone's [ability to read any text longer than a few sentences]. It would not be the first time that a large group of
humans has lost the ability to [read] and [write].[^9] LLMs join a parade of assassins of reading and writing in my
lifetime including the internet, text messaging, and social media. Those had a good [head start] before ChatGPT showed
up. I wouldn't be surprised if ChatGPT and Claude are doing their part, but I do not want to use LLMs regarldess of
whether they're hastening the end of literacy, numeracy, and critical thinking.

I'm going to focus on my subjective experience of reading text in the post-LLM era. Text consumption may seem pretty
narrow, but I produce and consume a lot of text -- I write tens of thousands of lines of code per year, I read tens
of thousands of lines of my coworkers' code, and I read and write massive volumes of work stuff in Confluence and Jira.
I also read books sometimes, but I don't have a handy way to figure out a word count estimate there. I spend a lot of
time reading text, most of which is written for explanation rather than entertainment, where the text's most important
job is that I should understand what the author was trying to communicate, and if I don't, we should be able to work
out what they were trying to say together.

Focusing on my subjective experience is partly cheating. You can't tell me that my subjective experience of consuming
text is wrong, it's _my_ subjective experience. But I don't want to try to convince you of which of 93 competing
visions of the future is the right one. [Forecasting] is hard -- I don't know, you don't know, and I don't want to take
this discussion to prediction markets. If my subjective experience is similar to your subjective experience though,
maybe we could both, just, like, not use LLMs for everything, or maybe you'll join me in not using them for anything.

## Code text

A lot of the text I read every year is code. Sometimes I do that joyfully, like for the two weeks of [Advent of Code] I
have time for each year when I get stuck and have to learn something I might have learned in an algorithms course if
I'd had one. Most of the time I read code because it's my job.

Professional software engineering requires you to write code that solves a problem, that you can change when it turns
out it didn't solve the problem exactly how you thought it would or the problem changes, and that other people on your
team can understand well enough to tell whether they think it meets the other two criteria.

Code text also has a weird property where because of [`git`] you can see the entire history of who changed one specific
line of code that you're looking at. There's something nostalgic about this -- you can rewind a function from the
unmaintainable 600-line mess that it is today to the well-tested single-responsibility baby (with no docstring of
course; nothing's perfect) it started as, seeing who made each change. "Remember when we had any idea what this
function was for, before its unholy collection of appendages reached out and ensnared every dream of beauty and
parsimony we've had?" "Ah yes, `x` was here then, she must never see what's become of this function or of us." It's
nice.

The person responsible for each change is the _author_, and authorship is treated as a reliable indicator of who you
can bug about something if you're confused about how it's related to some other broken thing.

In general, it's good to have less code. It isn't always the case that a function with five lines is better than a
function with ten lines, but it's probably generally true that solving a problem in 5,000 lines of code is better than
solving it in 10,000.

LLMs generate too much code. They're fine, I guess, at generating code that solves the current problem, but they suffer
at the rest and ruin authorship as an indication of understanding. The argument in their favor to me sounds exactly
like the argument for Python -- it's so easy to get something working _right now_! I just think that's the third most
of the responsibilities of professional software engineering.

As the mixture of LLM-generated and human-generated code shifts more to LLM-generated code, it's harder to make changes
without the aid of an LLM, and it's harder for anyone else to tell whether the changes are good changes. The pervasive
tech debt problem, where there's never enough time to refactor / remove code, adds a ratchet -- most of the slop is
here to stay.

A recent LeadDev[^6] article called ["AI code causes real world production outages"] includes these extremely
unsurprising lines:

> Their teams are generating more code, but that’s not necessarily feeding through to delivering working software more
> quickly. [...] if an agent generates a large chunk of an application in seconds, the engineer responsible for
> maintaining it can likely never end up with the detailed knowledge that those who wrote it in previous generations
> would.

If you think the hard part of software engineering is producing code, that's weird. Surely there's no reason that
humans producing code should do a better job than tireless machines producing code. But if you think it's important for
someone on the team to have a theory of what the software is for and how it relates to the rest of the software your
team is responsible for, it's obvious.[^5]

<!-- WARN: actually watch the talk and make sure he says what I think he's going to say -->

I wrote most of this before watching any of Bryan Cantrill's [talk at Rust NYC] about "Rust in the LLM Age." He's
focused there on systems programming, but the argument about the different experiences of the author (who, for
LLM-generated code, is barely an author) and the operator applies just as well in a 900-microservice Frankenstein. An
author prone to generating un-operatable garbage should have _more_ roadblocks thrown in their way, not fewer.

## Non-code business text

A good chunk of the remainder of the text I read is documentation of one kind or another. Three big categories are
planning documents written before we write software, descriptions of the software we've actually produced written
afterward, and aspirations for what our software might do in the far future (aka three or more months from now).

A common format for describing work that needs to be done is a Jira ticket. Jira tickets have titles, which are
supposed to be short descriptions of what needs to occur, and descriptions, which are free text descriptions of what
that work entails.

<figure> <img src="https://cdn.prod.website-files.com/5ea704591b73e7337746aa7b/641b44cb0597b55b5ef676c5_image4.png"/>
<figcaption>A Jira ticket (image sourced from
[Wrangle.io](https://www.wrangle.io/post/jira-ticketing-system-an-introductory-guide))</figcaption> </figure>

Historically it's been difficult to convince software engineers to write prose of any kind. I have never worked
somewhere that didn't have some trouble convincing software engineers to write descriptions.

One common solution to this problem is _templates_. With a template, the software engineer writing a ticket doesn't
have to figure out the structure on the fly and can instead just fill in the sections. I have also never worked
anywhere without seeing someone delete or ignore an entire template.

Jira tickets are the simplest writing of the three categories. Typically, even if you write all of the details like why
the problem is a problem, what indicates that the problem is solved, and some links to evidence of the problem's
existence, they're still a few hundred words, but software engineers still don't want to write them. As a group we're
more reluctant to write the other two categories -- descriptions of what we've actually done and dreams for the far
future.

I bring all this up to head off any critique that I'm nostalgic for a past glorious era of software engineering
writing. I'm not. I know how bad it was.

However bad the authorship problem is for LLM-generated code, it's worse for LLM-generated non-code. Code has some
ability to describe the conditions under which it's correct. All programming languages[^978] have syntactical
rules that describe whether a given sequence of characters can be interpreted as a program in that language. Some
languages have compilers which will tell you whether a syntactically valid sequence of characters attempts to do
something nonsensical, like evaluate `3 / "hello"`. I haven't yet seen a programming language that doesn't let you
write tests that will verify more complicated behaviors that it would be challenging to encode in types.

None of that is true for non-code text. There's syntax, there are rules, and various descendants of Clippy will give
you green or red underlines telling you when you've written something they don't like. But so much of producing good,
correct non-code text formerly depended on the author's ability to form an idea and choose the right syntactically
valid mostly rule-following words to express it.

That barrier is gone post-LLMs. Any partially formed idea can become a thoughtful-looking document with minimal effort.
This is fine if you think that the point of professional writing is to produce documents, but it's _very bad_ if you
think the point of professional writing is refine your ideas and share them with your coworkers.

People who generate large documents with LLMs seem to understand this. They are not surprised to be asked whether their
plausible-seeming LLM-generated sentences actually mean anything, and that's the problem. Everyone knows they're
slinging slop, but it's my job as a reader to point it out.

In the past, software engineers, product managers, etc. didn't write docs, so docs didn't exist, and it sucked. Now,
software engineers, product managers, etc. still don't write docs, but _tons of docs exist_, and it's much worse.

Of course the people who wrote plenty of carefully considered documents before continue to write, often even with
careful consideration. To make matters worse though, those documents receive lengthy LLM-generated responses, where
earnest argument gets polluted with whatever a statistically average person might say if they ingested all of
your company's internal documentation.

Before slop, there was [bullshit] -- speech or writing where the speaker/author doesn't care whether what they
communicate is true or not. Before LLMs, even bullshitting took effort though; even plagiarizing bullshit took effort!
Now, instead, if you want a document that resembles what you might have written if you'd thought hard about something,
you can have one for cheap.

## I don't buy that LLMs have made people so much more productive

Some of these problems might be bearable if I bought the benefits, but I do not. I
don't think faster prose and code production is the same as more productive prose and code authorship. But beyond that,
I'd expect to see something in productivity data somewhere if so many people were individually so much more productive.

First, here's year-over-year percentage change in [total factor productivity] from 1988 to present from FRED graph:

<div class="flex-container"> <iframe
src="https://fred.stlouisfed.org/graph/graph-landing.php?g=1ZMSy&width=670&height=475" scrolling="no" frameborder="0"
style="overflow:hidden; width:670px; height:525px;" allowTransparency="true" loading="lazy"></iframe> </div>

It's hard to tell from looking at that plot when ChatGPT came out (September 2023 is the answer; you can hover over the
line to see the year of each data point). You might counter that the first ChatGPT was so much worse than later LLMs
that it shouldn't yet have made everyone a ton more productive, but then what's the deal with the lower productivity
growth in 2025? We don't have a lot of data yet on productivity growth in the post-LLM world, but if the argument is
that these things are already a transformational technology, then I want to see some transformation.[^43]

It's possible that any productivity gains that would be visible today are limited to software engineering and related
fields. I'd be suspicious of anyone who's selling you a coherent notion of software engineering productivity though.
There are plenty of [books], [research articles], and [blog posts] from fancy software engineering thought leadership
cabal people that explain some of the difficulty in measuring software engineering productivity and some of the ways
people get it wrong. If LLMs are supposed to make specifically software engineers more productive, then someone making
that argument should put forward a coherent statement of what they mean by software engineer productivity.

It's also possible that the productivity narrative is driven by personal vibes and performative grinding. _Accelerate_
kicks off its discussion of performance measurement by discussing why lines-of-code is a terrible productivity measure,
but it's easy to be seduced into thinking you've really _done something_ by seeing a bunch of lines of code materialize
from nothing. It's hard to argue against people's feelings of being more productive -- especially when I started off
with claiming that you can't argue with my subjective experience -- so I won't. But I'm not going to take any feelings
of personal productivity seriously without seeing some broader evidence about LLMs and productivity.

## Fin

I like working on a team. I like being on teams in general. I play on a soccer team and a pool team, and I like
cooperative board games.[^10] I'm not Denholm from the IT crowd,

<div class="flex-container" width="100%">
<iframe display="flex" width="560" height="315" src="https://www.youtube.com/embed/EoMW8VYb_GE?si=kinsv1zwNAbXeuVx" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen></iframe>
</div>

but I think teams are pretty good.

Since I haven't work in an office with my team in any of my last three jobs, we've had to have a lot of trust. Some of
that trust was built in person when we've all gone to an office at the same time, but a lot of it was built through
reviewing each other's code and other written artifacts. In a work environment where code and prose authorship no
longer mean anything about what the author unerstands / believes, I don't think that mechanism works anymore.

However capable LLMs become at generating code,[^897] their destruction of the substrate that team trust is built on
makes them to me a net negative.

[`git`]: https://git-scm.com/
[obvious]: #more-llm-generated-code-now-means-harder-debugging-later
[reasons]: #llms-have-not-made-people-i-work-with-better-communicators
[make a lot of your opinions into hard constraints on the code]: https://blog.jle.im/entry/llms-and-haskell-1-constraint-evading-behavior.html
[don't think I'm alone]: https://leaddev.com/ai/meet-the-developers-rejecting-ai
[Umbra]: https://umbra.space/
[ability to read any text longer than a few sentences]: https://www.smbc-comics.com/comic/summary
[read]: https://en.wikipedia.org/wiki/Middle_Ages#Church_and_monasticism
[write]: https://jackghazi.com/essays/bronze-age-collapse-forgot-writing/
[forecasting]: https://danluu.com/futurist-predictions/
[head start]: https://nces.ed.gov/surveys/piaac/2023/national_results.asp
["AI code causes real world production outages"]: https://leaddev.com/ai/ai-code-causes-real-world-production-outages
[rubber duck]: https://en.wikipedia.org/wiki/Rubber_duck_debugging
[XY problem]: https://xyproblem.info/
[`jq`]: https://jqlang.org/
[books]: https://www.amazon.com/dp/1942788339?lv=shuf&channelId=500&plpRedirect=mhFallback
[research articles]: https://queue.acm.org/doi/10.1145/3454122.3454124
[blog posts]: https://newsletter.pragmaticengineer.com/p/measuring-developer-productivity
[total factor productivity]: https://www.bls.gov/productivity/educational-material/labor-productivity-total-factor-productivity-comparison.htm
[Advent of Code]: https://adventofcode.com/
[talk at Rust NYC]: https://www.youtube.com/watch?v=5D7QPOIZ7gY
[bullshit]: https://www2.csudh.edu/ccauthen/576f12/frankfurt__harry_-_on_bullshit.pdf
[^1]: http://ozark.hendrix.edu/~yorgey/forest/009L/index.xml,
    https://sboots.ca/2026/03/11/generative-ai-vegetarianism/, each of which has links to spicier takes.
[^2]: Low-stakes gambling is a really great way to resolve disagreements about future states of the world.
    Obviously, I have some agency over the outcome of this bet because I can keep refusing to use LLMs, which you might
    think makes it unfair. But so does my manager -- if it comes down to it, he can fire me. We're both insider
    trading on this burrito bet, so it's fair.
[^3]: Just tech lead, not a [tech lead manager](https://lethain.com/tech-lead-managers/)
[^5]: I think there's a vibe-y consensus that writing code yourself is better for understanding than reading code,
       but I want to justify my belief anyway. My belief that software engineers are bad at reading code comes from
       Felienne Hermans's _The Programmer's Brain_. I think a general ineffectiveness of code reading can be read into
       Peter Naur's ["Programming as Theory Building"](https://pages.cs.wisc.edu/~remzi/Naur.pdf), where the lines of
       text aren't the most important artifact of programming, and authoring software and developing a theory of
       the software's function are overlapping activities.
[^6]: I don't think LeadDev is gospel, but the LeadDev vibe is pretty AI-enthusiastic, so I think it's noteworthy that
    in the last few months I've noticed a lot more "things that aren't going great with AI" articles.
[^9]:  To be clear, I think that's bad and that reading and writing are pretty good. Just, you know, history is pretty
    weird, and it's hard to pick causes for things.
[^10]: I even like the annoying parts of working on a team, like when anyone who isn't me has a great idea.
[^43]: I'm not cherry-picking, labor productivity looks [basically the
    same](https://fred.stlouisfed.org/graph/?g=20Cn4).
[^1234]: I'm not a vegetarian, I have taken like 10 flights this year, I often forget to turn lights off when I leave a
    room.
[^978]: I'm pretty sure? Like I don't know a ton about [brainfuck](https://en.wikipedia.org/wiki/Brainfuck) or other
    golfing languages or extremely-narrow-purpose languages.
[^897]: which was never the hard part of software jobs to begin with!
