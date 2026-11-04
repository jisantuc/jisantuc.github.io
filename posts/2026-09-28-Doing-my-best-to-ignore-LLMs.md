---
title: Doing my best to ignore LLMs
date: 2026-09-28
description: Doing my best to ignore LLMs
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
have an eco-consciousness purity test. While the energy mix in the US _today_ is abysmal, there's probably a set of die
rolls where an AI crash happens after a bunch of relatively clean power gets added to the grid for data center mania,
and like with the railroad and dot-com bubbles, maybe we get something useful for very dumb reasons. Or the US could
stumble into enough wars of choice with fossil fuel exporters that the renewable energy revolution gets forced on us
and everyone says "obviously I supported clean energy independence the whole time." I don't know man, the future is
very long and far away and probably too weird to predict with much specificity. While I think the short-term and most
likely medium- and long-term outlooks for LLMs and energy/land use are alarming, I do not want to use LLMs regardless
of whether they're terrible for the planet.

I'm also not going to try to convince you that AI and LLMs are going to send us back to the Dark Ages by eroding
everyone's [ability to read any text longer than a few sentences]. It wouldn't be the first time that a large group of
humans has lost the ability to [read] and [write].[^9] LLMs join a parade of assassins of reading and writing in my
lifetime including the internet, text messaging, and social media. Those had a good [head start] before ChatGPT showed
up. I wouldn't be surprised if ChatGPT and Claude are doing their part, but I do not want to use LLMs regarldess of
whether they're hastening the end of literacy, numeracy, and critical thinking.

I'm going to focus on my subjective experience of reading text in the post-LLM era. Text consumption may seem pretty
narrow, but I produce and consume _a lot_ of text -- I write tens of thousands of lines of code per year, I read tens
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

A lot of the text I read every year is code. Sometimes I do that joyfully, like for the two weeks of [Advent of
Code] I have time for each year, when I get stuck and have to learn something I might have learned in an algorithms
course if I'd had one to get unstuck. Most of the time I read code because it's my job.

Professional software engineering requires you to write code that solves the current problem, that you can change when
it turns out it didn't solve the current problem exactly how you thought it would or the current problem changes, and
that other people on your team can understand well enough to tell whether they think it meets the other two criteria.

Code text also has a weird property where because of [`git`] you can see the entire history of who changed one specific
line of code that you're looking at. There's something nostalgic about this -- you can trace a team's evolution through
years by rewinding a function from the unmaintainable 600-line mess that it is today to the well-tested
single-responsibility baby (with no docstring of course, but nothing's perfect) it started as, including seeing who
made each change. "Remember when we had any idea what this function was for, before its unholy collection of appendages
reached out and ensnared every dream of beauty and parsimony we've had?" "Ah yes, `x` was here then, she must never
see what's become of this function or of us." It's nice.

The person responsible for each change is the _author_, and authorship is treated as a
reliable indicator of who you can bug about something if you're confused about how it's related to some other broken
thing.

In general, it was good to have less code. It isn't always the case that a function with five lines is better than a
function with ten lines, but it's probably generally true that solving a problem in 5,000 lines of code is better than
solving it in 10,000.

LLMs generate too much code. They're fine, I guess? at generating code that solves the current problem, but they suffer
at the rest and ruin authorship as an indication of understanding.

<!-- TODO: pull everything in here up -->

LLMs generate too much code. One problem is they're more verbose than necessary for specific
problems, ignoring library code and inventing new constants indistinguishable from existing constants.[^4] Across
many problems, they are too efficient at producing mountains of code, and software engineers are pretty bad at reading
code.[^19] Mostly that's _fine_ in team-based software engineering because someone on the team wrote the code and
has a wrote-the-code level of understanding of what's going on. But it stops being fine if you can't get from `git
blame` to anyone who has a theory of what the software is for.[^5]

This concern isn't hypothetical. This recent LeadDev[^6] article called ["AI code causes real world production outages"]
includes these extremely unsurprising lines:

> Their teams are generating more code, but that’s not necessarily feeding through to delivering working software more
> quickly. [...] if an agent generates a large chunk of an application in seconds, the engineer responsible for
> maintaining it can likely never end up with the detailed knowledge that those who wrote it in previous generations
> would.

If you think developers primarily produce text, that's weird. Surely there's no reason that developers producing text
should do a better job than tireless machines producing text. If instead you think developers produce text only
incidentally as an artifact of learning, it's an obvious consequence. Producing software with LLMs skips the learning
step. Choosing to generate any considerable portion of your software with LLMs is a choice to know less about your work
a week/a month/a year from now than you know today. I don't think you should make that choice, and I won't.

Coworkers of mine have also used LLMs to write lengthy prose documents. These documents are also more verbose than I'd
like them to be, but beyond the verbosity, they're _socially bad_.

<!-- TODO: problem 1, no more trust that the "author" believes the words on the page; ... is there a problem 2? -->

tktktk edit within

I've found prose documentation more difficult to understand since LLM contribution became common. In the past, if a
document was unclear, it was easy to understand why -- the person adding words to the document couldn't find the right
words for what they were trying to express. That's fine, like many things, writing clearly is pretty hard. But now
there's an added possibility that the person whose words you're supposedly reading has never even read the unclear
part.

Similarly, I've had LLMs record my supposed agreement to things in meetings that I spoke up against and then seen
that supposed agreement show up in people's post-meeting summaries.

I don't think human writers or human notetakers are perfect, but when a human writer / notetaker makes a mistake, you
can ask them what they meant or what they heard in a meeting that caused them to believe something. When an LLM creates
unclear text and someone shares that without reading it, there's no recourse to what the author meant -- the author
doesn't know what they meant, they never found out their ideas are unclear by trying to organize them, and it's now
your problem as the reader. When an LLM summarizes a meeting, unless participants quickly review the results, they can
create a false picture of what people on different teams have agreed to, and if enough time passes, there's no recourse
to the recollections of the meeting's participants.

Widespread LLM use ruins text as a medium for human communication.[^11] This is no way for working people to communicate
with each other. I think being able to trust the text I read and to expect authors of documents to understand their
contents, or at least I expect them to be able to explain anything that's unclear. If I think LLMs get in the way of
that, then the non-hypocritical thing for me to do is not to use LLMs to write prose.

tktktk edit above

## LLMs have made teamwork worse on my team

<!-- TODO: I think this is basically the writing argument again -->

I'm not sure what a team is for if Codex is the first teammate everyone talks to and Codex doesn't talk to anyone else.

One thing that's nice about working on a team is that you get to use other people's brains to solve problems. This
feature is great for big picture build-something-no-one's-ever-seen-before and think-about-why-this-won't-work reasons,
but it's also really nice for shared experience of the same software environments. Something goes wrong in someone's
dev environment, someone fixes it, they let people know what happened, later when it happens to you, you're prepared
already. Even when people don't know the solution yet, it was once pretty common for people to [rubber duck] publicly.

In my experience, that has largely vanished. The first stop for debugging is Codex, and there has been little follow up
to share the existence of the problem or the solution publicly. For a recent example, recently in a python development
environment, I had a stale version of a locally installed package. That stale version caused some version linting to
fail. When I brought this up in my team's Slack channel, I learned that three people had run into a similar issue, but
no one had said a word about it publicly. When people debug in public, I've also had more [XY problem] interactions,
where Y is _way farther_ from X than it was ever likely to be before.

For me, this feeling has been different from the general challenges of remote work. It's definitely challenging to work
together as a team remotely. Online whiteboards are just worse than real whiteboards, crosstalk in video meetings is
worse and harder to handle than crosstalk in person, it's harder to chance on good ideas from eavesdropping on
coworkers' unrelated conversations, and typing is in general worse than talking to get ideas across. But my feeling now
is that prototyping and useful learning from failure that used to happen in public within the team now happens
primarily in Codex chats, so we have all of the challenges of remoteness but few of the benefits of a team.

I _like_ working on a team. I like being on teams in general! I play on a soccer team and a pool team, and I like
cooperative board games.[^10] I'm not Denholm from the IT crowd,

<div class="flex-container" width="100%">
<iframe display="flex" width="560" height="315" src="https://www.youtube.com/embed/EoMW8VYb_GE?si=kinsv1zwNAbXeuVx" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen></iframe>
</div>

but I think teams are pretty good. As far as I can tell, whatever LLMs may or may not have done to make individuals on
my team more effective, they have made my experience of being on a team worse. If I think LLMs are bad for teams and I
think teams are improtant, then the non-hypocritical thing for me to do is not to use LLMs as my first teammate.

## I don't buy that LLMs have made people so much more productive

Some of these problems might be bearable if I bought the benefits, but I do not. For [obvious] [reasons], I don't think
faster prose and code production is the same as more productive prose and code authorship. But beyond that, I'd expect
to see something in productivity data somewhere if so many people were individually so much more productive.

First, here's [total factor productivity] from 1988 to present from FRED graph:

<div class="flex-container">
<iframe src="https://fred.stlouisfed.org/graph/graph-landing.php?g=1ZMSy&width=670&height=475" scrolling="no" frameborder="0" style="overflow:hidden; width:670px; height:525px;" allowTransparency="true" loading="lazy"></iframe>
</div>

It's hard to tell from looking at that plot when ChatGPT came out (September 2023 is the answer). You might counter
that the first ChatGPT shouldn't have made everyone a ton more productive, but then why is productivity growth lower in
later years? We don't have a lot of data yet on productivity growth in the post-LLM world, but if the argument is that
these things are already a transformational technology, then I want to see some transformation.[^43]

It's possible that any productivity gains that would be visible today are more limited to software engineering and
related fields. I'd be suspicious of anyone who's selling you a coherent notion of software engineering productivity
though. There are plenty of [books], [research articles], and [blog posts] from fancy software engineering thought
leadership people that explain some of the difficulty in measuring software engineering productivity and some of the
ways people get it wrong. If LLMs are supposed to make specifically software engineers more productivity, then someone
should put forward a coherent statement of what they mean by software engineer productivity.

It's also possible that the productivity narrative is driven by personal vibes and performative grinding. _Accelerate_
kicks off its discussion of performance measurement by discussing why lines-of-code is a terrible productivity measure,
but it's easy to be seduced into thinking you've really _done something_ by seeing a bunch of software materialize from
nothing. It's hard to argue against people's feelings of being more productive -- especially when I started off with
claiming that you can't argue with my subjective experience -- so I won't. But I'm not going to take any feelings of
personal productivity seriously without seeing some broader evidence about LLMs and productivity.

* economic productivity:
  * [labor productivity](https://fred.stlouisfed.org/series/PRS85006092)
  * [total factor productivity](https://fred.stlouisfed.org/series/RTFPNAUSA632NRUG)
  * [alt TFP percentage
    changes](https://www.frbsf.org/research-and-insights/data-and-indicators/total-factor-productivity-tfp/)
* software engineer productivity:
  * [can you measure
    it?](https://newsletter.pragmaticengineer.com/p/measuring-developer-productivity)
  * [SPACE](https://space-framework.com/) framework, where the S is "feelings"
* Suppose you wanted to measure your personal productivity -- what would you even measure?
  _output_ is usually measured in economic output, which is GDP; you don't have a GDP, you
  have a salary. You can do expenditure accounting, but doing more work won't necessarily
  increase your expenditures.
* You could also SPACE framework yourself, starting with your feelings. If you take the survey
  once sober and once on drugs that make you feel more relaxed, has your productivity
  increased? I don't know the rules, man.
* Individual productivity is just a vibe, and hustle culture is the worst, and I'm not going
  to adopt a tool because I can do _so much more_.
* something something more stuff in computer land is not more productive, go touch grass?

## Some kind of wrap up

maybe?

It's not about _caring_ -- it's about trying. It's ridiculous to ask everyone to care about their job and presumptuous
to think that I care an amount that makes me special. I probably care an average amount. The line I'll draw here is
between emotional investment and effort. Whether or not you feel emotionally invested, you can still show up and put in
the work of understanding what problems need solving and coming up with efficient solutions to those problems.

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
[^1]: http://ozark.hendrix.edu/~yorgey/forest/009L/index.xml,
    https://sboots.ca/2026/03/11/generative-ai-vegetarianism/, each of which has links to spicier takes.
[^2]: Low-stakes gambling is a really great way to resolve disagreements about future states of the world.
    Obviously, I have some agency over the outcome of this bet because I can keep refusing to use LLMs, which you might
    think makes it unfair. But so does my manager -- if it comes down to it, he can fire me. We're both insider
    trading, so it's fine, or at least fair.
[^3]: Just tech lead, not a [tech lead manager](https://lethain.com/tech-lead-managers/)
[^4]: Though of course they do all of that. Googling for "LLMs defensive programming" or "LLMs ignore libraries" will
    get you a nice AI-summary explaining why LLMs do this, with plenty of links to other people writing about how to
    keep their agents from writing garbage code.
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
[^10]: I even like the annoying parts of working on a team, like when someone who isn't me has a great idea.
[^11]: This claim is similar to the a claim made in ["On the Death of Amazon's Writing
    Culture"](https://alifeengineered.substack.com/p/on-the-death-of-amazons-writing-culture). I have never worked
    anywhere with Amazon's writing culture (probably just Amazon!), but I was sad reading that.
[^19]: The volume of code LLMs produce is why I don't care about arguments like "Claude is _really_ good at Python"
        -- _I'm_ really good at Python! I've had that covered for years! It's one of the reasons businesses have given
        me money for work! Claude is better at writing Python than I am by some metrics, but Claude doesn't move at a
        human pace, and I think pace matters.
[^43]: I'm not cherry-picking, labor productivity looks [basically the
    same](https://fred.stlouisfed.org/series/OPHNFB).
[^1234]: I'm not a vegetarian, I have taken like 10 flights this year, I often forget to turn lights off when I leave a
    room.

2. Rejecting the premise
    * I wanted to write code. It took me a while to get to write code. Taking that from me isn't a
      "you don't have to" thing where my burdens have been relieved.
    * I also like to write prose. It didn't take me a while to get to write prose (writing decent
      one-page summaries for our main government client was one of the main responsibilities of my
      first job), but it did take me a while to write like a normal person and recognize myself in
      things I wrote. Taking that from me isn't a "you don't have to" thing either.
    * I like to try to do a good job. like, effort is satisfying for me. Next word prediction is in
      general about the most likely next word, which is a flattening of everything to the average
      version of it you'd get if you asked the internet. That kinda sucks?
    * I like working on teams. You can hit that flow state where people anticipate what each other
      are going to need. Mostly this is a team sports phenomenon, but it can happen in work teams
      too. I don't think I'd feel the flow state with a team of AI agents.
3. Rejecting the bit
    * Productivity is fake and anyone telling you they know how to measure short-term change in it
      is lying.
    * I produce two things as a software engineer: code and understanding of the relationship
      between code and the business.
      * What if I produce a lot more code? Do I _also_ produce a lot more understanding?
      * I don't think so. I think there are a few reasons why people might feel that way:
        * fluency illusion -- people think things they can recall easily are things they understand
          well (see ch. 5 of _Make It Stick_ "Avoid Illusions of Knowing"
          https://www.retrievalpractice.org/make-it-stick)
        * depends on code reading, and people are _pretty bad_ at code reading (see
          https://www.manning.com/books/the-programmers-brain)
        * people respond to incentives, and it's easier to create incentives for moving Jira tickets
          in the next three to six months than it is to create incentives for producing more
          _understanding units_, so even people who would want to be careful and understand will be
          pressured not to do so
      * if you think the production of understanding units is subsidiary to the production of lines
        of code, this is probably fine, but I think the relationship is the opposite (see
        [_Programming as Theory Building_](https://pages.cs.wisc.edu/~remzi/Naur.pdf)).
