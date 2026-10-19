---
title: Doing my best to ignore LLMs
date: 2026-09-28
description: Doing my best to ignore LLMs
---

Hi, I'm a professional software engineer, and I don't use LLMs personally or in my day job. I [don't think I'm alone]
in my suspicion of the value of LLMs (at least, relative to their costs).[^1] But I'm pretty alone in my own
professional circle, so I wanted to record my rejection and its justification in a single place so I can stop
stammering over the answer every time this comes up.

I'm a senior software engineer at [Umbra]. I have not yet been compelled to integrate AI tooling into my daily work,
and to the best of my knowledge token consumption is not part of my performance review this year. At this point, I
think my refusal is being treated as a curiosity. I have a burrito bet with my manager about whether I'll have
integrated any generative AI tooling into my daily work by June 2027.[^2] I'm also my team's tech lead[^3], so
the proportion of my responsibility that production of code takes up has shrunk since January 2025. That shift
might matter for how useful LLMs are likely to be for me. It also might not -- writing prose ate up most of that shift
in responsibility, and that's also something people are turning to LLMs for. But in any case, if I could generate good
code 100x faster, it wouldn't make me 100x more effective at my current job.

I'm going to skip environmental arguments about AI and data centers. The US [energy source mix] is more than 70%
fossil fuels, so additional electricity consumption today is _pretty bad_ for how soon we melt the ice caps. I don't
make all of my choices around minimizing CO2 emissions though (I'm not a vegetarian, I have taken like 10 flights this
year, I often forget to turn lights off when I leave a room) and I don't want to have some emissions reduction purity
test. And while the energy mix in the US _today_ is abysmal, there's probably a set of die rolls where an AI crash
happens after a bunch of relatively clean power gets added to the grid to power all the data centers from the mania,
and like with the railroad and dot-com bubbles, maybe we get something useful after the crash for very dumb reasons. I
don't know man, the future is very long and pretty far away, and I'm not going to guess at how many wars of choice the
US will stumble into with fossil fuel exporters or what the consequences will be for the energy mix that powers the
next 20 years of data centers. I do not want to use LLMs regardless of whether they're terrible for the planet.

I'm also going to skip arguments about whether AI and LLMs are going to send us back to the Dark Ages by eroding
everyone's critical thinking and [ability to read any text longer than a few sentences]. It wouldn't be the first time
that a large group of humans has lost the ability to [read] and [write].[^9] LLMs join a parade of assassins of
reading and writing including the internet, text messaging, and social media. Maybe those things are succeeding in
killing literacy -- they had a good [head start] on it before ChatGPT showed up -- but either way, I don't think a
world where all of the LLMs vanished tomorrow is a world where books are so back. I do not want to use LLMs regarldess
of whether they're hastening the end of literacy.

[Forecasting] is hard. I'm not going to claim any special knowledge of the future here, and I'm not going to
cherry-pick the forecasts that make my refusal to use LLMs look smart and good. Instead, I'll focus on my subjective
experience of living and working, and in my subjective experience of living and working, LLMs suck _today_. This is, in
part, cheating. You can't tell me that my subjective experience of living and working is wrong, it's _my_ subjective
experience. It's also why arguments like "Claude is _really_ good at Python" mean nothing to me -- _I'm_ really good at
Python! I've had that covered for years! It's one of the reasons businesses have given me money for work.

## More LLM-generated code now means harder debugging later

LLMs generate too much code. I don't just mean that they're more verbose than necessary for specific problems
or that they ignore library code in favor of rewriting things from scratch or that they invent new constants
indistinguishable from existing constants.[^4] I mean that the volume of code that it's possible to produce in a
short time is too high for people to understand.

We (software engineers) are pretty bad at reading code. You can espouse whatever reasons you want for that,[^5] but
it's a broadly underdeveloped skill. Mostly that's _fine_ in team-based software engineering because someone on the
team wrote the code and has a wrote-the-code level of understanding of what's going on and how it relates to the rest
of the software. That balance shifts if the `git blame` points you to someone who also only has a read-the-code level
of understanding.

This concern isn't hypothetical. This recent LeadDev[^6] article called ["AI code causes real world production outages"]
includes these extremely unsurprising lines:

> Their teams are generating more code, but that’s not necessarily feeding through to delivering working software more
> quickly. [...] if an agent generates a large chunk of an application in seconds, the engineer responsible for
> maintaining it can likely never end up with the detailed knowledge that those who wrote it in previous generations
> would.

If your model of the role of a developer is that developers primarily produce text, that's weird. There's a lot of text
available quickly, and the new text should be just as good as the old text. If your model is instead that software
engineers learn the software as they produce it, it's an obvious consequence. Producing software with LLMs skips the
learning step. I think that's bad. I think it's a collective choice to know less about our work a year from now than we
know today. Engineers could be a bulwark against software enshittification, but we're shirking that responsibility.

I'm a part of an on-call rotation for the services my software team owns. I don't want my team to steadily understand
less of the software we maintain because I can be paged at 2am if that software is broken in a way that no one
anticipated. LLMs writing a ton of code makes "things no one anticipated" a bigger category and makes it less likely
that anyone will understand what caused the surprise. I also don't want the teams that own the services that interact
with my team's services to understand steadily less of their software. If I don't want people to ship code that they
only half understand and I think the pressures to do that are strong once you let LLMs generate code for you, then the
non-hypocritical thing for me to do is not to use LLMs to write code.

I've seen some people in DataHaskell Discord and in blogs argue that a lot of the understanding penalty of LLMs goes
away if you use Haskell to [make a lot of your opinions into hard constraints on the code]. I also have a coworker
who's LLM-ing through a Rust CLI for geospatial-aware [`jq`]-like querying into GeoJSON who says that's going really
well. It may be the case that languages with better cultures of representing human knowledge in code are less subject
to this critique and that my _real_ problem is that I'm responsible for a mountain of python. I am currently
responsible for a mountain of python though, and that mountain is in a range full of other mountains of python, so for
now, this is important to me.

## LLMs have not made people I work with better communicators

One promise of LLMs is that everyone can be an average writer by letting the machine do it. In practice though, when
I've read docs that have had large disclosed LLM contributions, I've found them less parseable than even bad docs
written by humans.

I've found original documents more difficult to understand since LLM contribution became common. In the past, if a
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

Widespread LLM use kills text as a medium for human communication. This is no way for working people to communicate
with each other. I think being able to trust the text I read and to expect authors of documents to understand their
contents, or at least I expect them to be able to explain anything that's unclear. If I think LLMs get in the way of
that, then the non-hypocritical thing for me to do is not to use LLMs to write prose.

## LLMs have made teamwork worse on my team

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

[obvious]: #more-llm-generated-code-now-means-harder-debugging-later
[reasons]: #llms-have-not-made-people-i-work-with-better-communicators
[make a lot of your opinions into hard constraints on the code]: https://blog.jle.im/entry/llms-and-haskell-1-constraint-evading-behavior.html
[don't think I'm alone]: https://leaddev.com/ai/meet-the-developers-rejecting-ai
[Umbra]: https://umbra.space/
[energy source mix]: https://www.iea.org/countries/united-states/energy-mix
[ability to read any text longer than a few sentences]: https://www.smbc-comics.com/comic/summary
[read]: https://en.wikipedia.org/wiki/Middle_Ages#Church_and_monasticism
[write]: https://jackghazi.com/essays/bronze-age-collapse-forgot-writing/
[forecasting]: https://danluu.com/futurist-predictions/
[head start]: https://nces.ed.gov/surveys/piaac/2023/national_results.asp
["AI code causes real world production outages"]: https://leaddev.com/ai/ai-code-causes-real-world-production-outages
[rubber duck]: https://en.wikipedia.org/wiki/Rubber_duck_debugging
[XY problem]: https://xyproblem.info/
[`jq`]: https://jqlang.org/
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
[^5]: Whether that's based on something like Felienne Hermanns's claim in _The Programmer's Brain_ that computer
    science education is hyper-focused on writing code or something like Peter Naur's argument in ["Programming as
    Theory Building"](https://pages.cs.wisc.edu/~remzi/Naur.pdf) that the lines of text aren't the important part of
    what the activity of programming produces, I think there's a vibe-y consensus that writing code yourself is better
    for understanding than reading code.
[^6]: I don't think LeadDev is gospel, but the LeadDev vibe isn't super AI-skeptical.
[^9]:  To be clear, I think that's bad and that reading and writing are pretty good. Just, you know, history is pretty
    weird, and it's hard to pick causes for things.
[^10]: I even like the annoying parts of working on a team, like when someone who isn't me has a great idea.

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
