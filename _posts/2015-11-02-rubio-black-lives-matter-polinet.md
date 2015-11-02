---
layout: post
title: Rubio's Black Lives Matter Comment and Politically Homogeneous Social Networks
category: blog
tags: 
  - blacklivesmatter
  - twitter
  - charts and graphs
  - marco rubio
  - election2016
  - politics
---

<link href="/_site/assets/css/lightbox.css" rel="stylesheet">

*This is part 1 of 3 and uses the [IndicoIO](https://indico.io) political sentiment API with no modification.

About a month ago, Janell Ross at the Washington Post noticed that about a month
and a half prior to *that*, [Rubio had said on Fox
news](https://www.washingtonpost.com/news/the-fix/wp/2015/10/03/marco-rubios-black-lives-matter-commentary-is-suddenly-relevant-for-some-reason/)
that the Black Lives Matter was a political organization with real grievances to
address. This utterance should have been a signal that not all Republicans have
the same "all lives matter" dismissive attitude about the movement, and that it
wasn't, the logic goes, shows further that liberals don't watch Fox News, hear
from conservatives, or engage with political ideologies outside of their
narrowly defined, Slate-approved doctrine. I'm being dramatic and she's just as
hard on conservatives and journalists, but that's the most absurd version of the
gist of it. She has charts and graphs and survey results that make the point
pretty convincingly. I don't have viewership data, but I do have the internet
and API keys for twitter and Indico, so I did some Twitter networks political
classification.

# Politically Scoring Twitter Networks

## First try

To do this, I wrote [a small class](https://www.github.com/jisantuc/pypolinet) that includes methods to:

1. Read in a twitter user's tweets,
2. Submit each of those tweets individually to the IndicoIO political sentiment API,
3. Return the mean score in Conservative, Green, Liberal, and Libertarian likelihood over all submitted tweets,
4. Grab the most recent 150 people that user followed and scores their tweets, and
5. Plot the user's likelihood and their network's likelihood in each category.

The goal was to show similarity in an easy to understand way. If two users' lines are really close together, their tweets have similar political sentiment.

That... didn't go so well. Scores in each category didn't get too far from about
0.25 for anyone, and anything that can't tell whether Donald Trump is twice as
likely to be Conservative as Green isn't really worth thinking much more about.

In addition to not showing many differences for each user, it also was eating up all of my October budget API calls to Indico.

## Second try

To solve both problems at once, I tried a second approach in which all of the tweets collected for each user were first aggregated into a single body of text. This gave the political sentiment API a lot more features to respond to, and we started to see some differentiation (higher peaks, lower valleys).

Here, for example, is my network with aggregated tweets:

![Turns out I'm not green enough to be like Brad Plumer]({{ site.url }}/images/polinet/jisantuccipolinet.png)

Increasing the differentiation was one nice feature, but the most similar/least
similar comparisons are where the jokes are. Donalt Trump it turns out tweets
just like Bill O'Reilly:

![Donald Trump tweets like Bill O'Reilly]({{ site.url}}
/images/polinet/realDonaldTrumppolinet.png)

Dean Baker tweets like Suzy Khimm, confirming in a single data point the
existence of Twitter-inflected liberal wonkspeak as a distinct English dialect:

![Dean Baker tweets like Suzy Khimm]({{ site.url}}
/images/polinet/DeanBaker13polinet.png)

And In These Times tweets like the New Left Review:
![In These Times tweets like N Plus One]({{ site.url }}
/images/polinet/inthesetimesmagpolinet.png)

However, I have some validity concerns. Note, for example, that Donald Trump's
twitter political sentiment is about as likely to be liberal as conservative,
and so is Bill O'Reilly's. It may be the case that tweets contain too little
political substance for more precise classifications. After all, a good (estimated) 183% of Trump's tweets are quotes of other people's tweets about how great Trump is with a "Thanks!" or something similar.

<blockquote class="twitter-tweet" lang="en"><p lang="en" dir="ltr">&quot;<a
href="https://twitter.com/Morning_Joe">@Morning_Joe</a>: Online poll: <a
href="https://twitter.com/realDonaldTrump">@realDonaldTrump</a> &#39;best to
handle economy&#39; by far <a
href="https://t.co/SZvSzNzoIk">pic.twitter.com/SZvSzNzoIk</a>&quot; Very true,
thanks!</p>&mdash; Donald J. Trump (@realDonaldTrump) <a
href="https://twitter.com/realDonaldTrump/status/661157516009697281">November 2,
2015</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

Self-congratulatory speech *probably* doesn't reveal much about political
alignment, so the big reveal here might be that some twitter timelines just by
chance happen to be vapid walls of text.

Here are all of the plots for the first run:

<a href="/images/polinet/DeanBaker13polinet.png" data-lightbox="image-1"
data-title="DeanBaker13">Dean Baker</a>
<a href="/images/polinet/FoxNewspolinet.png" data-lightbox="image-2"
data-title="DeanBaker13">Fox News</a>
<a href="/images/polinet/GarAlperovitzpolinet.png" data-lightbox="image-3"
data-title="DeanBaker13">Gar Alperovitz</a>
<a href="/images/polinet/inthesetimesmagpolinet.png" data-lightbox="image-4"
data-title="DeanBaker13">In These Times Mag</a>
<a href="/images/polinet/jisantuccipolinet.png" data-lightbox="image-5"
data-title="DeanBaker13">Me</a>
<a href="/images/polinet/jonathanchaitpolinet.png" data-lightbox="image-6"
data-title="DeanBaker13">Jonathan Chait</a>
<a href="/images/polinet/marcorubiopolinet.png" data-lightbox="image-7"
data-title="DeanBaker13">Marco Rubio</a>
<a href="/images/polinet/marty_walshpolinet.png" data-lightbox="image-8"
data-title="DeanBaker13">Marty Walsh</a>
<a href="/images/polinet/mattklewispolinet.png" data-lightbox="image-9"
data-title="DeanBaker13">Matt Lewis</a>
<a href="/images/polinet/mattyglesiaspolinet.png" data-lightbox="image-10"
data-title="DeanBaker13">Matt Yglesias</a>
<a href="/images/polinet/realDonaldTrumppolinet.png" data-lightbox="image-11"
data-title="DeanBaker13">Donald Trump</a>
<a href="/images/polinet/RedStatepolinet.png" data-lightbox="image-12"
data-title="DeanBaker13">Red State</a>
<a href="/images/polinet/RichardTrumkapolinet.png" data-lightbox="image-13"
data-title="DeanBaker13">Richard Trumka</a>
<a href="/images/polinet/zac_bearspolinet.png" data-lightbox="image-14"
data-title="DeanBaker13">Zac Bears</a>

The next run will include a refinement to plotting and an alternative
classification system. Should be end of the month/beginning of next

<script src="/_site/assets/js/lightbox.js"></script>
