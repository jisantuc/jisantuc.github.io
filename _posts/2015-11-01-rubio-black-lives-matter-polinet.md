---
layout: post
title: Rubio's Black Lives Matter Comment and Politically Homogeneous Social Networks
category: blog
breadcrumb: True
---

*This is part 1 of 2 and uses the [IndicoIO](https://indico.io) political sentiment API with no modification. Part 2 coming soon.

About a month ago, FILLIN at the Washington Post noticed that about a month prior to *that*, Rubio had said on Fox news that the Black Lives Matter was a political organization with real grievances to address. This utterance should have been a signal that not all Republicans have the same "all lives matter" dismissive attitude about the movement, and that it wasn't, the logic goes, shows further that liberals don't watch Fox News or hear from conservatives. I don't have viewership data so I can't comment on the Fox News piece of the claim (footnote: but come on, it's probably right) but I thought it should be possible to check the second piece.

# Politically Scoring Twitter Networks

## First try

To do this, I wrote [a small class](https://www.github.com/jisantuc/pypolinet) that includes methods to:

1. Read in a twitter user's tweets,
2. Submit each of those tweets individually to the IndicoIO political sentiment API,
3. Return the mean score in Conservative, Green, Liberal, and Libertarian likelihood over all submitted tweets,
4. Grab the most recent 150 people that user followed and scores their tweets, and
5. Plot the user's likelihood and their network's likelihood in each category.

The goal was to show similarity in an easy to understand way. If two users' lines are really close together, their tweets have similar political sentiment.

That... didn't go so well. Here are my scores and my network's scores:

****IMAGE****
image caption: 140 characters isn't much information to work with, it turns out.

In addition to not showing many differences for each user, it also was eating up all of my October budget API calls to Indico.

## Second try

To solve both problems at once, I tried a second approach in which all of the tweets collected for each user were first aggregated into a single body of text. This gave the political sentiment API a lot more features to respond to, and we started to see some differentiation (higher peaks, lower valleys).

Here, for example, is my network with aggregated tweets:

****IMAGE****
image caption: turns out I'm not green enough to be like Brad Plumer

Increasing the differentiation was one nice feature, but the most similar/least similar comparisons are where the jokes are.

****IMAGE****
image caption: Donald Trump tweets like Bill O'Reilly

****IMAGE****
image caption: 

****IMAGE****
image caption:

However, I have some validity concerns. Note, for example, that Donald Trump's twitter political sentiment is about as likely to be liberal as conservative, and so is Bill O'Reilly's. It may be the case that tweets contain too little political substance for more accurate classifications. After all, a good (estimated) 183% of Trump's tweets are quotes of other people's tweets about how great Trump is with a "Thanks!" or something similar.

# Results

The gallery of plots produced for a (sure, *small*) collection of users is pretty consistent: people follow people who tweet like them. Here are some of those plots again, this time with the network mean added.

The similarity is even stronger if you roll Libertarian probabilities into Conservative and Green probabilities into Liberal (footnote: please, please don't tell Jill Stein or Rand Paul that I did this).
