---
title: Don't fire Styer
date: 2026-02-24
description: Don't fire Styer
---

In early December last year, Team USA played Team Europe in the 31st Mosconi Cup, a pool tournament run by Matchroom
Pool pitting five players representing USA against five players representing Europe. Each matchup was a race to five racks.
These were the teams and each player's [Fargo rating]:


| Europe | USA |
| :----- | :-- |
| Josh Filler (859) | Fedor Gorst (847) |
| Jayson Shaw (834) | Shane van Boening (846) |
| Moritz Neuhausen (819)| Skyler Woodward (812) |
| David Alcaide (817) | Tyler Styer (791) |
| Pijus Labutis (812) | Billy Thorpe (778) |

Europe brought a strong team. Josh Filler is the top-ranked player in the world by [Fargo rating] (which is like
chess's [Elo rating system]). About two months before the Mosconi Cup, Neuhausen won the Peri 9 Ball Open a week before
losing to Pijus Labutis in the Hanoi Open final. David Alcaide won the Philippines Open two weeks after that. Jayson
Shaw didn't win a major tournament in 2025 but remains in the world's 15 highest ranked players and lost the semifinal
at the Philippines Open on an [inexplicable] 9 ball miss.

On the USA side, Fedor Gorst is typically in the top-ranked players on the World Nineball Tour and was the top-ranked
player in 2025, and Shane van Boening is a legend who consistently has one of the top five ratings in the world.
Styer and Thorpe were the only two players on either team ranked outside the world [top 100], while Woodward is rated
similarly to Alcaide, Labutis, and Neuhausen. The team's recent form was worse than Team Europe's. In the three
tournaments I mentioned above:

* Thorpe and Woodward both made the quarterfinals of the Peri 9 Ball Open, where Woodward lost 10-2 to Neuhausen.
* Gorst, Woodward, and Thorpe all lost in the last 64 at the Hanoi Open.
* Neither of Gorst nor Thorpe advanced to stage 2 of the Philippines Open.

In World Nineball Tour rankings, Shaw was the lowest ranked player on team Europe at 16th, while van Boening
was the second highest ranked player on Team USA at 15th (Styer, Woodward, and Thorpe were ranked 26th, 27th,
and 34th)[^2].

It went poorly for the United States, who lost 11-3.
Reddit's solution is to [fire Tyler Styer].[^1]

11-3 is a drubbing, but whether you look at ratings, recent tournament results, or tour rankings, Europe were clear
favorites. Because of the obvious gap, I was curious just how bad an 11-3 result was relative to expectations,
since, with two non-flubbed game endings, the score is 9-5 after 14 games, which is... I mean it's not _close_,
but it's sort of possible to imagine a comeback from there. So what would have been a reasonable expectation for
how many matchups the US team should win?

My guess was that given the differences in the players' ratings on the two teams, 11-3 wasn't that unlucky, but
I was wrong -- 11-3 was about a 1 in 20 outcome given the players'
ratings. The bad news for Team USA though is that they still lost a big majority of the simulated Mosconi Cups that
didn't end 11-3.

If you want to read how I tried to answer that question, check out [nerd stuff](#nerd-stuff). If you just want to see
some plots, you can skip to [plots](#the-results).

## Nerd stuff

### Generating a schedule

According to the [rules posted on the Matchroom website], each day has matches proceed in a set order, with
requirements that in some series of matches, everyone on the team has to play. The three kinds of matchups are team
matchups, doubles matchups, and singles matchups. All matchups are races to five racks.

Team USA lost 11-3 with the _actual_ schedule, but that schedule is only one of many possible schedules.
Even in the first team match, there are 120 different lineups each of the two teams can choose, so there are 14,400
possible matchups for day one / match one. On the broadcast, we were told that teams can't re-use the same lineup
from a previous team matchup, so the second team matchup only had 119 x 119 = 14,161 possible pairs of lineups.
That makes over 200 million possible schedules for just
two of the team matches.

There are way too many possible schedules to simulate all of them. I added an argument to the command
line interface for running the simulation to control how many schedules to generate. For each day, I randomly picked
the play order for each team subject to the [rules posted on the Matchroom website], i.e. respecting the constraints
about sets of matches in which all five players on a team had to play at least once. I didn't bother requiring the
team matchup lineups to vary.[^3]

Here's one random schedule I created, so you can check whether the schedules
I generated were legal schedules (Europe won this one 69% of the time, average racks won for the US was about 7.8):

<details><summary>Example schedule</summary>

<details><summary>Day 1</summary>

* Team match: Styer/Thorpe/van Boening/Woodward/Gorst vs. Filler/Alcaide/Shaw/Labutis/Neuhausen
* Doubles: van Boening/Thorpe vs. Alcaide/Filler
* Singles: Styler vs. Shaw
* Doubles: Woodward/Gorst vs. Labutis/Neuhausen

</details>

<details><summary>Day 2</summary>
* Team match: Woodward/van Boening/Styer/Thorpe/Gorst vs. Labutis/Shaw/Filler/Neuhausen/Alcaide
* Singles: Woodward vs. Labutis
* Doubles: Styer/van Boening vs. Shaw/Labutis
* Singles: Woodward vs. Filler
* Doubles: Gorst/Thorp vs. Alcaide/Neuhausen
</details>

<details><summary>Day 3</summary>
* Team match: Styer/Gorst/Woodward/Thorpe/van Boening vs. Shaw/Labutis/Filler/Neuhausen/Alcaide
* Singles: Styer vs. Shaw
* Doubles: Woodward/Styer vs. Neuhausen/Shaw
* Singles: Thorpe vs. Filler
* Doubles: van Boening/Gorst vs. Labutis/Alcaide
* Singles: Woodward vs. Filler
</details>

<details><summary>Day 4</summary>
* Singles: Woodward vs. Neuhausen
* Singles: Gorst vs. Labutis
* Singles: Thorpe vs. Shaw
* Singles: van Boening vs. Filler
* Singles: Styer vs. Alcaide
* Singles: van Boening vs. Alcaide
</details>

</details>

### Picking a winner for each matchup

Fargo ratings can be translated into win probabilities per rack. 
The Wikipedia [page on Elo rating systems] says that, for two players with ratings $R_A$ and $R_B$, the win
probability for player $A$ ($E_A$) is

<div class="math-container">
$E_A = \frac{1}{1 + 10^\frac{(R_B - R_A)}{s}}$
</div>

where $R_A$ is the rating for player $A$, $R_B$ is the rating for player $B$, $s$ is some "scaling factor," and $E_A$
is the probability that player $A$ wins whatever thing the rating applies to. In pool, the rating applies to individual
racks.

The FargoRate [FAQ] explains:

> When two players are 100 points apart, say a 300 versus a 400, the ratio of game wins will be near 1:2, as in 5
> games to 10 games, or 50 games to 100 games.

That's consistent with a scaling factor of 400 as in the Wikipedia examples, so that's the value I picked.

#### Singles matchups

Singles matchups are straightforward. In each rack, the player representing Team USA has some probability of winning,
let's say it's 40%. I generated a random number between 0 and 1, and if it was less than that probability, I gave
Team USA the rack. I repeated this until one of the players reached five racks.

Here's how the probability of victory in each rack changes based on the difference in rating between two players:

<canvas id="chart_FRGolCcDdcyibi4rPMyiwWlhyalO4zBXvuZwYf95PG2y9wP91fy6R" style="width:100%;max-width:600px;height:400px"></canvas>
<script src="https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.9.4/Chart.min.js"></script>
<script>
setTimeout(function() { new Chart("chart_FRGolCcDdcyibi4rPMyiwWlhyalO4zBXvuZwYf95PG2y9wP91fy6R", {
  type: "line",
  data: {
    labels: [0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0,12.0,13.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,21.0,22.0,23.0,24.0,25.0,26.0,27.0,28.0,29.0,30.0,31.0,32.0,33.0,34.0,35.0,36.0,37.0,38.0,39.0,40.0,41.0,42.0,43.0,44.0,45.0,46.0,47.0,48.0,49.0,50.0,51.0,52.0,53.0,54.0,55.0,56.0,57.0,58.0,59.0,60.0,61.0,62.0,63.0,64.0,65.0,66.0,67.0,68.0,69.0,70.0,71.0,72.0,73.0,74.0,75.0,76.0,77.0,78.0,79.0,80.0,81.0,82.0,83.0,84.0,85.0,86.0,87.0,88.0,89.0,90.0,91.0,92.0,93.0,94.0,95.0,96.0,97.0,98.0,99.0,100.0,101.0,102.0,103.0,104.0,105.0,106.0,107.0,108.0,109.0,110.0,111.0,112.0,113.0,114.0,115.0,116.0,117.0,118.0,119.0,120.0,121.0,122.0,123.0,124.0,125.0,126.0,127.0,128.0,129.0,130.0,131.0,132.0,133.0,134.0,135.0,136.0,137.0,138.0,139.0,140.0,141.0,142.0,143.0,144.0,145.0,146.0,147.0,148.0,149.0,150.0,151.0,152.0,153.0,154.0,155.0,156.0,157.0,158.0,159.0,160.0,161.0,162.0,163.0,164.0,165.0,166.0,167.0,168.0,169.0,170.0,171.0,172.0,173.0,174.0,175.0,176.0,177.0,178.0,179.0,180.0,181.0,182.0,183.0,184.0,185.0,186.0,187.0,188.0,189.0,190.0,191.0,192.0,193.0,194.0,195.0,196.0,197.0,198.0,199.0,200.0],
    datasets: [
    {
      label: "probability",
      data: [0.5,0.4985608882908471,0.4971218004251891,0.49568276024494023,0.49424379158885506,0.49280491829094863,0.4913661641789174,0.48992755307256153,0.4884891087822083,0.48705085510713625,0.48561281583400134,0.4841750147352649,0.4827374755676238,0.48130022207044143,0.47986327796418354,0.47842666694885455,0.4769904127024377,0.47555453887933863,0.4741190691088309,0.4726840269935071,0.4712494361077314,0.469815319996098,0.46838170217189273,0.46694860611555894,0.46551605527316847,0.4640840730548977,0.4626526828335072,0.46122190794282847,0.45979177167625435,0.45836229728523653,0.4569335079777882,0.4555054269169921,0.4540780772195163,0.45265148195413535,0.4512256641402582,0.449800646746463,0.4483764526890395,0.4469531048305375,0.44553062597832394,0.4441090388831469,0.44268836623770724,0.44126863067523897,0.4398498547680971,0.43843206102635396,0.43701527189640416,0.4355995097595788,0.43418479693076684,0.4327711556570478,0.43135860811633236,0.42994717641601177,0.4285368825916186,0.4271277486054956,0.42571979634547563,0.4243130476235704,0.422907524174671,0.42150324765525726,0.42010023964211857,0.41869852163108506,0.41729811503576986,0.41589904118632204,0.4145013213281905,0.41310497662089996,0.41171002813683716,0.41031649686004995,0.4089244036850566,0.4075337694156682,0.4061446147638222,0.4047569603484281,0.403370826694226,0.4019862342306564,0.40060320329074317,0.399221754109989,0.39784190682528303,0.39646368147382194,0.3950870979920432,0.3937121762145718,0.39233893587318014,0.39096739659576046,0.389597577905311,0.3882294992189357,0.3868631798468569,0.3854986389914422,0.384135895746244,0.38277496909505365,0.38141587791096926,0.3800586409554767,0.3787032768775454,0.3773498042127379,0.37599824138233284,0.3746486066924632,0.373300918333268,0.3719551943780582,0.37061145278249774,0.3692697113837977,0.3679299878999264,0.3665922999288324,0.3652566649476832,0.36392310031211805,0.36259162325551547,0.3612622508882752,0.35993500019711494,0.35860988804438204,0.3572869311673796,0.35596614617770733,0.3546475495606176,0.35333115767438533,0.3520169867496943,0.3507050528890364,0.3493953720661275,0.34808796012533627,0.34678283278113026,0.34548000561753406,0.344179494087605,0.34288131351292145,0.34158547908308745,0.34029200585525193,0.33900090875364175,0.33771220256911116,0.33642590195870453,0.3351420214452352,0.3338605754168779,0.33258157812677697,0.33130504369266833,0.3300309860965169,0.32875941918416823,0.3274903566650149,0.32622381211167784,0.3249597989597013,0.3236983305072631,0.32243941991489944,0.3211830802052434,0.31992932426277854,0.3186781648336063,0.3174296145252281,0.3161836858063414,0.3149403910066497,0.3136997423166875,0.3124617517876582,0.31122643133128686,0.30999379271968613,0.308763847585237,0.3075366074204823,0.3063120835780346,0.30509028727049736,0.3038712295704002,0.30265492141014677,0.30144137358197703,0.3002305967379419,0.29902260138989206,0.29781739790947936,0.29661499652817136,0.29541540733727906,0.2942186402879975,0.293024705191459,0.2918336117187994,0.29064536940123664,0.2894599876301622,0.2882774756572447,0.287097842594546,0.2859210974146497,0.2847472489508014,0.28357630589706123,0.28240827680846853,0.2812431701012181,0.2800809940528481,0.2789217568024399,0.2777654663508294,0.27661213056082945,0.27546175715746396,0.27431435372821295,0.27316992772326887,0.2720284864558038,0.27089003710224746,0.269754586702576,0.2686221421606117,0.2674927102443324,0.2663662975861926,0.26524291068345335,0.2641225558985236,0.2630052394593107,0.2618909674595814,0.2607797458593321,0.25967158048516964,0.25856647703070046,0.25746444105693017,0.25636547799267223,0.25526959313496533,0.2541767916495005,0.2530870785710568,0.2520004588039456,0.2509169371224644,0.24983651817135802,0.2487592064662893,0.24768500639431762,0.24661392221438547,0.24554595805781354,0.24448111792880314,0.24341940570494724,0.2423608251377483,0.24130537985314435,0.2402530733520421],
      fill: false,
      borderColor: "rgb(255, 99, 132)",
      tension: 0.1
    }
    ]
  },
  options: {
    title: { display: true, text: "Win probability with rating difference" },
    scales: {
      xAxes: [{ scaleLabel: { display: true, labelString: "Rating difference (100 means opponent is 100 points higher rated)" } }],
      yAxes: [{ scaleLabel: { display: true, labelString: "Win probability" }}]
    }
  }
})}, 100);
</script>

#### Team matchups

Team matchups are cycles of individual matchups. The team matchup cycles throught the five players from each team in
orders determined by the teams' captains. In each rack, picking a winner is exactly the same as in a singles matchup.

#### Doubles matchups

There's probably some mathematically correct way to combine ratings for pairs of players, but I don't know what it is.
To pick probabilities for each team, I took the average of the win probabilities of each of the four possible matchups,
e.g. if Woodward and Thorpe played Labutis and Alcaide, I calculated win probabilities for Woodward vs. Labutis,
Woodward vs. Alcaide, Thorpe vs. Labutis, and Thorpe vs. Alcaide, then I averaged those four values.[^4]

After I had a win probability, I picked a winner for the matchup the same way as in singles matchups.

### Checking my implementation

Being able to generate schedules and results is a good start, but I needed a way to validate that the results made
sense. To test how whether the simulation results were reasonable, I matched up two fake teams against actual team
Europe. One team was all Josh Fillers, i.e., in every matchup except singles matchups against real Josh Filler,
Team Filler Clones was favored. That team won the Mosconi Cup about
85% of the time. The other team was all me, with my 553 rating. That team won 0% of the time.[^5] Those both seemed
reasonable to me. Here's the win percentage curve for homogeneous teams of players with a bunch of ratings between mine
and a mix between Josh Filler and a Terminator. I generated 1,000 schedules and simulated each one 50 times.

<canvas id="chart_monoteam" style="width:100%;max-width:600px;height:400px"></canvas>
<script>
setTimeout(function() { new Chart("chart_monoteam", {
  type: "line",
  data: {
    labels: [700, 725, 750, 775, 800, 825, 850, 875, 900, 925, 950],
    datasets: [
    {
      label: "Clone team win rate",
      data: [0.006, 0.036, 0.544, 4.092, 17.888, 45.288, 76.142, 93.414, 99.050, 99.916, 99.986],
      fill: false,
      backgroundColor: "rgb(255, 99, 132)",
      borderColor: "rgb(255, 99, 132)",
      tension: 0.1
    }
    ]
  },
  options: {
    title: { display: true, text: "Clone team win rates" },
    scales: {
      xAxes: [{ scaleLabel: { display: true, labelString: "Clone team Fargo rating" } }],
      yAxes: [{ scaleLabel: {
        display: true,
        labelString: "Win percentage against actual Europe" }
      }],
      y: {
        min: 0, max: 100
      }
    }
  }
})}, 100);
</script>

Clone teams look like they'd win about half the time with a rating near 830, which makes intuitive sense, since that's
around the average for Team Europe's ratings.

## The results

I generated 10,000 schedules and simulated each one 100 times. This plot shows how often team USA won each number of
racks:

<canvas id="chart_DjUrNvW8lwntMx2b4QGbc1sTrCMLtiPSPfWoDkPcwA9Ps99n4w" style="width:100%;max-width:600px;height:400px"></canvas>
<script>
setTimeout(function() { new Chart("chart_DjUrNvW8lwntMx2b4QGbc1sTrCMLtiPSPfWoDkPcwA9Ps99n4w", {
  type: "bar",
  data: {
    labels: [0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0],
    datasets: [
    {
      label: "count",
      data: [1172.0,6030.0,16728.0,33681.0,53878.0,73428.0,90380.0,99392.0,101911.0,98251.0,89024.0,336125.0],
      backgroundColor: "rgb(255, 99, 132)",
      borderColor: "rgb(255, 99, 132)",
      tension: 0.1
    }
    ]
  },
  options: {
    title: { display: true, text: "How often team USA won each number of racks" },
    scales: {
      xAxes: [{ scaleLabel: { display: true, labelString: "Team USA Racks Won" } }],
      y: {
        min: 0, max: 35000
      }
    }
  }
})}, 100);
</script>

This plot answers the main question I had, and the answer is that only 3 racks for Team USA is _pretty rare_. About 95%
of the time they do better than that. Unfortunately, there's a big gap between doing better than three racks and
winning. About a third of the time, the US team wins 8, 9, or 10 matchups, another third-ish of the time they win, and
the remaining third is losing with 7 or fewer racks.

### So why not fire Styer?

Team USA were the US's five highest ranked players in Matchroom's World Nineball Tour rankings and the first, second,
third, seventh, and twelfth highest-rated US players by Fargo rating. Team Europe were Europe's third, fifth, sixth,
seventh, and tenth ranked players by World Nineball Tour rankings and first, third, eleventh, thirteenth, and
seventeenth highest-rated players by Fargo rating. It's easy to look at the missed 9 balls and imagine a universe where
the US team makes them instead and goes on to win the whole thing, but the odds were against them from there anyway.
It's even easier to imagine different players representing the US team altogether, but it's hard to come up with a way
to pick the teams that gives the US an advantage.

If instead of the actual teams you let each team bring their top five players by Fargo rating, the matchup gets worse.
Those teams would be:


| Europe | USA |
| :----- | :-- |
| Josh Filler (859) | Fedor Gorst (847) |
| Francisco Sanchez Ruiz (846) | Shane van Boening (846) |
| Jayson Shaw (834) | Skyler Woodward (812) |
| Wojciech Szewczyk (832) | Mike Dechaine[^6] (803) |
| Albin Ouschan (831) | Thorsten Hohmann (793) |

In sims of these matchups, the US won about 26% of the time.

<canvas id="chart_Rjey9ta8QWT98npfhTy9WLoDf84RcswizCKFgty69w9tQ" style="width:100%;max-width:600px;height:400px"></canvas>
<script>
setTimeout(function() { new Chart("chart_Rjey9ta8QWT98npfhTy9WLoDf84RcswizCKFgty69w9tQ", {
  type: "bar",
  data: {
    labels: [0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0],
    datasets: [
    {
      label: "count",
      data: [1958.0,9420.0,24292.0,45885.0,69365.0,89618.0,104182.0,108894.0,105924.0,96963.0,83027.0,260472.0],
      fill: false,
      backgroundColor: "rgb(255, 99, 132)",
      borderColor: "rgb(255, 99, 132)",
      tension: 0.1
    }
    ]
  },
  options: {
    title: { display: true, text: "USA matchup win frequency with top Fargo rated teams" },
    scales: {
      xAxes: [{ scaleLabel: { display: true, labelString: "Team USA Racks Won" } }],
      y: {
        min: 0, max: 350000
      }
    }
  }
})}, 100);
</script>

The US on average wins fewer racks in this matchup, wins 3 or fewer racks about 8% of the time instead of 5% of the
time, and wins overall 8 percentage points less often.

If both countries instead brought their top teams by World Nineball Tour rankings, the US team doesn't change at all,
and Team Europe is pretty similar. Shaw, Alcaide, and Neuhausen are gone, but Kaci, Sanchez Ruiz, and Krause bring
about as many total rating points.


| Europe | USA |
| :----- | :-- |
| Kaci (831) | Gorst (847) |
| Sanchez Ruiz (846) | van Boening (846) |
| Filler (859) | Styer (791) |
| Krause (794) | Woodward (812) |
| Labutis (812) | Thorpe (778) |


With similar teams to the actual matchups, it's not surprising that Team USA wins a similar number of Mosconi Cups
in this simulation or that the shape of the curve looks about the same as the first plot:

<canvas id="chart_SQGY413bNGisDqKiMhe79Qlw6SmKFK3cRTvWm5HH89d3NQKjLRwf" style="width:100%;max-width:600px;height:400px"></canvas>
<script>
setTimeout(function() { new Chart("chart_SQGY413bNGisDqKiMhe79Qlw6SmKFK3cRTvWm5HH89d3NQKjLRwf", {
  type: "bar",
  data: {
    labels: [0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0,11.0],
    datasets: [
    {
      label: "count",
      data: [1187.0,5690.0,16499.0,32515.0,53004.0,72762.0,88561.0,98327.0,101515.0,98803.0,90105.0,341032.0],
      backgroundColor: "rgb(255, 99, 132)",
      borderColor: "rgb(255, 99, 132)",
      tension: 0.1
    }
    ]
  },
  options: {
    title: { display: true, text: "USA matchup win frequency with top WNT rated teams" },
    scales: {
      xAxes: [{ scaleLabel: { display: true, labelString: "Team USA Racks Won" } }],
      y: {
        min: 0, max: 35000
      }
    }
  }
})}, 100);
</script>

Europe can send many different strong teams. For the US, that isn't the case --
this team was close to as good as it gets in terms of Fargo ratings and was the best team the US could assemble by
World Nineball Tour rankings, but it's a significant underdog against any of the European teams.

Pool doesn't currently have a strong statistical backing. It's not like baseball, where there's a small number of
outcomes for a plate appearance, or basketball, where it's easy to track how often an offense can generate open,
valuable shots, or hockey / soccer, where you can tell whether a team is winning on average by whether they're keeping
possession in attacking areas. Without a robust statistical explanation of how one player beats another, it's easy
to fixate on specific high leverage events to explain a loss.

I think in this case, focusing on Styer's misses misses the point. Overall, Team Europe won 63 of the 108 racks played.
Hand the two flubbed 9 balls to Styer and that drops to 61 out of 108. This US team against that Europe team was always
losing. It could have happened less dramatically, but a comeback was unlikely either way. Hypothetical other US teams
against other European teams have the same disadvantage. Firing Styer is one thing you could do if you wanted to
assemble a different team with a better chance to win, but the first problem you'd run into is finding someone to fill
the slot as good as Tyler Styer.

[fire Tyler Styer]: https://www.reddit.com/r/billiards/comments/1pdjqvx/fire_styer/
[Elo rating system]: https://en.wikipedia.org/wiki/Elo_rating_system
[Fargo rating]: https://fargorate.com/
[top 100]: https://fargorate.com/top-ten-lists
[rules posted on the Matchroom website]: https://matchroompool.com/news/event-guide-2025-sportsbet-io-mosconi-cup/
[page on Elo rating systems]: https://en.wikipedia.org/wiki/Elo_rating_system#Mathematical_details
[FAQ]: https://fargorate.com/#faq
[inexplicable]: https://youtu.be/qbJI4W5SCbs?si=rDnovyzts3lAEvZN&t=747
[^1]: Billy Thorpe and Skyler Woodward too. To be fair to the person who posted that/to un-strawman a bit, they weren't
specifically claiming that bringing someone other than
Styer would have made the US win the Mosconi Cup.
[^2]: All ratings and rankings quoted as of what I can see on 2025/02/10.
[^3]: I decided that the probability of identical team matchups was low enough, and that the probability of those
identical team matchups affecting the results was low enough, that it wasn't important. There's a $\frac{1}{120}$
chance that a team's lineup for the second team match matches the first one and a $\frac{1}{60}$ chance that the third
team match lineup matches either of the first two, for a 2.5% chance total. Over a bunch of simulations, I'll get some
repeats. Oh well.
[^4]: I also ran the simulation with averaging the ratings and calculating the win probabilities with the average
ratings. Results were broadly similar.
[^5]: Not rounded -- literally zero of the 100,000 sims I ran. One discouraging/funny thing I learned is that
I could be 200 points higher rated -- or basically, get twice as good as I currently am _twice_ -- and a team made up
entirely of that superhuman wins the Mosconi cup about a half percent of the time. I'm pretty good! Pros are monsters
though.
[^6]: Nevermind that Dechaine [played four tournaments](https://www.azbilliards.com/person/mike-dechaine/) in 2025,
none of them at the level of the big international tournaments.
