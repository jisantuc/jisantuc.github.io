---
title: "Haskell data visualization part 2: Hello, slightly different plots"
date: 2026-03-09
description: |
  Part 2 of a short series on making plots in Haskell. Part 1 covered making some 
  simple scatter plots. This part focuses on changing attributes of your plots.
  Part 3 will cover more complex plotting.
---

## Plotting with Haskell Libraries, Part 2

In [part one], I made a few scatter plots with default settings (or examples from docs if
"default settings" wasn't a meaningful category). In part two, I'll look at how plot configuration works in each of the five libraries I used in part one. Those libraries are:

* [`dataframe`]
* [`granite`]
* [`hvega`]
* [`chart-svg`]
* [`Chart`]

I'm interested in a few kinds of plot configuration, specifically:

* Axis control:
    * How do you change an axis's scale and limits?
* Labeling:
    * How do you re-title a plot?
    * How do you control axis labels?
* Appearance:
    * How do you change the color cycle / color map used for symbols on a plot?
    * How do you change the font used in a plot's text?
    * How do you make your plot bigger or smaller?

In general, these are the kinds of plot features that you want to change when you want to share a plot or collection
of plots with someone else, whether that's through embedding them in a report of some kind or pasting into chats.

### `dataframe`

[`dataframe` part 1]

The code example in part 1 used [`plotScatter`], but there's a collection of different scatter plot methods available:
`plotScatterWith`, `plotScatterBy`, and `plotScatterByWith` all take a few extra arguments.

The `*With` functions take a [`PlotConfig`], which answers the question of re-titling and controlling the plot size.
The `*By*` functions take a reference to a column to choose different colors for points based on what the value of the
label column. Together, they let you use the color channel for some information and control some plot aspects.

<div class="flex-container">
<canvas id="chart_FXMxePX5AGtOM8r3AZuD4LRP7Cd1Pete2PjN9Irh23jUF12dt" style="width:100%;max-width:300px;height:300px"></canvas>
</div>
<script src="https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.9.4/Chart.min.js"></script>
<script>
setTimeout(function() { new Chart("chart_FXMxePX5AGtOM8r3AZuD4LRP7Cd1Pete2PjN9Irh23jUF12dt", {
  type: "scatter",
  data: {
    datasets: [
    {
      label: "'h'",
      data: [{x:0.3534645438194275, y:0.7646807432174683},{x:0.3448813557624817, y:0.42339348793029785},{x:0.9110702276229858, y:0.3404604196548462},{x:0.7724000811576843, y:0.9107091426849365},{x:0.5484182238578796, y:0.89446622133255},{x:0.631543755531311, y:0.5476692914962769},{x:0.26684606075286865, y:4.2594075202941895e-3},{x:0.3627259135246277, y:0.6756367087364197},{x:4.698789119720459e-2, y:0.12035775184631348},{x:0.22164326906204224, y:0.9486547708511353}],
      pointRadius: 4,
      pointBackgroundColor: "rgb(255, 99, 132)"
    },
    {
      label: "'c'",
      data: [{x:0.32620739936828613, y:0.24561679363250732},{x:0.16701936721801758, y:0.28365814685821533},{x:0.1984427571296692, y:0.9260008931159973},{x:4.3488502502441406e-2, y:0.6190537214279175},{x:0.3938533663749695, y:0.15677368640899658},{x:0.11577272415161133, y:0.6882503032684326},{x:0.6248728036880493, y:0.8084238767623901},{x:0.14767831563949585, y:0.2762610912322998},{x:0.9253231287002563, y:0.4546962380409241},{x:0.5128378868103027, y:0.538031280040741},{x:0.3819701075553894, y:0.3312155604362488},{x:0.9521323442459106, y:6.321358680725098e-2},{x:0.8647952079772949, y:0.43305128812789917},{x:0.5770595073699951, y:0.7527228593826294}],
      pointRadius: 4,
      pointBackgroundColor: "rgb(54, 162, 235)"
    },
    {
      label: "'a'",
      data: [{x:0.5295222997665405, y:0.44256341457366943},{x:0.7071211338043213, y:0.29371726512908936},{x:2.5850534439086914e-3, y:0.7928040027618408},{x:0.5957183837890625, y:0.58250892162323},{x:0.1372065544128418, y:0.9879699945449829},{x:0.5081974267959595, y:0.7359441518783569},{x:0.8246698379516602, y:0.8133428692817688},{x:0.9070404171943665, y:0.7608577609062195}],
      pointRadius: 4,
      pointBackgroundColor: "rgb(255, 206, 86)"
    },
    {
      label: "'f'",
      data: [{x:0.46277785301208496, y:0.31038039922714233},{x:0.3093075156211853, y:0.9398903846740723},{x:0.4116531014442444, y:0.2679063081741333},{x:0.44633740186691284, y:0.25183558464050293},{x:0.9207983016967773, y:7.917124032974243e-2},{x:0.9732285737991333, y:0.5688970685005188},{x:6.755787134170532e-2, y:0.9560316205024719},{x:0.5423336029052734, y:0.12009698152542114},{x:0.9173020720481873, y:0.45381754636764526},{x:0.9128061532974243, y:4.847770929336548e-2},{x:0.5497339963912964, y:8.383971452713013e-2},{x:0.1304464340209961, y:0.4757652282714844},{x:0.14964228868484497, y:3.9104342460632324e-2},{x:0.828194797039032, y:0.6226693987846375}],
      pointRadius: 4,
      pointBackgroundColor: "rgb(75, 192, 192)"
    },
    {
      label: "'e'",
      data: [{x:0.6625450849533081, y:1.5111148357391357e-2},{x:0.2622825503349304, y:0.2885952591896057},{x:0.22069913148880005, y:0.9215577840805054},{x:0.5319154262542725, y:0.34964942932128906},{x:0.967324435710907, y:0.2033606767654419},{x:0.25629138946533203, y:2.0924031734466553e-2},{x:0.3827521800994873, y:0.46921753883361816},{x:0.5296564698219299, y:3.247523307800293e-2},{x:0.8823338747024536, y:0.9922868013381958},{x:0.4718790650367737, y:0.2412390112876892},{x:0.44432806968688965, y:0.7042456865310669},{x:0.44349467754364014, y:0.6241000890731812},{x:0.9597281217575073, y:0.5522154569625854},{x:0.5110119581222534, y:0.9187237024307251}],
      pointRadius: 4,
      pointBackgroundColor: "rgb(153, 102, 255)"
    },
    {
      label: "'d'",
      data: [{x:0.6760194301605225, y:1.4743328094482422e-2},{x:0.4490765333175659, y:0.2123563289642334},{x:0.16634488105773926, y:0.9851366877555847},{x:0.671101987361908, y:0.9590272307395935},{x:0.748245120048523, y:0.9869019985198975},{x:0.602918267250061, y:0.2017972469329834},{x:3.626072406768799e-2, y:0.9394771456718445},{x:8.078658580780029e-2, y:8.87221097946167e-3}],
      pointRadius: 4,
      pointBackgroundColor: "rgb(255, 159, 64)"
    },
    {
      label: "'i'",
      data: [{x:0.6883180141448975, y:0.7867722511291504},{x:0.8433091640472412, y:0.4271125793457031},{x:0.9706964492797852, y:0.9047513604164124},{x:0.330502450466156, y:0.3041015863418579},{x:0.577117919921875, y:0.5104788541793823},{x:0.348103404045105, y:0.17407846450805664},{x:0.9838802814483643, y:0.7751061320304871}],
      pointRadius: 4,
      pointBackgroundColor: "rgb(255, 99, 132)"
    },
    {
      label: "'j'",
      data: [{x:0.2286723256111145, y:0.269214928150177},{x:0.7828733325004578, y:0.2657080292701721},{x:0.5074955821037292, y:0.8574939370155334},{x:0.36216408014297485, y:5.0656795501708984e-2},{x:4.538452625274658e-2, y:0.667798638343811},{x:0.3928583264350891, y:0.4869365692138672},{x:7.814359664916992e-2, y:0.4179497957229614},{x:0.5313090682029724, y:0.385262131690979},{x:4.082685708999634e-2, y:0.5151517987251282},{x:0.23742926120758057, y:0.7539982795715332},{x:0.5123244524002075, y:0.19890618324279785}],
      pointRadius: 4,
      pointBackgroundColor: "rgb(54, 162, 235)"
    },
    {
      label: "'g'",
      data: [{x:0.5955027341842651, y:0.4679107666015625},{x:0.5328551530838013, y:0.4424973726272583},{x:0.9574474096298218, y:0.7811559438705444},{x:0.18175959587097168, y:0.5639089345932007},{x:0.4858781099319458, y:0.654837965965271},{x:0.4949987530708313, y:3.5435259342193604e-2},{x:0.5944989919662476, y:0.3890954852104187},{x:0.1863141655921936, y:0.172044038772583}],
      pointRadius: 4,
      pointBackgroundColor: "rgb(255, 206, 86)"
    },
    {
      label: "'b'",
      data: [{x:0.18407487869262695, y:0.5023034811019897},{x:0.8531350493431091, y:0.2430042028427124},{x:0.2909538745880127, y:0.12964683771133423},{x:0.6942615509033203, y:0.5047047138214111},{x:0.9903135895729065, y:0.2509654760360718},{x:0.6753919124603271, y:0.9627551436424255}],
      pointRadius: 4,
      pointBackgroundColor: "rgb(75, 192, 192)"
    }
    ]
  },
  options: {
    title: { display: true, text: "Little plot" },
    scales: {
      xAxes: [{ scaleLabel: { display: true, labelString: "x" } }],
      yAxes: [{ scaleLabel: { display: true, labelString: "y" } }]
    }
  }
})}, 100);
</script>

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text.IO as Text
import qualified DataFrame.Display.Web.Plot as DfPlot
import qualified DataFrame.Typed as DT
import ExampleData (LabeledDfSchema)

dataframeScatterConfig :: DT.TypedDataFrame LabeledDfSchema -> IO ()
dataframeScatterConfig typedDf =
  let plotConfig =
        DfPlot.PlotConfig
          { DfPlot.plotWidth = 200,
            DfPlot.plotTitle = "Little plot",
            DfPlot.plotHeight = 200,
            DfPlot.plotFile = Nothing,
            DfPlot.plotType = DfPlot.Scatter
          }
   in DfPlot.plotScatterByWith "x" "y" "tag" plotConfig (DT.thaw typedDf)
        >>= ( \(DfPlot.HtmlPlot plotText) ->
                Text.writeFile "plots/dataframeScatterConfig.html" plotText
            )

```

There's not a ton else you can do -- you can't pick different axis limits switch to a log scale, change the axis
labels, pick a different color cycle, or change the font.

I also tried out using the `TypedDataFrame` API here. It didn't provide any additional safety in the plotting code in
this case, since I could still pick columns that didn't exist in my declared dataframe schema without the compiler
yelling at me, but I think that will provide some safety in later examples, and it's fun to imagine a `TypedDataFrame`
plotting API that prevents you from trying to produce impossible plots _before_ you finish running some data pipeline.

### `granite`

[`granite` part 1]

`granite`'s part 1 code example used all the same tools that this example uses, except in part 1, it used [`defPlot`]
unmodified. `defPlot` is the default plot configuration.

<div class="flex-container">
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 490 556" width="490" height="556" font-family="system-ui, -apple-system, sans-serif">
<rect width="100%" height="100%" fill="white"/>
<text x="220" y="26" text-anchor="middle" fill="#222" font-size="14">Little plot</text>
<line x1="70" y1="514" x2="370" y2="514" stroke="#aaa" stroke-width="1"/>
<line x1="70" y1="34" x2="70" y2="514" stroke="#aaa" stroke-width="1"/>
<line x1="70" y1="34" x2="66" y2="34" stroke="#aaa" stroke-width="1"/>
<text x="62" y="38" text-anchor="end" fill="#555" font-size="11">1.5</text>
<line x1="70" y1="34" x2="370" y2="34" stroke="#eee" stroke-width="0.50"/>
<line x1="70" y1="274.50" x2="66" y2="274.50" stroke="#aaa" stroke-width="1"/>
<text x="62" y="278.50" text-anchor="end" fill="#555" font-size="11">0.5</text>
<line x1="70" y1="274.50" x2="370" y2="274.50" stroke="#eee" stroke-width="0.50"/>
<line x1="70" y1="514" x2="66" y2="514" stroke="#aaa" stroke-width="1"/>
<text x="62" y="518" text-anchor="end" fill="#555" font-size="11">-0.5</text>
<line x1="70" y1="514" x2="370" y2="514" stroke="#eee" stroke-width="0.50"/>
<line x1="70" y1="514" x2="70" y2="518" stroke="#aaa" stroke-width="1"/>
<text x="70" y="530" text-anchor="middle" fill="#555" font-size="11">-1.0</text>
<line x1="70" y1="34" x2="70" y2="514" stroke="#eee" stroke-width="0.50"/>
<line x1="220.50" y1="514" x2="220.50" y2="518" stroke="#aaa" stroke-width="1"/>
<text x="220.50" y="530" text-anchor="middle" fill="#555" font-size="11">0.5</text>
<line x1="220.50" y1="34" x2="220.50" y2="514" stroke="#eee" stroke-width="0.50"/>
<line x1="370" y1="514" x2="370" y2="518" stroke="#aaa" stroke-width="1"/>
<text x="370" y="530" text-anchor="middle" fill="#555" font-size="11">2.0</text>
<line x1="370" y1="34" x2="370" y2="514" stroke="#eee" stroke-width="0.50"/>
<circle cx="205.35" cy="210.48" r="3" fill="#2ecc71"/>
<circle cx="202.62" cy="335.05" r="3" fill="#2ecc71"/>
<circle cx="222.95" cy="287.78" r="3" fill="#2ecc71"/>
<circle cx="204.49" cy="292.39" r="3" fill="#2ecc71"/>
<circle cx="261.11" cy="312.29" r="3" fill="#2ecc71"/>
<circle cx="216.28" cy="319.51" r="3" fill="#2ecc71"/>
<circle cx="236.25" cy="390.37" r="3" fill="#2ecc71"/>
<circle cx="196.23" cy="324.74" r="3" fill="#2ecc71"/>
<circle cx="192.07" cy="172.83" r="3" fill="#2ecc71"/>
<circle cx="237.60" cy="390.46" r="3" fill="#2ecc71"/>
<circle cx="214.91" cy="343.03" r="3" fill="#2ecc71"/>
<circle cx="238.83" cy="205.17" r="3" fill="#2ecc71"/>
<circle cx="200.93" cy="168.43" r="3" fill="#2ecc71"/>
<circle cx="211.17" cy="329.70" r="3" fill="#2ecc71"/>
<circle cx="240.71" cy="323.51" r="3" fill="#2ecc71"/>
<circle cx="186.70" cy="325.92" r="3" fill="#2ecc71"/>
<circle cx="192.87" cy="329.39" r="3" fill="#2ecc71"/>
<circle cx="186.63" cy="157.57" r="3" fill="#2ecc71"/>
<circle cx="189.84" cy="171.76" r="3" fill="#2ecc71"/>
<circle cx="247.24" cy="175.43" r="3" fill="#2ecc71"/>
<circle cx="248.29" cy="330.23" r="3" fill="#2ecc71"/>
<circle cx="223.19" cy="310.08" r="3" fill="#2ecc71"/>
<circle cx="174.35" cy="245.43" r="3" fill="#2ecc71"/>
<circle cx="214.63" cy="333.56" r="3" fill="#2ecc71"/>
<circle cx="254.33" cy="291.49" r="3" fill="#2ecc71"/>
<circle cx="266.73" cy="345.19" r="3" fill="#2ecc71"/>
<circle cx="237.11" cy="163.83" r="3" fill="#2ecc71"/>
<circle cx="229.55" cy="281.70" r="3" fill="#2ecc71"/>
<circle cx="262.08" cy="375.00" r="3" fill="#2ecc71"/>
<circle cx="195.63" cy="388.98" r="3" fill="#2ecc71"/>
<circle cx="224.84" cy="179.33" r="3" fill="#2ecc71"/>
<circle cx="223.29" cy="287.80" r="3" fill="#2ecc71"/>
<circle cx="267.32" cy="257.46" r="3" fill="#2ecc71"/>
<circle cx="267.07" cy="176.86" r="3" fill="#2ecc71"/>
<circle cx="209.39" cy="356.37" r="3" fill="#2ecc71"/>
<circle cx="244.82" cy="157.14" r="3" fill="#2ecc71"/>
<circle cx="233.15" cy="262.56" r="3" fill="#2ecc71"/>
<circle cx="265.74" cy="206.52" r="3" fill="#2ecc71"/>
<circle cx="181.58" cy="228.82" r="3" fill="#2ecc71"/>
<circle cx="208.28" cy="281.39" r="3" fill="#2ecc71"/>
<circle cx="222.97" cy="386.21" r="3" fill="#2ecc71"/>
<circle cx="258.23" cy="155.85" r="3" fill="#2ecc71"/>
<circle cx="170.26" cy="203.73" r="3" fill="#2ecc71"/>
<circle cx="232.49" cy="199.98" r="3" fill="#2ecc71"/>
<circle cx="217.19" cy="336.10" r="3" fill="#2ecc71"/>
<circle cx="184.77" cy="327.70" r="3" fill="#2ecc71"/>
<circle cx="203.05" cy="321.02" r="3" fill="#2ecc71"/>
<circle cx="220.75" cy="188.20" r="3" fill="#2ecc71"/>
<circle cx="229.57" cy="254.20" r="3" fill="#2ecc71"/>
<circle cx="214.43" cy="224.98" r="3" fill="#2ecc71"/>
<circle cx="176.76" cy="164.55" r="3" fill="#2ecc71"/>
<circle cx="227.71" cy="271.49" r="3" fill="#2ecc71"/>
<circle cx="214.35" cy="244.22" r="3" fill="#2ecc71"/>
<circle cx="206.22" cy="381.84" r="3" fill="#2ecc71"/>
<circle cx="262.53" cy="284.87" r="3" fill="#2ecc71"/>
<circle cx="188.18" cy="258.66" r="3" fill="#2ecc71"/>
<circle cx="174.54" cy="233.73" r="3" fill="#2ecc71"/>
<circle cx="218.59" cy="236.84" r="3" fill="#2ecc71"/>
<circle cx="183.72" cy="156.89" r="3" fill="#2ecc71"/>
<circle cx="221.28" cy="264.87" r="3" fill="#2ecc71"/>
<circle cx="224.23" cy="365.18" r="3" fill="#2ecc71"/>
<circle cx="208.20" cy="314.51" r="3" fill="#2ecc71"/>
<circle cx="188.41" cy="273.45" r="3" fill="#2ecc71"/>
<circle cx="209.29" cy="277.14" r="3" fill="#2ecc71"/>
<circle cx="255.31" cy="335.68" r="3" fill="#2ecc71"/>
<circle cx="219.50" cy="385.50" r="3" fill="#2ecc71"/>
<circle cx="261.73" cy="285.08" r="3" fill="#2ecc71"/>
<circle cx="199.10" cy="362.88" r="3" fill="#2ecc71"/>
<circle cx="261.28" cy="382.37" r="3" fill="#2ecc71"/>
<circle cx="230.29" cy="345.57" r="3" fill="#2ecc71"/>
<circle cx="177.81" cy="293.69" r="3" fill="#2ecc71"/>
<circle cx="223.13" cy="301.54" r="3" fill="#2ecc71"/>
<circle cx="229.45" cy="300.62" r="3" fill="#2ecc71"/>
<circle cx="174.08" cy="270.36" r="3" fill="#2ecc71"/>
<circle cx="188.63" cy="352.71" r="3" fill="#2ecc71"/>
<circle cx="196.68" cy="392.98" r="3" fill="#2ecc71"/>
<circle cx="220.82" cy="217.37" r="3" fill="#2ecc71"/>
<circle cx="239.43" cy="272.87" r="3" fill="#2ecc71"/>
<circle cx="252.47" cy="198.80" r="3" fill="#2ecc71"/>
<circle cx="206.27" cy="231.85" r="3" fill="#2ecc71"/>
<circle cx="174.70" cy="365.11" r="3" fill="#2ecc71"/>
<circle cx="269.03" cy="333.77" r="3" fill="#2ecc71"/>
<circle cx="265.21" cy="378.83" r="3" fill="#2ecc71"/>
<circle cx="204.81" cy="352.22" r="3" fill="#2ecc71"/>
<circle cx="173.63" cy="168.53" r="3" fill="#2ecc71"/>
<circle cx="265.97" cy="261.47" r="3" fill="#2ecc71"/>
<circle cx="260.70" cy="211.39" r="3" fill="#2ecc71"/>
<circle cx="224.97" cy="373.88" r="3" fill="#2ecc71"/>
<circle cx="221.10" cy="173.51" r="3" fill="#2ecc71"/>
<circle cx="178.08" cy="391.87" r="3" fill="#2ecc71"/>
<circle cx="183.04" cy="279.82" r="3" fill="#2ecc71"/>
<circle cx="184.96" cy="384.61" r="3" fill="#2ecc71"/>
<circle cx="256.48" cy="290.07" r="3" fill="#2ecc71"/>
<circle cx="237.54" cy="162.94" r="3" fill="#2ecc71"/>
<circle cx="192.16" cy="166.32" r="3" fill="#2ecc71"/>
<circle cx="193.74" cy="213.04" r="3" fill="#2ecc71"/>
<circle cx="268.39" cy="207.97" r="3" fill="#2ecc71"/>
<circle cx="227.71" cy="213.35" r="3" fill="#2ecc71"/>
<circle cx="221.23" cy="346.26" r="3" fill="#2ecc71"/>
<circle cx="252.82" cy="244.56" r="3" fill="#2ecc71"/>
<rect x="385" y="39" width="12" height="12" fill="#2ecc71"/>
<text x="401" y="49" text-anchor="start" fill="#555" font-size="11">points</text>
</svg>
</div>

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text.IO as Text
import qualified DataFrame.Display.Web.Plot as DfPlot
import qualified DataFrame.Typed as DT
import qualified Granite as G
import qualified Granite.Svg as GSvg

graniteSvgScatterConfig :: DT.TypedDataFrame LabeledDfSchema -> IO ()
graniteSvgScatterConfig df =
  let xs = DT.columnAsList @"x" df
      ys = DT.columnAsList @"y" df
      plotConfig =
        G.defPlot
          { G.widthChars = 30,
            G.heightChars = 30,
            G.plotTitle = "Little plot",
            G.xBounds = (Just (-1), Just 2),
            G.yBounds = (Just (-0.5), Just 1.5),
            G.colorPalette = [G.BrightGreen, G.BrightBlack]
          }
      plot = GSvg.scatter [G.series "points" (zip xs ys)] plotConfig
   in Text.writeFile "plots/graniteSvgScatterConfig.html" plot
```

The [`PlotConfig`] value from `defPlot` can be updated with a different title, a different plot size, replacement
axis bounds for the `x` and `y` axes, and a different color palette. In the part 1 plot, since I didn't modify
anything, the values were plotted in the sensible range from 0 to 1 on both axes using the default first color from
`granite`'s color palette. In this version, the axis ranges are now very dumb because I hand-picked them to be,
the color is `BrightGreen` instead of `BrightBlue`, I changed the title, and I changed the
size of the output.

The width and height of the plot are specified in "chars", which get converted into a size for the plot
based on constants annotated `Pixels per terminal character height.` and `Pixels per terminal character width.`
For SVG output, the terminal pixel sizes aren't relevant, and those two constants not being equal means that my plot
that looks like it ought to be "30x30" in mystery units isn't square. That was surprising to me, but not a big deal.

As with `dataframe`, there's no way to switch from a linear to a log scale for either axis, you can't change the
font, and you can't change the axis labels.

### `hvega`

[`hvega` part 1]

`hvega`'s part 1 code example was more complicated than most of the other examples. Its part 2 example is also more
complicated, but in part 2, part of the reason for that is clear. If you want, you can use `hvega` to take pretty
fine-grained control over every aspect of your plot. If you need a long label for your `y` axis and want to make it
bigger for some reason, you can! If you want different fonts for your title, axis labels, and legends, you can!
If you want a monochrome/gradated purple scatter plot on a purple background, that's your prerogative!

<script src="https://cdn.jsdelivr.net/npm/vega@5"></script>
<script src="https://cdn.jsdelivr.net/npm/vega-lite@4"></script>
<script src="https://cdn.jsdelivr.net/npm/vega-embed"></script>
<div class="flex-container">
<div id="vis"></div>
</div>
<script type="text/javascript">
  var spec = {"$schema":"https://vega.github.io/schema/vega-lite/v4.json","background":"rgba(20, 0, 50, 0.2)","data":{"values":[{"tag":"h","x":0.3534645438194275,"y":0.7646807432174683},{"tag":"c","x":0.32620739936828613,"y":0.24561679363250732},{"tag":"a","x":0.5295222997665405,"y":0.44256341457366943},{"tag":"h","x":0.3448813557624817,"y":0.42339348793029785},{"tag":"h","x":0.9110702276229858,"y":0.3404604196548462},{"tag":"f","x":0.46277785301208496,"y":0.31038039922714233},{"tag":"e","x":0.6625450849533081,"y":1.5111148357391357e-2},{"tag":"e","x":0.2622825503349304,"y":0.2885952591896057},{"tag":"e","x":0.22069913148880005,"y":0.9215577840805054},{"tag":"d","x":0.6760194301605225,"y":1.4743328094482422e-2},{"tag":"d","x":0.4490765333175659,"y":0.2123563289642334},{"tag":"i","x":0.6883180141448975,"y":0.7867722511291504},{"tag":"f","x":0.3093075156211853,"y":0.9398903846740723},{"tag":"f","x":0.4116531014442444,"y":0.2679063081741333},{"tag":"a","x":0.7071211338043213,"y":0.29371726512908936},{"tag":"c","x":0.16701936721801758,"y":0.28365814685821533},{"tag":"j","x":0.2286723256111145,"y":0.269214928150177},{"tag":"d","x":0.16634488105773926,"y":0.9851366877555847},{"tag":"c","x":0.1984427571296692,"y":0.9260008931159973},{"tag":"h","x":0.7724000811576843,"y":0.9107091426849365},{"tag":"j","x":0.7828733325004578,"y":0.2657080292701721},{"tag":"e","x":0.5319154262542725,"y":0.34964942932128906},{"tag":"c","x":4.3488502502441406e-2,"y":0.6190537214279175},{"tag":"f","x":0.44633740186691284,"y":0.25183558464050293},{"tag":"i","x":0.8433091640472412,"y":0.4271125793457031},{"tag":"e","x":0.967324435710907,"y":0.2033606767654419},{"tag":"d","x":0.671101987361908,"y":0.9590272307395935},{"tag":"g","x":0.5955027341842651,"y":0.4679107666015625},{"tag":"f","x":0.9207983016967773,"y":7.917124032974243e-2},{"tag":"e","x":0.25629138946533203,"y":2.0924031734466553e-2},{"tag":"h","x":0.5484182238578796,"y":0.89446622133255},{"tag":"g","x":0.5328551530838013,"y":0.4424973726272583},{"tag":"f","x":0.9732285737991333,"y":0.5688970685005188},{"tag":"i","x":0.9706964492797852,"y":0.9047513604164124},{"tag":"c","x":0.3938533663749695,"y":0.15677368640899658},{"tag":"d","x":0.748245120048523,"y":0.9869019985198975},{"tag":"h","x":0.631543755531311,"y":0.5476692914962769},{"tag":"g","x":0.9574474096298218,"y":0.7811559438705444},{"tag":"c","x":0.11577272415161133,"y":0.6882503032684326},{"tag":"e","x":0.3827521800994873,"y":0.46921753883361816},{"tag":"e","x":0.5296564698219299,"y":3.247523307800293e-2},{"tag":"e","x":0.8823338747024536,"y":0.9922868013381958},{"tag":"a","x":2.5850534439086914e-3,"y":0.7928040027618408},{"tag":"c","x":0.6248728036880493,"y":0.8084238767623901},{"tag":"e","x":0.4718790650367737,"y":0.2412390112876892},{"tag":"c","x":0.14767831563949585,"y":0.2762610912322998},{"tag":"i","x":0.330502450466156,"y":0.3041015863418579},{"tag":"j","x":0.5074955821037292,"y":0.8574939370155334},{"tag":"a","x":0.5957183837890625,"y":0.58250892162323},{"tag":"e","x":0.44432806968688965,"y":0.7042456865310669},{"tag":"f","x":6.755787134170532e-2,"y":0.9560316205024719},{"tag":"i","x":0.577117919921875,"y":0.5104788541793823},{"tag":"e","x":0.44349467754364014,"y":0.6241000890731812},{"tag":"j","x":0.36216408014297485,"y":5.0656795501708984e-2},{"tag":"c","x":0.9253231287002563,"y":0.4546962380409241},{"tag":"g","x":0.18175959587097168,"y":0.5639089345932007},{"tag":"j","x":4.538452625274658e-2,"y":0.667798638343811},{"tag":"g","x":0.4858781099319458,"y":0.654837965965271},{"tag":"a","x":0.1372065544128418,"y":0.9879699945449829},{"tag":"c","x":0.5128378868103027,"y":0.538031280040741},{"tag":"f","x":0.5423336029052734,"y":0.12009698152542114},{"tag":"c","x":0.3819701075553894,"y":0.3312155604362488},{"tag":"b","x":0.18407487869262695,"y":0.5023034811019897},{"tag":"j","x":0.3928583264350891,"y":0.4869365692138672},{"tag":"b","x":0.8531350493431091,"y":0.2430042028427124},{"tag":"g","x":0.4949987530708313,"y":3.5435259342193604e-2},{"tag":"f","x":0.9173020720481873,"y":0.45381754636764526},{"tag":"b","x":0.2909538745880127,"y":0.12964683771133423},{"tag":"f","x":0.9128061532974243,"y":4.847770929336548e-2},{"tag":"d","x":0.602918267250061,"y":0.2017972469329834},{"tag":"j","x":7.814359664916992e-2,"y":0.4179497957229614},{"tag":"j","x":0.5313090682029724,"y":0.385262131690979},{"tag":"g","x":0.5944989919662476,"y":0.3890954852104187},{"tag":"j","x":4.082685708999634e-2,"y":0.5151517987251282},{"tag":"g","x":0.1863141655921936,"y":0.172044038772583},{"tag":"h","x":0.26684606075286865,"y":4.2594075202941895e-3},{"tag":"a","x":0.5081974267959595,"y":0.7359441518783569},{"tag":"b","x":0.6942615509033203,"y":0.5047047138214111},{"tag":"a","x":0.8246698379516602,"y":0.8133428692817688},{"tag":"h","x":0.3627259135246277,"y":0.6756367087364197},{"tag":"h","x":4.698789119720459e-2,"y":0.12035775184631348},{"tag":"b","x":0.9903135895729065,"y":0.2509654760360718},{"tag":"c","x":0.9521323442459106,"y":6.321358680725098e-2},{"tag":"i","x":0.348103404045105,"y":0.17407846450805664},{"tag":"d","x":3.626072406768799e-2,"y":0.9394771456718445},{"tag":"e","x":0.9597281217575073,"y":0.5522154569625854},{"tag":"a","x":0.9070404171943665,"y":0.7608577609062195},{"tag":"f","x":0.5497339963912964,"y":8.383971452713013e-2},{"tag":"e","x":0.5110119581222534,"y":0.9187237024307251},{"tag":"d","x":8.078658580780029e-2,"y":8.87221097946167e-3},{"tag":"f","x":0.1304464340209961,"y":0.4757652282714844},{"tag":"f","x":0.14964228868484497,"y":3.9104342460632324e-2},{"tag":"c","x":0.8647952079772949,"y":0.43305128812789917},{"tag":"b","x":0.6753919124603271,"y":0.9627551436424255},{"tag":"h","x":0.22164326906204224,"y":0.9486547708511353},{"tag":"j","x":0.23742926120758057,"y":0.7539982795715332},{"tag":"i","x":0.9838802814483643,"y":0.7751061320304871},{"tag":"c","x":0.5770595073699951,"y":0.7527228593826294},{"tag":"j","x":0.5123244524002075,"y":0.19890618324279785},{"tag":"f","x":0.828194797039032,"y":0.6226693987846375}]},"encoding":{"color":{"field":"tag","scale":{"scheme":{"count":10,"name":"purples"}},"type":"nominal"},"x":{"axis":{"title":"The x values"},"field":"x","scale":{"domain":[0,1.2]},"type":"quantitative"},"y":{"axis":{"title":"The very important y values","titleFontSize":18},"field":"y","scale":{"type":"log"},"type":"quantitative"}},"height":200,"mark":"point","title":{"font":"Hasklug Nerd Font","fontStyle":"italic","text":"Wide purple plot >>="},"width":600};
  vegaEmbed('#vis', spec).then(function(result) {
  // Access the Vega view instance (https://vega.github.io/vega/docs/api/view/) as result.view
  }).catch(console.error);
</script>

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified DataFrame.Typed as DT
import ExampleData (LabeledDfSchema)
import qualified Graphics.Vega.VegaLite as V

hvegaScatterConfig :: DT.TypedDataFrame LabeledDfSchema -> IO ()
hvegaScatterConfig df =
  let vegaColumns =
        [ V.dataColumn "x" (V.Numbers (DT.columnAsList @"x" df)),
          V.dataColumn "y" (V.Numbers (DT.columnAsList @"y" df)),
          V.dataColumn "tag" (V.Strings ((Text.pack . pure <$> DT.columnAsList @"tag" df)))
        ]
      vegaData = foldl' (.) (V.dataFromColumns []) vegaColumns
      enc =
        V.encoding
          . V.position
            V.X
            [ V.PName "x",
              V.PmType V.Quantitative,
              -- control axis extent
              V.PScale [V.SDomain (V.DNumbers [0, 1.2])],
              V.PAxis [V.AxTitle "The x values"]
            ]
          . V.position
            V.Y
            [ V.PName "y",
              V.PmType V.Quantitative,
              -- set a log scale on y
              V.PScale [V.SType V.ScLog],
              V.PAxis [V.AxTitle "The very important y values", V.AxTitleFontSize 18]
            ]
          -- color the points based on tag using the "purples" scale
          . V.color [V.MName "tag", V.MmType V.Nominal, V.MScale [V.SScheme "purples" [10]]]
      -- set a different title with a different font
      title = V.title "Wide purple plot >>=" [V.TFont "Hasklug Nerd Font", V.TFontStyle "italic"]
   in V.toHtmlFile "plots/vegaScatterConfig.html" $
        V.toVegaLite
          [ vegaData [],
            V.mark V.Point [],
            enc [],
            title,
            -- change the plot dimensions
            V.width 600,
            V.height 200,
            -- set a background color even though that wasn't part of the challenge
            V.background "rgba(20, 0, 50, 0.2)"
          ]
```

If you also have the `Hasklug Nerd Font`, you'll see nice ligatures on the `>>=` in the plot title.

As a Vega / Vega Lite novice, I didn't have the easiest time figuring out what values I needed to provide in order to
configure the different aspects of the plot, and this example is about three times as many lines of code as the example
in part 1. The trade for this complexity is _power_.[^1]

`hvega` [targets version 4] of the Vega specification. The current version of the Vega specification is version 6.
One cost of targeting an older version of the specification is that if you click on the three dots next to the plot
and choose "Open in Vega Editor," you'll get a warning about how the editor wants version 6, but if you want to edit
the plot using Vega Editor, you can just lie and bump the `"$scheme"` property to `v6.json` instead.

### `chart-svg`

[`chart-svg` part 1]

<div class="flex-container">
<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" width="600" height="200" viewBox="-1.5 -0.5 3.0 1.0">
        <style>
                svg {color-scheme: light dark;} {.canvas g, .title g, .axisbar
                g, .ticktext g, .tickglyph g, .ticklines g, .legendContent g
                text {fill: rgb(5%, 5%, 5%);} .ticklines g, .tickglyph g,
                .legendBorder g {stroke: rgb(5%, 5%, 5%);} .legendBorder g
                {fill: rgb(94%, 94%, 94%);}} @media (prefers-color-scheme:dark)
                {.canvas g, .title g, .axisbar g, .ticktext g, .tickglyph g,
                .ticklines g, .legendContent g text {fill: rgb(94%, 94%, 94%);}
                .ticklines g, .tickglyph g, .legendBorder g {stroke: rgb(94%,
                94%, 94%);} .legendBorder g {fill: rgb(5%, 5%, 5%);}}svg {
                font-family: "Hasklug Nerd Font"; }
        </style>
        <g class="chart">
                <g class="titled-scatter">
                        <g stroke-width="0.0030" stroke="rgb(2%, 29%, 48%)" stroke-opacity="1.0" fill="rgb(96%, 60%, 92%)" fill-opacity="1.0">
                                <rect width="0.0150" height="0.0150" x="-0.4516" y="-0.2905"/>
                                <rect width="0.0150" height="0.0150" x="1.3007" y="0.2825"/>
                                <rect width="0.0150" height="0.0150" x="0.6275" y="-0.0270"/>
                                <rect width="0.0150" height="0.0150" x="1.0984" y="-0.1041"/>
                                <rect width="0.0150" height="0.0150" x="0.5489" y="0.1209"/>
                                <rect width="0.0150" height="0.0150" x="-0.5807" y="-0.2152"/>
                                <rect width="0.0150" height="0.0150" x="-0.0901" y="-0.4055"/>
                                <rect width="0.0150" height="0.0150" x="1.1565" y="-0.1553"/>
                                <rect width="0.0150" height="0.0150" x="0.7492" y="-0.0895"/>
                                <rect width="0.0150" height="0.0150" x="-0.0791" y="0.2613"/>
                                <rect width="0.0150" height="0.0150" x="0.6496" y="0.1597"/>
                                <rect width="0.0150" height="0.0150" x="-0.3593" y="0.2558"/>
                                <rect width="0.0150" height="0.0150" x="0.3978" y="-0.3511"/>
                                <rect width="0.0150" height="0.0150" x="1.3749" y="0.1824"/>
                                <rect width="0.0150" height="0.0150" x="0.3339" y="-0.1332"/>
                                <rect width="0.0150" height="0.0150" x="0.9458" y="-0.0578"/>
                                <rect width="0.0150" height="0.0150" x="-0.3216" y="-0.1276"/>
                                <rect width="0.0150" height="0.0150" x="-0.1031" y="0.1486"/>
                                <rect width="0.0150" height="0.0150" x="-0.4147" y="0.0873"/>
                                <rect width="0.0150" height="0.0150" x="-0.3674" y="0.0936"/>
                                <rect width="0.0150" height="0.0150" x="-0.9868" y="0.1108"/>
                                <rect width="0.0150" height="0.0150" x="-0.3707" y="-0.2156"/>
                                <rect width="0.0150" height="0.0150" x="1.3539" y="-0.3410"/>
                                <rect width="0.0150" height="0.0150" x="-1.0505" y="-0.2919"/>
                                <rect width="0.0150" height="0.0150" x="0.0017" y="0.0517"/>
                                <rect width="0.0150" height="0.0150" x="-0.3801" y="-0.1526"/>
                                <rect width="0.0150" height="0.0150" x="-0.0526" y="-0.2982"/>
                                <rect width="0.0150" height="0.0150" x="0.3089" y="-0.2065"/>
                                <rect width="0.0150" height="0.0150" x="-1.0514" y="0.2346"/>
                                <rect width="0.0150" height="0.0150" x="-0.7235" y="-0.3110"/>
                                <rect width="0.0150" height="0.0150" x="1.4133" y="-0.3436"/>
                                <rect width="0.0150" height="0.0150" x="-0.9486" y="0.1274"/>
                                <rect width="0.0150" height="0.0150" x="0.7736" y="-0.2738"/>
                                <rect width="0.0150" height="0.0150" x="1.2207" y="-0.1758"/>
                                <rect width="0.0150" height="0.0150" x="0.1509" y="0.0935"/>
                                <rect width="0.0150" height="0.0150" x="0.1270" y="-0.3922"/>
                                <rect width="0.0150" height="0.0150" x="0.5379" y="-0.0709"/>
                                <rect width="0.0150" height="0.0150" x="-0.8900" y="-0.2870"/>
                                <rect width="0.0150" height="0.0150" x="0.7295" y="-0.0584"/>
                                <rect width="0.0150" height="0.0150" x="0.0366" y="-0.3371"/>
                                <rect width="0.0150" height="0.0150" x="1.2961" y="-0.1638"/>
                                <rect width="0.0150" height="0.0150" x="0.5369" y="0.0382"/>
                                <rect width="0.0150" height="0.0150" x="-1.1378" y="0.0930"/>
                                <rect width="0.0150" height="0.0150" x="-0.7716" y="-0.3700"/>
                                <rect width="0.0150" height="0.0150" x="0.7358" y="0.0789"/>
                                <rect width="0.0150" height="0.0150" x="0.4442" y="-0.2721"/>
                                <rect width="0.0150" height="0.0150" x="-0.1756" y="-0.0474"/>
                                <rect width="0.0150" height="0.0150" x="-0.7149" y="0.0063"/>
                                <rect width="0.0150" height="0.0150" x="-0.0993" y="0.0637"/>
                                <rect width="0.0150" height="0.0150" x="0.5462" y="0.1884"/>
                                <rect width="0.0150" height="0.0150" x="-0.2619" y="-0.1577"/>
                                <rect width="0.0150" height="0.0150" x="-0.2910" y="-0.1320"/>
                                <rect width="0.0150" height="0.0150" x="-0.6835" y="0.1384"/>
                                <rect width="0.0150" height="0.0150" x="0.5587" y="0.2898"/>
                                <rect width="0.0150" height="0.0150" x="0.0356" y="0.0188"/>
                                <rect width="0.0150" height="0.0150" x="0.2595" y="-0.2427"/>
                                <rect width="0.0150" height="0.0150" x="0.3423" y="-0.0524"/>
                                <rect width="0.0150" height="0.0150" x="1.0371" y="-0.3846"/>
                                <rect width="0.0150" height="0.0150" x="0.8184" y="0.1063"/>
                                <rect width="0.0150" height="0.0150" x="-1.1972" y="0.1469"/>
                                <rect width="0.0150" height="0.0150" x="1.3645" y="0.2851"/>
                                <rect width="0.0150" height="0.0150" x="-0.6515" y="-0.3517"/>
                                <rect width="0.0150" height="0.0150" x="0.0575" y="-0.1836"/>
                                <rect width="0.0150" height="0.0150" x="-0.7569" y="-0.0668"/>
                                <rect width="0.0150" height="0.0150" x="0.4221" y="-0.2815"/>
                                <rect width="0.0150" height="0.0150" x="0.4948" y="0.2671"/>
                                <rect width="0.0150" height="0.0150" x="-0.2792" y="0.0120"/>
                                <rect width="0.0150" height="0.0150" x="1.1261" y="-0.4041"/>
                                <rect width="0.0150" height="0.0150" x="-0.2337" y="-0.3893"/>
                                <rect width="0.0150" height="0.0150" x="1.1526" y="0.1029"/>
                                <rect width="0.0150" height="0.0150" x="1.2041" y="-0.1516"/>
                                <rect width="0.0150" height="0.0150" x="-0.8203" y="-0.1948"/>
                                <rect width="0.0150" height="0.0150" x="-0.3841" y="-0.2139"/>
                                <rect width="0.0150" height="0.0150" x="0.8738" y="-0.0775"/>
                                <rect width="0.0150" height="0.0150" x="-0.2311" y="-0.3663"/>
                                <rect width="0.0150" height="0.0150" x="-0.3889" y="-0.2139"/>
                                <rect width="0.0150" height="0.0150" x="0.6564" y="0.1757"/>
                                <rect width="0.0150" height="0.0150" x="0.0304" y="-0.2503"/>
                                <rect width="0.0150" height="0.0150" x="0.8293" y="0.2510"/>
                                <rect width="0.0150" height="0.0150" x="-0.6900" y="0.1359"/>
                                <rect width="0.0150" height="0.0150" x="1.3539" y="-0.3232"/>
                                <rect width="0.0150" height="0.0150" x="0.1831" y="-0.1504"/>
                                <rect width="0.0150" height="0.0150" x="-1.0879" y="-0.2250"/>
                                <rect width="0.0150" height="0.0150" x="-1.1932" y="0.0756"/>
                                <rect width="0.0150" height="0.0150" x="0.7193" y="-0.3236"/>
                                <rect width="0.0150" height="0.0150" x="1.0135" y="-0.2757"/>
                                <rect width="0.0150" height="0.0150" x="1.3628" y="-0.0733"/>
                                <rect width="0.0150" height="0.0150" x="-0.6028" y="0.1840"/>
                                <rect width="0.0150" height="0.0150" x="-0.6708" y="-0.0725"/>
                                <rect width="0.0150" height="0.0150" x="-0.0173" y="-0.1282"/>
                                <rect width="0.0150" height="0.0150" x="-1.2270" y="-0.3498"/>
                                <rect width="0.0150" height="0.0150" x="0.4452" y="-0.1233"/>
                                <rect width="0.0150" height="0.0150" x="0.7288" y="-0.0329"/>
                                <rect width="0.0150" height="0.0150" x="0.1125" y="0.2305"/>
                                <rect width="0.0150" height="0.0150" x="-0.4432" y="-0.3266"/>
                                <rect width="0.0150" height="0.0150" x="-0.3392" y="-0.2192"/>
                                <rect width="0.0150" height="0.0150" x="-0.1832" y="-0.0615"/>
                                <rect width="0.0150" height="0.0150" x="1.1444" y="-0.2296"/>
                                <rect width="0.0150" height="0.0150" x="0.7076" y="-0.1043"/>
                                <rect width="0.0150" height="0.0150" x="-0.7236" y="-0.2183"/>
                        </g>
                </g>
                <g class="datapadding">
        </g>
</g>
<g class="hud">
        <g class="title">
                <g stroke-width="0.0" stroke="none" fill="rgb(5%, 5%, 5%)" fill-opacity="1.0" font-size="0.0500" text-anchor="middle">
                        <text x="0.1022" y="-0.4433">
                                titled scatter
                        </text>
                </g>
        </g>
        <g class="frame">
                <g stroke-width="0" stroke="rgb(0%, 0%, 0%)" stroke-opacity="0" fill="rgb(100%, 100%, 100%)" fill-opacity="0.02">
                        <rect width="2.6595" height="0.7082" x="-1.2276" y="-0.4044"/>
                </g>
        </g>
        <g class="title">
                <g stroke-width="0.0" stroke="none" fill="rgb(5%, 5%, 5%)" fill-opacity="1.0" font-size="0.12" text-anchor="middle">
                        <text x="0.1022" y="0.4001">
                                x label
                        </text>
                </g>
        </g>
        <g class="title">
                <g stroke-width="0.0" stroke="none" fill="rgb(5%, 5%, 5%)" fill-opacity="1.0" font-size="0.12" text-anchor="middle">
                        <text x="-1.2957" y="-0.0230" transform="rotate(-90.0, -1.2957, -0.0230)">
                                y label
                        </text>
                </g>
        </g>
        <g class="axis">
                <g class="axisbar">
                        <g stroke-width="0" stroke="rgb(0%, 0%, 0%)" stroke-opacity="0" fill="rgb(5%, 5%, 5%)" fill-opacity="0.4">
                                <rect width="2.6631" height="0.0028" x="-1.2294" y="0.3109"/>
                        </g>
                </g>
                <g class="ticks">
                        <g class="ticklines">
                                <g stroke-width="0.0050" stroke="rgb(5%, 5%, 5%)" stroke-opacity="0.05" fill="none">
                                        <polyline points="-1.2276,0.3038 -1.2276,-0.3999"/>
                                        <polyline points="-0.6957,0.3038 -0.6957,-0.3999"/>
                                        <polyline points="-0.1638,0.3038 -0.1638,-0.3999"/>
                                        <polyline points="0.3681,0.3038 0.3681,-0.3999"/>
                                        <polyline points="0.9000,0.3038 0.9000,-0.3999"/>
                                        <polyline points="1.4319,0.3038 1.4319,-0.3999"/>
                                </g>
                        </g>
                        <g class="tickglyph">
                                <g stroke-width="0.0028" stroke="rgb(5%, 5%, 5%)" stroke-opacity="0.4" fill="rgb(5%, 5%, 5%)" fill-opacity="0.4">
                                        <polyline points="-1.2276,0.3336
                                                -1.2276,0.3123"/>
                                                <polyline points="-0.6957,0.3336 -0.6957,0.3123"/>
                                                <polyline points="-0.1638,0.3336 -0.1638,0.3123"/>
                                                <polyline points="0.3681,0.3336 0.3681,0.3123"/>
                                                <polyline points="0.9000,0.3336 0.9000,0.3123"/>
                                                <polyline points="1.4319,0.3336
                                                        1.4319,0.3123"/>
                                                </g>
                                        </g>
                                        <g class="ticktext">
                                                <g stroke-width="0.0" stroke="none" fill="rgb(5%, 5%, 5%)" fill-opacity="1.0" font-size="0.0400" text-anchor="middle">
                                                        <text x="-1.2276" y="0.4632">
                                                                0.0
                                                        </text>
                                                        <text x="-0.6957" y="0.4632">
                                                                0.2
                                                        </text>
                                                        <text x="-0.1638" y="0.4632">
                                                                0.4
                                                        </text>
                                                        <text x="0.3681" y="0.4632">
                                                                0.6
                                                        </text>
                                                        <text x="0.9000" y="0.4632">
                                                                0.8
                                                        </text>
                                                        <text x="1.4319" y="0.4632">
                                                                1.0
                                                        </text>
                                                </g>
                                        </g>
                                </g>
                        </g>
                        <g class="axis">
                                <g class="axisbar">
                                        <g stroke-width="0" stroke="rgb(0%, 0%, 0%)" stroke-opacity="0" fill="rgb(5%, 5%, 5%)" fill-opacity="0.4">
                                                <rect width="0.0036" height="0.7066" x="-1.2401" y="-0.4014"/>
                                        </g>
                                </g>
                                <g class="ticks">
                                        <g class="ticklines">
                                                <g stroke-width="0.0050" stroke="rgb(5%, 5%, 5%)" stroke-opacity="0.05" fill="none">
                                                        <polyline points="-1.2276,0.3038 1.4319,0.3038"/>
                                                        <polyline points="-1.2276,0.1631 1.4319,0.1631"/>
                                                        <polyline points="-1.2276,0.0223 1.4319,0.0223"/>
                                                        <polyline points="-1.2276,-0.1184 1.4319,-0.1184"/>
                                                        <polyline points="-1.2276,-0.2592 1.4319,-0.2592"/>
                                                        <polyline points="-1.2276,-0.3999 1.4319,-0.3999"/>
                                                </g>
                                        </g>
                                        <g class="tickglyph">
                                                <g stroke-width="0.0036" stroke="rgb(5%, 5%, 5%)" stroke-opacity="0.4" fill="rgb(5%, 5%, 5%)" fill-opacity="0.4">
                                                        <polyline points="-1.2652,0.3038
                                                                -1.2383,0.3038"/>
                                                                <polyline points="-1.2652,0.1631 -1.2383,0.1631"/>
                                                                <polyline points="-1.2652,0.0223 -1.2383,0.0223"/>
                                                                <polyline points="-1.2652,-0.1184 -1.2383,-0.1184"/>
                                                                <polyline points="-1.2652,-0.2592 -1.2383,-0.2592"/>
                                                                <polyline points="-1.2652,-0.3999
                                                                        -1.2383,-0.3999"/>
                                                                </g>
                                                        </g>
                                                        <g class="ticktext">
                                                                <g stroke-width="0.0" stroke="none" fill="rgb(5%, 5%, 5%)" fill-opacity="1.0" font-size="0.0400" text-anchor="end">
                                                                        <text x="-1.3996" y="0.3116">
                                                                                0.0
                                                                        </text>
                                                                        <text x="-1.3996" y="0.1709">
                                                                                0.2
                                                                        </text>
                                                                        <text x="-1.3996" y="0.0301">
                                                                                0.4
                                                                        </text>
                                                                        <text x="-1.3996" y="-0.1106">
                                                                                0.6
                                                                        </text>
                                                                        <text x="-1.3996" y="-0.2514">
                                                                                0.8
                                                                        </text>
                                                                        <text x="-1.3996" y="-0.3922">
                                                                                1.0
                                                                        </text>
                                                                </g>
                                                        </g>
                                                </g>
                                        </g>
                                        <g class="frame">
                                                <g stroke-width="0" stroke="rgb(0%, 0%, 0%)" stroke-opacity="0" fill="rgb(0%, 0%, 0%)" fill-opacity="0">
                                                        <rect width="3.0" height="1.0" x="-1.5" y="-0.5"/>
</g>
</g>
</g>
</svg>
</div>


```haskell 
chartSvgScatter :: DT.TypedDataFrame LabeledDfSchema -> IO ()
chartSvgScatter df =
  let xs = DT.columnAsList @"x" df
      ys = DT.columnAsList @"y" df
      points = zipWith ChartSVG.Point xs ys
      -- change mark color
      style = ChartSVG.defaultGlyphStyle & #color .~ ChartSVG.palette 123 & #size .~ 0.015
      chart = ChartSVG.GlyphChart style points
      scatterExample =
        mempty
          -- title a plot
          & set #chartTree (ChartSVG.named "titled-scatter" [chart])
          & #hudOptions
            .~ ( ChartSVG.defaultHudOptions
                   -- title a plot
                   & #titles
                     .~ [ ChartSVG.Priority 0 $
                            ChartSVG.defaultTitleOptions "<$> titled scatter <$>"
                              & #style % #size .~ 0.05,
                          -- add specific labels for x and y
                          ChartSVG.Priority 1 $
                            ChartSVG.defaultTitleOptions "x label" & #place .~ PlaceBottom,
                          ChartSVG.Priority 2 $
                            ChartSVG.defaultTitleOptions "y label"
                              & #place .~ PlaceLeft
                        ]
               )
          -- change font
          & #markupOptions % #cssOptions % #fontFamilies .~ "svg { font-family: \"Hasklug Nerd Font\"; }"
          -- resize the plot
          & #markupOptions % #markupHeight .~ Just 200
          & #markupOptions % #chartAspect .~ ChartSVG.FixedAspect 3 ::
          ChartSVG.ChartOptions
   in ChartSVG.writeChartOptions "plots/chartSvgScatterConfig.svg" scatterExample
```

Configuring some parts of the `chart-svg` output were easier than others. The use of the 
[`OverloadedLabels`] with `optics` in the examples really pays off when trying to configure any part of the chart.
[`chart-svg` docs] note:

> `Chart`, `HudOptions` and associated chart configuration types are big and sometimes deep syntax trees, and simple
> optics; getting, setting and modding, makes manipulation more pleasant.

I agree. Especially because I have a single module with a bunch of qualified imports, the alternative where I would
have used record update syntax instead seemed really tedious.

Even when they're easy to set though, some of the styles are just raw CSS strings, like many of the properties under
`cssOptions`. To set font for a title, I used

```haskell
mempty
  -- ...
  & #markupOptions % #cssOptions % #fontFamilies .~ something
```

That `something` is a raw bytestring, which it's on you as the developer to ensure is valid CSS targeting the thing
you care about. Some people spend a lot of time writing CSS and are good at it. I am not, so this was a rough API for
me.

I did not figure out how to log scale one of the axes or change the axis limits.[^2]

### `Chart`

[`Chart` part 1]

[`dataframe`]: #dataframe
[`granite`]: #granite
[`hvega`]: #hvega
[`chart-svg`]: #chart-svg
[`Chart`]: #chart
[`dataframe` part 1]: ./2026-03-05-Haskell-data-visualization.html#dataframe
[`granite` part 1]: ./2026-03-05-Haskell-data-visualization.html#granite
[`hvega` part 1]: ./2026-03-05-Haskell-data-visualization.html#hvega
[`chart-svg` part 1]: ./2026-03-05-Haskell-data-visualization.html#chart-svg
[`Chart` part 1]: ./2026-03-05-Haskell-data-visualization.html#chart
[part one]: ./2026-03-05-Haskell-data-visualization.html
[`plotScatter`]: https://hackage-content.haskell.org/package/dataframe-0.7.0.0/docs/DataFrame-Display-Web-Plot.html#v:plotScatter
[`PlotConfig`]: https://hackage-content.haskell.org/package/dataframe-0.7.0.0/docs/DataFrame-Display-Web-Plot.html#t:PlotConfig
[`defPlot`]: https://hackage-content.haskell.org/package/granite-0.4.0.0/docs/Granite.html#v:defPlot
[targets version 4]: https://github.com/DougBurke/hvega/blob/903a146eb6e659267d1768f74d20247869785234/README.md#package-hvega
[v5.0.0]: https://github.com/vega/vega/releases/tag/v5.0.0
[`chart-svg` docs]: https://hackage-content.haskell.org/package/chart-svg-0.8.3.2/docs/Chart.html#g:7
[`OverloadedLabels`]: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_labels.html
[^1]: Not that changing axis labels is the most power anyone can imagine in a charting library, but it's more power
than _not_ changing axis labels.
[^2]:  I also somehow broke the exported `svg` by including `<$>` in the title text for the main plot, so I've edited
it slightly here so it will embed in this page correctly. The export went fine when saving it off as its own file, so
I'm guessing it's something to do with pandoc's render of the markdown containing the svg, but I don't really know.
Anyway, I don't think it was `chart-svg`'s fault. Computers, man.
