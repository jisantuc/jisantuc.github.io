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
* [`Chart`]
* [`chart-svg`]

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
  let xs = float2Double <$> DT.columnAsList @"x" df
      ys = float2Double <$> DT.columnAsList @"y" df
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

<!-- TODO: after finishing up todos in goofing-off, include plot -->
<!-- TODO: after finishing up todos in goofing-off, include code -->

[`dataframe`]: #dataframe
[`granite`]: #granite
[`granite`]: #hvega
[`dataframe` part 1]: ./2026-03-05-Haskell-data-visualization.html#dataframe
[`granite` part 1]: ./2026-03-05-Haskell-data-visualization.html#granite
[`hvega` part 1]: ./2026-03-05-Haskell-data-visualization.html#hvega
[part one]: ./2026-03-05-Haskell-data-visualization.html
[`plotScatter`]: https://hackage-content.haskell.org/package/dataframe-0.7.0.0/docs/DataFrame-Display-Web-Plot.html#v:plotScatter
[`PlotConfig`]: https://hackage-content.haskell.org/package/dataframe-0.7.0.0/docs/DataFrame-Display-Web-Plot.html#t:PlotConfig
[`defPlot`]: https://hackage-content.haskell.org/package/granite-0.4.0.0/docs/Granite.html#v:defPlot
