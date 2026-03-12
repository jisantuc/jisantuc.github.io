---
title: "Haskell data visualization part 2: Hello, slightly different plots"
date: 2026-03-09
description: |
  Part 2 of a short series on making plots in Haskell. Part 1 covered making some 
  simple scatter plots. This part focuses on changing attributes of your plots.
  Part 3 will cover more complex plotting.
---

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

## `dataframe`

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
import ExampleData (LabeledDfSchema, labeledPointsDf)

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

## `granite`

[`granite` part 1]

[`dataframe`]: #dataframe
[`granite`]: #granite
[`dataframe` part 1]: ./2026-03-05-Haskell-data-visualization.html#dataframe
[`granite` part 1]: ./2026-03-05-Haskell-data-visualization.html#granite
[part one]: ./2026-03-05-Haskell-data-visualization.html
[`plotScatter`]: https://hackage-content.haskell.org/package/dataframe-0.7.0.0/docs/DataFrame-Display-Web-Plot.html#v:plotScatter
[`PlotConfig`]: https://hackage-content.haskell.org/package/dataframe-0.7.0.0/docs/DataFrame-Display-Web-Plot.html#t:PlotConfig
