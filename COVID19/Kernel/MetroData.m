(* Wolfram Language Package *)

BeginPackage["COVID19`"]

COVID19`MetroData
COVID19`DeployMetroData
Begin["`Private`"] 

COVID19`MetroData[args___]:=Catch[metroData[args]]

Options[COVID19`MetroData]=Options[metroData]={"UpdateData"->False,"SmoothingDays"->7,"MinimumCases"->50,"MinimumDeaths"->10,"MetroName"->""};

metroData[counties:{_Entity..},opts:OptionsPattern[]]:=Module[
	{timeseries, totalpop,mintime,maxtime, cases, deaths,alignedcases,aligneddeaths},
	updateProgress[$covidprogessid, "Finding Populations"];
	totalpop = Total[#["Population"] & /@ counties]; (* entity cache will store populations for all counties *)
	
	updateProgress[$covidprogessid, "Getting Data From NY Times"];
	timeseries = If[TrueQ[OptionValue["UpdateData"]],
		COVID19`NYTimesData["USCountiesTimeSeries","New"],
		COVID19`NYTimesData["USCountiesTimeSeries"]
	];
	
	updateProgress[$covidprogessid, "Processing data"];
	timeseries = KeyTake[timeseries[All, KeyDrop["State"]], counties];
	If[!TrueQ[Length[timeseries]>0],Throw[Failure["nocountydata",<|"Message"->"No Counties matched your spec."|>]]];
	mintime = timeseries[Min, All, #["FirstDate"] &];
	maxtime = timeseries[Max, All, #["LastDate"] &];
	updateProgress[$covidprogessid, "Totalling Metro Area"];
	timeseries = timeseries[All, All, TimeSeries[TimeSeriesInsert[#, {mintime, 0}]["DatePath"], 
 			ResamplingMethod -> {"Interpolation", InterpolationOrder -> 0}] &];
	timeseries = Prepend[timeseries, "Metro Area Total" -> Normal[timeseries[Total]]];
	
	updateProgress[$covidprogessid, "Formatting Time Series"];
	cases = timeseries[All,ResourceFunction["TimeSeriesSelect"][#Cases, #2 > OptionValue["MinimumCases"] &] &][Select[#["PathLength"] > OptionValue["SmoothingDays"] &]];
	deaths = timeseries[All,ResourceFunction["TimeSeriesSelect"][#Deaths, #2 > OptionValue["MinimumDeaths"] &] &][Select[#["PathLength"] > OptionValue["SmoothingDays"] &]];
	updateProgress[$covidprogessid, "Creating Timeline Grid"];
	{metroPlotGrid[
		counties,
		cases,
		deaths,
		maxtime,
		OptionValue["MetroName"]<>"Metro Area COVID-19 Timelines",
		opts
	]
	,
	updateProgress[$covidprogessid, "Creating Data Table"];
	Labeled[timeseries[All, {"Cases", "Deaths"}, tabledata],DateString[maxtime, "Date"], Top]
	}
]

metroData[area_]:=Block[{$covidprogessid=CreateUUID[]},
	updateProgress[$covidprogessid, "Finding Counties"];
	metroData[MetroAreaCounties[area]]
]


metroData[___]:=$Failed

pop[counties_,Key[ent_Entity]] := ent["Population"]
pop[counties_,Key["Metro Area Total"]] = Total[#["Population"] & /@ counties]

tozerotime[ts_] := TimeSeries[With[{first = #[[1, 1]]},
     MapAt[(# - first)/86400 &, #, {All, 1}]] &[ts["Path"]]]

stylemap[counties_]:= (stylemap[counties]=MapIndexed[# -> ColorData[1][#2[[1]]] &,Prepend[counties, "Metro Area Total"]]);

optsfunc[counties_,data_] := {PlotRange -> {{"March 12", Automatic}, Automatic},
    ImageSize -> 400, PlotLegends -> None, 
   FrameTicks -> {{Automatic, All}, {Automatic, False}}, 
   PlotStyle -> Values@KeyTake[stylemap[counties], Normal@Keys[data]], 
   GridLines -> {None, Automatic}};
   
   
metroPlotGrid[
	counties_,
	cases_,
	deaths_,
	maxtime_,
	title_:"Metro Area COVID-19 Timelines",
	opts:OptionsPattern[metroData]
]:=With[{smoothing=OptionValue["SmoothingDays"],alignedcases = cases[All, ResourceFunction["TimeSeriesAlign"][#, 0] &][All, tozerotime],
	aligneddeaths = deaths[All, ResourceFunction["TimeSeriesAlign"][#, 0] &][All, tozerotime]},
	updateProgress[$covidprogessid, "Cumulative Plots"];
Grid[{{
	Style[Column[{title, "From NY Times data on " <> DateString[maxtime, "DateShort"]}, Alignment -> Center], 24], SpanFromLeft, ""},
	Append[Style[#, 18, Italic] & /@ {
		"Cases (> "<>ToString[OptionValue["MinimumCases"]]<>")", "Deaths (> " <> ToString[OptionValue["MinimumDeaths"]] <> ")"}, SpanFromAbove],
  {DateListLogPlot[cases, optsfunc[counties,cases], 
    PlotLabel -> "Cumulative (Log)"],
   DateListLogPlot[deaths, optsfunc[counties,cases], 
    PlotLabel -> "Cumulative (Log)"], 
   SwatchLegend[Lookup[stylemap[counties], #], #] &@
    ResourceFunction["SortLike"][Normal[Union[Keys[cases], Keys[deaths]]], 
     Prepend[counties, "Metro Area Total"]]},
  {
	updateProgress[$covidprogessid, "Per Capita Plots"];
  	DateListLogPlot[cases[MapIndexed[#1/QuantityMagnitude[pop[counties,#2[[1]]]] &]], optsfunc[counties,cases], PlotLabel -> "Cumulative per Capita (Log)"],
   DateListLogPlot[deaths[MapIndexed[#1/QuantityMagnitude[pop[counties,#2[[1]]]] &]],optsfunc[counties,deaths], PlotLabel -> "Cumulative per Capita (Log)"], SpanFromAbove},
  {
	updateProgress[$covidprogessid, "Daily Cases Plots"];
	DateListPlot[cases[All, (MovingAverage[#, Quantity[smoothing, "Days"]] &) /* Differences], optsfunc[counties,cases], 
    PlotLabel ->"New Cases (" <> ToString[smoothing] <> " day average)"],
   DateListPlot[deaths[All, (MovingAverage[#, Quantity[smoothing, "Days"]] &) /* Differences], optsfunc[counties,deaths], 
    PlotLabel ->"New Deaths (" <> ToString[smoothing] <> " day average)"], SpanFromAbove},
  {
	updateProgress[$covidprogessid, "Growth Ratio Plots"];DateListPlot[cases[All, (MovingAverage[#, Quantity[smoothing, "Days"]] &) /*Ratios], optsfunc[counties,cases], 
    PlotLabel -> "Growth Ratio (" <> ToString[smoothing] <> " day average)"],
   DateListPlot[deaths[All, (MovingAverage[#, Quantity[smoothing, "Days"]] &) /* Ratios], optsfunc[counties,deaths], 
    PlotLabel -> "Growth Ratio (" <> ToString[smoothing] <> " day average)"], SpanFromAbove},
  {
	updateProgress[$covidprogessid, "Aligned Cumulative Plots"];
	growthPlotWithTrendLines[alignedcases,"Cases", Sequence @@ Normal@KeyDrop[optsfunc[counties,alignedcases], PlotRange], 
    PlotLabel -> "Aligned Cumulative Plot (Log vs Days since "<>ResourceFunction["OrdinalNumberString"][OptionValue["MinimumCases"]]<>" case)"],
  	growthPlotWithTrendLines[aligneddeaths,"Deaths", Sequence @@ Normal@KeyDrop[optsfunc[counties,aligneddeaths], PlotRange], 
    PlotLabel -> "Aligned Cumulative Plot (Log vs Days since "<>ResourceFunction["OrdinalNumberString"][OptionValue["MinimumDeaths"]]<>" death)"],
   SpanFromAbove}
  }]
]


tabledata[ts_] := With[{path = Normal[ts]},
  Association @@ {
    "Total Today" -> path[[-1, 2]],
    "New Today (Yesterday)" -> 
     Row[{(path[[-1, 2]] - path[[-2, 2]]), 
       " (", (path[[-2, 2]] - path[[-3, 2]]), ")"}],
    "New This Week (Last Week)" -> 
     Row[{path[[-1, 2]] - path[[-8, 2]], 
       " (", (path[[-8, 2]] - path[[-15, 2]]), ")"}]
    }
  ]

DeployMetroData[area_,location_]:=(Quiet[DeleteObject[CloudObject[location]];
	With[{res=COVID19`MetroData[area]},
		CloudDeploy[Notebook[{Cell["Timelines", "Section"],
		   Cell[BoxData[ToBoxes[res[[1]]]], "Output"],
		   Cell["Data", "Section"],
		   Cell[BoxData[ToBoxes[res[[2]]]], "Output"]
		   }, "ClickToCopyEnabled" -> 
		   False], location, 
		 Permissions -> "Public"]
		]
])

End[] 
EndPackage[]