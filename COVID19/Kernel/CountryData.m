(* Wolfram Language Package *)

BeginPackage["COVID19`"]

COVID19`CountryPlotsAndData
Begin["`Private`"] 

COVID19`CountryPlotsAndData[args___]:=Block[{$covidprogessid=CreateUUID[],
	$CovidEntityType="Country"},
	updateProgress[$covidprogessid, "Finding Countries"];
	Catch[countryData[args]]
]

Options[COVID19`CountryPlotsAndData]=Options[countryData]={"UpdateData"->False,"SmoothingDays"->7,"MinimumCases"->50,"MinimumDeaths"->10,"Name"->""};

countryData[countries:{_Entity..}|All|Automatic,opts:OptionsPattern[]]:=Module[
	{timeseries,mintime,maxtime, cases, deaths,countrylist},
	
	updateProgress[$covidprogessid, "Getting Data From Wolfram Data Repository"];
	timeseries = If[TrueQ[OptionValue["UpdateData"]],
		COVID19`EpidemicData["WorldCountries","New"],
		COVID19`EpidemicData["WorldCountries"]
	];
	
	updateProgress[$covidprogessid, "Processing data"];
	
	timeseries = timeseries[Association, #Country -> 
    Association@@{
    	"Cases"->ResourceFunction["TimeSeriesSelect"][#ConfirmedCases, #2 > OptionValue["MinimumCases"] &],
    	"Deaths"->ResourceFunction["TimeSeriesSelect"][#Deaths, #2 > OptionValue["MinimumDeaths"] &]
    }&];
	Which[ListQ[countries],
		timeseries = KeyTake[timeseries, countries];
		If[!TrueQ[Length[timeseries]>0],Throw[Failure["nocountydata",<|"Message"->"No Countries matched your spec."|>]]],
		countries===Automatic,
		timeseries=timeseries[TakeLargestBy[Max[#Cases]&,9]];
	];
	countrylist=Normal@Keys[timeseries];
	mintime = timeseries[Min, All, #["FirstDate"] &];
	maxtime = timeseries[Max, All, #["LastDate"] &];
	updateProgress[$covidprogessid, "Totalling Countries"];
	timeseries = timeseries[All, All, TimeSeries[TimeSeriesInsert[#, {mintime, 0}]["DatePath"], 
 			ResamplingMethod -> {"Interpolation", InterpolationOrder -> 0}] &];
	timeseries = Prepend[timeseries, "Combined Total" -> Normal[timeseries[Total]]];
	
	updateProgress[$covidprogessid, "Formatting Time Series"];
	cases = timeseries[All,ResourceFunction["TimeSeriesSelect"][#Cases, #2 > OptionValue["MinimumCases"] &] &][Select[#["PathLength"] > OptionValue["SmoothingDays"] &]];
	deaths = timeseries[All,ResourceFunction["TimeSeriesSelect"][#Deaths, #2 > OptionValue["MinimumDeaths"] &] &][Select[#["PathLength"] > OptionValue["SmoothingDays"] &]];
	updateProgress[$covidprogessid, "Creating Timeline Grid"];
	{metroPlotGrid[
		countrylist,
		cases,
		deaths,
		{mintime,maxtime},
		OptionValue["Name"]<>"COVID-19 Timelines",
		opts
	]
	,
	updateProgress[$covidprogessid, "Creating Data Table"];
	Labeled[timeseries[All, {"Cases", "Deaths"}, tabledata],DateString[maxtime, "Date"], Top]
	}
]

countryData[area_, opts:OptionsPattern[]]:=Block[{$covidprogessid=CreateUUID[]},
	updateProgress[$covidprogessid, "Finding Counties"];
	countryData[ChooseCounties[area],opts]
]


countryData[___]:=$Failed


End[] 
EndPackage[]