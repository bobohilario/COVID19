(* Wolfram Language Package *)

BeginPackage["COVID19`"]

COVID19`GrowthRatePlot
Begin["`Private`"] 

COVID19`GrowthRatePlot[args___]:=Catch[growthRatePlot[args]]

Options[COVID19`GrowthRatePlot]=Options[growthRatePlot]={"SmoothingDays"->7,"MinimumCases"->50,"MinimumDeaths"->5};


growthRatePlot[___]:=$Failed



growthPlotWithTrendLines[data_, type_, opts___]:=ListLogPlot[data, opts, 
    Joined -> True, Epilog -> {Dashed, makeline[type, #] & /@ {3, 5, 10}}]


$hackValue=7;

linedays["Deaths"] = 12;
linedays[_] = 20;
startlinevalue["Deaths"] := 2.4;
startlinevalue[_] := 4;

makeline[t_, a_] := With[{sl = startlinevalue[t], ld = linedays[t]},
  N@{Darker[ColorData["TemperatureMap"][1/2 + 1/(a/2)], .2],
    Line[{{0, sl}, {ld, sl + (ld/a)}}],
    Text["Doubles every " <> ToString[a] <> " days", 
     textlocation[sl, ld, a]]
    }]


textlocation[sl_, ld_, a_] := If[2^(ld/a) < 7, {ld, sl + (ld/a)},
  With[{d = 
     First[\[FormalX] /. 
       NSolve[2^(\[FormalX]/a) == $hackValue && \[FormalX] > 
          0, \[FormalX]]]},
   {d - 2, sl + N@Log[2, $hackValue]}
   ]
  ]
End[] 
EndPackage[]