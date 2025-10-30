(* ::Package:: *)

(*Author: Rick*)

(*Email: xie_jinxiao@126.com*)

BeginPackage["PLOT`"]
(*\:7ed8\:56fe\:51fd\:6570*)
plot::usage = "Same as Plot"

logplot::usage = "
New Options 
	additionalticks->
		{1,4,5...} which add more ticks in the y axes 
Same as LogPlot"

loglogplot::usage = "
New Options 
	additionalticks->
		{{1,4,5...},{5...}} which add more ticks in the x and y axes 
Same as LogLogPlot"

listplot::usage = "
New Options 
	PlotLegends->
		{
			{'LegendLabel1','LegendLabel2'...},
			{{LegendOffset ({0,0} set plot legend at the right top side of the plot),LegendLabelMargin}
		},
	errorinterval->{errordata1,errordata2...}
		(errordata1={{downerrorpoint1,uperrorpoint1},{downerrorpoint2,uperrorpoint2},...}),
	markersize->1.5(the amplify size of marker)
Other options are same as ListPlot"

listlogplot::usage = "
New Options 
	PlotLegends->
		{
			{'LegendLabel1','LegendLabel2'...},
			{LegendOffset ({0,0} set plot legend at the right top side of the plot),LegendLabelMargin}
		}
	additionalticks->
		{1,4,5...} which add more ticks in the y axes ,
	errorinterval->{errordata1,errordata2...}
		(errordata1={{downerrorpoint1,uperrorpoint1},{downerrorpoint2,uperrorpoint2},...}),
	markersize->1.5(the amplify size of marker)
Other options are same as ListLogPlot"

listloglogplot::usage = "
New Options 
	PlotLegends->
		{
			{'LegendLabel1','LegendLabel2'...},
			{LegendOffset ({0,0} set plot legend at the right top side of the plot),LegendLabelMargin}
		}
	additionalticks->
		{{1,4,5...},{5...}} which add more ticks in the x and y axes,
	errorinterval->{errordata1,errordata2...}
		(errordata1={{downerrorpoint1,uperrorpoint1},{downerrorpoint2,uperrorpoint2},...}),
	markersize->1.5(the amplify size of marker)
Other options are same as ListLogLogPlot"

listlineplot::usage = "
New Options 
	PlotLegends->
		{
			{'LegendLabel1','LegendLabel2'...},
			{LegendOffset ({0,0} set plot legend at the right top side of the plot),LegendLabelMargin}
		}
Other options are same as ListLinePlot"

listdensityplot::usage = "
New Options 
	plotlegends->
		{
			datarange({-0.8,0,8} means the data cross from -0.8 to 0.8),
			legendsize,
			legendoffset({0,0} first number means the x offset, the other means y)
		}

	legendlabel->'LegendlabelString'
Other options are same as listdensityplot"

listlineplot3d::usage = "A function to draw ridgeline plot. different listlineplots were painted on y axis.
Use Option PlotLegends to add legends on y axis."
(*\:5185\:7f6e\:9ed8\:8ba4\:53c2\:6570*)
colorlistdefault::usage = "An colorlist copy from NATURE"
regionlistdefault::usage = "An markerlist copy from Science"
gradientcolorlist::usage = "An gradient colorlist generator"

Begin["Private`"]
openregion[region_]:=Module[{innerregion,outerregion,finalshape,areafactor,rootfactor,interfactor,finalregion,compactness},
(*\:8c03\:6574\:5916\:90e8\:9762\:79ef\:7684\:5927\:5c0f\:ff0c\:5148\:9762\:79ef\:5f52\:4e00\:5316\:ff0c\:518d\:6839\:636e\:7d27\:51d1\:5ea6\:6307\:6570\:7f29\:653e\:ff0c\:56e0\:4e3a\:677e\:6563\:7684\:56fe\:5f62\:89c6\:89c9\:6548\:679c\:5927*)
areafactor=1/Sqrt[Area[region]];
compactness=((4\[Pi] Area[region])/Perimeter[region]^2)^0.35;
outerregion=RegionResize[region,Scaled[compactness*areafactor]];
rootfactor=
FindRoot[(Area[outerregion]-Area[RegionResize[outerregion,Scaled[interfactor]]])/(Perimeter[outerregion]+Perimeter[RegionResize[outerregion,Scaled[interfactor]]])==0.11,{interfactor,0.5}];
innerregion=RegionResize[outerregion,Scaled[interfactor/.rootfactor[[1]]]];
outerregion=TransformedRegion[outerregion,TranslationTransform[-RegionCentroid@outerregion]];

innerregion=TransformedRegion[innerregion,TranslationTransform[-RegionCentroid@innerregion]];
finalregion=RegionDifference[BoundaryDiscretizeRegion[outerregion],BoundaryDiscretizeRegion[innerregion]]
];
openregionInner[region_]:=Module[{innerregion,outerregion,finalshape,areafactor,rootfactor,interfactor,finalregion,compactness},
(*\:8c03\:6574\:5916\:90e8\:9762\:79ef\:7684\:5927\:5c0f\:ff0c\:5148\:9762\:79ef\:5f52\:4e00\:5316\:ff0c\:518d\:6839\:636e\:7d27\:51d1\:5ea6\:6307\:6570\:7f29\:653e\:ff0c\:56e0\:4e3a\:677e\:6563\:7684\:56fe\:5f62\:89c6\:89c9\:6548\:679c\:5927*)
areafactor=1/Sqrt[Area[region]];
compactness=((4\[Pi] Area[region])/Perimeter[region]^2)^0.35;
outerregion=RegionResize[region,Scaled[compactness*areafactor]];
rootfactor=
FindRoot[(Area[outerregion]-Area[RegionResize[outerregion,Scaled[interfactor]]])/(Perimeter[outerregion]+Perimeter[RegionResize[outerregion,Scaled[interfactor]]])==0.11,{interfactor,0.5}];
innerregion=RegionResize[outerregion,Scaled[interfactor/.rootfactor[[1]]]];
innerregion=TransformedRegion[innerregion,TranslationTransform[-RegionCentroid@innerregion]];
BoundaryDiscretizeRegion[innerregion]
];
closeregion[region_]:=Module[{innerregion,outerregion,finalshape,areafactor,rootfactor,interfactor,finalregion,compactness},
(*\:8c03\:6574\:5916\:90e8\:9762\:79ef\:7684\:5927\:5c0f\:ff0c\:5148\:9762\:79ef\:5f52\:4e00\:5316\:ff0c\:518d\:6839\:636e\:7d27\:51d1\:5ea6\:6307\:6570\:7f29\:653e\:ff0c\:56e0\:4e3a\:677e\:6563\:7684\:56fe\:5f62\:89c6\:89c9\:6548\:679c\:5927*)
areafactor=1/Sqrt[Area[region]];
compactness=((4\[Pi] Area[region])/Perimeter[region]^2)^0.35;
outerregion=RegionResize[region,Scaled[compactness*areafactor]]
];

colorlistdefault= {RGBColor["#ED2A28"], RGBColor["#3F7CAC"], RGBColor["#F6A13A"
    ], RGBColor["#9E9E49"], RGBColor["#874F9E"], RGBColor["#E96DA8"], RGBColor[
    "#F27C2F"], RGBColor["#2ABCB8"], RGBColor["#D81859"], RGBColor["#367E44"
    ], RGBColor["#4E4E4E"], RGBColor["#FFD700"], RGBColor["#A0522D"]};
regionlistdefault={
RegularPolygon[3],
RegularPolygon[4],
Polygon[Table[0.8 {0.7 Sqrt[4 / (4 * Cot[Pi / 4])] Sin[2 \[Pi] k / 4], Sqrt[4 / (4* Cot[Pi /4])] Cos[2 \[Pi] k /4]}, {k, 1,4}]],
RegularPolygon[6],
Disk[],
RegularPolygon[5]};

markerlistcreate[n_,regionlist_,colorlist_,factor_:1 ,openness_:True, coverness_:True]:=Module[{colorlistlength=Length@colorlist,regionlistlength=Length@regionlist,bound,markersizefactor,markerregionlist,innerregionlist},
markerregionlist=If[openness,openregion/@regionlist,closeregion/@regionlist];
If[coverness,innerregionlist=openregionInner/@regionlist];
Table[
bound=RegionBounds[markerregionlist[[Mod[i-1,regionlistlength-1]+1]]]\[Transpose];
markersizefactor=bound[[1]]-bound[[2]]//Norm;
{
If[coverness,
Graphics[{colorlist[[Mod[i-1,colorlistlength-1]+1]],
markerregionlist[[Mod[i-1,regionlistlength-1]+1]],
White,innerregionlist[[Mod[i-1,regionlistlength-1]+1]]
}]
,
Graphics[{colorlist[[Mod[i-1,colorlistlength-1]+1]],
markerregionlist[[Mod[i-1,regionlistlength-1]+1]]}]]
,
Scaled[0.02*markersizefactor*factor]},
{i,1,n}]
];
legendlistcreate[n_,regionlist_,colorlist_,factor_:1,openness_:True, coverness_:True]:=Module[{colorlistlength=Length@colorlist,regionlistlength=Length@regionlist,bound,legendsizefactor,markerregionlist},
markerregionlist=If[openness,openregion/@regionlist,closeregion/@regionlist];
Table[
bound=RegionBounds[markerregionlist[[Mod[i-1,regionlistlength-1]+1]]]\[Transpose];
legendsizefactor=Abs[(bound[[1]]-bound[[2]])[[1]]];
Graphics[{colorlist[[Mod[i-1,colorlistlength-1]+1]],
markerregionlist[[Mod[i-1,regionlistlength-1]+1]]},ImageSize->8*legendsizefactor*factor],
{i,1,n}]
];
stylecolorlistcreate[n_,colorlist_]:=Module[{colorlistlength=Length@colorlist},
Table[colorlist[[Mod[i-1,colorlistlength-1]+1]],{i,1,n}]];
gradientcolorlist[colors_List,n_]:=Table[Blend[colors,x],{x,0,1,1/n}];
(*ListPlot*)
(*1. \:52a0\:5165markerlist\:9009\:9879\:ff0c\:53ef\:4ee5\:8f93\:5165\:4e00\:4e2aregionlist\:4f5c\:4e3amarker*)

Options[listplot] = {FrameLabel -> None, PlotLegends -> False, PLOT`errorinterval 
-> False, PLOT`markersize->1, PLOT`markerlist -> regionlistdefault, PLOT`colorlist -> colorlistdefault, PLOT`legendsize -> 1, PLOT`markeropenness->True, PLOT`markercoverness->True}

listplot[a_, opts___] :=
    ListPlot[
        If[errorinterval /. {opts} /. Options[listplot],
            a
            ,
            a
            ,
            Table[{a[[i, j, 1]], Around[a[[i, j, 2]], (errorinterval 
                /. {opts} /. Options[listplot])[[i, j]]]}, {i, 1, Length[a]}, {j, 1, 
                Length[a[[i]]]}]
        ]
        ,
        Evaluate @ DeleteCases[{opts}, (FrameLabel -> __) | (PlotLegends
             -> __) | (errorinterval -> __) | (markersize -> __)|(markerlist->__)|(colorlist-> __)|(legendsize -> __)|(markeropenness->__)|(markercoverness->__)]
        ,
        FrameLabel -> Evaluate[Style[#, Italic]& /@ (FrameLabel /. {opts
            } /. Options[listplot])]
        ,
        IntervalMarkersStyle -> Directive[Thickness[0.002]]
        ,
        Epilog ->
            If[PlotLegends /. {opts} /. Options[listplot],
                Inset[Style[Framed[Grid[{
                legendlistcreate[Length[a],markerlist/. {opts} /. Options[listplot],colorlist/. {opts} /. Options[listplot],legendsize/. {opts} /. Options[listplot], 
        markeropenness/.{opts} /. Options[listplot],
        markercoverness/.{opts} /. Options[listplot]],
                 Style[#, Black, 10, FontFamily -> "Myriad Pro"]& /@ ({"Legend 1",
                     "Legend 2", "Legend 3", "Legend 4", "Legend 5", "Legend 6", "Legend 7",
                     "Legend 8", "Legend 9", "Legend 10"}[[1 ;; Length[a]]])}\[Transpose]], FrameMargins
                     -> 5, ImageMargins -> Automatic, ContentPadding -> False, FrameStyle
                     -> Directive[Gray, Thickness[0.5]]], Background -> White], Offset[{-
                    2, -2}, Scaled[{1, 1}]], {Right, Top}]
                ,
                {}
                ,
                Inset[Style[Framed[Grid[{
                legendlistcreate[Length[a],markerlist/. {opts} /. Options[listplot],colorlist/. {opts} /. Options[listplot],legendsize/. {opts} /. Options[listplot], 
        markeropenness/.{opts} /. Options[listplot],
        markercoverness/.{opts} /. Options[listplot]],
                 Style[#, Black, 10, FontFamily -> "Myriad Pro"]& /@ ((PlotLegends
                     /. {opts})[[1]][[1 ;; Length[a]]])}\[Transpose], Spacings -> Automatic], FrameMargins
                     -> 5 + (PlotLegends /. {opts})[[2, 2]], ImageMargins -> Automatic, ContentPadding
                     -> False, FrameStyle -> Directive[Gray, Thickness[0.5]]], Background
                     -> White], Offset[{-2, -2} - (PlotLegends /. {opts})[[2, 1]], Scaled[
                    {1, 1}]], {Right, Top}]
            ]
        ,
        PlotMarkers -> markerlistcreate[Length[a],
        markerlist/. {opts} /. Options[listplot],
        colorlist/. {opts} /. Options[listplot], 
        markersize/. {opts} /. Options[listplot], 
        markeropenness/.{opts} /. Options[listplot],
        markercoverness/.{opts} /. Options[listplot]]
        ,
        PlotStyle -> Table[Directive[stylecolorlistcreate[Length[a],colorlist/. {opts} /. Options[listplot]][[i]],
        Thickness[0.0022]], {i, 1, Length[a]}]
        ,
        Frame -> True
        ,
        LabelStyle -> Directive[Black, 12, FontFamily -> "Myriad Pro"
            ]
        ,
        Axes -> False
        ,
        FrameStyle -> Directive[Black, Thickness[.0020]]
    ]

End[]

EndPackage[]

