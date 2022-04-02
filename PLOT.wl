(* ::Package:: *)

(*Author:Rick*)

BeginPackage["PLOT`"]

colorlist::usage = "An colorlist copy from NATURE"

openmarkerlist::usage = "An Option for PlotMarkers"

openmarkerlegend::usage = "Graphics List of the Openmarker for Making Plot Legend"

plot::usage = "Same as Plot"

logplot::usage= "
New Options 
	additionalticks->
		{1,4,5...} which add more ticks in the y axes 
Same as LogPlot"

loglogplot::usage= "
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
		}
Other options are same as ListPlot"

listlogplot::usage = "
New Options 
	PlotLegends->
		{
			{'LegendLabel1','LegendLabel2'...},
			{LegendOffset ({0,0} set plot legend at the right top side of the plot),LegendLabelMargin}
		}
	additionalticks->
		{1,4,5...} which add more ticks in the y axes 
Other options are same as ListLogPlot"

listloglogplot::usage = "
New Options 
	PlotLegends->
		{
			{'LegendLabel1','LegendLabel2'...},
			{LegendOffset ({0,0} set plot legend at the right top side of the plot),LegendLabelMargin}
		}
	additionalticks->
		{{1,4,5...},{5...}} which add more ticks in the x and y axes 
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

Begin["Private`"]

colorlist = {RGBColor["#ED2A28"], RGBColor["#3F7CAC"], RGBColor["#F6A13A"
    ], RGBColor["#9E9E49"], RGBColor["#874F9E"], RGBColor["#E96DA8"], RGBColor[
    "#F27C2F"], RGBColor["#2ABCB8"], RGBColor["#D81859"], RGBColor["#367E44"
    ]}

openmarkerlist = {{Graphics[{Thickness[0.22], Line[{{-1 / 2, 0}, {1 /
     2, 0}, {0, Sqrt[3] / 2}, {-1 / 2, 0}}]}], Scaled[0.025]}, {Graphics[
    {Thickness[0.25], Line[{{-1 / 2, -1 / 2}, {-1 / 2, 1 / 2}, {1 / 2, 1 
    / 2}, {1 / 2, -1 / 2}, {-1 / 2, -1 / 2}}]}], Scaled[0.021]}, {Graphics[
    {Thickness[0.25], Line[{{-1 / 2, 0}, {0, 1.5 / 2}, {1 / 2, 0}, {0, -1.5
     / 2}, {-1 / 2, 0}}]}], Scaled[0.033]}, {Graphics[{Thickness[0.25], Line[
    Table[{Cos[\[Pi] / 2 + (i - 1) / 6 2 \[Pi]], Sin[\[Pi] / 2 + (i - 1) / 6 2 \[Pi]]}, {
    i, 1, 7}]]}], Scaled[0.023]}, {Graphics[{Thickness[0.25], Circle[]}],
     Scaled[0.023]}}

openmarkerlegend = {Graphics[{Thickness[0.22], colorlist[[1]], Line[{
    {-1 / 2, 0}, {1 / 2, 0}, {0, Sqrt[3] / 2}, {-1 / 2, 0}}]}, ImageSize 
    -> 5 * 1.087], Graphics[{Thickness[0.25], colorlist[[2]], Line[{{-1 /
     2, -1 / 2}, {-1 / 2, 1 / 2}, {1 / 2, 1 / 2}, {1 / 2, -1 / 2}, {-1 / 
    2, -1 / 2}}]}, ImageSize -> 5 * 0.913], Graphics[{Thickness[0.25], colorlist
    [[3]], Line[{{-1 / 2, 0}, {0, 1.5 / 2}, {1 / 2, 0}, {0, -1.5 / 2}, {-
    1 / 2, 0}}]}, ImageSize -> 5 * 1.035], Graphics[{Thickness[0.25], colorlist
    [[4]], Line[Table[{Cos[\[Pi] / 2 + (i - 1) / 6 2 \[Pi]], Sin[\[Pi] / 2 + (i - 1) 
    / 6 2 \[Pi]]}, {i, 1, 7}]]}, ImageSize -> 5], Graphics[{Thickness[0.25], 
    colorlist[[5]], Circle[{0, 0}, 0.023]}, ImageSize -> 5], Graphics[{Thickness[
    0.22], colorlist[[6]], Line[{{-1 / 2, 0}, {1 / 2, 0}, {0, Sqrt[3] / 2
    }, {-1 / 2, 0}}]}, ImageSize -> 5 * 1.087], Graphics[{Thickness[0.25],
     colorlist[[7]], Line[{{-1 / 2, -1 / 2}, {-1 / 2, 1 / 2}, {1 / 2, 1 /
     2}, {1 / 2, -1 / 2}, {-1 / 2, -1 / 2}}]}, ImageSize -> 5 * 0.913], Graphics[
    {Thickness[0.25], colorlist[[8]], Line[{{-1 / 2, 0}, {0, 1.5 / 2}, {1
     / 2, 0}, {0, -1.5 / 2}, {-1 / 2, 0}}]}, ImageSize -> 5 * 1.035], Graphics[
    {Thickness[0.25], colorlist[[9]], Line[Table[{Cos[\[Pi] / 2 + (i - 1) / 6
     2 \[Pi]], Sin[\[Pi] / 2 + (i - 1) / 6 2 \[Pi]]}, {i, 1, 7}]]}, ImageSize -> 5], 
    Graphics[{Thickness[0.25], colorlist[[10]], Circle[]}, ImageSize -> 5
    ]}

(*Plot*)

Options[plot] = {FrameLabel -> None}

plot[a_, b_, opts___] :=
    Plot[a, b, Evaluate @ DeleteCases[{opts}, FrameLabel -> _], FrameLabel
         -> Evaluate[Style[#, Italic]& /@ (FrameLabel /. {opts} /. Options[plot
        ])], PlotStyle -> colorlist, Frame -> True, LabelStyle -> Directive[Black,
         12, FontFamily -> "Myriad Pro"], Axes -> False, FrameStyle -> Directive[
        Black, Thickness[.0020]]]
(*LogPlot*)

Options[logplot]= {FrameLabel->None,PLOT`additionalticks
     -> {}}

logplot[a_,b_,opts___]:=
	LogPlot[a,b,Evaluate @ DeleteCases[{opts}, (FrameLabel -> __ )| (additionalticks -> __)], FrameLabel
         -> Evaluate[Style[#, Italic]& /@ (FrameLabel /. {opts} /. Options[logplot
        ])], PlotStyle -> colorlist, Frame -> True, LabelStyle -> Directive[Black,
         12, FontFamily -> "Myriad Pro"], Axes -> False, FrameStyle -> Directive[
        Black, Thickness[.0020]],
        FrameTicks -> {{Table[{10^i, Superscript[10, i], 0.013}, {i, 
            -20, 15}] ~ Join ~ Flatten[Table[{k * 10^j, ToString[k]<>" \[Times]" Superscript[10, j], 0.013
            }, {k, (additionalticks /. {opts} /. Options[logplot])}, {j, -20,
             15}], 1] ~ Join ~ Flatten[Table[{k * 10^j, Null, 0.004}, {k, 2, 9}, 
            {j, -20, 15}], 1], None}, {Automatic, None}}
	]

(*LogLogPlot*)

Options[loglogplot]= {FrameLabel->None,PLOT`additionalticks
     -> {{},{}}}

loglogplot[a_,b_,opts___]:=
	LogLogPlot[a,b,Evaluate @ DeleteCases[{opts}, (FrameLabel -> __ )| (additionalticks -> __)], FrameLabel
         -> Evaluate[Style[#, Italic]& /@ (FrameLabel /. {opts} /. Options[loglogplot
        ])], PlotStyle -> colorlist, Frame -> True, LabelStyle -> Directive[Black,
         12, FontFamily -> "Myriad Pro"], Axes -> False, FrameStyle -> Directive[
        Black, Thickness[.0020]],  FrameTicks -> {{Table[{10^i, Superscript[10, i], 0.013}, {i, 
            -20, 15}] ~ Join ~ Flatten[Table[{k * 10^j, ToString[k]<>" \[Times]" Superscript[10, j], 0.013
            }, {k, (additionalticks /. {opts} /. Options[loglogplot])[[2]]}, 
            {j, -20, 15}], 1] ~ Join ~ Flatten[Table[{k * 10^j, Null, 0.004}, {k,
             2, 9}, {j, -20, 15}], 1], None}, {Table[{10^i, Superscript[10, i], 0.013
            }, {i, -20, 15}] ~ Join ~ Flatten[Table[{k * 10^j, ToString[k]<>" \[Times]" Superscript[10, j], 0.013}, {k, (additionalticks /. {opts} /. Options[loglogplot
            ])[[1]]}, {j, -20, 15}], 1] ~ Join ~ Flatten[Table[{k * 10^j, Null, 0.004
            }, {k, 2, 9}, {j, -20, 15}], 1], None}}
	]

(*ListPlot*)

Options[listplot] = {FrameLabel -> None, PlotLegends -> False}

listplot[a_, opts___] :=
    ListPlot[
        a
        ,
        Evaluate @ DeleteCases[{opts}, (FrameLabel -> __) | (PlotLegends
             -> __)]
        ,
        FrameLabel -> Evaluate[Style[#, Italic]& /@ (FrameLabel /. {opts
            } /. Options[listplot])]
        ,
        Epilog ->
            If[PlotLegends /. {opts} /. Options[listplot],
                Inset[Style[Framed[Grid[{openmarkerlegend[[1 ;; Length[
                    a]]], Style[#, Black, 10, FontFamily -> "Myriad Pro"]& /@ ({"Legend 1",
                     "Legend 2", "Legend 3", "Legend 4", "Legend 5", "Legend 6", "Legend 7",
                     "Legend 8", "Legend 9", "Legend 10"}[[1 ;; Length[a]]])}\[Transpose]], FrameMargins
                     -> 5, ImageMargins -> Automatic, ContentPadding -> False, FrameStyle
                     -> Directive[Gray, Thickness[0.5]]], Background -> White], Offset[{-
                    2, -2}, Scaled[{1, 1}]], {Right, Top}]
                ,
                {}
                ,
                Inset[Style[Framed[Grid[{openmarkerlegend[[1 ;; Length[
                    a]]], Style[#, Black, 10, FontFamily -> "Myriad Pro"]& /@ ((PlotLegends
                     /. {opts})[[1]][[1 ;; Length[a]]])}\[Transpose], Spacings -> Automatic], FrameMargins
                     -> 5 + (PlotLegends /. {opts})[[2, 2]], ImageMargins -> Automatic, ContentPadding
                     -> False, FrameStyle -> Directive[Gray, Thickness[0.5]]], Background
                     -> White], Offset[{-2, -2} - (PlotLegends /. {opts})[[2, 1]], Scaled[
                    {1, 1}]], {Right, Top}]
            ]
        ,
        PlotMarkers -> openmarkerlist
        ,
        PlotStyle -> Table[Directive[colorlist[[Mod[i - 1, 10] + 1]],
             Thickness[0.0022]], {i, 1, 20}]
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

(*ListLogPlot*)

Options[listlogplot] = {FrameLabel -> None, PlotLegends -> False, PLOT`additionalticks
     -> {}}

listlogplot[a_, opts___] :=
    ListLogPlot[
        a
        ,
        Evaluate @ DeleteCases[{opts}, (FrameLabel -> __) | (PlotLegends
             -> __) | (additionalticks -> __)]
        ,
        FrameLabel -> Evaluate[Style[#, Italic]& /@ (FrameLabel /. {opts
            } /. Options[listlogplot])]
        ,
        Epilog ->
            If[PlotLegends /. {opts} /. Options[listlogplot],
                Inset[Style[Framed[Grid[{openmarkerlegend[[1 ;; Length[
                    a]]], Style[#, Black, 10, FontFamily -> "Myriad Pro"]& /@ ({"Legend 1",
                     "Legend 2", "Legend 3", "Legend 4", "Legend 5", "Legend 6", "Legend 7",
                     "Legend 8", "Legend 9", "Legend 10"}[[1 ;; Length[a]]])}\[Transpose]], FrameMargins
                     -> 5, ImageMargins -> Automatic, ContentPadding -> False, FrameStyle
                     -> Directive[Gray, Thickness[0.5]]], Background -> White], Offset[{-
                    2, -2}, Scaled[{1, 1}]], {Right, Top}]
                ,
                {}
                ,
                Inset[Style[Framed[Grid[{openmarkerlegend[[1 ;; Length[
                    a]]], Style[#, Black, 10, FontFamily -> "Myriad Pro"]& /@ ((PlotLegends
                     /. {opts})[[1]][[1 ;; Length[a]]])}\[Transpose], Spacings -> Automatic], FrameMargins
                     -> 5 + (PlotLegends /. {opts})[[2, 2]], ImageMargins -> Automatic, ContentPadding
                     -> False, FrameStyle -> Directive[Gray, Thickness[0.5]]], Background
                     -> White], Offset[{-2, -2} - (PlotLegends /. {opts})[[2, 1]], Scaled[
                    {1, 1}]], {Right, Top}]
            ]
        ,
        PlotMarkers -> openmarkerlist
        ,
        PlotStyle -> Table[Directive[colorlist[[Mod[i - 1, 10] + 1]],
             Thickness[0.0022]], {i, 1, 20}]
        ,
        Frame -> True
        ,
        LabelStyle -> Directive[Black, 12, FontFamily -> "Myriad Pro"
            ]
        ,
        Axes -> False
        ,
        FrameStyle -> Directive[Black, Thickness[.0020]]
        ,
        FrameTicks -> {{Table[{10^i, Superscript[10, i], 0.013}, {i, 
            -20, 15}] ~ Join ~ Flatten[Table[{k * 10^j, ToString[k]<>" \[Times]" Superscript[10, j], 0.013
            }, {k, (additionalticks /. {opts} /. Options[listlogplot])}, {j, -20,
             15}], 1] ~ Join ~ Flatten[Table[{k * 10^j, Null, 0.004}, {k, 2, 9}, 
            {j, -20, 15}], 1], None}, {Automatic, None}}
    ]

(*ListLogLogPlot*)

Options[listloglogplot] = {FrameLabel -> None, PlotLegends -> False, 
    PLOT`additionalticks -> {{}, {}}}

listloglogplot[a_, opts___] :=
    ListLogLogPlot[
        a
        ,
        Evaluate @ DeleteCases[{opts}, (FrameLabel -> __) | (PlotLegends
             -> __) | (additionalticks -> __)]
        ,
        FrameLabel -> Evaluate[Style[#, Italic]& /@ (FrameLabel /. {opts
            } /. Options[listloglogplot])]
        ,
        Epilog ->
            If[PlotLegends /. {opts} /. Options[listloglogplot],
                Inset[Style[Framed[Grid[{openmarkerlegend[[1 ;; Length[
                    a]]], Style[#, Black, 10, FontFamily -> "Myriad Pro"]& /@ ({"Legend 1",
                     "Legend 2", "Legend 3", "Legend 4", "Legend 5", "Legend 6", "Legend 7",
                     "Legend 8", "Legend 9", "Legend 10"}[[1 ;; Length[a]]])}\[Transpose]], FrameMargins
                     -> 5, ImageMargins -> Automatic, ContentPadding -> False, FrameStyle
                     -> Directive[Gray, Thickness[0.5]]], Background -> White], Offset[{-
                    2, -2}, Scaled[{1, 1}]], {Right, Top}]
                ,
                {}
                ,
                Inset[Style[Framed[Grid[{openmarkerlegend[[1 ;; Length[
                    a]]], Style[#, Black, 10, FontFamily -> "Myriad Pro"]& /@ ((PlotLegends
                     /. {opts})[[1]][[1 ;; Length[a]]])}\[Transpose], Spacings -> Automatic], FrameMargins
                     -> 5 + (PlotLegends /. {opts})[[2, 2]], ImageMargins -> Automatic, ContentPadding
                     -> False, FrameStyle -> Directive[Gray, Thickness[0.5]]], Background
                     -> White], Offset[{-2, -2} - (PlotLegends /. {opts})[[2, 1]], Scaled[
                    {1, 1}]], {Right, Top}]
            ]
        ,
        PlotMarkers -> openmarkerlist
        ,
        PlotStyle -> Table[Directive[colorlist[[Mod[i - 1, 10] + 1]],
             Thickness[0.0022]], {i, 1, 20}]
        ,
        Frame -> True
        ,
        LabelStyle -> Directive[Black, 12, FontFamily -> "Myriad Pro"
            ]
        ,
        Axes -> False
        ,
        FrameStyle -> Directive[Black, Thickness[.0020]]
        ,
        FrameTicks -> {{Table[{10^i, Superscript[10, i], 0.013}, {i, 
            -20, 15}] ~ Join ~ Flatten[Table[{k * 10^j, ToString[k]<>" \[Times]" Superscript[10, j], 0.013
            }, {k, (additionalticks /. {opts} /. Options[listloglogplot])[[2]]}, 
            {j, -20, 15}], 1] ~ Join ~ Flatten[Table[{k * 10^j, Null, 0.004}, {k,
             2, 9}, {j, -20, 15}], 1], None}, {Table[{10^i, Superscript[10, i], 0.013
            }, {i, -20, 15}] ~ Join ~ Flatten[Table[{k * 10^j, ToString[k]<>" \[Times]" Superscript[10, j], 0.013}, {k, (additionalticks /. {opts} /. Options[listloglogplot
            ])[[1]]}, {j, -20, 15}], 1] ~ Join ~ Flatten[Table[{k * 10^j, Null, 0.004
            }, {k, 2, 9}, {j, -20, 15}], 1], None}}
    ]

(*ListLinePlot*)

Options[listlineplot] = {FrameLabel -> None, PlotLegends -> False}

listlineplot[a_, opts___] :=
    ListLinePlot[
        a
        ,
        Evaluate @ DeleteCases[{opts}, (FrameLabel -> __) | (PlotLegends
             -> __)]
        ,
        FrameLabel -> Evaluate[Style[#, Italic]& /@ (FrameLabel /. {opts
            } /. Options[listlineplot])]
        ,
        Epilog ->
            If[PlotLegends /. {opts} /. Options[listlineplot],
                Inset[Style[Framed[Grid[{(Graphics[{#, Thickness[0.07
                    ], Line[{{0, 0}, {1, 0}}]}, AspectRatio -> .1, ImageSize -> 20]& /@ colorlist
                    )[[1 ;; Length[a]]], Style[#, Black, 10, FontFamily -> "Myriad Pro"]&
                     /@ ({"Legend 1", "Legend 2", "Legend 3", "Legend 4", "Legend 5", "Legend 6",
                     "Legend 7", "Legend 8", "Legend 9", "Legend 10"}[[1 ;; Length[a]]])}
                    \[Transpose]], FrameMargins -> 5, ImageMargins -> Automatic, ContentPadding -> False,
                     FrameStyle -> Directive[Gray, Thickness[0.5]]], Background -> White],
                     Offset[{-2, -2}, Scaled[{1, 1}]], {Right, Top}]
                ,
                {}
                ,
                Inset[Style[Framed[Grid[{(Graphics[{#, Thickness[0.07
                    ], Line[{{0, 0}, {1, 0}}]}, AspectRatio -> .1, ImageSize -> 20]& /@ colorlist
                    )[[1 ;; Length[a]]], Style[#, Black, 10, FontFamily -> "Myriad Pro"]&
                     /@ ((PlotLegends /. {opts})[[1]][[1 ;; Length[a]]])}\[Transpose], Spacings -> Automatic
                    ], FrameMargins -> 5 + (PlotLegends /. {opts})[[2, 2]], ImageMargins 
                    -> Automatic, ContentPadding -> False, FrameStyle -> Directive[Gray, 
                    Thickness[0.5]]], Background -> White], Offset[{-2, -2} - (PlotLegends
                     /. {opts})[[2, 1]], Scaled[{1, 1}]], {Right, Top}]
            ]
        ,
        PlotStyle -> Table[Directive[colorlist[[Mod[i - 1, 10] + 1]],
             Thickness[0.0020]], {i, 1, 20}]
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

(*ListDensityPlot*)

Options[listdensityplot] = {FrameLabel -> None, PLOT`plotlegends -> {
    {-1, 1}, 255, {1, 0.54}}, PLOT`legendlabel -> ""}

listdensityplot[a_, opts___] :=
    ListDensityPlot[
        a
        ,
        Evaluate @ DeleteCases[{opts}, ((FrameLabel -> __) | (plotlegends
             -> __) | (legendlabel -> __))]
        ,
        FrameLabel -> Evaluate[Style[#, Italic]& /@ (FrameLabel /. {opts
            } /. Options[listdensityplot])]
        ,
        LabelStyle -> Directive[Black, 12, FontFamily -> "Myriad Pro"
            ]
        ,
        FrameStyle -> Directive[Black, Thickness[.0020]]
        ,
        (*Color*)
        ColorFunctionScaling -> False
        ,
        ColorFunction -> (Blend[{RGBColor["#004D98"], White, RGBColor[
            "#A70042"]}, Rescale[#, (plotlegends /. {opts} /. Options[listdensityplot
            ])[[1]]]]&)
        ,
        (*Legend*)
        PlotLegends -> Placed[BarLegend[{(Blend[{RGBColor["#004D98"],
             White, RGBColor["#A70042"]}, Rescale[#, (plotlegends /. {opts} /. Options[
            listdensityplot])[[1]]]]&), (plotlegends /. {opts} /. Options[listdensityplot
            ])[[1]]}, LegendMarkerSize -> (plotlegends /. {opts} /. Options[listdensityplot
            ])[[2]] + 255, LegendLayout -> "Column", LegendMargins -> -3, LegendLabel
             -> Placed[Style[(legendlabel /. {opts} /. Options[listdensityplot]),
             Italic], Right, Rotate[#, -90 Degree]&]], (plotlegends /. {opts} /. 
            Options[listdensityplot])[[3]] + {1, 0.54}]
    ]

End[]

EndPackage[]
