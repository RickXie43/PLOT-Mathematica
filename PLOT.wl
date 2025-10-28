(* ::Package:: *)

(*Author: Rick*)

(*Email: xie_jinxiao@126.com*)

BeginPackage["PLOT`"]

colorlist::usage = "An colorlist copy from NATURE"

openmarkerlist::usage = "An Option for PlotMarkers"

openmarkerlegend::usage = "Graphics List of the Openmarker for Making Plot Legend"

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

Begin["Private`"]

colorlist = {RGBColor["#ED2A28"], RGBColor["#3F7CAC"], RGBColor["#F6A13A"
    ], RGBColor["#9E9E49"], RGBColor["#874F9E"], RGBColor["#E96DA8"], RGBColor[
    "#F27C2F"], RGBColor["#2ABCB8"], RGBColor["#D81859"], RGBColor["#367E44"
    ], RGBColor["#4E4E4E"], RGBColor["#FFD700"], RGBColor["#A0522D"]}

openmarkerlist =
    {
        {
            Graphics[
                {
                    n = 3;
                    Polygon[Table[0.8 {Sqrt[4 / (n * Cot[Pi / n])] Sin[
                        2 \[Pi] k / n], Sqrt[4 / (n * Cot[Pi / n])] Cos[2 \[Pi] k / n]}, {k, 1, n}]]
                    ,
                    White
                    ,
                    Polygon[0.34 * Table[{Sqrt[4 / (n * Cot[Pi / n])]
                         Sin[2 \[Pi] k / n], Sqrt[4 / (n * Cot[Pi / n])] Cos[2 \[Pi] k / n]}, {k, 1, 
                        n}]]
                }
            ]
            ,
            Scaled[0.025]
        }
        ,
        {
            Graphics[
                {
                    n = 4;
                    Polygon[Table[0.8 {Sqrt[4 / (n * Cot[Pi / n])] Sin[
                        2 \[Pi] k / n + \[Pi] / 4], Sqrt[4 / (n * Cot[Pi / n])] Cos[2 \[Pi] k / n + \[Pi] / 4
                        ]}, {k, 1, n}]]
                    ,
                    White
                    ,
                    Polygon[0.46 * Table[{Sqrt[4 / (n * Cot[Pi / n])]
                         Sin[2 \[Pi] k / n + \[Pi] / 4], Sqrt[4 / (n * Cot[Pi / n])] Cos[2 \[Pi] k / n + 
                        \[Pi] / 4]}, {k, 1, n}]]
                }
            ]
            ,
            Scaled[0.021]
        }
        ,
        {
            Graphics[
                {
                    n = 4;
                    Polygon[Table[0.8 {0.7 Sqrt[4 / (n * Cot[Pi / n])
                        ] Sin[2 \[Pi] k / n], Sqrt[4 / (n * Cot[Pi / n])] Cos[2 \[Pi] k / n]}, {k, 1,
                         n}]]
                    ,
                    White
                    ,
                    Polygon[0.43 * Table[{0.7 Sqrt[4 / (n * Cot[Pi / 
                        n])] Sin[2 \[Pi] k / n], Sqrt[4 / (n * Cot[Pi / n])] Cos[2 \[Pi] k / n]}, {k,
                         1, n}]]
                }
            ]
            ,
            Scaled[0.033]
        }
        ,
        {
            Graphics[
                {
                    n = 6;
                    Polygon[Table[0.8 {Sqrt[4 / (n * Cot[Pi / n])] Sin[
                        2 \[Pi] k / n], Sqrt[4 / (n * Cot[Pi / n])] Cos[2 \[Pi] k / n]}, {k, 1, n}]]
                    ,
                    White
                    ,
                    Polygon[0.48 * Table[{Sqrt[4 / (n * Cot[Pi / n])]
                         Sin[2 \[Pi] k / n], Sqrt[4 / (n * Cot[Pi / n])] Cos[2 \[Pi] k / n]}, {k, 1, 
                        n}]]
                }
            ]
            ,
            Scaled[0.023]
        }
        ,
        {Graphics[{Disk[{0, 0}, 1], White, Disk[{0, 0}, 0.58]}], Scaled[
            0.022]}
        ,
        {
            Graphics[
                {
                    n = 5;
                    Polygon[Table[0.8 {Sqrt[4 / (n * Cot[Pi / n])] Sin[
                        2 \[Pi] k / n], Sqrt[4 / (n * Cot[Pi / n])] Cos[2 \[Pi] k / n]}, {k, 1, n}]]
                    ,
                    White
                    ,
                    Polygon[0.47 * Table[{Sqrt[4 / (n * Cot[Pi / n])]
                         Sin[2 \[Pi] k / n], Sqrt[4 / (n * Cot[Pi / n])] Cos[2 \[Pi] k / n]}, {k, 1, 
                        n}]]
                }
            ]
            ,
            Scaled[0.023]
        }
    }

openmarkerlegend =
    {
        Graphics[
            {
                colorlist[[1]]
                ,
                n = 3;
                Polygon[Table[0.8 {Sqrt[4 / (n * Cot[Pi / n])] Sin[2 
                    \[Pi] k / n], Sqrt[4 / (n * Cot[Pi / n])] Cos[2 \[Pi] k / n]}, {k, 1, n}]]
                ,
                White
                ,
                Polygon[0.34 * Table[{Sqrt[4 / (n * Cot[Pi / n])] Sin[
                    2 \[Pi] k / n], Sqrt[4 / (n * Cot[Pi / n])] Cos[2 \[Pi] k / n]}, {k, 1, n}]]
            }
            ,
            ImageSize -> 5 * 1.287
        ]
        ,
        Graphics[
            {
                colorlist[[2]]
                ,
                n = 4;
                Polygon[Table[0.8 {Sqrt[4 / (n * Cot[Pi / n])] Sin[2 
                    \[Pi] k / n + \[Pi] / 4], Sqrt[4 / (n * Cot[Pi / n])] Cos[2 \[Pi] k / n + \[Pi] / 4]},
                     {k, 1, n}]]
                ,
                White
                ,
                Polygon[0.46 * Table[{Sqrt[4 / (n * Cot[Pi / n])] Sin[
                    2 \[Pi] k / n + \[Pi] / 4], Sqrt[4 / (n * Cot[Pi / n])] Cos[2 \[Pi] k / n + \[Pi] / 4
                    ]}, {k, 1, n}]]
            }
            ,
            ImageSize -> 5 * 0.913
        ]
        ,
        Graphics[
            {
                colorlist[[3]]
                ,
                n = 4;
                Polygon[Table[0.8 {0.7 Sqrt[4 / (n * Cot[Pi / n])] Sin[
                    2 \[Pi] k / n], Sqrt[4 / (n * Cot[Pi / n])] Cos[2 \[Pi] k / n]}, {k, 1, n}]]
                ,
                White
                ,
                Polygon[0.43 * Table[{0.7 Sqrt[4 / (n * Cot[Pi / n])]
                     Sin[2 \[Pi] k / n], Sqrt[4 / (n * Cot[Pi / n])] Cos[2 \[Pi] k / n]}, {k, 1, 
                    n}]]
            }
            ,
            ImageSize -> 5 * 1.035
        ]
        ,
        Graphics[
            {
                colorlist[[4]]
                ,
                n = 6;
                Polygon[Table[0.8 {Sqrt[4 / (n * Cot[Pi / n])] Sin[2 
                    \[Pi] k / n], Sqrt[4 / (n * Cot[Pi / n])] Cos[2 \[Pi] k / n]}, {k, 1, n}]]
                ,
                White
                ,
                Polygon[0.48 * Table[{Sqrt[4 / (n * Cot[Pi / n])] Sin[
                    2 \[Pi] k / n], Sqrt[4 / (n * Cot[Pi / n])] Cos[2 \[Pi] k / n]}, {k, 1, n}]]
            }
            ,
            ImageSize -> 5
        ]
        ,
        Graphics[{colorlist[[5]], Disk[{0, 0}, 1], White, Disk[{0, 0},
             0.58]}, ImageSize -> 5.5]
        ,
        Graphics[
            {
                colorlist[[6]]
                ,
                n = 5;
                Polygon[Table[0.8 {Sqrt[4 / (n * Cot[Pi / n])] Sin[2 
                    \[Pi] k / n], Sqrt[4 / (n * Cot[Pi / n])] Cos[2 \[Pi] k / n]}, {k, 1, n}]]
                ,
                White
                ,
                Polygon[0.47 * Table[{Sqrt[4 / (n * Cot[Pi / n])] Sin[
                    2 \[Pi] k / n], Sqrt[4 / (n * Cot[Pi / n])] Cos[2 \[Pi] k / n]}, {k, 1, n}]]
            }
            ,
            ImageSize -> 5
        ]
        ,
        Graphics[
            {
                colorlist[[7]]
                ,
                n = 3;
                Polygon[Table[0.8 {Sqrt[4 / (n * Cot[Pi / n])] Sin[2 
                    \[Pi] k / n], Sqrt[4 / (n * Cot[Pi / n])] Cos[2 \[Pi] k / n]}, {k, 1, n}]]
                ,
                White
                ,
                Polygon[0.34 * Table[{Sqrt[4 / (n * Cot[Pi / n])] Sin[
                    2 \[Pi] k / n], Sqrt[4 / (n * Cot[Pi / n])] Cos[2 \[Pi] k / n]}, {k, 1, n}]]
            }
            ,
            ImageSize -> 5 * 1.287
        ]
        ,
        Graphics[
            {
                colorlist[[8]]
                ,
                n = 4;
                Polygon[Table[0.8 {Sqrt[4 / (n * Cot[Pi / n])] Sin[2 
                    \[Pi] k / n + \[Pi] / 4], Sqrt[4 / (n * Cot[Pi / n])] Cos[2 \[Pi] k / n + \[Pi] / 4]},
                     {k, 1, n}]]
                ,
                White
                ,
                Polygon[0.46 * Table[{Sqrt[4 / (n * Cot[Pi / n])] Sin[
                    2 \[Pi] k / n + \[Pi] / 4], Sqrt[4 / (n * Cot[Pi / n])] Cos[2 \[Pi] k / n + \[Pi] / 4
                    ]}, {k, 1, n}]]
            }
            ,
            ImageSize -> 5 * 0.913
        ]
        ,
        Graphics[
            {
                colorlist[[9]]
                ,
                n = 4;
                Polygon[Table[0.8 {0.7 Sqrt[4 / (n * Cot[Pi / n])] Sin[
                    2 \[Pi] k / n], Sqrt[4 / (n * Cot[Pi / n])] Cos[2 \[Pi] k / n]}, {k, 1, n}]]
                ,
                White
                ,
                Polygon[0.43 * Table[{0.7 Sqrt[4 / (n * Cot[Pi / n])]
                     Sin[2 \[Pi] k / n], Sqrt[4 / (n * Cot[Pi / n])] Cos[2 \[Pi] k / n]}, {k, 1, 
                    n}]]
            }
            ,
            ImageSize -> 5 * 1.035
        ]
        ,
        Graphics[
            {
                colorlist[[10]]
                ,
                n = 6;
                Polygon[Table[0.8 {Sqrt[4 / (n * Cot[Pi / n])] Sin[2 
                    \[Pi] k / n], Sqrt[4 / (n * Cot[Pi / n])] Cos[2 \[Pi] k / n]}, {k, 1, n}]]
                ,
                White
                ,
                Polygon[0.48 * Table[{Sqrt[4 / (n * Cot[Pi / n])] Sin[
                    2 \[Pi] k / n], Sqrt[4 / (n * Cot[Pi / n])] Cos[2 \[Pi] k / n]}, {k, 1, n}]]
            }
            ,
            ImageSize -> 5
        ]
        ,
        Graphics[{colorlist[[11]], Disk[{0, 0}, 1], White, Disk[{0, 0
            }, 0.58]}, ImageSize -> 5.5]
        ,
        Graphics[
            {
                colorlist[[12]]
                ,
                n = 5;
                Polygon[Table[0.8 {Sqrt[4 / (n * Cot[Pi / n])] Sin[2 
                    \[Pi] k / n], Sqrt[4 / (n * Cot[Pi / n])] Cos[2 \[Pi] k / n]}, {k, 1, n}]]
                ,
                White
                ,
                Polygon[0.47 * Table[{Sqrt[4 / (n * Cot[Pi / n])] Sin[
                    2 \[Pi] k / n], Sqrt[4 / (n * Cot[Pi / n])] Cos[2 \[Pi] k / n]}, {k, 1, n}]]
            }
            ,
            ImageSize -> 5
        ]
    }

(*Plot*)

Options[plot] = {FrameLabel -> None}

plot[a_, b_, opts___] :=
    Plot[a, b, Evaluate @ DeleteCases[{opts}, FrameLabel -> _], FrameLabel
         -> Evaluate[Style[#, Italic]& /@ (FrameLabel /. {opts} /. Options[plot
        ])], PlotStyle -> colorlist, Frame -> True, LabelStyle -> Directive[Black,
         12, FontFamily -> "Myriad Pro"], Axes -> False, FrameStyle -> Directive[
        Black, Thickness[.0020]]]

(*LogPlot*)

Options[logplot] = {FrameLabel -> None, PLOT`additionalticks -> {}}

logplot[a_, b_, opts___] :=
    LogPlot[a, b, Evaluate @ DeleteCases[{opts}, (FrameLabel -> __) |
         (additionalticks -> __)], FrameLabel -> Evaluate[Style[#, Italic]& /@
         (FrameLabel /. {opts} /. Options[logplot])], PlotStyle -> colorlist,
         Frame -> True, LabelStyle -> Directive[Black, 12, FontFamily -> "Myriad Pro"
        ], Axes -> False, FrameStyle -> Directive[Black, Thickness[.0020]], FrameTicks
         -> {{Table[{10^i, Superscript[10, i], 0.013}, {i, -20, 15}] ~ Join ~
         Flatten[Table[{k * 10^j, ToString[k] <> " \[Times]" Superscript[10, j], 0.013
        }, {k, (additionalticks /. {opts} /. Options[logplot])}, {j, -20, 15}
        ], 1] ~ Join ~ Flatten[Table[{k * 10^j, Null, 0.004}, {k, 2, 9}, {j, 
        -20, 15}], 1], None}, {Automatic, None}}]

(*LogLogPlot*)

Options[loglogplot] = {FrameLabel -> None, PLOT`additionalticks -> {{
    }, {}}}

loglogplot[a_, b_, opts___] :=
    LogLogPlot[a, b, Evaluate @ DeleteCases[{opts}, (FrameLabel -> __
        ) | (additionalticks -> __)], FrameLabel -> Evaluate[Style[#, Italic]&
         /@ (FrameLabel /. {opts} /. Options[loglogplot])], PlotStyle -> colorlist,
         Frame -> True, LabelStyle -> Directive[Black, 12, FontFamily -> "Myriad Pro"
        ], Axes -> False, FrameStyle -> Directive[Black, Thickness[.0020]], FrameTicks
         -> {{Table[{10^i, Superscript[10, i], 0.013}, {i, -20, 15}] ~ Join ~
         Flatten[Table[{k * 10^j, ToString[k] <> " \[Times]" Superscript[10, j], 0.013
        }, {k, (additionalticks /. {opts} /. Options[loglogplot])[[2]]}, {j, 
        -20, 15}], 1] ~ Join ~ Flatten[Table[{k * 10^j, Null, 0.004}, {k, 2, 
        9}, {j, -20, 15}], 1], None}, {Table[{10^i, Superscript[10, i], 0.013
        }, {i, -20, 15}] ~ Join ~ Flatten[Table[{k * 10^j, ToString[k] <> " \[Times]"
         Superscript[10, j], 0.013}, {k, (additionalticks /. {opts} /. Options[
        loglogplot])[[1]]}, {j, -20, 15}], 1] ~ Join ~ Flatten[Table[{k * 10^
        j, Null, 0.004}, {k, 2, 9}, {j, -20, 15}], 1], None}}]

(*ListPlot*)

Options[listplot] = {FrameLabel -> None, PlotLegends -> False, PLOT`errorinterval
     -> False, PLOT`markersize->1}

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
             -> __) | (errorinterval -> __) | (markersize -> __)]
        ,
        FrameLabel -> Evaluate[Style[#, Italic]& /@ (FrameLabel /. {opts
            } /. Options[listplot])]
        ,
        IntervalMarkersStyle -> Directive[Thickness[0.002]]
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
        PlotMarkers -> (ReplacePart[#,{-1,-1}->#[[-1,-1]]*markersize]&/@openmarkerlist)/. {opts} /. Options[listplot]
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
     -> {}, PLOT`errorinterval -> False, PLOT`markersize->1}

listlogplot[a_, opts___] :=
    ListLogPlot[
        If[errorinterval /. {opts} /. Options[listlogplot],
            a
            ,
            a
            ,
            Table[{a[[i, j, 1]], Around[a[[i, j, 2]], (errorinterval 
                /. {opts} /. Options[listlogplot])[[i, j]]]}, {i, 1, Length[a]}, {j, 
                1, Length[a[[i]]]}]
        ]
        ,
        Evaluate @ DeleteCases[{opts}, (FrameLabel -> __) | (PlotLegends
             -> __) | (additionalticks -> __) | (errorinterval -> __)| (markersize -> __)]
        ,
        FrameLabel -> Evaluate[Style[#, Italic]& /@ (FrameLabel /. {opts
            } /. Options[listlogplot])]
        ,
        IntervalMarkersStyle -> Directive[Thickness[0.002]]
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
        PlotMarkers ->(ReplacePart[#,{-1,-1}->#[[-1,-1]]*markersize]&/@openmarkerlist)/. {opts} /. Options[listlogplot]
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
            -20, 15}] ~ Join ~ Flatten[Table[{k * 10^j, ToString[k] <> " \[Times]" Superscript[
            10, j], 0.013}, {k, (additionalticks /. {opts} /. Options[listlogplot
            ])}, {j, -20, 15}], 1] ~ Join ~ Flatten[Table[{k * 10^j, Null, 0.004},
             {k, 2, 9}, {j, -20, 15}], 1], None}, {Automatic, None}}
    ]

(*ListLogLogPlot*)

Options[listloglogplot] = {FrameLabel -> None, PlotLegends -> False, 
    PLOT`additionalticks -> {{}, {}}, PLOT`errorinterval -> False, PLOT`markersize->1}

listloglogplot[a_, opts___] :=
    ListLogLogPlot[
        If[errorinterval /. {opts} /. Options[listloglogplot],
            a
            ,
            a
            ,
            Table[{a[[i, j, 1]], Around[a[[i, j, 2]], (errorinterval 
                /. {opts} /. Options[listloglogplot])[[i, j]]]}, {i, 1, Length[a]}, {
                j, 1, Length[a[[i]]]}]
        ]
        ,
        Evaluate @ DeleteCases[{opts}, (FrameLabel -> __) | (PlotLegends
             -> __) | (additionalticks -> __) | (errorinterval -> __)| (markersize -> __)]
        ,
        FrameLabel -> Evaluate[Style[#, Italic]& /@ (FrameLabel /. {opts
            } /. Options[listloglogplot])]
        ,
        IntervalMarkersStyle -> Directive[Thickness[0.002]]
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
        PlotMarkers ->(ReplacePart[#,{-1,-1}->#[[-1,-1]]*markersize]&/@openmarkerlist)/. {opts} /. Options[listloglogplot]
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
            -20, 15}] ~ Join ~ Flatten[Table[{k * 10^j, ToString[k] <> " \[Times]" Superscript[
            10, j], 0.013}, {k, (additionalticks /. {opts} /. Options[listloglogplot
            ])[[2]]}, {j, -20, 15}], 1] ~ Join ~ Flatten[Table[{k * 10^j, Null, 0.004
            }, {k, 2, 9}, {j, -20, 15}], 1], None}, {Table[{10^i, Superscript[10,
             i], 0.013}, {i, -20, 15}] ~ Join ~ Flatten[Table[{k * 10^j, ToString[
            k] <> " \[Times]" Superscript[10, j], 0.013}, {k, (additionalticks /. {opts}
             /. Options[listloglogplot])[[1]]}, {j, -20, 15}], 1] ~ Join ~ Flatten[
            Table[{k * 10^j, Null, 0.004}, {k, 2, 9}, {j, -20, 15}], 1], None}}
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
        ColorFunction -> (Blend[{RGBColor["#0000FF"], White, RGBColor[
            "#FF0000"]}, Rescale[#, (plotlegends /. {opts} /. Options[listdensityplot
            ])[[1]]]]&)
        ,
        (*Legend*)
        PlotLegends -> Placed[BarLegend[{(Blend[{RGBColor["#0000FF"],
             White, RGBColor["#FF0000"]}, Rescale[#, (plotlegends /. {opts} /. Options[
            listdensityplot])[[1]]]]&), (plotlegends /. {opts} /. Options[listdensityplot
            ])[[1]]}, LegendMarkerSize -> (plotlegends /. {opts} /. Options[listdensityplot
            ])[[2]] + 255, LegendLayout -> "Column", LegendMargins -> -3, LegendLabel
             -> Placed[Style[(legendlabel /. {opts} /. Options[listdensityplot]),
             Italic], Right, Rotate[#, -90 Degree]&]], (plotlegends /. {opts} /. 
            Options[listdensityplot])[[3]] + {1, 0.54}]
    ]

(*listlineplot3d*)

Options[listlineplot3d] = {AxesLabel -> None, PlotLegends -> Table[Null,
     {i, 1, 10}], Filling -> Bottom, Mesh -> Full}

listlineplot3d[a_, opts___] :=
    ListLinePlot3D[Table[{a[[i]]\[Transpose][[1]], Table[i, {j, 1, Length[a[[i]]
        \[Transpose][[1]]]}], a[[i]]\[Transpose][[2]]}\[Transpose], {i, 1, Length @ a}], Evaluate @ DeleteCases[
        {opts}, (AxesLabel -> __) | (PlotLegends -> __) | (Filling -> __) | (
        Mesh -> __)], AxesLabel -> Evaluate[Style[#, Italic]& /@ (AxesLabel /.
         {opts} /. Options[listlineplot3d])], PlotStyle -> Table[Directive[colorlist
        [[Mod[i - 1, 10] + 1]], Thickness[0.003]], {i, 1, 20}], Ticks -> {Automatic,
         {Range[Length[a]], (PlotLegends /. {opts} /. Options[listlineplot3d]
        )[[1 ;; Length[a]]]}\[Transpose], Automatic}, LabelStyle -> Directive[Black, 12,
         FontFamily -> "Myriad Pro"], AxesStyle -> Directive[Black, 12, Italic,
         Thickness[.0020], FontFamily -> "Myriad Pro"], Boxed -> False, AxesEdge
         -> {{-1, -1}, {1, -1}, {-1, -1}}, Filling -> (Filling /. {opts} /. Options[
        listlineplot3d]), Mesh -> (Mesh /. {opts} /. Options[listlineplot3d])
        ]

End[]

EndPackage[]
