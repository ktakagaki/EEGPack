(* ::Package:: *)

(* ::Section:: *)
(* BeginPackage *)


BeginPackage["EEGPack`Plots`", {"EEGPack`Data`","KazukazuM`","KazukazuM`Utilities`","KazukazuM`Signal`","KazukazuM`Plots`","KazukazuM`Filters`"}];


(* ::Section:: *)
(* Declarations *)


(* ::Subsection::Closed:: *)
(*Montages*)


EEGMontage::usage = "Montage  ";
	EEGChannels::usage="Component of EEGMontage, denotes montage channels.";
	EEGChannelColors::usage="Component of EEGMontage, denotes colors assigned to montage channels.";


(* ::Subsection::Closed:: *)
(*EEGPlot*)


EEGPlot::usage = "EEGPlot  ";
EEGStackTraces::usage="Option for EEGPlot, how high to stack traces.";
EEGPageLength::usage="Option for EEGPlot, how long to make EEGPages.";


Options[EEGPlot] = 
JoinOptionLists[
{ BandpassFilter->None, EEGStackTraces->100, EEGMontage -> "DoubleBanana", EEGPageLength->10,
BaselineSubtract->10, Rasterize->False, EEGPageLabel->None,

PlotRange -> All,  PlotRangePadding -> None, AxesOrigin -> {0, 0}, ImageSize -> Automatic, GridLines -> Automatic, 
PlotStyle->Automatic,BaseStyle->{FontFamily->"Arial",FontSize->8},Axes->{False,True},Ticks->Automatic,
AspectRatio -> Automatic, Axes -> False},

Options[ListLinePlot]
];


(* ::Subsection::Closed:: *)
(*Layout*)


$DetectorRadius=4;


$DetectorCoordinates=
	{{"Cz",{0.`,0.`}},{"T4",{40.`,0.`}},{"F8",{32.3606797749979`,23.511410091698927`}},{"Fp2",{12.360679774997898`,38.04226065180614`}},{"Fp1",{-12.360679774997898`,38.04226065180614`}},{"F7",{-32.3606797749979`,23.511410091698927`}},{"T3",{-40.`,0.`}},{"T5",{-32.3606797749979`,-23.511410091698927`}},{"O1",{-12.360679774997898`,-38.04226065180614`}},{"O2",{12.360679774997898`,-38.04226065180614`}},{"T6",{32.3606797749979`,-23.511410091698927`}},{"C4",{20.`,0.`}},{"Fz",{0.`,20.`}},{"C3",{-20.`,0.`}},{"Pz",{0.`,-20.`}},{"F4",{16.470736916200696`,19.079445364405824`}},{"F3",{-16.470736916200696`,19.079445364405824`}},{"P4",{16.470736916200696`,-19.079445364405824`}},{"P3",{-16.470736916200696`,-19.079445364405824`}}}


$GetDetectorCoordinates::usage=" ";


$DetectorsPlot::usage=" ";
Options[$DetectorsPlot]={};

$DetectorNumbersPlot::usage=" ";
Options[$DetectorNumbersPlot]={};


(* ::Subsection:: *)
(*FramePlot*)


FramePlot::usage=" ";
ColorMap::usage=" ";
ChannelOutlines::usage=" ";


Options[FramePlot]=
JoinOptionLists[
	{ColorMap->"TemperatureMap", ChannelOutlines->True},
	Options[Graphics]
];


(* ::Subsection::Closed:: *)
(*$FrameBackgroundPlot, $FrameBackgroundPlot3D*)


$FrameBackgroundPlot::usage=" ";
Options[$FrameBackgroundPlot]=
JoinOptionLists[{PlotRange->All, ChannelOutlines->True}, Options[Graphics]];


$FrameBackgroundPlot3D::usage=" ";
Options[$FrameBackgroundPlot3D]=JoinOptionLists[{PlotRange->All, ChannelOutlines->True},Options[Show]];


(* ::Subsection:: *)
(*CoherencyPlot*)


CoherencyPlot::usage=" ";


Options[CoherencyPlot]=
JoinOptionLists[
	{PlotRange->All, CoherencyThreshold->0.5, CoherencyScaleMax->1, ThresholdOpacity->0.25, MacaroniRadiusFactor->1},
	Options[Graphics3D]
];


(* ::Section:: *)
(* Begin *)


Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*Montages*)


EEGMontage["DoubleBanana"]:={
EEGChannels->{{"Fp1","F7"},{"F7","T3"},{"T3","T5"},{"T5","O1"},
 {"Fp2","F8"},{"F8","T4"},{"T4","T6"},{"T6","O2"},
 {"Fp1","F3"},{"F3","C3"},{"C3","P3"},{"P3","O1"},
 {"Fp2","F4"},{"F4","C4"},{"C4","P4"},{"P4","O2"},
 {"Fz","Cz"},{"Cz","Pz"}},
EEGChannelColors->Join[Table[Darker[Blue],{4}],Table[Darker[Red],{4}],Table[Darker[Blue],{4}],Table[Darker[Red],{4}],Table[Darker[Green],{4}]]
};


EEGMontage["ReferentialA1"]:={
EEGChannels->{{"Fp1","A1"},{"Fp2","A2"},{"F7","A1"},{"F8","A2"},
{"F3","A1"},{"F4","A2"},{"T3","A1"},{"T4","A2"},
{"C3","A1"},{"C4","A2"},{"T5","A1"},{"T6","A2"},
{"P3","A1"},{"P4","A2"},{"O1","A1"},{"O2","A2"},
{"Fz",{"A1","A2"}},{"Cz",{"A1","A2"}},{"Pz",{"A1","A2"}}
 },
EEGChannelColors->Join[
	Flatten[Table[{Darker[Blue],Darker[Red]},{8}]],
	Table[Darker[Green],{3}]
	]
};


EEGMontage[args___] := Message[EEGMontage::invalidArgs, {args}];


(* ::Subsection::Closed:: *)
(*EEGPlot *)


Options[EEGPlotImpl] =Options[EEGPlot];


EEGPlotImpl[tempPlotData_, tempMaxMin_, tempLength_, 
			opMontage_, opChannels_,opEEGStackTraces_, sampleRate_, 
			opts : OptionsPattern[]] :=
	Module[{tempOptions,opPlotStyle,opColors},

	(*====Option Handling for Plot====*)
	tempOptions = {opts};
	If[OptionValue[GridLines] == Automatic,
		PrependTo[tempOptions, GridLines -> {Range[0, tempLength, 1], None}]
	];
	If[OptionValue[Ticks] == Automatic,
		PrependTo[tempOptions, Ticks -> 
			{None,Table[{opEEGStackTraces*(n),(#[[1]]<>"-"<>#[[2]])&[opChannels[[-n]]]},
			{n,1,Length[opChannels]}]}
		]
	];
	opPlotStyle=OptionValue[PlotStyle];
	opColors=(EEGChannelColors /. opMontage);
	If[opPlotStyle === Automatic, PrependTo[tempOptions, PlotStyle->opColors],
		If[Head[opPlotStyle] === Opacity, PrependTo[tempOptions, PlotStyle-> (Directive[#,opPlotStyle]& /@ opColors)],
			If[Head[opPlotStyle] === Directive, PrependTo[tempOptions, PlotStyle-> (Join[Directive[#],opPlotStyle]& /@ opColors)]
	]]];
(*Print[tempOptions];*)
	If[OptionValue[ImageSize] == Automatic, PrependTo[tempOptions, ImageSize -> 8*72*(tempLength/10)]];
	If[OptionValue[AspectRatio] == Automatic, PrependTo[tempOptions, AspectRatio -> 10*tempMaxMin/2000/tempLength]];

	tempOptions = Join[tempOptions,
			 {DataRange->{0,tempLength}},
			 FilterRules[Options[EEGPlotImpl], Except[tempOptions]]
	];
	If[OptionValue[EEGPageLabel]=!= None, 
			PrependTo[tempOptions, PlotLabel -> OptionValue[EEGPageLabel]]
	];
	tempOptions = FilterRules[tempOptions, Options[ListLinePlot]];

	(*====Plot====*)
	If[OptionValue[Rasterize],
		Rasterize[ListLinePlot[tempPlotData, tempOptions]],
		ListLinePlot[tempPlotData, tempOptions]
	]
];  


EEGPlotImpl[args___] := Message[EEGPlot::invalidArgs, {args}];


EEGPlot[eegData_EEGData, opts:OptionsPattern[]] :=
	Module[{tempPlotData, sampleRate, tempMaxMin, tempLength,temp,
	 opChannels, opBandpassFilter, opMontage, opEEGStackTraces, opEEGPageLength, opEEGBaselineSubtract,opEEGPageLabel},

	(*====Option Processing====*)
	opMontage=OptionValue[EEGMontage];
		opMontage=Switch[Head[opMontage], List, opMontage, String, EEGMontage[opMontage], _, EEGMontage["DoubleBanana"]];
	opChannels=(EEGChannels /. opMontage);
	opEEGStackTraces=OptionValue[EEGStackTraces];
	opBandpassFilter=OptionValue[BandpassFilter];
	opEEGPageLength=OptionValue[EEGPageLength];
	opEEGBaselineSubtract=OptionValue[BaselineSubtract];

	(*====Data Formatting====*)
	sampleRate = OptionValue[eegData, SampleRate];

	If[OptionValue[BaselineSubtract] === False,
		tempPlotData = eegData /@ (opChannels),
		tempPlotData = (Evaluate[EEGBaselineSubtract[eegData, opEEGBaselineSubtract]] /@ (opChannels))
	];
(*Print[tempPlotData];*)
	If[opBandpassFilter =!= False && opBandpassFilter =!= None, 
		(*tempPlotData = FilterBandpass[tempPlotData, opBandpassFilter, Options[eegData, SampleRate][[1,2]]];*)
		tempPlotData = FilterBandpass[#, opBandpassFilter, Options[eegData, SampleRate][[1,2]]]& /@ tempPlotData;
	];
	tempPlotData = StackTraces[tempPlotData, opEEGStackTraces];
	(*tempMaxMin = (Max[#] - Min[#]) & [Flatten[tempPlotData]];*)
	tempMaxMin = opEEGStackTraces * (Length[tempPlotData] + 1);
	tempLength = Length[tempPlotData[[1]]]/sampleRate;
	
	opEEGPageLabel=OptionValue[EEGPageLabel];


	(*====Divide into Pages and Plot====*)
	If[opEEGPageLabel=!= None, 
		tempCounter=-1;
		If[opEEGPageLength>=tempLength,
			EEGPlotImpl[tempPlotData, tempMaxMin, tempLength, opMontage, opChannels, opEEGStackTraces, sampleRate, 
				EEGPageLabel -> ToString[tempCounter++;tempCounter*10+{1,10}], opts],
			EEGPlotImpl[#, tempMaxMin, temp=Length[#[[1]]]/sampleRate, opMontage, opChannels, opEEGStackTraces, sampleRate, 
				EEGPageLabel-> ToString[tempCounter++; tempCounter*10+{1,temp}], opts]&
				/@ Transpose[Partition[#, opEEGPageLength*sampleRate, opEEGPageLength*sampleRate,{1,1},{}]& 
						/@ tempPlotData]
		],
		If[opEEGPageLength>=tempLength,
			EEGPlotImpl[tempPlotData, tempMaxMin, tempLength, opMontage, opChannels, opEEGStackTraces, sampleRate, opts],
			EEGPlotImpl[#, tempMaxMin, Length[#[[1]]]/sampleRate, opMontage, opChannels, opEEGStackTraces, sampleRate, opts]&
				/@ Transpose[Partition[#, opEEGPageLength*sampleRate, opEEGPageLength*sampleRate,{1,1},{}]& 
						/@ tempPlotData]
		]
	]
];  


tempCounter=1;


EEGPlot[args___] := Message[EEGPlot::invalidArgs, {args}];


(* ::Subsection::Closed:: *)
(*Layout*)


channelsTemp=$DetectorCoordinates[[All,1]];


$GetDetectorCoordinates[channel_String] := 
$GetDetectorCoordinates[channel]=
Module[{pos},
	pos=Flatten[Position[channelsTemp, channel]];
	If[Length[pos]==0, 
		Message[$GetDetectorCoordinates::noSuchChannel,channel],
		$DetectorCoordinates[[pos[[1]], 2]]
	]
];


$GetDetectorCoordinates[{ch1_String,ch2_String}] := 
$GetDetectorCoordinates[{ch1, ch2}]=
Module[{pos1, pos2},
	pos1=Flatten[Position[channelsTemp, ch1]];
	pos2=Flatten[Position[channelsTemp, ch2]];
	If[Length[pos2]==0,
		If[Length[pos1]==0,
			Message[$GetDetectorCoordinates::noSuchChannel, {ch1, ch2}],
			$DetectorCoordinates[[pos1[[1]], 2]]
		],
		Mean[ {$DetectorCoordinates[[pos1[[1]], 2]],$DetectorCoordinates[[pos2[[1]], 2]]}]
	]
];


$GetDetectorCoordinates[args___] := Message[$GetDetectorCoordinates::invalidArgs, {args}];


$GetDetectorCoordinates::noSuchChannel="The specified channel `1` was not found in the current layout!";


$DetectorNumbersPlot[]:=Graphics[Circle[#[[2]],$DetectorRadius]& /@$DetectorCoordinates];
$DetectorsPlot[]:=Graphics[Text[#[[1]],#[[2]]]& /@$DetectorCoordinates];


(* ::Subsection::Closed:: *)
(*FramePlot*)


FramePlot[data_List, channels:{_String ..}, opts:OptionsPattern[]]:=
	Module[{tempGr, dataScaled, opColorMap},
		(*Argument Check*)
		If[Length[data]!=Length[channels], Message[FramePlot::invalidArgs, channels]];

		dataScaled=Rescale[data];

		opColorMap=OptionValue[ColorMap];
		If[Head[opColorMap]==String,
			opColorMap=ColorData[opColorMap],
			If[!FunctionQ[opColorMap],
				Message[FramePlot::invalidArgs,opColorMap,"ColorMap"]
			]
		];

		tempGr= {opColorMap[#[[1]]],Disk[$GetDetectorCoordinates[#[[2]]], $DetectorRadius]}& 
			/@ Transpose[{dataScaled, channels}];

		tempGr=Flatten[{EdgeForm[Thin], tempGr}];
		Show[$FrameBackgroundPlot[ChannelOutlines->OptionValue[ChannelOutlines]], Graphics[tempGr], 
			Sequence@@FilterRules[{opts}, Options[Graphics]]
		]
	];


FramePlot[data_List, channels:{{_String, _String} ..}, opts:OptionsPattern[]]:=
	Module[{tempGr, dataScaled, opColorMap},
		(*Argument Check*)
		If[Length[data]!=Length[channels], Message[FramePlot::invalidArgs, channels]];

		dataScaled=Rescale[data];

		opColorMap=OptionValue[ColorMap];
		If[Head[opColorMap]==String,
			opColorMap=ColorData[opColorMap],
			If[!FunctionQ[opColorMap],
				Message[FramePlot::invalidArgs,opColorMap,"ColorMap"]
			]
		];

		tempGr= {opColorMap[#[[1]]],Disk[$GetDetectorCoordinates[#[[2]]], $DetectorRadius]}& 
			/@ Transpose[{dataScaled, channels}];

		tempGr=Flatten[{EdgeForm[None], tempGr}];
		Show[$FrameBackgroundPlot[ChannelOutlines->OptionValue[ChannelOutlines]], Graphics[tempGr], 
			Sequence@@FilterRules[{opts}, Options[Graphics]]
		]
	];


(*FramePlot[data_List, channels:{_String ..}, opts:OptionsPattern[]]:=
	Module[{tempGr, dataScaled, colorMap},
		If[Length[data]!=Length[channels], Message[FramePlot::invalidArgs, channels]];
		dataScaled=Rescale[data];
		colorMap=ColorData[OptionValue[ColorMap]];
		tempGr= {colorMap[#[[1]]],Disk[$GetDetectorCoordinates[#[[2]]], $DetectorRadius]}& 
			/@ Transpose[{dataScaled, channels}];
		tempGr=Flatten[{EdgeForm[None], tempGr}];
		Show[$FrameBackgroundPlot[], Graphics[tempGr], opts]
	];*)


FramePlot[args___] := Message[FramePlot::invalidArgs, {args}];


(* ::Subsection::Closed:: *)
(*$FrameBackgroundPlot, $FrameBackgroundPlot3D*)


(*$FrameBackgroundPlot[opts:OptionsPattern[]]:=
Graphics[ 
{White,EdgeForm[Thick],
Disk[{-50,0},10],Disk[{50,0},10],
Black, Disk[{0,20},40],
White,Disk[{0,0},50]},
Sequence@@JoinOptionLists[Graphics,
	{BaseStyle->{FontFamily->"Helvetica"}}, opts, Options[$FrameBackgroundPlot]]
]*)


$FrameBackgroundPlot[opts:OptionsPattern[]]:=
	Module[{gr},
		gr={ Thick,
			Circle[{0,0}, 50],
			Circle[{50,0}, 10,{-95 Degree, 95 Degree}],
			Circle[{-50,0}, 10,{85 Degree, 275 Degree}],
			Line[{{Cos[-5 Degree]*10,Sin[-5 Degree]*10+50},{0,60},
						{Cos[185 Degree]*10,Sin[185  Degree]*10+50}}]
		};
		If[OptionValue[ChannelOutlines]==True,
			gr=Join[gr,{Thin, Black}, Circle[#, $DetectorRadius]& /@ $DetectorCoordinates[[All, 2]] ];
		];
		
	Graphics[Graphics[gr],
		Sequence@@JoinOptionLists[{opts},Options[$FrameBackgroundPlot]]
	]
];


$FrameBackgroundPlot[args___] := Message[$FrameBackgroundPlot::invalidArgs, {args}];


$FrameBackgroundPlot3D[opts:OptionsPattern[]]:=
Module[{gr},
	gr={ParametricPlot3D[{Cos[theta],Sin[theta],0}*50, {theta,0,2 Pi},PlotStyle->Thick],
		ParametricPlot3D[{Cos[theta]*10+50,Sin[theta]*10,0}, {theta,-95 Degree,95 Degree}, PlotStyle->Thick],
		ParametricPlot3D[{Cos[theta]*10-50,Sin[theta]*10,0}, {theta,85 Degree,275 Degree}, PlotStyle->Thick],
		Graphics3D[{
			Thick,Line[{{Cos[-5 Degree]*10,Sin[-5 Degree]*10+50,0},{0,60,0},
			{Cos[185 Degree]*10,Sin[185  Degree]*10+50,0}}]
		}]
		};
	If[OptionValue[ChannelOutlines]==True,
		gr=Join[gr,
			ParametricPlot3D[{Cos[theta],Sin[theta],0}*$DetectorRadius+{#[[1]],#[[2]],0}, 
				{theta,0,2 Pi},PlotStyle->Directive[Black, Thin]]& /@ $DetectorCoordinates[[All, 2]] ]
	];
	
	Show[gr, Sequence@@JoinOptionLists[Show,{opts},Options[$FrameBackgroundPlot3D]]]
];


$FrameBackgroundPlot3D[args___] := Message[$FrameBackgroundPlot3D::invalidArgs, {args}];


(* ::Subsection:: *)
(*CoherencyPlot*)


Options[CoherencyPlotImpl]=Options[CoherencyPlot];


CoherencyPlotImpl[ch1_String,ch2_String, coherence_, threshold_, opts:OptionsPattern[]]:=
Module[{coord1, coord2, vect},
	coord1=$GetDetectorCoordinates[ch1];
	coord2=$GetDetectorCoordinates[ch2];
	vect=coord2-coord1;
	CoherencyPlotImpl[Norm[vect]/2, ArcTan@@vect, (coord1+coord2)/2, coherence, threshold, opts]
];


CoherencyPlotImpl[ch1:{_String,_String},ch2:{_String,_String}, coherence_, threshold_, opts:OptionsPattern[]]:=
Module[{coord1, coord2, vect},
	coord1=$GetDetectorCoordinates[ch1];
	coord2=$GetDetectorCoordinates[ch2];
	vect=coord2-coord1;
	CoherencyPlotImpl[Norm[vect]/2, ArcTan@@vect, (coord1+coord2)/2, coherence, threshold, opts]
];


CoherencyPlotImpl[radius_,angle_,{x_,y_}, coherence_, threshold_, opts:OptionsPattern[]]:=
Module[{temp, argFact, absFact, tempOpacity, rad, opThresholdOpacity},
	argFact=Abs[Arg[coherence]]/Pi; 
	absFact=Min[Abs[coherence]/OptionValue[CoherencyScaleMax],1];
	opThresholdOpacity=OptionValue[ThresholdOpacity];

	tempOpacity=Rescale[Abs[N[coherence]],{threshold, OptionValue[CoherencyScaleMax]},{opThresholdOpacity,1}];
	(*absFact=Min[Abs[coherence]/OptionValue[CoherencyScaleMax],1];
	tempOpacity=(Abs[N[coherence]]-threshold)/(1-threshold)*(1-opThresholdOpacity)+opThresholdOpacity;*)
	rad=$DetectorRadius*absFact*OptionValue[MacaroniRadiusFactor];

	ParametricPlot3D[
		{{Cos[angle],-Sin[angle],0},{Sin[angle],Cos[angle],0},{0,0,1}}.
		{Cos[t]*(rad*Cos[t2]+radius),rad*Sin[t2],
		Sin[t]*(rad*Cos[t2]+radius)}
		+{x,y,0},
		{t,0, Pi}, {t2 (*tube*), 0, 2 Pi},
		ColorFunctionScaling->False,
		ColorFunction->If[Arg[coherence]>= 0,
			Function[{xf,yf,z,u,v},Hue[-u/Pi/3,1,argFact,tempOpacity]],(**4]],*)
			Function[{xf,yf,z,u,v},Hue[(u/Pi-1)/3,1,argFact,tempOpacity]](**4]]*)   
		](*If*),
		(*PlotStyle->Opacity[Abs[coherence]*1],*)
		MeshStyle->None
	](*ParametricPlot3D*)
];


CoherencyPlot[coherencies_List, pairs_List, opts:OptionsPattern[]]:=
Module[{cohPairs, opCoherencyThreshold},
	opCoherencyThreshold=OptionValue[CoherencyThreshold];
	cohPairs=Flatten[#,1]&/@Transpose[{pairs,coherencies}];
	(*cohPairs=Flatten[#,1]&/@Transpose[{pairs,coherencies}];*)
	cohPairs=Select[cohPairs,Abs[#[[3]]]>opCoherencyThreshold&];

	Show[
		Sequence@@Table[CoherencyPlotImpl[n[[1]],n[[2]],n[[3]],opCoherencyThreshold, opts], {n,cohPairs}],
		$FrameBackgroundPlot3D[],
		
		Sequence@@JoinOptionLists[Graphics3D,
			{opts},{Lighting->"Neutral",PlotRange->All(*{{-65,65},{-65,65},{-1,50}}*),
			BaseStyle->{FontFamily->"Helvetica"}},Options[CoherencyPlot]
		]
	]
];


CoherencyPlot[args___] := Message[CoherencyPlot::invalidArgs, {args}];


(* ::Section:: *)
(*End, EndPackage*)


End[];
EndPackage[];


(*CoherencyPlotImpl[radius_,angle_,{x_,y_}, coherence_, threshold_, opts:OptionsPattern[]]:=
Module[{temp,fact, rad, opThresholdOpacity},
	fact=Abs[Arg[coherence]]/Pi;
	rad=$DetectorRadius*Abs[coherence];
	opThresholdOpacity=OptionValue[ThresholdOpacity];

	ParametricPlot3D[
		{{Cos[angle],-Sin[angle],0},{Sin[angle],Cos[angle],0},{0,0,1}}.
		{Cos[t]*(rad*Cos[t2]+radius),rad*Sin[t2],
		Sin[t]*(rad*Cos[t2]+radius)}
		+{x,y,0},
		{t,0, Pi}, {t2 (*tube*), 0, 2 Pi},
		ColorFunctionScaling->False,
		ColorFunction->If[Arg[coherence]>= 0,
			Function[{xf,yf,z,u,v},Hue[-u/Pi/3,1,fact,(Abs[N[coherence]]-threshold)/(1-threshold)*(1-opThresholdOpacity)+opThresholdOpacity]],(**4]],*)
			Function[{xf,yf,z,u,v},Hue[(u/Pi-1)/3,1,fact,(Abs[N[coherence]]-threshold)/(1-threshold)*(1-opThresholdOpacity)+opThresholdOpacity]](**4]]*)   
		](*If*),
		(*PlotStyle->Opacity[Abs[coherence]*1],*)
		MeshStyle->None
	](*ParametricPlot3D*)
];*)
