(* ::Package:: *)

(* :Title: EEGPack *)


(* ::Section:: *)
(*Pre-Package Global*)


(*<<KazukazuM`;*)


(* ::Section:: *)
(* BeginPackage *)


BeginPackage["EEGPack`Data`"(*, {"KazukazuM`","KazukazuM`Signal`","KazukazuM`Plots`","KazukazuM`Filters`"}*)];


(* ::Section:: *)
(* Declarations *)


ImportEEG::usage="Imports EEG Files to an EEGData[] object";
Options[ImportEEG]={Verbose->False};


EEGData::usage="Data structure containing EEG data, channel information, \
sampling rate (SampleRate), and patient (PatientID) information.";
Options[EEGData]={SampleRate->Automatic, PatientID->None};


PatientID::usage="PatientID within EEGData Object.";


EEGBaselineSubtract::usage=" ";


(* ::Section:: *)
(* Begin *)


Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*ImportEEG*)


ImportEEG[fileName_/;StringMatchQ[fileName, __ ~~ ".eeg"],opts:OptionsPattern[]]:=
Module[{fileStream,header,data,gain,
hSamples,hPatientID,hRate,  hChannelList,hMontageRawTypes,hNchanFile,hSegments,
hUvPerBit,hDCUvPerBit},

fileStream=OpenRead[fileName,BinaryFormat->True];
SetStreamPosition[fileStream,-4];
hSamples=BinaryRead[fileStream,"Integer32",ByteOrdering->-1]/2;
SetStreamPosition[fileStream,hSamples*2];

(*Read and process header information*)
header=ReadList[fileStream,"String"];
hPatientID=Flatten[StringCases[header,
 RegularExpression["\\s*PatientId\\s*=\\s*(.*)\\x*"]->"$1"
]][[1]];
hRate=ToExpression[Flatten[StringCases[header,WhitespaceCharacter... ~~"Rate =" ~~WhitespaceCharacter... ~~ x:NumberString ~~WhitespaceCharacter... ~~ "Hz" ~~ WhitespaceCharacter... ->x]][[1]]];

hChannelList=Flatten[StringCases[header,WhitespaceCharacter... ~~"MontageRaw =" ~~WhitespaceCharacter... ~~ x__ ~~ EndOfString ->x]];
(*hChannelList=Drop[ToExpression["{"<>hChannelList[[1]]<>"Null }"],-1];*)
hChannelList=Flatten[hChannelList];hChannelList=StringSplit[hChannelList[[1]],","];

hMontageRawTypes=StringCases[header,WhitespaceCharacter... ~~"MontageRawTypes =" ~~WhitespaceCharacter... ~~ x__ ~~ EndOfString ->x];(*hMontageRawTypes=Drop[ToExpression["{"<>Flatten[hMontageRawTypes][[1]]<>"Null}"],-1];*)
hMontageRawTypes=Flatten[hMontageRawTypes];hMontageRawTypes=StringSplit[hMontageRawTypes[[1]],","];

hNchanFile=ToExpression[Flatten[StringCases[header,WhitespaceCharacter... ~~"NchanFile =" ~~WhitespaceCharacter... ~~ x:NumberString~~ EndOfString ->x]][[1]]];
If[hNchanFile!=Length[hChannelList],
hChannelList=PadRight[hChannelList,hNchanFile]
];
If[hNchanFile!=Length[hMontageRawTypes],
hMontageRawTypes=PadRight[hMontageRawTypes,hNchanFile]
];

hUvPerBit=ToExpression[Flatten[StringCases[header,(WhitespaceCharacter... ~~"UvPerBit =" ~~WhitespaceCharacter... ~~ x:NumberString ~~  WhitespaceCharacter... ~~ EndOfString)->x]][[1]]];hDCUvPerBit=ToExpression[Flatten[StringCases[header,(WhitespaceCharacter... ~~"DCUvPerBit =" ~~WhitespaceCharacter... ~~ x:NumberString ~~  WhitespaceCharacter... ~~ EndOfString)->x]][[1]]];

(*Read and process data*)
gain=ReplaceAll[hMontageRawTypes, "EEG"-> hUvPerBit];
gain=ReplaceAll[gain, "DC"-> hDCUvPerBit];

(*Actually reading binary data*)
SetStreamPosition[fileStream,0];
data=BinaryReadList[fileStream,"Integer16", hSamples];
data=gain*Transpose[Partition[data,hNchanFile]];

(*Data Segments*)
hSegments=Flatten[StringCases[header,(WhitespaceCharacter...~~x:NumberString~~WhitespaceCharacter...~~y:NumberString ~~WhitespaceCharacter...~~ "1"
~~WhitespaceCharacter... ~~ "Sample")->{x,y}],1];
If[Length[hSegments]>0,
hSegments=Transpose[hSegments][[2]];
hSegments=Prepend[ToExpression/@hSegments, 0];
hSegments=Append[hSegments,Dimensions[data][[2]]];
hSegments=Union[hSegments];  (*Eliminate 0-element long segments*)
hSegments=Plus[#,{1,0}]& /@ Partition[hSegments,2,1];
];

(*Verbose Output*)
If[OptionValue[Verbose],
Print[TextCell[
	">> " <> fileName <> "\n" <>
	" PatientID -> "<> ToString[hPatientID] <> "\n" <>
	" SampleRate -> "<> ToString[hRate] <> "\n" <>
	" Dimensions: "<>ToString[Dimensions[data]] <> 
	" Data Segments: "<>ToString[hSegments]<>"\n" <>
	" Channels = "<> ToString[hChannelList],
	"Code"]
]];

Close[fileStream];

(*Return data*)
If[Length[hSegments]>0,
EEGData[data[[All,#]],hChannelList,  SampleRate->hRate, PatientID->hPatientID]& /@ Apply[Span ,hSegments,2],
EEGData[data,hChannelList,  SampleRate->hRate, PatientID->hPatientID]
]

];


(* ::Subsection::Closed:: *)
(*ImportEEG -- EDF files*)


ImportEEG[edfFile_/;StringMatchQ[edfFile, __ ~~ ".edf"], opts:OptionsPattern[]]:=
Module[{data, nondatapos,
	(*hSamples,*) hPatientID, hRecordCount,hRecordLength,hRate,hChannelList},

data=Import[edfFile];
(*Sometimes data includes non-traces like Pulse Rate*)
nondatapos=Position[(Length /@ data), x_/;x!=Length[data[[1]]]];
data=Delete[data, nondatapos];

hChannelList=Import[edfFile,"Labels"];
hChannelList=Delete[hChannelList, nondatapos];
hChannelList=ReleaseHold[
StringReplace[hChannelList,
{"EEG " ~~ x__ ~~ xn:{NumberString, "z"} ~~ {"","-"} ~~ y__ ~~ yn:{NumberString, "z"}  -> Hold["{"<>x<>xn<>", "<>y<>yn<>"}"],
"EEG " ~~ x__ ~~ xn:{NumberString, "z"} ~~ {"","-"} ~~{"REF","LE"}  ->Hold[ x<>xn],
"EEG " ~~ {"EKG","ECG"} ~~ {"","-"} ~~{"REF","LE"}  -> "EKG",
"EEG "~~ x__ ~~ "-" ~~{"REF","LE"}  -> Hold[x],
x__ ~~ "-" ~~{"REF","LE"}  -> Hold[x],
"ECG EKG"  -> "EKG"
}
]];
hChannelList=If[StringMatchQ[#,"{"~~___~~","~~___~~"}"],ToString /@ ToExpression[#],#]& /@ hChannelList;

hPatientID=Import[edfFile,"PatientID"];
hRecordCount=Import[edfFile,"RecordCount"];(*number of records*)
hRecordLength=Import[edfFile,"RecordLength"];(*length of each record*)
hRate=Dimensions[data][[2]] / hRecordCount / hRecordLength;

(*Verbose Output*)
If[OptionValue[Verbose],
Print[TextCell[
	">> " <> edfFile <> "\n" <>
	" PatientID -> "<> ToString[hPatientID] <> "\n" <>
	" SampleRate -> "<> ToString[hRate] <> "\n" <>
	" Dimensions: "<>ToString[Dimensions[data]] <> 
	" (Length "<>ToString[hRecordCount*hRecordLength] <> " seconds)\n" <>
	" Channels = "<> ToString[hChannelList],
	"Code"]
]];

(*Return data*)
EEGData[data, hChannelList,  SampleRate->hRate, PatientID->hPatientID]

];


(* ::Subsection::Closed:: *)
(*ImportEEG -- Catch for invalid arguments*)


ImportEEG[args___] := Message[ImportEEG::invalidArgs, {args}];


(* ::Subsection:: *)
(*EEGData (data extraction) *)


EEGData[data_/;MatrixQ[data], channels_List, opts:OptionsPattern[]][ch_String,span_Span]:=
	data[[ Position[channels,ch][[1,1]], span]];
EEGData[data_/;MatrixQ[data], channels_List, opts:OptionsPattern[]][ch_String]:= 
	EEGData[data, channels, opts][ch, ( ;; )];


EEGData[data_/;MatrixQ[data], channels_List, opts:OptionsPattern[]][{ch1_String,ch2_String}]:= 
Module[{pos1, pos2},
	pos1=Position[channels,{ch1,ch2}];
	If[Length[pos1]>0, 
		data[[ pos1[[1,1]] ]],

		pos1=Position[channels,ch1];
		pos2=Position[channels,ch2];

		If[Length[pos1]*Length[pos2]>0,
			data[[ pos1[[1,1]] ]] - data[[ pos2[[1,1]] ]],
			Message[EEGData::missingChannels, {ch1, ch2}]
		]

		(*If[Length[pos1]*Length[pos2]>0,
			data[[ pos1[[1,1]] ]] - data[[ pos2[[1,1]] ]],
			pos1=Position[channels,ch1<>"-"<>ch2];
			If[Length[pos1]>0,
				data[[ pos1[[1,1]] ]],
				pos2=Position[channels,ch2<>"-"<>ch1];
				If[Length[pos2]>0,
					-data[[ pos2[[1,1]] ]]
				]
			]
		]*)
	]
];


EEGData[data_/;MatrixQ[data], channels_List, opts:OptionsPattern[]][{ch1_String,{ch2_String,ch3_String}}]:= 
Module[{pos1, pos2, pos3},

	(*pos1=Position[channels,{ch1,ch2,ch3}];
	If[Length[pos1]>0, 
		data[[ pos1[[1,1]] ]],*)

		pos1=Position[channels,ch1];
		pos2=Position[channels,ch2];
		pos3=Position[channels,ch3];

		If[Length[pos1]*Length[pos2]*Length[pos3]>0,
			data[[ pos1[[1,1]] ]] - (data[[ pos2[[1,1]] ]] + data[[ pos3[[1,1]] ]])/2,
			Message[EEGData::missingChannels, {ch1, {ch2, ch3}}];
		]
	(*]*)
];


EEGData[data_/;MatrixQ[data], channels_List, opts:OptionsPattern[]][{ch1_String,ch2_String}, span_Span]:= 
		EEGData[data, channels, opts][{ch1,ch2}][[span]];


EEGData::missingChannels = "Not all specified channels `1` were found!";


(* ::Subsection:: *)
(*InvertChannel*)


InvertChannel[data_EEGData, ch1_String]:= 
Module[{pos1},
	pos1=Position[data[[2]],ch1];
	If[Length[pos1]>0,
		pos1=pos1[[1,1]];
		ReplacePart[data, {1, pos1} -> (-1 * data[[1, pos1]])  ],
		Message[EEGData::missingChannels, {ch1}]
	]
];


InvertChannel[args___] := Message[InvertChannel::invalidArgs, {args}];


(* ::Subsection:: *)
(*SwitchChannel*)


SwitchChannel[data_EEGData, {ch1_String, ch2_String}]:= 
Module[{pos1, pos2, data1, data2},
	pos1=Position[data[[2]],ch1];
	pos2=Position[data[[2]],ch2];

	If[Length[pos1]*Length[pos2]>0,
		pos1=pos1[[1,1]];
		pos2=pos2[[1,1]];
		data1=data[[1, pos1]];
		data2=data[[1, pos2]];

		ReplacePart[data, {{1, pos1} -> data2, {1, pos2} -> data1} ],

		Message[EEGData::missingChannels, {ch1}]
	]
];


SwitchChannel[args___] := Message[SwitchChannel::invalidArgs, {args}];


(* ::Subsection:: *)
(*EEGBaselineSubtract*)


EEGBaselineSubtract[eegData_EEGData, number_Integer] :=
Module[{tempData, baselines},

	tempData=eegData[[1]];
	baselines=Mean /@ tempData[[All, 1;;number]];
	EEGData[tempData-baselines, Sequence@@Rest[eegData]]
];  


EEGBaselineSubtract[args___] := Message[EEGBaselineSubtract::invalidArgs, {args}];


(* ::Subsection:: *)
(*Filter*)


(* ::Subsection:: *)
(*Hilbert*)


(* ::Section:: *)
(*End, EndPackage*)


End[];
EndPackage[];


(* ::Section::Closed:: *)
(*Backup*)


(*EEGPlot[eegData_EEGData, opts : OptionsPattern[]] :=
	Module[{tempPlotData, tempMaxMin, tempLength,
		tempOptions, tempOptionsSegment, sampleRate,opBandpassFilter,
		k, tempPlotDataSeg},

	tempPlotData = eegData /@ OptionValue[Montage];
	opBandpassFilter=OptionValue[BandpassFilter];
	If[opBandpassFilter =!= False && opBandpassFilter =!= None, 
		tempPlotData = FilterBandpass[tempPlotData, opBandpassFilter, Options[eegData, SampleRate][[1,2]]];
	];

	tempPlotData = StackTraces[tempPlotData, 100];
	tempMaxMin = (Max[#] - Min[#]) & [Flatten[tempPlotData]];
	sampleRate = SampleRate /. Cases[eegData, x_ /; Head[x] === Rule];
	tempLength = Length[tempPlotData[[1]]]/sampleRate;

	tempOptions = {opts};
	If[OptionValue[GridLines] == Automatic,
		PrependTo[tempOptions, 
		GridLines -> {Range[0, #2, sampleRate]&, None}]
	];
  
	If[tempLength <= 10,
		If[OptionValue[ImageSize] == Automatic,
			PrependTo[tempOptions, ImageSize -> 8*72*(tempLength/10)]];
		If[OptionValue[AspectRatio] == Automatic,
			PrependTo[tempOptions, AspectRatio -> 10*tempMaxMin/2000/tempLength]];
		tempOptions = 
			Join[tempOptions,
					FilterRules[Options[EEGPlot], Except[tempOptions]]
			];
		tempOptions = FilterRules[tempOptions,Options[ListLinePlot]];
		Print[Rasterize[ListLinePlot[tempPlotData, tempOptions]]],

		For[k = 1, k <= Ceiling[tempLength/10], k++,
			tempOptionsSegment = tempOptions;

			tempPlotDataSeg = 
				tempPlotData[[All, 
						(k - 1)*10*sampleRate + 1 ;;
						Min[k*10*sampleRate, Length[ tempPlotData[[1]] ] ]
							]];

			If[OptionValue[ImageSize] == Automatic,
				PrependTo[tempOptionsSegment, 
				ImageSize -> 8*72*((Length[tempPlotDataSeg[[1]]]/sampleRate)/10)]
			];
			If[OptionValue[AspectRatio] == Automatic,
				PrependTo[tempOptionsSegment, 
				AspectRatio -> 
					10*tempMaxMin/2000/(Length[tempPlotDataSeg[[1]]]/sampleRate)]
			];
			(*If[OptionValue[PlotRange]==Automatic,
				PrependTo[tempOptionsSegment, 
				PlotRange->{(k-1)*10*sampleRate+{1,Length[tempPlotDataSeg[[1]]]}, All}]
			];*)

			tempOptionsSegment = 
				Join[tempOptionsSegment, 
					FilterRules[Options[EEGPlot], Except[tempOptionsSegment]]
				];
    		tempOptionsSegment = FilterRules[tempOptionsSegment,Options[ListLinePlot]];
			Print[Rasterize[ListLinePlot[tempPlotDataSeg, tempOptionsSegment]]];
		];
	](*If[tempLength < 10,*)
];  *)
