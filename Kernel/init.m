(* ::Package:: *)

(* Mathematica Init File *)

Get[ "EEGPack`Data`"];
Get[ "EEGPack`Plots`"];


Block[{tempData},
tempData= FindFile["EEGPack`PacletInfo`"];
If[tempData =!= $Failed,
	tempData=Import[tempData];
	tempData=(Version /. List@@tempData),
	(*Alternate paclet path*)
	tempData= "(PacletInfo.m not found)"
];

Print["<<EEGPack version "<>tempData<>" loaded on: "<> DateString[]<>">>"]
];
