(* ::Package:: *)

BeginPackage["SLHA`"];

(* ----------------------------------------------------------------------
   SLHA data format :
   <SLHA>         = List[<BLOCK>, <BLOCK>, ... ]
   <BLOCK>        = List[<TITLE>, <BLOCKOPTION>, <LINE>, <LINE>, ... ]
   <TITLE>        = String
   <BLOCKOPTIONS> = List[<SRULE>, <SRULE>, ... ]
   <SRULE>        = Rule[String, Expression]
   <LINE>         = List[<DATA>, <COMMENT>]
   <COMMENT>      = String
   <DATA>         = List[Expression, Expression, ... ]
   ---------------------------------------------------------------------- *)

(* ---------------------------------------------------------------------- *)
(* Usage Messages                                                         *)
(* ---------------------------------------------------------------------- *)
ReadSLHA::usage = "ReadSLHA[filename] reads the SLHA file and returns SLHA data.";
Data::usage     = "Data[SLHA, blockname, keys...] returns the corresponding data.";
WriteSLHA::usage = "ReadSLHA[filename, SLHA] writes the SLHA data to the file.";

Begin["`Private`"];
(* ----------------------------------------------------------------------
      Low Level Tools
   ---------------------------------------------------------------------- *)
ReadNumber[str_] :=Read[StringToStream[str],Number]


(* ----------------------------------------------------------------------
      READ
   ---------------------------------------------------------------------- *)
ReadSLHA::OrphanLineFound="Lines with no block found: `1`.";
ReadSLHA::InvalidBlockLineFound="Invalid lines found: `1`.";
ReadSLHA[inputfilename_]:=LinesToBlocks[ReadList[inputfilename,Record]]

LinesToBlocks[lines_]:=Module[{data,blockname,q},
  data=Select[LineSplitWithComments/@lines,Length[#[[1]]]>0&];
  blockname="";
  Reap[
    (
      If[ToUpperCase[#[[1,1]]]=="BLOCK",
        blockname=ToUpperCase[#[[1,2]]];
        Sow[blockname,blockname];
        If[And[Length[#[[1]]]>=3, (q = ParseQRule[StringJoin[ToString/@#[[1,3;;]]]])!=""],
          Sow[{"Q"->ReadNumber[q]},blockname],
          Sow[{},blockname]
        ];
        Sow[#,blockname],
        If[blockname=="",Message[ReadSLHA::OrphanLineFound,#];Abort[]];
        Sow[{ReadNumber/@#[[1]],#[[2]]},blockname]
      ];
    )&/@data
  ][[2]]
]
LineSplitWithComments[line_]:=Module[{datacomment,data,q},
  datacomment=StringSplit[line,"#",2];
  {StringSplit[datacomment[[1]]],If[Length[datacomment]==1,"",datacomment[[2]]]}
]
ParseQRule[str_]:=Module[{Qmatch},
  Qmatch=StringCases[str,RegularExpression["^\\s*Q\\s*=\\s*([\\d\\+\\.\\-ED]+)\\s*$"]->"$1",IgnoreCase->True];
  If[Length[Qmatch]==1,
    Qmatch[[1]],
    If[StringMatchQ[str,RegularExpression["^\\s*$"]],
      "",
      Message[ReadSLHA::InvalidBlockLine,str]
    ]
  ]
]

(* ----------------------------------------------------------------------
      DATA
   ---------------------------------------------------------------------- *)
Data[SLHA_,blockname_,keys___]:=Module[{block,lines,keylist,keylength},
  block=Select[SLHA,#[[1]]==ToUpperCase[blockname]&];
  If[Length[block]==0,Message[Data::BlockNotFound,blockname];Abort[]];
  If[Length[block]>1,Message[Data::MultiBlocksFound,blockname]];
  keylist={keys};
  keylength=Length[keylist];
  lines=Select[block[[1,3;;]],
    If[keylength==0,True,
      And[Length[#[[1]]]==keylength+1,#[[1,1;;keylength]]==keylist]&
    ]
  ];
  If[Length[lines]==0,Message[Data::ColumnNotFound,blockname,keylist];Abort[]];
  If[Length[lines]>1,Message[Data::MultiColumnsFound,blockname,keylist]];
  lines[[1,1,-1]]
]
Data::BlockNotFound="Block \"`1`\" not found.";
Data::MultiBlocksFound="Block \"`1`\" found more than one. First one is used.";
Data::ColumnNotFound="Column `2` in Block \"`1`\" not found.";
Data::MultiColumnsFound="Column `2` in Block \"`1`\" found more than one. First one is used.";
(* ----------------------------------------------------------------------
      WRITE
   ---------------------------------------------------------------------- *)
StringPadding[str_,len_]:=If[StringLength[str]<len,StringJoin@@Prepend[Table[" ",{len-StringLength[str]}],str],str]
IntegerPadding[int_,len_]:=Module[{s},s=ToString[int];If[StringLength[s]<len,StringJoin@@Append[Table[" ",{len-StringLength[s]}],s],s]]
ToFString[num_Real]:=Module[{m,e},
  {m,e}=If[num==0.,{0,1},MantissaExponent[N[num,9]]];
  " "<>ToString[PaddedForm[m*10,{9,8},NumberSigns->{"-"," "}]]<>"e"<>ToString[PaddedForm[e-1,2,NumberSigns->{"-","+"},SignPadding->True,NumberPadding->"0"]]
]
ToFString[num_Integer]:=StringPadding[IntegerPadding[num,11],16]
WriteSLHA[outputfilename_,SLHA_]:=Module[{ofs},
  ofs=OpenWrite[outputfilename,PageWidth->Infinity];
  WriteBlock[ofs,#]&/@SLHA;
  Close[ofs]
]
WriteBlock[ofs_,block_]:=Module[{},
  WriteString[ofs,BuildBlockLine[block]];
  WriteString[ofs,BuildDataLine[block[[1]],#]]&/@block[[4;;]];
  WriteString[ofs,"#\n"];
]
BuildBlockLine[block_]:=Module[{q},
  q="Q"//.block[[2]];
  StringJoin@@{"Block ",block[[1]],If[NumberQ[q]," Q= "<>ToFString[N[q]],""]}<>If[block[[3,2]]=="",""," #"<>block[[3,2]]]<>"\n"
]
BuildDataLine[blockname_,line_]:=Module[{q},
  Switch[blockname,
    "MASS",
      " "<>IntegerPadding[line[[1,1]],9]<>"   "<>ToFString[line[[1,2]]]<>"   #"<>line[[2]]<>"\n",
    "NMIX"|"UMIX"|"VMIX"|"STOPMIX"|"SBOTMIX"|"STAUMIX",
      " "<>IntegerPadding[line[[1,1]],2]<>" "<>IntegerPadding[line[[1,2]],3]<>"   "<>ToFString[line[[1,3]]]<>"   #"<>line[[2]]<>"\n",
    "ALPHA",
      "         "<>ToFString[line[[1,1]]]<>"   #"<>line[[2]]<>"\n",
    "SPINFO",
      " "<>IntegerPadding[line[[1,1]],5]<>"   "<>StringPadding[line[[1,2]],16]<>" #"<>line[[2]]<>"\n",
    _,
      IntegerPadding[line[[1,1]],6]<>"   "<>ToFString[line[[1,2]]]<>"   #"<>line[[2]]<>"\n"
]]
End[];

EndPackage[];



