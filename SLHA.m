(* ::Package:: *)

(* Time-Stamp: <2015-01-03 13:31:08 misho> *)

(* :Context: SLHA` *)

(* :Author: Sho Iwamoto *)

(* :Summary:
    Package to read and write SLHA files.
*)

(* :Copyright: 2012-2015 Sho Iwamoto *)

(* :Package Version: 2.0beta *)

(* :Mathematica Version: 10.0 *)

(* :History:
   Version 1.0 [Oct 2013, SI] Initial version.
   Version 2.0 [Jan 2015, SI] ... implementing
*)

(* :Keywords: *)

(* :Discussion: *)

(* :Warning: *)

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

(* Usage messages *)

ReadSLHA::usage  = "ReadSLHA[filename] reads the SLHA file and returns SLHA data.";
Data::usage      = "Data[SLHA, blockname, keys...] returns the corresponding data, or Null.";
GetData::usage   = "GetData[SLHA, blockname, keys...] is similar to Data, but does not allow Null.";
GetBlock::usage  = "GetBlock[SLHA, blockname] copys the block.";
Decay::usage     = "Decay[SLHA, pid] returns decay data of the particle pid.";
Width::usage     = "Width[SLHA, pid] returns the decay width of the particle pid.";
Br::usage        = "Br[SLHA, pid, {daughters}] returns BR(pid -> daughters).";
WriteSLHA::usage = "ReadSLHA[filename, SLHA] writes the SLHA data to the file.";
WriteBlock::usage = "WriteBlock[ofs, block] is an internal command to output a block.";
WriteBlockAsComment::usage = "WriteBlockAsComment[ofs, block] is an internal command to output a block as a comment.";

Begin["`Private`"];
(* ----------------------------------------------------------------------
      Low Level Tools
   ---------------------------------------------------------------------- *)
ReadNumber[x_List]   := ReadNumber /@ x;
ReadNumber[x_String] := Read[StringToStream[x],Number]
ReadNumberAtFirst[x_List] := ReplacePart[x, 1 -> ReadNumber[x[[1]]]];

ReadDecayLine[x_List] := Module[{values},
  values = ReadNumber[x];
  If[Or[Length[values] < 2, Length[values] != 2+values[[2]]],
    Message[ReadSLHA::InvalidLineFound, Join[str, " "]]
  ];
  Join[values[[;; 2]], Sort[values[[3 ;;]]]]
];

(* ----------------------------------------------------------------------
      READ
   ---------------------------------------------------------------------- *)
ReadSLHA::InvalidLineFound="Invalid lines found: `1`.";
ReadSLHA::OrphanLineFound="Lines with no block found: `1`.";
ReadSLHA::InvalidBlockLineFound="Invalid lines found: `1`.";
ReadSLHA[inputfilename_]:=LinesToBlocks[ReadList[inputfilename,Record]]

LinesToBlocks[lines_]:=Module[{data,blockname,pid,key,q},
  data=Select[LineSplitWithComments/@lines,Length[#[[1]]]>0&];
  blockname="";
  Reap[
    (
      Which[
        ToUpperCase[#[[1,1]]] == "BLOCK",
          blockname=ToUpperCase[#[[1,2]]];
          Sow[blockname,blockname];
          If[And[Length[#[[1]]]>=3, (q = ParseQRule[StringJoin[ToString/@#[[1,3;;]]]])!=""],
            Sow[{"Q"->ReadNumber[q]},blockname]
          ,
            Sow[{},blockname]
          ];
          Sow[#,blockname], (* Sow the line again. *)
        ToUpperCase[#[[1,1]]] == "DECAY",
          If[length[#[[1]]] != 3,
            Message[ReadSLHA::InvalidBlockLine,str];
            Abort[];
          ];
          blockname = ToUpperCase[#[[1,1]] <> " " <> #[[1,2]]]; (* ex. "DECAY 1000006" *)
          Sow[blockname,blockname];
          Sow[{"WIDTH"->ReadNumber[#[[1,3]]]}, blockname];
          Sow[#,blockname], (* Sow the line again. *)
        True,
          Which[
            blockname == "",
              Message[ReadSLHA::OrphanLineFound,#];Abort[],
            blockname == "SPINFO",
              (* SPINFO block contains string value *)
              Sow[{ReadNumberAtFirst[#[[1]]],#[[2]]},blockname],
            StringPosition[blockname,"DECAY"] != {},
              Sow[{ReadDecayLine[#[[1]]], #[[2]]}, blockname],
            True,
              Sow[{ReadNumber[#[[1]]],#[[2]]},blockname]
          ];
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
Data::MultiBlocksFound = "Multi blocks with name `1` found.";
Data::MultiColumnsFound = "Block `1` has multi column for `2`.";
Data[SLHA_,blockname_,keys___]:=Module[{block,lines,keylist,keylength},
  block=Select[SLHA,#[[1]]==ToUpperCase[blockname]&];
  If[Length[block]==0,Return[]];
  If[Length[block]>1,Message[Data::MultiBlocksFound,blockname];Abort[]];
  keylist={keys};
  keylength=Length[keylist];
  lines=Select[block[[1,3;;]],
    If[keylength==0,True,
      And[Length[#[[1]]]==keylength+1,#[[1,1;;keylength]]==keylist]&
    ]
  ];
  If[Length[lines]==0,Return[]];
  If[Length[lines]>1,Message[Data::MultiColumnsFound,blockname,keylist];Abort[]];
  lines[[1,1,-1]]];

GetData::BlockNotFound = "Block `1` required but not found.";
GetData::ColumnNotFound = "Block `1` Column `2` required but not found.";
GetData[SLHA_,blockname_,keys___] := Module[
    {v = Data[SLHA, blockname, keys]},
    If[v === Null,
       If[Length[Select[SLHA,#[[1]]==ToUpperCase[blockname]&]] == 0,
          Message[GetData::BlockNotFound,blockname],
          Message[GetData::ColumnNotFound,blockname,{keys}]];
       Abort[]];
    v];

GetBlock[slha_, blockname_] := Module[
    {d},
    d = Select[slha, #[[1]] == ToUpperCase[blockname]&];
    If[Length[d] == 0, Return[]];
    If[Length[d] > 1, Message[Global::MultiBlocksFound, blockname]; Abort[]];
    d[[1]]];

Decay[SLHA_, pid_Integer] :=
  Module[{blockname, width, brs},
   blockname = "DECAY " <> ToString[pid];
   block = Select[SLHA, #[[1]] == ToUpperCase[blockname] &];
   If[Length[block] == 0, Message[GetData::BlockNotFound, blockname]; Abort[]];
   If[Length[block] > 1, Message[GetData::MultiBlocksFound, blockname]];
   width = "WIDTH" //. block[[1, 2]];
   brs = block[[1, 4 ;;, 1]];
   {width, {#[[3 ;;]], #[[1]]} & /@ brs}
   ];
Width[SLHA_, pid_Integer] := Decay[SLHA, pid][[1]]
Br[SLHA_, pid_Integer, x_List] := Module[{lines},
  lines = Select[Decay[SLHA, pid][[2]], #[[1]] == Sort[x] &];
  If[Length[lines] > 1, Message[Data::MultipleBrLineFound, pid, x]; Abort[]];
  If[Length[lines] == 0, 0, lines[[1, 2]]]]

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
WriteBlockAsComment[ofs_,block_]:=Module[{},
  WriteString[ofs,"#"<>BuildBlockLine[block]];
  WriteString[ofs,StringReplacePart[BuildDataLine[block[[1]],#], "#", {1,1}]] & /@ block[[4;;]];
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
    "NMIX"|"UMIX"|"VMIX"|"STOPMIX"|"SBOTMIX"|"STAUMIX"|"AU"|"AD"|"AE"|"YU"|"YD"|"YE",
      " "<>IntegerPadding[line[[1,1]],2]<>IntegerPadding[line[[1,2]],3]<>"   "<>ToFString[line[[1,3]]]<>"   #"<>line[[2]]<>"\n",
    "ALPHA",
      "         "<>ToFString[line[[1,1]]]<>"   #"<>line[[2]]<>"\n",
    "SPINFO",
      " "<>IntegerPadding[line[[1,1]],5]<>"   "<>StringPadding[line[[1,2]],16]<>" #"<>line[[2]]<>"\n",
    _,
      IntegerPadding[line[[1,1]],6]<>"   "<>ToFString[line[[1,2]]]<>"   #"<>line[[2]]<>"\n"
]]
End[];
EndPackage[];

