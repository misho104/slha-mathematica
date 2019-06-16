(* ::Package:: *)

(* Time-Stamp: <2019-06-12 09:57:22> *)

(* :Context: SLHA` *)

(* :Author: Sho Iwamoto *)

(* :Summary:
    Package to read and write SLHA files.
*)

(* :Copyright: 2012-2015 Sho Iwamoto *)

(* :Package Version: 2.0beta *)

(* :Mathematica Version: 10.0 *)

(* :History:
   Version 1.0 [    2012, SI] Initial version.
   Version 2.0 (beta version) Rewrite everything.
*)

(* :Keywords: *)

(* :Discussion: *)

(* :Warning: *)



BeginPackage["SLHA`"];


(* Usage messages *)

F := "\!\(\*StyleBox[\""<>#1<>"\", \""<>#2<>"\"]\)" &;
M := "\""<>F[#,"MR"]<>"\"" &;
T := F[#,"TI"] &;

ReadSLHA::usage  = "ReadSLHA["<>T["filename"]<>"] returns an SLHA object read from an SLHA file "<>T["filename"]<>".";
IsMissing::usage = "IsMissing["<>T["object"]<>"] checks whether the head of "<>T["object"]<>" is 'Missing' or not. This check is performed recursively in order to be used when a value in a missing block is referred; i.e., so that IsMissing[Missing[___][___]] returns True.";
IfMissing::usage = "IfMissing is an option for objects with SLHA packages. Values are case insensitive.
For blocks, "<>M["missing"]<>" (default), "<>M["abort"]<>", or "<>M["create"]<>" can be specified.
For values, "<>M["missing"]<>" (default), "<>M["abort"]<>", or numeric values can be specified; the numeric value is treated as the default value, i.e., if the item is missing, the value is returned.";

slha::usage = "SLHA package utilizes local variables "<>T["slha$xxx"]<>" as SLHA objects. Available operations are as follows.
slha["<>M["block"]<>", "<>T["name"]<>", "<>T["(option)"]<>"] returns a BLOCK object with the name "<>T["name"]<>". Options are: IfMissing->"<>T["value"]<>".
slha["<>T["name"]<>", "<>T["(option)"]<>"] is a syntax sugar of slha["<>M["block"]<>", "<>T["name"]<>", "<>T["(option)"]<>"].
slha["<>M["decay"]<>", "<>T["pid"]<>", "<>T["(option)"]<>"] returns a 'decay' object for the particle "<>T["pid"]<>". Options are: IfMissing->"<>T["value"]<>".
slha["<>M["blocks"]<>"] returns a list of the names of the blocks in the SLHA object.
slha["<>M["tostring"]<>"] returns string expression of the SLHA object as a list of lines.
slha["<>M["writetofile"]<>", "<>T["filename"]<>"] writes out the SLHA object to the file.";

block::usage = "SLHA package utilizes local variables "<>T["block$xxx"]<>" as BLOCK objects.

The object can be modified by the following accessors.
block["<>M["headcomment"]<>"] contains the comment string associated to the first line (beginning with \"BLOCK\") of the BLOCK.
block["<>M["Q"]<>"] contains the Q-value of the BLOCK.
block["<>T["key_Integer ..."]<>", "<>T["(option)"]<>"] contains the values associated to "<>T["key"]<>". Options are: IfMissing->"<>T["value"]<>".
block["<>M["c"]<>", "<> T["_Integer ..."] <> "] contains the comment string associated to the value.

The following functions are defined in addition.
block["<>M["keys"]<>"] returns a list of the keys associated to the BLOCK.
block["<>M["tostring"]<>", "<>T["(option)"]<>"] returns string expression of the BLOCK as a list of lines. Options are: Order->"<>T["list"]<>".";

Order::usage = "Order is an option for block["<>M["tostring"]<>", which must be List[List[Integer...]...].";

decay::usage = "SLHA package utilizes local variables "<>T["decay$xxx"]<>" as DECAY objects. Available operations are as follows.

The object can be modified by the following accessors.
decay["<>M["headcomment"]<>"] contains the comment string associated to the first line (beginning with \"DECAY\") of the DECAY.
decay["<>M["rate"]<>"] contains the decay rate of the particle.
decay["<>T["pid_Integer .."]<>", "<>T["(option)"]<>"] contains the branching ratio into "<>T["pid"]<>". Options are: IfMissing->"<>T["value"]<>".
decay["<>M["c"]<>", "<> T["_Integer .."] <> "] contains the comment string associated to the value.

The following functions are defined in addition.
decay["<>M["pid"]<>"] returns the particle id of the mother particle.
decay["<>M["keys"]<>"] returns a list of the keys (daughter particles) associated to the DECAY.
decay["<>M["tostring"]<>"] returns string expression of the DECAY as a list of lines.";

Remove[SLHA`F, SLHA`M, SLHA`T];

(* Error messages *)

SLHA::ClassError       = "Class error: `1`.";
SLHA::MultipleBlock    = "Multiple block with name `1` found.";
SLHA::BlockNotFound    = "Block `1` not found.";
SLHA::ValueNotFound    = "Block `1` does not have value for `2`.";
SLHA::InvalidIfMissing = "Invalid value is specified for option IfMissing.";
BlockToString::OrderOptionInvalid    = "Invalid value specified for the option 'Order'.";
BlockToString::UnexpectedLineIgnored = "Unexpected line with key `1` and value `2` ignored.";
ReadSLHA::ParsedAsString     = "Non standard line found, parsed as string: `1`.";
ReadSLHA::InvalidLine        = "Invalid lines found: `1`.";
ReadSLHA::OrphanLine         = "Lines with no block found: `1`.";
ReadSLHA::InvalidBlockLine   = "Invalid lines found: `1`.";



Begin["`Private`"];

(* --- Low Level Tools --- *)

ReadNumber[x_List]   := ReadNumber /@ x;
ReadNumber[x_String] := Read[StringToStream[x],Number];
StringPadding[str_,len_]:=If[StringLength[str]<len,StringJoin@@Prepend[Table[" ",{len-StringLength[str]}],str],str];
IntegerPadding[int_,len_]:=Module[{s},s=ToString[int];If[StringLength[s]<len,StringJoin@@Append[Table[" ",{len-StringLength[s]}],s],s]];
ToFString[num_Real]:=Module[{m,e},
  {m,e}=If[num==0.,{0,1},MantissaExponent[N[num,9]]];
  " "<>ToString[PaddedForm[m*10,{9,8},NumberSigns->{"-"," "}]]<>"e"<>ToString[PaddedForm[e-1,2,NumberSigns->{"-","+"},SignPadding->True,NumberPadding->"0"]]];
ToFString[num_Integer]:=StringPadding[IntegerPadding[num,11],16];
ToFString[num_Rational]:=ToFString[N[num]];



(* --- Objects --- *)

IsSLHA [obj_] := obj[TYPE] === "SLHA";
IsBlock[obj_] := obj[TYPE] === "Block";
IsDecay[obj_] := obj[TYPE] === "Decay";


(* SLHA *)

NewSLHA[] := Module[
    {slha},
    slha[TYPE]   = "SLHA";
    slha[BLOCKS] = {};
    slha[DECAYS] = {};
    (* functions returning a reference for data manipulation*)
    slha["block", f_String,  opt:OptionsPattern[SLHAGetBlock]] := SLHAGetBlock[slha, f, opt];
    slha[f_String,           opt:OptionsPattern[SLHAGetBlock]] := SLHAGetBlock[slha, f, opt];
    slha["decay", f_Integer, opt:OptionsPattern[SLHAGetDecay]] := SLHAGetDecay[slha, f, opt];
    (* functions *)
    slha["blocks"] := #[NAME] &/@ slha[BLOCKS];
    slha["tostring"] := SLHAToString[slha];
    slha["writetofile", filename_] := SLHAToFile[slha, filename];
    slha];

SLHAAdd[slha_, block_] := Which[
    IsSLHA[slha] && IsBlock[block], AppendTo[slha[BLOCKS], block]; block,
    IsSLHA[slha] && IsDecay[block], AppendTo[slha[DECAYS], block]; block,
    True, Message[SLHA::ClassError, "SLHAAdd"]; Abort[]];

SLHADelete[slha_, block_] := Which[
    (* Note that the block object is not cleared. *)
    IsSLHA[slha] && IsBlock[block], slha[BLOCKS] = DeleteCases[slha[BLOCKS], block],
    IsSLHA[slha] && IsDecay[block], slha[DECAYS] = DeleteCases[slha[DECAYS], block],
    True, Message[SLHA::ClassError, "SLHADelete"]; Abort[]];

Options[SLHAGetBlock] = {IfMissing -> "missing"};
SLHAGetBlock[slha_, name_String, OptionsPattern[]] := Module[
    {x},
    If[Not[IsSLHA[slha]], Message[SLHA::ClassError, "SLHAGetBlock"]; Abort[]];
    x = Select[slha[BLOCKS], ToUpperCase[#[NAME]] === ToUpperCase[name] &];
    If[Length[x] > 1, Message[SLHA::MultipleBlock, name]; Abort[]];
    If[Length[x] == 1,
       x[[1]],
       Which[
           ToUpperCase[OptionValue[IfMissing]] === "MISSING",   Missing["KeyAbsent", "BLOCKNAME"->name],
           ToUpperCase[OptionValue[IfMissing]] === "ABORT",  Message[SLHA::BlockNotFound, name]; Abort[],
           ToUpperCase[OptionValue[IfMissing]] === "CREATE", SLHAAdd[slha, NewBlock[ToUpperCase[name]]],
           True, Message[SLHA::InvalidIfMissing]; Abort[]]]];

Options[SLHAGetDecay] := {IfMissing -> "missing"};
SLHAGetDecay[slha_, pid_Integer, OptionsPattern[]] := Module[
    {x = Select[slha[DECAYS], #[PID] === pid &]},
    If[Length[x] > 1, Message[SLHA::MultipleBlock, "Decay " <> ToString[pid]]; Abort[]];
    If[Length[x] == 1,
       x[[1]],
       Which[
           ToUpperCase[OptionValue[IfMissing]] === "MISSING",   Missing["KeyAbsent", "BLOCKNAME"->"Decay "<>ToString[pid]],
           ToUpperCase[OptionValue[IfMissing]] === "ABORT",  Message[SLHA::BlockNotFound, "Decay " <> ToString[pid]]; Abort[],
           ToUpperCase[OptionValue[IfMissing]] === "CREATE", SLHAAdd[slha, NewDecay[pid]],
           True, Message[SLHA::InvalidIfMissing]; Abort[]]]];

SLHAToString[slha_] := Module[
    {list},
    If[Not[IsSLHA[slha]], Message[SLHA::ClassError, "SLHAToString"]; Abort[]];
    Join[{#["tostring"]& /@ slha[BLOCKS],
          #["tostring"]& /@ slha[DECAYS]}]//Flatten];

SLHAToFile[slha_, filename_] := Module[
    {blocks = Select[slha["blocks"], Length[slha[#]["keys"]] > 0 &],
     ofs = OpenWrite[filename,PageWidth->Infinity] },
    WriteString[ofs, #<>"\n"] &/@ Flatten[Riffle[slha[#]["tostring"] & /@ blocks, "#"]];
    Close[ofs] // AbsoluteFileName];


(* Block *)

NewBlock[name_, headcomment_: ""] := Module[
    {block},
    block[TYPE]                            = "Block";
    block[NAME]                            = name;
    (* public data, ready for manipulation *)
    block["headcomment"]                   = headcomment;
    block["Q"]                             = Missing[];
    block[_Integer...]                     = Missing[];                  (* Note that this can be LeftValue,         *)
    block[v:_Integer..., i:(IfMissing->_)]:= BlockGetValue[block, v, i]; (*   but this is not expected as LeftValue. *)
    block["c", _Integer...]                = "";
    (* functions *)
    block["keys"]     := BlockKeys[block];
    block["tostring", opt:OptionsPattern[BlockToString]] := BlockToString[block, opt];
    block];

BlockGetValue[block_, v:_Integer..., IfMissing->i_] := Module[
    {value = block[v]},
    If[Not[MatchQ[value,_Missing]],
       value,
       Which[
           ToUpperCase[i] === "MISSING", Missing[],
           ToUpperCase[i] === "ABORT", Message[SLHA::ValueNotFound, block[NAME], ToString[v]]; Abort[],
           NumericQ[i], i,
           True, Message[SLHA::InvalidIfMissing]; Abort[]]]];

BlockKeys[block_] := Module[
    {keys = DownValues[block // Evaluate] //. RuleDelayed[_[_[k___]], _] :> {k}},
    Select[keys, (AllTrue[#, IntegerQ] && Not[MatchQ[block[Sequence@@#], _Missing]]) &] // Sort];

Options[BlockToString] = {Order -> {}};
BlockToString[block_, OptionsPattern[]] := Module[
    {list, order, keys},
    If[Not[IsBlock[block]], Message[SLHA::ClassError, "BlockToString"]; Abort[]];
    order = Which[MatchQ[#, List[_Integer ...]], #,
                  MatchQ[#, _Integer], List[#],
                  True, Message[BlockToString::OrderOptionInvalid]; Abort[]] &/@ OptionValue[Order];
    order = Select[order, Not[MatchQ[block[Sequence@@#], _Missing]]&];
    keys  = DeleteDuplicates[Join[order, BlockKeys[block]]];

    list = {
        StringPadding["BLOCK " <> block[NAME]
                      <> If[MatchQ[block["Q"], _Missing], "", " Q= " <> ToFString[N[block["Q"]]]]
                      ,27] <> " " <> block["headcomment"] // StringTrim };
    list = Join[list, Module[
        { k = #,
          n = Length[#],
          v = block[Sequence@@#],
          c = block["c", Sequence@@#],
          tmp},
        tmp = Which[
            block[NAME] === "MASS", " "<>IntegerPadding[k[[1]],9]<>"   "<>ToFString[v]<>"   "<>c,
            StringQ[v] && n === 1, IntegerPadding[k[[1]],6]<>"   "<>StringPadding[v,19]<>c,
            n === 0, "         "<>ToFString[v]<>"   "<>c,
            n === 1, IntegerPadding[k[[1]],6]<>"   "<>ToFString[v]<>"   "<>c,
            n === 2, " "<>IntegerPadding[k[[1]],2]<>IntegerPadding[k[[2]],3]<>"   "<>ToFString[v]<>"   "<>c,
            True,
            Message[BlockToString::UnexpectedLineIgnored, ToString[k], ToString[v]];
            "# Invalid line ignored here"];
        StringTrim[tmp, RegularExpression[" *$"]]]& /@ keys];
    list];

(* Decay *)
NewDecay[pid_, decayrate_: 0, headcomment_: ""] := Module[
    {decay},
    SetAttributes[decay, Orderless];
    decay[TYPE]              = "Decay";
    decay[PID]               = pid;
    decay["pid"]            := Evaluate[decay[PID]];
    decay["headcomment"]     = headcomment;
    decay["rate"]            = decayrate;
    decay[_Integer ..]       = 0;
    decay["c", _Integer... ] = "";
    (* functions *)
    decay["keys"]     := DecayKeys[decay];
    decay["tostring"] := DecayToString[decay];
    decay];

DecayKeys[decay_] := Module[
    {keys = DownValues[decay // Evaluate] //. RuleDelayed[_[_[k___]], _] :> {k}},
    Select[keys, (Length[#] > 1 && AllTrue[#, IntegerQ] && decay[Sequence@@#] > 0) &] // Sort];

DecayToString[decay_] := Module[
    {keys, list, lens},
    If[Not[IsDecay[decay]], Message[SLHA::ClassError, "DecayToString"]; Abort[]];
    keys = Sort[#, Abs[#1] < Abs[#2] &] & /@ Sort[decay["keys"], decay[Sequence@@#1] > decay[Sequence@@#2] &];
    lens = 2 + (Max[StringLength[ToString /@ #]] &/@ Transpose[PadRight[#, Max[Length/@keys]] &/@ keys]);
    list = { StringPadding["DECAY " <>
                           IntegerPadding[decay[PID], 9] <> "   " <>
                           ToFString[decay["rate"]] <> "   ", 30+Total[lens]]
             <> decay["headcomment"] // StringTrim };
    list = Join[list, StringTrim[ StringPadding[
        "   " <> ToFString[decay[Sequence@@#]] <>
        "   " <> IntegerPadding[Length[#],2] <>
        "   " <> StringJoin[MapIndexed[IntegerPadding[#1, lens[[First[#2]]]] &, #]] <>
        "   ", 30+Total[lens]] <> decay["c", Sequence@@#], RegularExpression[" *$"]]& /@ keys];
    list];



(* --- Operations --- *)

ReadSLHA[inputfilename_] := Module[
    { lines = ReadList[inputfilename,Record],
      slha  = NewSLHA[],
      block = Null,
      q,
      dataline, data, comment, k, v, pid, rate},
    Do[
        {dataline, comment} = {#[[1]], If[Length[#] == 2, "#" <> #[[2]], ""]} & @ StringSplit[line, "#", 2];
        data = StringSplit[dataline];
        Which[
            Length[data] === 0, (* comment line as the file header *)
            Null;
            ,
            ToUpperCase[data[[1]]] === "BLOCK",
            If[length[data] < 2, Message[ReadSLHA::InvalidBlockLine, line]; Abort[]];
            block = slha["block", data[[2]], IfMissing->"Create"];
            block["headcomment"] = comment;
            If[Length[data] >= 3, block["Q"] = ParseQRule[StringJoin[ToString /@ data[[3;;]]]]];
            ,
            ToUpperCase[data[[1]]] === "DECAY",
            If[length[data] != 3, Message[ReadSLHA::InvalidBlockLine, line]; Abort[]];
            {pid, rate} = ReadNumber[{data[[2]], data[[3]]}];
            If[Not[IntegerQ[pid] && NumberQ[rate]], Message[ReadSLHA::InvalidBlockLine, line]; Abort[]];
            block = slha["decay", pid, IfMissing->"Create"];
            block["rate"] = rate;
            block["headcomment"] = comment;
            ,
            IsBlock[block] && (block[NAME] === "SPINFO" || block[NAME] === "DCINFO"),
            k = ReadNumber[data[[1]]];
            v = StringSplit[dataline//StringTrim, Whitespace, 2];
            If[Not[IntegerQ[k] && Length[v]==2], Message[ReadSLHA::InvalidLine, line]; Abort[]];
            block[k]     = v[[2]];
            block["c",k] = comment;
            ,
            IsBlock[block],
            {k, v} = {ToExpression /@ Most[data], ReadNumber[Last[data]]};
            If[Not[AllTrue[k, IntegerQ] && v =!= $Failed], (* Non Standard SLHA *)
               k = ReadNumber[data[[1]]];
               v = StringSplit[dataline//StringTrim, Whitespace, 2];
               If[Not[IntegerQ[k] && Length[v]==2], Message[ReadSLHA::InvalidLine, line]; Abort[]];
               Message[ReadSLHA::ParsedAsString, line <> " -> " <> ToString[k] <> " = \"" <> v[[2]] <> "\"."];
               v = v[[2]]];
            block[     Sequence@@k] = v;
            block["c", Sequence@@k] = comment;
            ,
            IsDecay[block],
            v = ReadNumber[data];
            If[Length[v] < 2 || Length[v] != 2+v[[2]], Message[ReadSLHA::InvalidLine, line]; Abort[]];
            block[     Sequence@@(v[[3;;]])] = v[[1]];
            block["c", Sequence@@(v[[3;;]])] = comment;
            ,
            True,
            Message[ReadSLHA::OrphanLine, line]; Abort[];
             ];
        , {line, lines}];
    If[block =!= Null, AddBlock[slha, block]];
    slha];

ParseQRule[str_]:=Module[
    { Qmatch = StringCases[str, RegularExpression["^\\s*Q\\s*=\\s*([\\d\\+\\.\\-ED]+)\\s*$"] -> "$1", IgnoreCase->True]},
    If[Length[Qmatch]==1,
       ReadNumber[Qmatch[[1]]],
       If[StringMatchQ[str,RegularExpression["^\\s*$"]],
          Null,
          Message[ReadSLHA::InvalidBlockLine,str]; Abort[]]]];



(* --- helper functions for users --- *)

IsMissing[obj_]  := Which[MatchQ[obj, _Missing], True, MatchQ[obj, _Symbol], False, True, IsMissing[Head[obj]]]; (* in order to return True for Missing[___][___] *)

Unprotect[Missing];  (* Missing is modified. *)
Missing["KeyAbsent", "BLOCKNAME"->b_][v:_Integer..., IfMissing->i_] := Which[
    ToUpperCase[i] === "MISSING", Missing["KeyAbsent", {"BlockName"->b, "KEY"->{a}}],
    ToUpperCase[i] === "ABORT",   Message[SLHA::ValueNotFound, b, ToString[v]]; Abort[],
    NumericQ[i],                  i,
    True, Message[SLHA::InvalidIfMissing]; Abort[]];
Protect[Missing];

Unprotect[Unset]; (* Unset is modified. *)
Unset[s_[b_String]]           := If[IsMissing[s[b]],          $Failed, SLHADelete[s, s["block", b]]; Null] /; IsSLHA[s];
Unset[s_["block", b_String]]  := If[IsMissing[s["block", b]], $Failed, SLHADelete[s, s["block", b]]; Null] /; IsSLHA[s];
Unset[s_["decay", p_Integer]] := If[IsMissing[s["decay", p]], $Failed, SLHADelete[s, s["decay", p]]; Null] /; IsSLHA[s];
Protect[Unset];



End[];
EndPackage[];
