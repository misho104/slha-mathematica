#!MathematicaScript -noinit -script

Needs["SLHA`", "../SLHA.m"];

UnitTest::Pass = "";
UnitTest::Fail = "";
UnitTest::Undetermined = "Assertion result not determined.";

Off[General::stop];

BeginUnitTest[module_] := ($AssertErrors = {module, {}});
EndUnitTest  [obj___]  := If[Length[$AssertErrors[[2]]] > 0,
                             (UnitTest::Fail = "\""<>$AssertErrors[[1]] <> "\" fails \"" <> # <> "\"."; Message[UnitTest::Fail]) &/@ $AssertErrors[[2]]; If[Length[List[obj]] > 0, Print[obj]]; Abort[],
                             (UnitTest::Pass = "\""<>$AssertErrors[[1]] <> "\" passed."); Message[UnitTest::Pass]
                            ];

Assert        [a_,     message_:"fail"] := If[Not[a], AppendTo[$AssertErrors[[2]], message], True, Message[UnitTest::Undetermined]; AppendTo[$AssertErrors[[2]], "internal error"]];
AssertReal    [a_,     message_:"fail"] := Assert[And @@ (Element[#, Reals] & /@ Flatten[{a}]), message];
AssertZero    [a_,     message_:"fail"] := Module[{zero}, zero = {a}*0; Assert[Chop[{a}] == zero, message]];
AssertEqual   [a_, b_, message_:"fail"] := Module[{zero}, zero = {a}*0; Assert[And[Dimensions[a] === Dimensions[b], Chop[{a}-{b}] == zero], message]];


SetAttributes[AssertAbort, HoldAll];
AssertAbort   [a_, e_, message_:"fail"] := Block[
    {$Messages = {}},
    Assert[CheckAbort[ReleaseHold[a], $Aborted] === $Aborted, message <> "(no abort)"];
    Assert[$MessageList[[-1]]===HoldForm[e], message <> "(different error)"]];

BeginUnitTest["ReadSLHA"];
slha = ReadSLHA["test.slha"];
EndUnitTest[slha];


BeginUnitTest["OneArgumentBlocks"];
Do[
    AssertEqual[f[slha, "oneargblock",  1],      10];
    AssertEqual[f[slha, "OneArgBlock",  2],     -20];
    AssertEqual[f[slha, "ONEARGBLOCK",  3],       0];
    AssertEqual[f[slha, "ONEARGBLOCK", 11], -1522.2];
    AssertEqual[f[slha, "ONEARGBLOCK", 12],     250];
    AssertEqual[f[slha, "ONEARGBLOCK", 13],    0.02];
    AssertEqual[f[slha, "ONEARGBLOCK", 14],  -0.003];
    ,{f, {Data, GetData}}];
EndUnitTest[];

BeginUnitTest["NonExistingData"];
Assert[Data[slha, "nonexistingblock"]          === Null];
Assert[Data[slha, "nonexistingblock", 1]       === Null];
Assert[Data[slha, "nonexistingblock", 1, 2, 3] === Null];
Assert[Data[slha, "ONEARGBLOCK"]               === Null];
Assert[Data[slha, "ONEARGBLOCK", 10000]        === Null];
Assert[Data[slha, "ONEARGBLOCK", 1, 1]         === Null];
AssertAbort[GetData[slha, "nonexistingblock"],   GetData::BlockNotFound];
AssertAbort[GetData[slha, "ONEARGBLOCK"],        GetData::ColumnNotFound];
AssertAbort[GetData[slha, "ONEARGBLOCK", 10000], GetData::ColumnNotFound];
AssertAbort[GetData[slha, "ONEARGBLOCK", 1, 1],  GetData::ColumnNotFound];
EndUnitTest[];

(*
BeginUnitTest["ZeroArgumentBlocks"];
AssertEqual[Data[slha, "NOARGBLOCKA"], 3.1415926535];
AssertEqual[Data[slha, "NOARGBLOCKB"], 1];
EndUnitTest[];
*)

BeginUnitTest["TwoArgumentBlocks"];
Do[AssertEqual[GetData[slha,"doubleargblock", i, j], i*j], {i, 1, 2}, {j, 1, 2}];
EndUnitTest[];

BeginUnitTest["StringBlocks"];
AssertEqual[GetData[slha, "unusualcase", 1], "some calculator returns"];
AssertEqual[GetData[slha, "unusualcase", 2], "these kind of error messages"];
AssertEqual[GetData[slha, "unusualcase", 3], "which of course is not expected in slha format."];
EndUnitTest[];

(*
AssertEqual[Getslha->Q("NOARGBLOCKA"), 123456.789));
AssertEqual[slha->Q("NOARGBLOCKB"), 123456.789));


print "---------------------------------------- Decay Rates\n";
dcmp($slha->d(      6),  1.45899677));
dcmp($slha->d(1000021), 13.4988503 ));
dcmp($slha->d(1000005), 10.7363639 ));

print "---------------------------------------- Branching Ratio (1)\n";
dcmp($slha->br(6,5,24), 1.000000000 ));
dcmp($slha->br(6,24,5), 1.000000000 ));

print "---------------------------------------- Branching Ratio (2)\n";
dcmp($slha->br(1000021,  1000001, -1), 0.0217368689));
dcmp($slha->br(1000021, -1000001,  1), 0.0217368689));

print "---------------------------------------- Branching Ratio (3)\n";
dcmp($slha->br(1000005,  1000022,  5), 0.0259311849));
dcmp($slha->br(1000005,  1000023,  5), 0.216401445));
dcmp($slha->br(1000005,  1000025,  5), 0.0159051554));
dcmp($slha->br(1000005,  1000035,  5), 0.0127036617));

print "---------------------------------------- Branching Ratio (4)\n";
dcmp($slha->br(1000005,  1, -2, -3),          0.378818883));
dcmp($slha->br(1000005,  1, -2, -3, 4),       0.378818883));
dcmp($slha->br(1000005,  1, -2, -3, 4, 5),    0.378818883));
dcmp($slha->br(1000005,  1, -2, -3, 4, 5, 6), 0.378818883));

print "---------------------------------------- Decay List\n";
@{$slha->dlist(      6)} == 1);
@{$slha->dlist(1000021)} == 2);
@{$slha->dlist(1000005)} == 8);

print "---------------------------------------- Manipulate (1)\n";
$slha->set_param("ONEARGBLOCK", 8, 300);
$slha->set_param("ONEARGBLOCK", 1, -4.01031);
GetData[slha,("ONEARGBlock", 8) == 300);
dcmp(GetData[slha,("ONEARGBlock", 1), -4.01031));

$slha->set_param("DoubleARGBLOCK", 2, 1, 4);
$slha->set_param("DoubleARGBLOCK", 2, 2, 9);
$slha->set_param("DoubleARGBLOCK", 2, 3, 16);
GetData[slha,("doubleARGBlock", 2, 1) == 4);
GetData[slha,("doubleARGBlock", 2, 2) == 9);
GetData[slha,("doubleARGBlock", 2, 3) == 16);

$slha->set_param("Noargblocka", -100);
dcmp(GetData[slha,("NOARGBlockA"), -100));

$slha->set_param("NewBlock", 6, 7, 10);
GetData[slha,("newblock", 6, 7) == 10);

print "---------------------------------------- Manipulate (2)\n";

$slha->set_Q("NoArgBlockA", 20);
$slha->set_Q("oneArgBlock", 22);
$slha->Q("NoArgBlockA") == 20);
$slha->Q("oneargBlock") == 22);


print "---------------------------------------- Manipulate (3)\n";
$slha->remove_param("oneargblock", 1);
!defined(GetData[slha,("OneaRGBLOCK", 1)));
!defined($slha->{data}->{ONEARGBLOCK}->{1}));

$slha->remove_param("doubleargblock", 2,1);
!defined(GetData[slha,("DOUBLEARGBLOCK", 2, 1)));
!defined($slha->{data}->{DOUBLEARGBLOCK}->{"2 1"}));

$slha->remove_param("NoArgBlockA");
!defined($slha->{data}->{NOARGBLOCKA}));


print "---------------------------------------- Modify Decay block (1)\n";
$slha->set_decay_rate(6, 1.2345);
dcmp($slha->d(      6),  1.2345));
dcmp($slha->d(1000005), 10.7363639 ));     # unchanged

dcmp($slha->br(6,5,24), 1.000000000 ));    # unchanged
dcmp($slha->br(6,24,5), 1.000000000 ));    # unchanged

dcmp($slha->br(1000021,  1000001, -1), 0.0217368689)); # unchanged
dcmp($slha->br(1000021, -1000001,  1), 0.0217368689)); # unchanged

print "---------------------------------------- Modify Decay block (2)\n";
$slha->clear_decay_channels(6);
dcmp($slha->d(      6),  1.2345));
dcmp($slha->br(6,5,24), 0));
@{$slha->dlist(      6)} == 0);

@{$slha->dlist(1000021)} == 2); #unchanged

$slha->add_decay_channel(6, 1, 3, 24); # 100% to 3 and 24
@{$slha->dlist(      6)} == 1);
dcmp($slha->br(6,24,3), 1));
dcmp($slha->br(6,24,5), 0));
dcmp($slha->br(6,3,24), 1));
dcmp($slha->br(6,5,24), 0));

print "---------------------------------------- Modify Decay block (3)\n";
$slha->clear_decay_channels(6);
$slha->add_decay_channel(6, 0.99, 5, 24); # 99% to 5 and 24
$slha->add_decay_channel(6, 0.01, 1, 24); #  1% to 1 and 24
@{$slha->dlist(      6)} == 2);
dcmp($slha->br(6,24,1), 0.01));
dcmp($slha->br(6,5,24), 0.99));

dcmp($slha->br(1000021,  1000001, -1), 0.0217368689)); # unchanged
dcmp($slha->br(1000021, -1000001,  1), 0.0217368689)); # unchanged

print "---------------------------------------- Modify Decay block (4)\n";
$slha->set_decay_rate(123, 2.4);
$slha->add_decay_channel(123, 0.5, -1, -2, -3, -4, -5);
$slha->add_decay_channel(123, 0.5, 1, 2, 3, 4, 5, 6, 7);
@{$slha->dlist(123)} == 2);
dcmp($slha->br(123,2,4,6,1,3,5,7), 0.5));

print "---------------------------------------- Copy\n";
my $copy = $slha->copy();
$copy->remove_block("doubleargblock");
defined($slha->{data}->{DOUBLEARGBLOCK}));
!defined($copy->{data}->{DOUBLEARGBLOCK}));

$copy->set_param("NewBlock", 6, 7, 2000);
GetData[slha,("newblock", 6, 7) == 10);
$copy->v("newblock", 6, 7) == 2000);

print "---------------------------------------- Write\n";
foreach($slha->write([qw/NewBlock NotExistingBlock/])){print;}

__END__
*)
