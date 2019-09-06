#!MathematicaScript -noinit -script

Needs["SLHA`", "../SLHA.m"];

Print["\n# Read SLHA file and its values"];
slha = ReadSLHA["simple.slha"];

{slha["A"][1], slha["A"][2], slha["A"][3], slha["B"][1, 1], slha["B"][10, 11], slha["C"][]} === {1, 2, 3, -1, 12, 999} // Print
slha["A"][1] === slha["block", "A"][1] // Print

Print["\n## List of blocks or keys"];
{
slha["blocks"]    === {"A", "B", "C"},
slha["a"]["keys"] === {{1}, {2}, {3}},
slha["b"]["keys"] === {{1, 1}, {10, 11}},
slha["c"]["keys"] === {{}}
} // Print

Print["\n## Missing values"];
IsMissing /@ {
  slha["A"][12345],
  slha["A"][1, 2, 3, 4, 5],
  slha["NONEXISTS"],
  slha["NONEXISTS"][1, 2, 3, 4, 5]
} // Print

{
 IsMissing[slha["A"]]    === False,
 IsMissing[slha["A"][1]] === False,
 IsMissing[slha["C"][]]  === False
} // Print

Print["\n## Behaviors for missing values"];
{slha["A"][1, IfMissing -> 999], slha["A"][9, IfMissing -> 999] } === {1, 999}  // Print
IsMissing[slha["A"][9]] // Print
slha["A"][9] === slha["A"][9, IfMissing -> "missing"] // Print
CheckAbort[slha["A"][9, IfMissing -> "abort"], $Aborted] === $Aborted // Print

Print["\n## Behaviors for missing blocks"];
IsMissing /@ {slha["NONEXIST"], slha["NONEXIST", IfMissing -> "MISSING"]} // Print
CheckAbort[slha["NONEXIST", IfMissing -> "abort"], $Aborted] === $Aborted // Print
IsMissing[slha["NewBlock"]] // Print
newblock = slha["NewBlock", IfMissing -> "create"];
{IsMissing[slha["NewBlock"]] === False, newblock === slha["newblock"]} // Print

Print["\n## Behaviors combination"];
slha["NONEXIST"][10, IfMissing->\[Pi]] === \[Pi] // Print
CheckAbort[slha["NONEXIST"][10, IfMissing -> "abort"], $Aborted] === $Aborted // Print
slha["ANOTHERBLOCK"][10, IfMissing -> E] === E // Print
IsMissing[slha["ANOTHERBLOCK"]["keys"]] // Print
slha["ANOTHERBLOCK", IfMissing -> "create"][10, IfMissing -> \[Pi]] === \[Pi] // Print
{IsMissing[slha["ANOTHERBLOCK"]] === False, slha["ANOTHERBLOCK"]["keys"] === {}} // Print

Print["\n# Manipulating SLHA object"];
Print["## Create and update"];
IsMissing[slha["X"]] // Print
IsMissing[slha["X", IfMissing -> "CREATE"]] === False // Print
slha["X"][10] = 123
{IsMissing[slha["X"]] === False, slha["X"][10] === 123} // Print

IsMissing /@ {slha["Y"], slha["alpha"]} // Print
slha["Y", IfMissing -> "create"][100] = -1
slha["Y"][100] === -1 // Print

alphablock = slha["alpha", IfMissing -> "Create"]
alphablock[] = 0.123
alphablock[] === 0.123 // Print
slha["alpha"][] === 0.123 // Print

Print["\n## Delete"]

Sort[slha["blocks"]] === Sort[{"A", "B", "C", "NEWBLOCK", "ANOTHERBLOCK", "X", "Y", "ALPHA"}] // Print

slha["Y"][100] = -1
slha["Y"][101] = -2
(IsMissing /@ {slha["Y"], slha["Y"][100], slha["Y"][101]}) === {False, False, False} // Print
slha["Y"][100] =.
(IsMissing /@ {slha["Y"], slha["Y"][100], slha["Y"][101]}) === {False, True, False} // Print
slha["Y"] =.
(IsMissing /@ {slha["Y"], slha["Y"][100], slha["Y"][101]}) === {True, True, True} // Print

Print["\n## What you should not do"]
b1 = slha["b1", IfMissing -> "Create"]
slha["b2"] = b1
slha["b1"][1] = 123
slha["b1"][1] === slha["b2"][1] === 123 // Print
