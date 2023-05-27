module BracketsSequenceTests

open NUnit.Framework
open FsUnit
open BracketsSequence.MyModule

[<Test>]
let Test1 () =
    checkBrackets "(" |> should be False
    checkBrackets "(}" |> should be False
    checkBrackets "(()))" |> should be False
    checkBrackets ")())" |> should be False
    checkBrackets "({[}])" |> should be False
    checkBrackets "{[(())]}{[(()}]" |> should be False
    checkBrackets "" |> should be True
    checkBrackets "[((){})[]]" |> should be True
    checkBrackets "(({}[()]))[()]{[()]}({})" |> should be True
    checkBrackets "[abbdf(sdf(dsg)hd{jfh}jkgkhj)l[hkl]j;]" |> should be True
    