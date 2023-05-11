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
    