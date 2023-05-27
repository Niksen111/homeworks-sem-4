module EvenNumbersTests

open EvenNumbers.EvenNumbers
open NUnit.Framework
open FsUnit
open FsCheck

let simpleList = [ 1 .. 10 ]
let oddNumbers = [ 1; 3; 5; 7 ]
let negativeNumbers = [ -10; -6; 10 ]

let checkResults f =
    simpleList |> f |> should equal 30
    [] |> f |> should equal 0
    oddNumbers |> f |> should equal 0
    negativeNumbers |> f |> should equal -6

[<Test>]
let ``EvenNumbersFold works`` () =
    EvenNumbersFold |> checkResults
    
[<Test>]
let ``EvenNumbersFilter works`` () =
    EvenNumbersFilter |> checkResults
    
[<Test>]
let ``EvenNumbersMap works`` () =
    EvenNumbersMap |> checkResults
    
[<Test>]
let ``FsCheck test`` () =
    let ``EvenNumbers Map and Filter are the same`` list =
        EvenNumbersMap list = EvenNumbersFilter list
    Check.QuickThrowOnFailure ``EvenNumbers Map and Filter are the same``

    let ``EvenNumbers Map and Fold are the same`` list =
        EvenNumbersMap list = EvenNumbersFold list
    Check.QuickThrowOnFailure ``EvenNumbers Map and Fold are the same``

    let ``EvenNumbers Fold and Filter are the same`` list =
        EvenNumbersFold list = EvenNumbersFilter list
    Check.QuickThrowOnFailure ``EvenNumbers Fold and Filter are the same``