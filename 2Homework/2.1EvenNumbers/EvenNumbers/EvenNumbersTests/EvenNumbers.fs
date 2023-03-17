module EvenNumbersTests

open EvenNumbers
open NUnit.Framework
open FsUnit
open FsCheck

let simpleList = [ 1 .. 10 ]
let oddNumbers = [ 1; 3; 5; 7 ]
let negativeNumbers = [ -10; -6; 10 ]

[<Test>]
let ``EvenNumbersFold works`` () =
    EvenNumbers.EvenNumbersFold(simpleList) |> should equal 30
    EvenNumbers.EvenNumbersFold([]) |> should equal 0
    EvenNumbers.EvenNumbersFold(oddNumbers) |> should equal 0
    EvenNumbers.EvenNumbersFold(negativeNumbers) |> should equal -6
    
[<Test>]
let ``EvenNumbersFilter works`` () =
    EvenNumbers.EvenNumbersFilter(simpleList) |> should equal 30
    EvenNumbers.EvenNumbersFilter([]) |> should equal 0
    EvenNumbers.EvenNumbersFilter(oddNumbers) |> should equal 0
    EvenNumbers.EvenNumbersFilter(negativeNumbers) |> should equal -6
    
[<Test>]
let ``EvenNumbersMap works`` () =
    EvenNumbers.EvenNumbersMap(simpleList) |> should equal 30
    EvenNumbers.EvenNumbersMap([]) |> should equal 0
    EvenNumbers.EvenNumbersMap(oddNumbers) |> should equal 0
    EvenNumbers.EvenNumbersMap(negativeNumbers) |> should equal -6
    
[<Test>]
let ``FsCheck test`` () =
    let ``EvenNumbers Map and Filter are the same`` list =
        EvenNumbers.EvenNumbersMap list = EvenNumbers.EvenNumbersFilter list
    Check.QuickThrowOnFailure ``EvenNumbers Map and Filter are the same``

    let ``EvenNumbers Map and Fold are the same`` list =
        EvenNumbers.EvenNumbersMap list = EvenNumbers.EvenNumbersFold list
    Check.QuickThrowOnFailure ``EvenNumbers Map and Fold are the same``

    let ``EvenNumbers Fold and Filter are the same`` list =
        EvenNumbers.EvenNumbersFold list = EvenNumbers.EvenNumbersFilter list
    Check.QuickThrowOnFailure ``EvenNumbers Fold and Filter are the same``