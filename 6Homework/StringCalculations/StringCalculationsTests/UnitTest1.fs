module StringCalculationsTests

open NUnit.Framework
open StringCalculations.Say
open FsUnit

[<Test>]
let ``Valid calculations works`` () =
    let result = calculate {
        let! x = "1"
        let! y = "2"
        let z = x + y
        return z
    }
    let expected = 3
    match result with
    | Ok x -> x |> should equal expected
    | Error _ -> Assert.Fail()