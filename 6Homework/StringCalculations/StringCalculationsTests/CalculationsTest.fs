module StringCalculationsTests

open NUnit.Framework
open StringCalculations.Calculations
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
    
[<Test>]
let ``Invalid calculations returns error``() =
    let result = calculate {
        let! x = "1"
        let! y = "Ъ"
        let z = x + y
        return z
    }
    match result with
    | Ok _ -> Assert.Fail()
    | Error msg ->
        msg |> should equal "Not a number."