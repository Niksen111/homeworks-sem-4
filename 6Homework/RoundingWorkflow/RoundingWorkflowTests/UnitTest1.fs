module RoundingWorkflowTests

open NUnit.Framework
open FsUnit
open RoundingWorkflow.Workflows

[<Test>]
let ``Workflow works`` () =
    let result = rounding 3u {
        let! a = 2.0 / 12.0
        let! b = 3.5
        return a / b
    }
    let expected = 0.048
    result |> should equal expected
    
[<Test>]
let ``Division by 0`` () =
    let result = rounding 2u {
        let! a = 2.1 / 0.0
        return a + 2.2
    }
    
    result |> should equal infinity