module PointFreeTests

open NUnit.Framework
open FsCheck
open FsUnit

let initialFunc x l = List.map (fun y -> y * x) l
let func3 x = List.map ((*) x)

let funcsAreTheSame x l = initialFunc x l = func3 x l

[<Test>]
let Test1 () =
    Check.QuickThrowOnFailure funcsAreTheSame

let rec compareLists list1 list2 =
    match list1, list2 with
    | [], [] -> true
    | [], _ -> false
    | _, [] -> false
    | x::xs, y::ys ->
        if x = y then
            compareLists xs ys
        else false

[<Test>]
let Test2 () = 
    let inputList = [1; 2; 3; 4]
    let expectedOutput = [2; 4; 6; 8]
    initialFunc 2 inputList |> should equal expectedOutput
    func3 2 inputList |> should equal expectedOutput
    