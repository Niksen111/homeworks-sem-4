module LazyTests

open System
open NUnit.Framework
open Lazy.Lazy
open FsUnit

let isSameObject = LanguagePrimitives.PhysicalEquality

[<Test>]
let ``LazyOneThread.Get returns same value if called multiple times``() = 
    let rand = Random()
    let lazyVal = LazyOneThread(fun () ->
        [rand.Next()])
    let actual1 = lazyVal.Get()
    let actual2 = lazyVal.Get()
    isSameObject actual1 actual2 |> should be True

[<Test>]
let rec ``BlockingLazy works``() =
    let rand = Random()
    let func = async {
        do! Async.Sleep(500)
        return [rand.Next]
    }
    let lazyVal = BlockingLazy(fun () -> func)
    ()