module LazyTests

open System
open System.Threading
open NUnit.Framework
open Lazy.Lazy
open FsUnit

let isSameObject = LanguagePrimitives.PhysicalEquality

[<Test>]
let ``LazyOneThread.Get returns same value if called multiple times``() = 
    let lazyVal = LazyOneThread(fun () ->
        [1])
    let actual1 = lazyVal.Get()
    let actual2 = lazyVal.Get()
    isSameObject actual1 actual2 |> should be True

[<Test>]
let rec ``Blocking and lock free lazy works``() =
    let mutable number1 = 0
    let mutable number2 = 0
    let blockingLazy = BlockingLazy(fun () -> Interlocked.Increment(&number1))
    let lockFreeLazy = LazyLockFree(fun () -> Interlocked.Increment(&number2))
    let tasks1 = [for _ in 0..1000 -> async{return blockingLazy.Get()}]
    let tasks2 = [for _ in 0..1000 -> async{return lockFreeLazy.Get()}]
    tasks1 |> Async.Parallel |> Async.RunSynchronously |> ignore
    tasks2 |> Async.Parallel |> Async.RunSynchronously |> Seq.distinct |> Seq.length |> should equal 1

    number1 |> should equal 1
    