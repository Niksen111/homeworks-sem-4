module LazyTests

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
let ``Blocking lazy works``() =
    let mutable number = 0
    let blockingLazy = BlockingLazy(fun () -> Interlocked.Increment(&number))
    let tasks = [for _ in 0..1000 -> async{return blockingLazy.Get()}]
    let uniqueResults = tasks |> Async.Parallel |> Async.RunSynchronously |> Seq.distinct
    uniqueResults |> Seq.length |> should equal 1
    uniqueResults |> Seq.take 1 |> should equal 1
    number |> should equal 1
    
[<Test>]
let ``LockFree lazy works``() =
    let mutable number = 0
    let lockFreeLazy = LazyLockFree(fun () -> Interlocked.Increment(&number))
    let tasks = [for _ in 0..1000 -> async{return lockFreeLazy.Get()}]
    let uniqueResults = tasks |> Async.Parallel |> Async.RunSynchronously |> Seq.distinct
    uniqueResults |> Seq.length |> should equal 1
    uniqueResults |> Seq.take 1 |> should equal 1
