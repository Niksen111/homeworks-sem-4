module BlockingQueueTests

open NUnit.Framework
open BlockingQueue.Control2

[<Test>]
let ``Enqueue and TryDequeue should add and remove an item respectively``() =
    let queue = BlockingQueue<int>()
    queue.Enqueue(1) |> ignore
    let result = queue.Dequeue()
    Assert.AreEqual(Some 1, result)

[<Test>]
let ``TryDequeue should return None when queue is empty``() =
    let queue = BlockingQueue<int>()
    let enqueueAsync =
        async {
            do Async.Sleep(1000) |> ignore
            queue.Enqueue 1 |> ignore
        }
    enqueueAsync |> Async.Start
    let result = queue.Dequeue()
    Assert.AreEqual(Some 1, result)

[<Test>]
let ``Count should return the number of items in the queue``() =
    let queue = BlockingQueue<int>()
    queue.Enqueue(1) |> ignore
    queue.Enqueue(2) |> ignore
    queue.Enqueue(3) |> ignore
    let count = queue.Count
    Assert.AreEqual(3, count)