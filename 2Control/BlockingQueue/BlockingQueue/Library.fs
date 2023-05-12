namespace BlockingQueue

open System.Collections.Generic
open System.Threading

module Control2 =
    type BlockingQueue<'a>() =
        let elems = new Queue<'a>()
        let lockObjDe = obj()
        let lockObjEn = obj()
        let isEmpty = new ManualResetEvent(false)
        
        member this.Count =
            lock lockObjEn (fun () -> elems.Count)
        
        member this.Enqueue x =
            lock lockObjEn (fun () -> elems.Enqueue x)
            isEmpty.Set()
        
        member this.Dequeue () =
            let mutable result = None
            lock lockObjDe (fun () ->
                lock lockObjEn (fun () ->
                    if (elems.Count <> 0) then
                        result <- Some(elems.Dequeue())
                    else
                        ())
                if result.IsNone then
                    let _ = isEmpty.WaitOne()
                    lock lockObjEn (fun () ->
                        result <- Some(elems.Dequeue())))
            result