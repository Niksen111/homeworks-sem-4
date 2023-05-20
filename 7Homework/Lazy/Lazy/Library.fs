namespace Lazy

open System.Threading

module Lazy =
    type ILazy<'a> =
        abstract member Get: unit -> 'a
        
    type LazyOneThread<'a>(func: unit -> 'a) =
        let mutable value = None
        member this.Get() = (this :> ILazy<_>).Get()
        interface ILazy<'a> with
            member this.Get() =
                match value with
                | None ->
                    value <- Some(func())
                    value |> Option.get
                | Some v -> v
    type BlockingLazy<'a>(func: unit -> 'a) =
        let mutable value = None
        let lockObj = obj()
        member this.Get() = (this :> ILazy<_>).Get()

        interface ILazy<'a> with
            member this.Get() =
                lock lockObj (fun () ->
                     match value with
                     | None ->
                         value <- Some(func())
                         value |> Option.get
                     | Some v -> v)
               
    type LazyLockFree<'a>(func: unit -> 'a) =
        let mutable value = None
        member this.Get() = (this :> ILazy<_>).Get()
        interface ILazy<'a> with
            member this.Get() =
                match value with
                | Some x -> x
                | None ->
                    let current = value
                    let newValue = Some(func())
                    Interlocked.CompareExchange(&value, newValue, current) |> ignore
                    value |> Option.get