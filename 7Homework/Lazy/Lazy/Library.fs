namespace Lazy

open System.Threading

module Lazy =
    type ILazy<'a> =
        abstract member Get: unit -> 'a
        
    type LazyOneThread<'a>(func: unit -> 'a) =
        let mutable value = None
        interface ILazy<'a> with
            member this.Get() =
                match value with
                | None ->
                    value <- Some(func())
                    value |> Option.get
                | Some v -> v
    type LazyMultiThread<'a>(func: unit -> 'a) =
        let mutable value = None
        let lockObj = obj()
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
        interface ILazy<'a> with
            member this.Get() =
                match value with
                | Some x -> x
                | None ->
                    let current = value
                    let newValue = Some(func())
                    if not <| obj.ReferenceEquals (current, Interlocked.CompareExchange(&value, newValue, current)) then
                        value |> Option.get
                    else
                        value <- newValue
                        newValue |> Option.get
                