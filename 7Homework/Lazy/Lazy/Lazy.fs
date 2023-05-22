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
                if value.IsNone then
                    value <- Some(func())
                value.Value
                
    type BlockingLazy<'a>(func: unit -> 'a) =
        let mutable value = None
        let lockObj = obj()
        member this.Get() = (this :> ILazy<_>).Get()
        interface ILazy<'a> with
            member this.Get() =
                if value.IsNone then
                    lock lockObj (fun () ->
                         if value.IsNone then
                             value <- Some(func()))
                value.Value

    type LazyLockFree<'a>(func: unit -> 'a) =
        let mutable value = None
        member this.Get() = (this :> ILazy<_>).Get()
        interface ILazy<'a> with
            member this.Get() =
                if value.IsNone then
                    let newValue = Some(func())
                    Interlocked.CompareExchange(&value, newValue, None) |> ignore
                value.Value