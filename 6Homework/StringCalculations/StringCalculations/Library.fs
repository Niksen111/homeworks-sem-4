namespace StringCalculations

open System
open Microsoft.FSharp.Core

module Say =
    type CalculationsBuilder() =
        member this.Bind(str: string, f) =
            match Int32.TryParse str with
            | true, x -> f x
            | _ -> Error("Not a number")
        member this.Return(x) =
            Ok(x)
            
    let calculate = CalculationsBuilder()