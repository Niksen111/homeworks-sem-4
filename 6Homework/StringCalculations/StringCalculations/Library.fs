namespace StringCalculations

open System
open Microsoft.FSharp.Core

module Say =
    let tryParse (str: string) =
        match Int32.TryParse str with
        | true, x -> Ok(x)
        | _ -> Error("Not a number.")
    
    type CalculationsBuilder() =
        member this.Bind(str: string, f) =
            let parsed = tryParse(str)
            match parsed with
            | Ok x -> f x
            | Error _ -> parsed
        member this.Result(x: string) =
            Ok(x)
            
    let calculations = CalculationsBuilder()