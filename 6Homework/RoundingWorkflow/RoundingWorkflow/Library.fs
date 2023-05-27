namespace RoundingWorkflow

open System

module Workflows =
    type RoundBuilder(accuracy: uint) =
        member this.Bind(x: float, f) =
            f <| Math.Round(x, accuracy |> int)
        member this.Return(x: float) =
            Math.Round(x, accuracy |> int)
    let rounding accuracy = RoundBuilder(accuracy)