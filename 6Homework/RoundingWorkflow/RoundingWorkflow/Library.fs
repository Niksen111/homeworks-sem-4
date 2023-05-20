namespace RoundingWorkflow

open System

module Workflows =
    type RoundBuilder(accuracy: int) =
        member this.Bind(x, f) =
            f x
        member this.Return(x) =
            Math.Round(x |> float, accuracy)
    let rounding = RoundBuilder(3)