﻿module LocalNetwork.Computer

type OS =
    | Win
    | Gnu
    | Mac

type Computer(system: OS, id: int, isInfected: bool) =
    let mutable isInfected = isInfected
    let mutable (neighbors: Computer list) = List.Empty
    let infect () =
        isInfected <- true
    member this.Id = id
    member val Neighbors = neighbors with get, set
    member val Random = System.Random() with get, set
    member this.TryInfect infectProb =
        if not isInfected && this.Random.NextDouble() <= infectProb system then
            infect()
            this.PrintState()
    member this.InfectNeighbors infectProb =
        let mutable possiblyInfect = false
        for neighbor in this.Neighbors do
            if neighbor.IsInfected |> not && neighbor.System |> infectProb > 0.0 then
                neighbor.TryInfect infectProb
                possiblyInfect <- true
        possiblyInfect
    member this.IsInfected = isInfected
    member this.System = system
    member this.PrintState() =
        if isInfected then
            printfn $"Computer {id} is infected."
        else
            printfn $"Computer {id} is not infected."
    new(system: OS, id: int) = Computer(system, id, false)
    