module LocalNetwork.Virus

open System.Collections.Generic
open LocalNetwork.Computer

type Virus(firstInfected: Computer list, infectionChance: OS -> float, random: unit -> float) =
    let infectedComputers = HashSet(firstInfected)
    
    let getNeighbours (computer: Computer) = computer.Neighbors

    let calculateInfectionCandidates lastInfected =
        lastInfected
        |> Seq.map getNeighbours
        |> Seq.concat
        |> Seq.distinct
        |> Seq.filter (not << infectedComputers.Contains)
        |> Seq.filter (fun x -> infectionChance x.System > 0)

    let mutable infectionCandidates = calculateInfectionCandidates infectedComputers

    member x.InfectedComputers = infectedComputers

    member x.AbleToInfect = not (Seq.isEmpty infectionCandidates)

    member x.SpreadInfection() =
        let newInfected = List()

        for computer in infectionCandidates do
            if random() < infectionChance computer.System then
                newInfected.Add(computer)
                computer.IsInfected <- true
                computer.PrintState()

        infectedComputers.UnionWith(newInfected)
        infectionCandidates <- calculateInfectionCandidates newInfected