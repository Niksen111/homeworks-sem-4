namespace LocalNetwork

open LocalNetwork.Computer
open LocalNetwork.Virus

module LocalNetwork =
    type Network(computers: Computer list, infectChance: OS -> float, random: unit -> float) =
        let virus = Virus(computers |> List.filter (fun comp -> comp.IsInfected), infectChance, random)
        member val LastIteration = 0 with get, set
        member this.Computers = computers
        member this.Step() =
            printfn $"Iteration {this.LastIteration + 1}"
            this.LastIteration <- this.LastIteration + 1
            virus.SpreadInfection()
        member this.Changeable = virus.AbleToInfect
        member this.WorkWhileChangeable() =
            while virus.AbleToInfect do
                this.Step()
            printfn "Work finished!"
        