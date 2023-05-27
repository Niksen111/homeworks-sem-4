namespace LocalNetwork

open LocalNetwork.Computer

module LocalNetwork =
    type Network(computers: Computer list, infectChance) =
        let mutable lastIteration = 0
        let mutable changeable = true
        member this.LastIteration = lastIteration
        member this.Computers = computers
        member this.Step() =
            printfn $"Iteration {lastIteration + 1}"
            lastIteration <- lastIteration + 1
            changeable <- false
            for computer in this.Computers do
                if computer.IsInfected then
                    let result = computer.InfectNeighbors infectChance
                    changeable <- changeable || result
            for computer in this.Computers do
                computer.ActivateVirus()
            
        member this.Changeable = changeable
        member this.WorkWileChangeable() =
            while this.Changeable do
                this.Step()
            printfn "Work finished!"
        