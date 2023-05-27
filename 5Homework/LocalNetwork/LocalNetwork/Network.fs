namespace LocalNetwork

open LocalNetwork.Computer

module LocalNetwork =
    type Network(computers: Computer list, infectChance) =
        let mutable iteration = 1
        let mutable changeable = true
        member this.Iteration = iteration
        member this.Computers = computers
        member this.Step() =
            printfn $"Iteration {iteration}"
            iteration <- iteration + 1
            changeable <- false
            for computer in this.Computers do
                if computer.IsInfected then
                    changeable <- changeable || computer.InfectNeighbors infectChance
        member this.Changeable = changeable
        member this.WorkWileChangeable() =
            while this.Changeable do
                this.Step()
            printfn "Work finished!"
        