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
            let mutable newInfected = List.Empty
            for computer in this.Computers do
                if computer.IsInfected && newInfected |> List.contains computer.Id |> not then
                    let result, localInfected = computer.InfectNeighbors infectChance
                    newInfected <- newInfected @ localInfected
                    changeable <- changeable || result
            
        member this.Changeable = changeable
        member this.WorkWileChangeable() =
            while this.Changeable do
                this.Step()
            printfn "Work finished!"
        