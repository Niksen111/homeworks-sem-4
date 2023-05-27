namespace LocalNetwork

open LocalNetwork.Computer

module LocalNetwork =
    type Network(computers: Computer list, connections: Computer list list, infectChance) =
        let rec addConnections (comp: Computer list) (conn: Computer list list) =
            if comp.Length <> conn.Length then
                None
            else
                match comp with
                | [] -> Some(computers)
                | hd::tl ->
                    hd.Neighbors <- conn.Head
                    addConnections tl conn.Tail
        let mutable iteration = 1
        let mutable changeable = true
        member this.Computers = addConnections computers connections
        member this.Step() =
            printfn $"Iteration {iteration}"
            iteration <- iteration + 1
            changeable <- false
            if this.Computers.IsNone then
                printfn "Computers/connections set up was failed."
            else
                for computer in this.Computers.Value do
                    if computer.IsInfected then
                        changeable <- changeable || computer.InfectNeighbors infectChance
        member this.Changeable = changeable
        member this.WorkWileChangeable =
            while this.Changeable do
                this.Step()
            printfn "Work finished!"
        