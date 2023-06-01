module LocalNetwork.Computer

type OS =
    | Win
    | Gnu
    | Mac

type Computer(system: OS, id: int, isInfected: bool) =
    let mutable neighbors: Computer list = List.Empty
    member this.Id = id
    member val Neighbors = neighbors with get, set
    member val IsInfected = isInfected with get, set
    member this.System = system
    member this.PrintState() =
        if isInfected then
            printfn $"Computer {id} is infected."
        else
            printfn $"Computer {id} is not infected."
    new(system: OS, id: int) = Computer(system, id, false)
    