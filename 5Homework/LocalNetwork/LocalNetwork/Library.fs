namespace LocalNetwork

module LocalNetwork =
    
    type OS =
        | Win
        | Gnu
        | Mac
        
    let isAdjacencyMatrix(matrix: int[,]) =
        let n = matrix.GetLength(0)
        if n <> matrix.Length then
            false
        else
            let mutable isAdjacency = true
            for i in 0..n-1 do
                for j in 0..n-1 do
                    if matrix[i,j] <> 0 && matrix[i,j] <> 1 then
                        isAdjacency <- false
                    if matrix[i,j] <> matrix[j,i] then
                        isAdjacency <- false
            isAdjacency
    
    let rec containsWith condition ls =
        match ls with
        | [] -> false
        | hd::tl -> (condition hd) || (containsWith condition tl)
    
    type Network() =
        let mutable winProb = -1.f
        let mutable gnuProb = -1.f
        let mutable macProb = -1.f
        let mutable probabilities = []
        let mutable adjacencyMatrix = Array2D.init 0 0 (fun _ _ -> 0)
        let mutable infectedComputers = List.Empty
        let random = System.Random()
        let isValidInfected (ls: int list) =
            if ls.IsEmpty || not infectedComputers.IsEmpty || adjacencyMatrix.Length = 0 then
                false
            else
                if containsWith (fun x -> x > ls.Length - 1) ls then
                    false
                else
                    true
        member this.WinProb
            with get () = winProb
            and set prob =
                if winProb <> -1.f && prob >= 0.f && prob <= 0.f then
                    winProb <- prob
        member this.GnuProb
            with get () = gnuProb
            and set prob =
                if gnuProb <> -1.f && prob >= 0.f && prob <= 0.f then
                    gnuProb <- prob
        member this.MacProb
            with get () = macProb
            and set prob =
                if macProb <> -1.f && prob >= 0.f && prob <= 0.f then
                    macProb <- prob
        member this.Infected
            with get () = infectedComputers
            and set (ls: int list) =
                if isValidInfected ls then
                    infectedComputers <- ls
        member this.AdjacencyMatrix
            with get () = adjacencyMatrix
            and set (matrix: int array2d) =
                if adjacencyMatrix.Length = 0 && isAdjacencyMatrix matrix then
                    adjacencyMatrix <- matrix
        member this.IsSetUp =
            winProb <> -1.f && gnuProb <> -1.f && macProb <> -1.f && adjacencyMatrix.Length <> 0 && not infectedComputers.IsEmpty
        member this.isPossibleMakeStep =
            false
        member this.PrintCurrentState =
            printfn "Kek"
        member this.MakeStep() =
            ()
        member this.Start() =
            while this.isPossibleMakeStep do
                this.PrintCurrentState
                this.MakeStep
            this.PrintCurrentState
              