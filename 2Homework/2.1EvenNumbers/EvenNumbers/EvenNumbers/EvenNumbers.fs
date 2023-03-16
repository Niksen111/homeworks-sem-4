namespace EvenNumbers

module EvenNumbers =
    let EvenNumbersFold list =
        (0, list) ||> List.fold (fun s v -> s + ((v + 1) % 2) * v)
        
    let EvenNumbersFilter list =
        list |> List.filter (fun n -> n % 2 = 0) |> Seq.sum
        
    let EvenNumbersMap list =
        Seq.map (fun x -> ((x + 1) % 2) * x) list |> Seq.sum