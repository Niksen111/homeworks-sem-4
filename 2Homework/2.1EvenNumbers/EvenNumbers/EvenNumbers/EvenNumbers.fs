namespace EvenNumbers

module EvenNumbers =
    let EvenNumbersFold list =
        (0, list) ||> List.fold (fun s v -> s + abs((v + 1) % 2) * v)
        
    let EvenNumbersFilter list =
        list |> List.filter (fun n -> n % 2 = 0) |> List.sum
        
    let EvenNumbersMap list =
        list |> List.map (fun x -> abs((x + 1) % 2) * x) |> List.sum