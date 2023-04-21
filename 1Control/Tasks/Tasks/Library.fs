namespace Tasks

module Tasks =
    let rec supermap l f =
        match l with
        | [] -> []
        | head :: tail -> f head @ supermap tail f

    let rhombus n =
        let rec rhombusRec (l: List<List<char>>) i =
            match i with
            | x when x > n * 2 - 1 -> l
            | x when x <= n ->
                ([for _ in 1..(n - i) -> ' ']
                 @ [for _ in 1..(i*2 - 1) -> '*']
                 @ [for _ in 1..(n - i) -> ' ']) :: l
            | _ ->
                ([for _ in 1..(i - n) -> ' ']
                 @ [for _ in 1..(n * 4 - 1 - i * 2) -> '*']
                 @ [for _ in 1..(i - n) -> ' ']) :: l
        if (n <= 0) then
            [[]]
        else
            rhombusRec [[]] 1
            
    let printRhombus n =
        let rec printRhombusRec (l: List<List<char>>) =
            match l with
            | [] -> ()
            | h::t ->
                printRhombusRec t
                printf "%s" <| (String.concat "" <| List.map string h)
                
        printRhombusRec <| rhombus n