namespace BracketsSequence

module MyModule =
    type OpenBrackets =
        | Round
        | Square
        | Curly
    
    let checkBrackets (s: string) =
        let rec checkBracketsRec l stack =
            match l with
            | [] ->
                match stack with
                | [] -> true
                | _ -> false
            | h::t ->
                match h with
                | '(' -> checkBracketsRec t (Round::stack)
                | '[' -> checkBracketsRec t (Square::stack)
                | '{' -> checkBracketsRec t (Curly::stack)
                | ')' ->
                    match stack with
                    | rnd::tail when rnd = Round -> checkBracketsRec t tail
                    | _ -> false
                | '}' ->
                    match stack with
                    | rnd::tail when rnd = Curly -> checkBracketsRec t tail
                    | _ -> false
                | ']' ->
                    match stack with
                    | rnd::tail when rnd = Square -> checkBracketsRec t tail
                    | _ -> false
                | _ -> checkBracketsRec t stack
                
        checkBracketsRec (s |> Seq.toList) List.Empty