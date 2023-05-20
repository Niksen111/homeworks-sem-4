namespace LambdaInterpreter

module LambdaInterpreter =
    type Var = char
    type Term =
        | Variable of Var
        | Applique of Term * Term
        | Abstraction of Var * Term
        
    let fv t =
        let rec fvRec t list =
            match t with
            | Variable v -> if List.contains v list then [] else [ v ] 
            | Applique(t1, t2) -> fvRec t1 list @ fvRec t2 list
            | Abstraction(v, t1) -> fvRec t1 (v :: list)
        fvRec t [] |> List.distinct
    let bv t =
        let rec bvRec t =
            match t with
            | Variable _ -> []
            | Applique(t1, t2) -> bvRec t1 @ bvRec t2
            | Abstraction(v, t1) -> v :: bvRec t1
        bvRec t |> List.distinct
    let getNewFreeVarList (ls: char list) =
        let allChars = ['a'..'z'] @ ['A'..'Z']
        allChars |> List.filter (fun c -> not (List.contains c ls))
    let isValidTerm term =
        let rec isValidRec term ls =
            match term with
            | Variable v -> true
            | Applique(t1, t2) -> (isValidRec t1 ls) && (isValidRec t2 ls)
            | Abstraction(v, t) ->
                if List.contains v ls then
                    false
                else
                    isValidRec t (v::ls)
        isValidRec term []
    let substitute term v term1 =   
        let rec substituteRec term2 term3 =
            match term2 with
            | Variable v1 when v1 = v -> term3
            | Variable v1 -> Variable(v1)
            | Applique(t1, t2) -> Applique(substituteRec t1 term3, substituteRec t2 term3)
            | Abstraction(v1, t1) -> Abstraction(v1, substituteRec t1 term3)
        substituteRec term term1
    let normalStrategy t =
        let mutable changed = true
        let mutable result = t
        let rec normStratRec term =
            match term with
            | Variable v -> Variable v
            | Applique(t1, t2) ->
                match t1 with
                | Abstraction(v, t3) ->
                    changed <- true
                    substitute t3 v t2
                | _ -> Applique(normStratRec t1, normStratRec t2)
        while changed do
            changed <- false
            result <- normStratRec result
        
            