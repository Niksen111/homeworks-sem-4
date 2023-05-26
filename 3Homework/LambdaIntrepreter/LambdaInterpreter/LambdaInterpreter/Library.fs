namespace LambdaInterpreter

module LambdaInterpreter =
    type Variable = char
    type Term =
        | Var of Variable
        | App of Term * Term
        | Abs of Variable * Term
    let freeVar t =
        let rec fvRec t list =
            match t with
            | Var v -> if List.contains v list then [] else [ v ] 
            | App(t1, t2) -> fvRec t1 list @ fvRec t2 list
            | Abs(v, t1) -> fvRec t1 (v :: list)
        fvRec t [] |> List.distinct
    let isFreeVar var term =
        freeVar term |> List.contains var
    let boundVar t =
        let rec bvRec t =
            match t with
            | Var _ -> []
            | App(t1, t2) -> bvRec t1 @ bvRec t2
            | Abs(v, t1) -> v :: bvRec t1
        bvRec t |> List.distinct
    let getOtherVariablesList (ls: char list) =
        let allChars = ['a'..'z'] @ ['A'..'Z']
        allChars |> List.filter (fun c -> not (List.contains c ls))
    let isValidTerm term =
        let rec isValidRec term ls =
            match term with
            | Var _ -> true
            | App(t1, t2) -> (isValidRec t1 ls) && (isValidRec t2 ls)
            | Abs(v, t) ->
                if List.contains v ls then
                    false
                else
                    isValidRec t (v::ls)
        isValidRec term []
    let rec substitute var body term =
        match body with
        | Var v1 when v1 = var -> term
        | Var _ -> body
        | App(t1, t2) -> App(substitute var t1 term, substitute var t2 term)
        | Abs(v1, _) when v1 = var -> body
        | Abs(v1, t1) when not <| isFreeVar v1 term && isFreeVar v1 t1 ->
            Abs(v1, substitute var t1 term)
        | Abs(v1, t1) ->
            let newVar = freeVar t1 @ freeVar term |> getOtherVariablesList |> List.head
            Abs(newVar, substitute var (substitute v1 t1 (Var newVar)) term)
    let rec betaReduce (term: Term) =
        match term with
        | Var _ -> term
        | Abs(var, term1) -> Abs(var, betaReduce term1)
        | App(Abs(var, term1), term2) -> substitute var term1 term2
        | App(term1, term2) -> App(betaReduce term1, betaReduce term2)