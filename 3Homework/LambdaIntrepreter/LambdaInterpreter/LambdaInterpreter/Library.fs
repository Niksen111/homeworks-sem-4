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
    let getOtherVariablesList (ls: char list) =
        let allChars = ['a'..'z'] @ ['A'..'Z']
        allChars |> List.filter (fun c -> not (List.contains c ls))
    let isValidTerm term =
        let rec isValidRec term ls =
            match term with
            | Variable _ -> true
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
    let fixVariables term1 v term2 =
        let bv1 = bv term1
        let bv2 = bv term2
        let intersection = bv1 |> List.filter (fun x -> bv2 |> List.contains x)
        let newBv = getOtherVariablesList (bv1 @ bv2) |> List.take (intersection |> List.length)
        let rec substituteRec term (leftIntersec: char list) (leftBv: char list)=
            if intersection.IsEmpty then
                term
            else
                substituteRec (substitute term leftIntersec.Head (Variable(leftBv.Head))) leftIntersec.Tail leftBv.Tail
        substituteRec term1 intersection newBv
    let normalStrategy (t: Term) =
        let rec loop t =
            match t with
            | Variable v -> (Variable(v), false)
            | Abstraction(c, term) ->
                let newTerm, isSuccessful = loop term
                (Abstraction(c, newTerm), isSuccessful)
            | Applique(term1, term2) ->
                match term1 with
                | Abstraction(v, term) ->
                    let term = fixVariables term v term2
                    (substitute term v term2, true)
                | _ ->
                    let newTerm1, isSuccessful = loop term1
                    if isSuccessful then
                        (Applique(newTerm1, term2), true)
                    else
                        let newTerm2, isSuccessful = loop term2
                        (Applique(newTerm1, newTerm2), isSuccessful)

        let rec normalStrategyRec (term: Term, isSuccessful: bool) =
            if isSuccessful then
                normalStrategyRec (loop term)
            else
                term

        if not (isValidTerm t) then
            t
        else
            normalStrategyRec(t, true)