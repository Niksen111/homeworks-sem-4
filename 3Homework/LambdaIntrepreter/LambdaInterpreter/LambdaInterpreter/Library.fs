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
            | Variable v -> if Seq.contains v list then [] else [ v ] 
            | Applique(t1, t2) -> fvRec t1 list @ fvRec t2 list
            | Abstraction(v, t1) -> fvRec t1 (v :: list)
        fvRec t [] |> Seq.distinct |> List.ofSeq
    let bv t =
        let rec bvRec t =
            match t with
            | Variable _ -> []
            | Applique(t1, t2) -> bvRec t1 @ bvRec t2
            | Abstraction(v, t1) -> v :: bvRec t1
        bvRec t |> Seq.distinct |> List.ofSeq
        
    let getNewFreeVar list =
        let rec getFreeVarRec c ls cnt =
            if not(List.contains c ls) then
                c
            else if cnt > 256 then
                char 666
            else getFreeVarRec (char (int c + 1)) ls (cnt + 1)
        getFreeVarRec (char 0) list 0
        
    let alphaRed var term1 term2 =
        let rec alphaRedRec var1 term3 =
            match term3 with
            | Variable item -> Variable(var1)
            | Applique(item1, item2) -> Applique(alphaRedRec var1 item1, alphaRedRec var1 item2)
            | Abstraction(item1, term) -> Abstraction(item1, alphaRedRec var1 term)
        let freeVar = fv term1 @ fv term2 |> Seq.distinct |> List.ofSeq
        if not(List.contains var freeVar) then
            Abstraction(var, term1)
        else
            let newVar = getNewFreeVar freeVar
            Abstraction(newVar, alphaRedRec newVar term1)
        
    let substitute term v term1 =   
        let rec substituteRec term2 v term3 =
            match term2 with
            | Variable v1 when v1 = v -> term3
            | Variable v1 -> Variable(v1)
            | Applique(t1, t2) -> Applique(substituteRec t1 v term3, substituteRec t2 v term3)
            | Abstraction(v1, t1) -> t1
        substituteRec term v term1