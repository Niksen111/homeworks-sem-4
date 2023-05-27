namespace ParseTree

module ParseTree =
    type Operation =
        | Plus = '+'
        | Minus = '-'
        | Multiply = '*'
        | Divide = '/'
    type ParseTree =
        | Value of int
        | Node of Operation * ParseTree * ParseTree
        
    let calculate tree =
        let oper o =
            match o with
            | Operation.Plus -> (+)
            | Operation.Minus -> (-)
            | Operation.Multiply -> (*)
            | Operation.Divide -> (/)
            | _ -> System.ArgumentOutOfRangeException() |> raise
            
        let rec calculateRec tree =
            match tree with
            | Value(x) -> x
            | Node(x, l, r) -> oper x (calculateRec l) (calculateRec r)
        calculateRec tree