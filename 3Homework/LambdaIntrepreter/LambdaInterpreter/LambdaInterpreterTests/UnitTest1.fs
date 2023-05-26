module LambdaInterpreterTests

open NUnit.Framework
open LambdaInterpreter.LambdaInterpreter
open FsUnit

let termId = Abs('l', Var('l'))
let termTrue = Abs('x', Abs('y', Var('x')))
let termFalse = Abs('z', Abs('w', Var('w')))
let termIf = Abs('b', Abs('t', Abs('f', App(Var('b'), App(Var('t'), Var('f'))))))
let termOr = Abs('a', Abs('b', App(termIf, App(Var 'a', App(termTrue, Var 'b')))))
// IF TRUE u v
// Result u
let term1 = App(App(App(termIf, termTrue), Var 'u'), Var 'v')
// FALSE u v
// Result v
let term2 = App(App(termFalse, Var 'u'), Var 'v')
// ID y
let term3 = App(termId, Var 'y')
// x y
let term4 = App(Var 'x', Var 'y')
// (\x.y) ((\x.x x) (\x.x x))
let term5 = App(Abs('x', Var 'y'), App(Abs('x', App(Var 'x', Var 'x')), Abs('x', App(Var 'x', Var 'x'))))

// (\x.\.y.y) a b


[<Test>]
let ``Get other variables works`` () =
    let expectedResult1 = ['a'..'z'] @ ['A'..'Z'] |> List.filter (fun c -> c <> 'z')
    let result1 = getOtherVariablesList ['z']
    let expectedResult2 = ['a'..'z'] @ ['A'..'Z'] |> List.filter (fun c -> not (List.contains c ['z'; 'y'; 'x']))
    let result2 = getOtherVariablesList ['z'; 'y'; 'x']
    result1 |> should equal expectedResult1
    result2 |> should equal expectedResult2
    
//[<Test>]
let ``BetaReduction works1`` () =
    let result = betaReduce term1
    match result with
    | Var 'u' -> ()
    | _ -> Assert.Fail()
    
//[<Test>]
let ``BetaReduction works 2`` () =
    let result = betaReduce term2
    match result with
    | Var 'v' -> ()
    | _ -> Assert.Fail()
    
[<Test>]
let ``BetaReduction works3`` () =
    let result = betaReduce term3
    match result with
    | Var 'y' -> ()
    | _ -> Assert.Fail()
    
[<Test>]
let ``BetaReduction works4`` () =
    let result = betaReduce term4
    match result with
    | App(Var 'x', Var 'y') -> ()
    | _ -> Assert.Fail()
    
[<Test>]
let ``BetaReduction works5`` () =
    let result = betaReduce term5
    match result with
    | Var 'y' -> ()
    | _ -> Assert.Fail()
    
[<Test>]
let ``bv works`` () =
    boundVar termIf |> should equal ['b'; 't'; 'f']
    boundVar term1 |> should equal ['b'; 't'; 'f'; 'x'; 'y']
    
[<Test>]
let ``fv works`` () =
    freeVar termIf |> List.isEmpty |> should be True
    freeVar term1 |> should equal ['u'; 'v']
    
[<Test>]
let ``isValidTerm works`` () =
    isValidTerm term1 |> should be True
    isValidTerm (Abs('x', Abs('x', Var 'y'))) |> should be False
    
[<Test>]
let ``substituteFv works`` () =
    let term = App(Var 'x', App(Var 'y', Var 'x'))
    let result = substitute 'x' term (Var 'z')
    match result with
    | App(Var('z'), App(Var('y'), Var('z'))) -> ()
    | _ -> Assert.Fail("Incorrect result.")