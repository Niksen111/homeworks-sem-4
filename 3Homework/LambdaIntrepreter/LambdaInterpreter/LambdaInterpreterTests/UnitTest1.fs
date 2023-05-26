module LambdaInterpreterTests

open NUnit.Framework
open LambdaInterpreter.LambdaInterpreter
open FsUnit

let termId = Abstraction('l', Variable('l'))
let termTrue = Abstraction('x', Abstraction('y', Variable('x')))
let termFalse = Abstraction('z', Abstraction('w', Variable('w')))
let termIf = Abstraction('b', Abstraction('t', Abstraction('f', Applique(Variable('b'), Applique(Variable('t'), Variable('f'))))))
let termOr = Abstraction('a', Abstraction('b', Applique(termIf, Applique(Variable 'a', Applique(termTrue, Variable 'b')))))
// IF TRUE u v
// Result u
let term1 = Applique(termIf, Applique(termTrue, Applique(Variable 'u', Variable 'v')))
// FALSE ID v
// Result v
let term2 = Applique(termFalse, Applique(termId, Variable 'v'))

[<Test>]
let ``Get other variables works`` () =
    let expectedResult1 = ['a'..'z'] @ ['A'..'Z'] |> List.filter (fun c -> c <> 'z')
    let result1 = getOtherVariablesList ['z']
    let expectedResult2 = ['a'..'z'] @ ['A'..'Z'] |> List.filter (fun c -> not (List.contains c ['z'; 'y'; 'x']))
    let result2 = getOtherVariablesList ['z'; 'y'; 'x']
    result1 |> should equal expectedResult1
    result2 |> should equal expectedResult2
    
[<Test>]
let ``NormalStrategy works 1`` () =
    let result = normalStrategy term2
    match result with
    | Variable v when v = 'v' -> ()
    | _ -> Assert.Fail()


[<Test>]
let ``NormalStrategy works2`` () =
    let result = normalStrategy term1
    match result with
    | Variable u when u = 'u' -> ()
    | _ -> Assert.Fail()
    
[<Test>]
let ``bv works`` () =
    bv termIf |> should equal ['b'; 't'; 'f']
    bv term1 |> should equal ['b'; 't'; 'f'; 'x'; 'y']
    
    
[<Test>]
let ``fv works`` () =
    fv termIf |> List.isEmpty |> should be True
    fv term1 |> should equal ['u'; 'v']
    
[<Test>]
let ``isValidTerm works`` () =
    isValidTerm term1 |> should be True
    isValidTerm (Abstraction('x', Abstraction('x', Variable 'y'))) |> should be False