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
    
[<Test>]
let ``substituteFv works`` () =
    let term = Applique(Variable 'x', Applique(Variable 'y', Variable 'x'))
    let result = substituteFv term 'x' (Variable 'z')
    match result with
    | Applique(Variable(x1), Applique(Variable(x2), Variable(x3)))
        when x1 = 'z' && x2 = 'y' && x3 = 'z' -> ()
    | _ -> Assert.Fail("Incorrect result.")
    
[<Test>]
let ``substituteBv works`` () =
    let term = Abstraction('x', Applique(Variable 'x', Variable 'y'))
    let result = substituteBv term 'x' 'z'
    match result with
    | Abstraction(x1, Applique(Variable(x2), Variable(x3)))
        when x1 = 'z' && x2 = 'z' && x3 = 'y' -> ()
    | _ -> Assert.Fail("Incorrect result.")

[<Test>]
let ``fixVariables works`` () =
    let result = fixVariables termTrue termTrue
    match result with
    | Abstraction(v1, Abstraction(v2, Variable v3)) when v1 = 'a' && v2 = 'b' && v3 = 'a' -> ()
    | _ -> Assert.Fail("Incorrect result.")