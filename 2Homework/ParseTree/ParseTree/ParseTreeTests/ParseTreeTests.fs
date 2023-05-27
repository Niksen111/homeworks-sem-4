module ParseTreeTests

open NUnit.Framework
open ParseTree.ParseTree
open FsUnit

let tree1 = Value(1)
let tree2 = Node(Operation.Plus, Value(1), Value(1))
let tree3 = Node(Operation.Divide, Node(Operation.Minus, Value(25), Value(5)), Node(Operation.Multiply, Value(2), Value(2)))

[<Test>]
let TestCalculate () =
    calculate tree1 |> should equal 1
    calculate tree2 |> should equal 2
    calculate tree3 |> should equal 5