module MapForTreesTests

open MapForTrees
open NUnit.Framework
open BinaryTree
open FsUnit

let treeHeight1 = Tip 5
let treeHeight2 = TreeFull ("abc", Tip "a", Tip "bc")
let treeHeight3 = TreeFull (1, TreeFull (2, Tip 3, Tip 4), TreeOne (5, Tip 6))

[<Test>]
let ``treeMap works`` () =
    treeMap ((*) 2) treeHeight1 |> should equal (Tip 10)
    treeMap String.length treeHeight2 |> should equal (TreeFull (3, Tip 1, Tip 2))
    treeMap (sprintf "%d") treeHeight3 |> should equal (TreeFull ("1", TreeFull ("2", Tip "3", Tip "4"), TreeOne ("5", Tip "6")))