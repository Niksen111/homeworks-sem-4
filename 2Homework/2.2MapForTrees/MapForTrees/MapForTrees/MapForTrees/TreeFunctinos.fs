namespace MapForTrees

module BinaryTree =
    type Tree<'a> =
        | TreeFull of 'a * Tree<'a> * Tree<'a>
        | TreeOne of 'a * Tree<'a>
        | Tip of 'a
    let rec treeMap func tree =
        match tree with
        | TreeFull(v, l, r) -> TreeFull (func v, treeMap func l, treeMap func r)
        | TreeOne(v, s) -> TreeOne (func v, treeMap func s)
        | Tip v -> Tip (func v)