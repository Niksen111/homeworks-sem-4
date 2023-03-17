namespace MapForTrees

module BinaryTree =
    type Tree<'a> =
        | TreeFull of 'a * Tree<'a> * Tree<'a>
        | TreeOne of 'a * Tree<'a>
        | Tip of 'a
    let rec TreeMap func tree =
        match tree with
        | TreeFull(v, l, r) -> TreeFull (func v, TreeMap func l, TreeMap func r)
        | TreeOne(v, s) -> TreeOne (func v, TreeMap func s)
        | Tip v -> Tip (func v)