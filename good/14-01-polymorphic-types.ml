-- Example from https://ocaml.org/docs/basic-data-types

type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree


let sum = fn x -> case x of
  | Leaf -> 0
  | Node (x, lft, rht) -> x + sum lft + sum rht


let _ = map assert [
  sum Leaf == 0,
  sum (Node (1, Node (2, Leaf, Leaf), Node (3, Leaf, Node (4, Leaf, Leaf)))) == 10
]
