-- Examples from https://ocaml.org/docs/basic-data-types

-- With data
type commit =
  | Hash of string
  | Tag of string
  | Branch of string
  | Head
  | Fetch_head
  | Orig_head
  | Merge_head
let commit_to_string = fn x -> case x of
  | Hash sha -> sha
  | Tag name -> name
  | Branch name -> name
  | Head -> "HEAD"
  | Fetch_head -> "FETCH_HEAD"
  | Orig_head -> "ORIG_HEAD"
  | Merge_head -> "MERGE_HEAD"


-- Polymorphic
type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree
let sum = fn x -> case x of
  | Leaf -> 0
  | Node (x, lft, rht) -> x + sum lft + sum rht


let _ = map assert [
  commit_to_string (Tag "x") == "x",
  commit_to_string Head == "HEAD",
  sum (Node (1, Node (2, Leaf, Leaf), Node (3, Leaf, Node (4, Leaf, Leaf)))) == 10
]
