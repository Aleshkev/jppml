-- Example from https://ocaml.org/docs/basic-data-types

type commit =
  | Hash of string
  | Tag of string
  | Branch of string
  | Head
  | Fetch_head
  | Orig_head
  | Merge_head

-- Simple pattern matching
let commit_to_string = fn x -> case x of
  | Hash sha -> sha
  | Tag name -> name
  | Branch name -> name
  | Head -> "HEAD"
  | Fetch_head -> "FETCH_HEAD"
  | Orig_head -> "ORIG_HEAD"
  | Merge_head -> "MERGE_HEAD"


let _ = map assert [
  commit_to_string (Tag "x") == "x",
  commit_to_string Head == "HEAD"
]
