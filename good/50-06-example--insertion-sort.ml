
-- From https://v2.ocaml.org/learn/taste.html

-- Insertion sort.
let sort = fn l ->
  let insert = fn e l -> case l of
    | [] -> [e]
    | x :: t -> if e < x then e :: x :: t
                else x :: insert e t
  in
    case l of
      | [] -> []
      | x :: t -> insert x (sort t)

let _ = map assert [
  sort [3, 4, 4, 1] == [1, 3, 4, 4]
]
