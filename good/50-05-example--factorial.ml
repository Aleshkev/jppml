
-- From https://v2.ocaml.org/learn/taste.html

let square = fn x -> x * x
let fact = fn x ->
  if x <= 1 then 1 else x * fact (x - 1)

-- Sum of the results of applying a function to each element of a list.
let sigma = fn f l -> case l of
  | [] -> 0
  | x :: t -> f x + sigma f t
let sumsquares = sigma (fn x -> x * x)

let _ = map assert [
  square 3 == 9,
  fact 5 == 120,
  sumsquares [1, 2, 3] == 1 + 4 + 9
]
