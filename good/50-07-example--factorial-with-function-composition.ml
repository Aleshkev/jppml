
-- From https://v2.ocaml.org/learn/taste.html

-- Factorial using function composition
let compose = fn f g x -> f (g x)
let power = fn f n ->
  if n == 0 then fn x -> x
  else compose f (power f (n - 1))
