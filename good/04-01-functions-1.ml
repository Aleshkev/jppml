
-- Simple
let id = fn x -> x

-- Multiple arguments
let plus = fn a b -> a + b

-- Recursion
let factorial = fn x -> if x == 0 then 1 else x * factorial (x - 1)

let _ = map assert [
  id 7 == 7,
  id "s" == "s",
  plus 2 3 == 5,
  factorial 5 == 120
]