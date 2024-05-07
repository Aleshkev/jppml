open List

-- Anonymous functions
let all_plus_1 = fn l -> map (fn x -> x + 1) l

-- Partial application
let all_plus_1' = map (fn x -> x + 1)

-- Higher-order functions
let flip = fn f a b -> f b a

-- Closures
let add_two =
  let two = 1 + 1 in
  fn x -> x + two

let _ = map assert [
  all_plus_1 [1, 2, 3] == [2, 3, 4],
  all_plus_1' [1, 2, 3] == [2, 3, 4],
  flip (fn a b -> a / b) 8 16 == 2,
  add_two 7 == 9
]
