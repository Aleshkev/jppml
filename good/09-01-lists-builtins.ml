
-- hd, tl, is_empty in the List module (names like in OCaml)
open List
let one = hd [1, 2, 3]
let two_and_three = tl [1, 2, 3]
let t = is_empty []
let f = is_empty [[]]

let _ = map assert [
  one == 1,
  two_and_three == [2, 3],
  t,
  not f
]

