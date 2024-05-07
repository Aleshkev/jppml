
open List

let _ = map assert [
  length [] == 0,
  length [1] == 1,
  length [1, 2, 3] == 3,
  length [[1, 2, 3]] == 1,

  hd [1] == 1,
  hd [1, 2, 3] == 1,
  hd [[[]]] == [[]],

  tl [1] == [],
  tl [1, 2, 3] == [2, 3],

  nth [1] 0 == 1,
  --nth_opt [] 0 == None,
  nth_opt [1, 2, 3] 9 == None,

  append [1, 2] [3, 4] == [1, 2, 3, 4],
  [1, 2] @ [3, 4] == [1, 2, 3, 4],
  rev [1, 2, 3] == [3, 2, 1],
  
  init 3 id == [0, 1, 2],
  init 3 (fn x -> 2 * (x + 1)) == [2, 4, 6],
  
  concat [[1], [2, 3]] == [1, 2, 3],
  concat [[[1], [2]], []] == [[1], [2]],
  
  lists_equal (fn a b -> a == b) [1, 2] [1, 2],
  lists_equal (fn a b -> b == 2 * a) [1, 3, 6] [2, 6, 12],

  mapi (fn i _x -> i) [5, 5, 5] == [0, 1, 2],
  mapi (fn i x -> i + x) [5, 5, 5] == [5, 6, 7],

  iter (fn x -> ()) [1, 2] == (),

  for_all id [True, True],
  for_all id [True, False] == False,
  for_all id [],

  exists (fn x -> x * x == 9) [1, 2, 3],
  exists id [] == False,

  mem 7 [] == False,
  mem 7 [7],
  mem [[], []] [[[], []]],

  merge [] [] == [],
  merge [1, 3] [2, 4] == [1, 2, 3, 4],

  sort [4, 1, 7] == [1, 4, 7],
  sort [4, 1, 7, 1, 3, 3, 2, 2, 3, 10] == [1, 1, 2, 2, 3, 3, 3, 4, 7, 10]
]
