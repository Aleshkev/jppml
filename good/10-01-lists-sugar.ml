
let a = [1, 2, 3]
let b = 1 :: 2 :: 3 :: []
let c = __Cons (1, __Cons (2, __Cons (3, __Empty)))


let _ = map assert [
  a == b,
  b == c
]
