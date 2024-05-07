
-- As declaration
let one = 1

-- As expression
let four = 
  let two = one + one in 
  two + two

-- Recursive
let ones = 1 :: ones
let a = 0 :: b and b = 1 :: a
let factorial = fn x -> if x == 0 then 1 else x * factorial (x - 1)

let _ = assert (factorial 5 == 120)
