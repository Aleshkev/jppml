type natural =
| Zero
| Succ of natural

let add = fn a b -> case a of
| Zero -> b
| Succ a -> Succ (add a b)

let mul = fn a b -> case (a, b) of
| (Zero, _) -> Zero
| (_, Zero) -> Zero
| (Succ a, b) -> add b (mul a b)

let length = fn l -> case l of
| [] -> Zero
| _ :: l' -> Succ (length l')

let to_int = fn x -> case x of
| Zero -> 0
| Succ x -> 1 + to_int x


let one = Succ Zero
let two = Succ one
let three = add two one
let four = mul two two
let five = add three two
let six = mul two three
let seven = add four three
let eight = mul four two
let nine = mul three three
let ten = add five five


let _ = map assert [
  to_int one == 1,
  to_int two == 2,
  to_int three == 3,
  to_int four == 4,
  to_int five == 5,
  to_int six == 6,
  to_int seven == 7,
  to_int eight == 8,
  to_int nine == 9,
  to_int ten == 10,

  to_int (length []) == 0,
  to_int (length [1, 2, 3]) == 3
]
