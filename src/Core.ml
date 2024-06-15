
-- Exceptions

exception Exit
exception Match_failure
exception Invalid_argument of string
exception Failure of string
exception Not_found
exception Division_by_zero

let invalid_arg = fn s -> raise (Invalid_argument s)
let failwith = fn s -> raise (Failure s)


-- Options

type 'a option =
  | None
  | Some of 'a

type bool = True | False

-- Comparisons

let min = fn a b -> if a < b then a else b
let max = fn a b -> if a > b then a else b

-- Boolean operations

let not = fn x -> case x of
  | True -> False
  | False -> True

let __ne = fn a b -> not (a == b)

let __and = fn x fy ->
  if x then fy () else False

let __or = fn x fy ->
  if x then True else fy ()

-- Integer arithmetic

let __neg = fn x -> 0 - x

let succ = fn x -> x + 1
let pred = fn x -> x - 1

let mod = fn x y -> x - x / y * y
let abs = fn x -> if x < 0 then -x else x

-- Unit operations.

let ignore = fn x -> ()

-- Function combinators.

let id = fn x -> x
let const = fn a b -> a
let flip = fn f a b -> f b a
let negate = fn f x -> not (f x)
let curry = fn f x -> case x of (a, b) -> f a b
let uncurry = fn f a b -> f (a, b)
let combine = fn g f x -> g (f x)

-- String conversion functions.

let cat = fn a b -> a ^ b

let bool_of_string = fn x -> case x of
  | "True" -> Some True 
  | "False" -> Some False
  | _ -> None

-- Pair operations.

let fst = fn x -> case x of (a, _) -> a
let snd = fn x -> case x of (_, b) -> b

-- List operations

type 'a list = __Nil | __Cons of 'a * 'a list

let __cons = fn x y -> __Cons (x, y)

let __append = fn a b -> case a of
  | h :: t -> h :: __append t b
  | [] -> b

let map = fn f l -> case l of 
  | [] -> []
  | x :: l' -> (f x) :: map f l'

-- Input / output

let print = fn x -> print_string (to_string x)
let print_endline = fn s -> print_string (s ^ "\n")
let print_int = fn x -> print (x + 0)

-- Result type

type ('a, 'b) result =
  | Ok of 'a
  | Err of 'b

-- Assertions

exception Assert_failure

let assert = fn x -> case x of
  | True -> ()
  | False -> raise Assert_failure
