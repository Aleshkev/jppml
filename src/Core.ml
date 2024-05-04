


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

let value = fn o default -> case o of Some x -> x | None -> default
let get = fn o -> case o of Some x -> x | None -> invalid_arg "option is None"


type bool = True | False

-- Comparisons

-- let __eq = fn x y -> __magicEq ("eq", x, y)
-- let __ne = fn x y -> not (x == y)

-- let __lt = fn x y -> __magicRelOp ("lt", x, y)

-- let min = fn a b -> if a < b then a else b
let max = fn a b -> if a > b then a else b

-- Boolean operations

let not = fn x -> case x of
  | True -> False
  | False -> True

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

-- String conversion functions.

let cat = fn a b -> a ^ b

let bool_of_string = fn x -> case x of
  | "True" -> Some True 
  | "False" -> Some False
  | _ -> None

-- Pair operations.

let fst = fn x -> case x of (a, _) -> a
let snd = fn x -> case x of (_, b) -> b

-- let __if = fn x ifT ifF -> __magicIf (x, ifT, ifF)


-- List operations

type 'a list = __Empty | __Cons of 'a * 'a list

let __cons = fn x y -> __Cons (x, y)
let cons = __cons

let __append = fn a b -> case a of
  | h :: t -> h :: __append t b
  | [] -> b
let append = __append

-- Input / output

let print_endline = fn s -> print (s ^ "\n")

-- Result type

type ('a, 'b) result =
  | Ok of 'a
  | Err of 'b


