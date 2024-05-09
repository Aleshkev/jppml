
-- From https://v2.ocaml.org/learn/taste.html

type expr =
  | Num of int
  | Var of string
  | Let of string * expr * expr
  | Binop of string * expr * expr
  
let assoc = fn a l -> case l of
  | [] -> raise Not_found
  | (x, y) :: t -> if x == a then y else assoc a t
let eval = fn env e -> case e of
  | Num i -> i
  | Var x -> assoc x env
  | Let (x, e1, in_e2) ->
      let val_x = eval env e1 in
      eval ((x, val_x) :: env) in_e2
  | Binop (op, e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      eval_op op v1 v2
and eval_op = fn op v1 v2 -> case op of
  | "+" -> v1 + v2
  | "-" -> v1 - v2
  | "*" -> v1 * v2
  | "/" -> v1 / v2
  | _ -> failwith ("Unknown operator: " ^ op)


let _ = map assert [
  eval [] (Let ("a", Num 2, Binop ("+", Var "a", Num 3))) == 2 + 3
]
