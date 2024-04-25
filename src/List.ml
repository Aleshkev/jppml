
-- API copied from OCaml's List module.

let length = fn l -> case l of
  | [] -> 0
  | _ :: l -> 1 + length l

let hd = fn l -> case l of
  | [] -> failwith "hd"
  | a :: _ -> a

let tl = fn l -> case l of
  | [] -> failwith "tl"
  | _ :: l -> l

let nth = fn l n ->
  if n < 0 then invalid_arg "nth" else
  (case (l, n) of
    | ([], _) -> failwith "nth"
    | (a :: _, 0) -> a
    | _ -> nth (tl l) (n - 1))

let append = fn a b -> a @ b

let rev_append = fn a b -> case a of 
  | [] -> b
  | x :: a' -> rev_append a' (x :: b)

let rev = fn l -> rev_append l []

let flatten = fn l -> case l of 
  | [] -> []
  | x :: l' -> x @ flatten l'

let concat = flatten

let map = fn f l -> case l of 
  | [] -> []
  | x :: l' -> (f x) :: map f l'

let mapi = fn f l ->
  let aux = fn i f l -> case l of
    | [] -> []
    | x :: l' -> (f i x) :: mapi (i + 1) f l
  in
  aux 0 f l

let iter = fn f l -> case l of
  | [] -> ()
  | x :: l' -> let _u = f x in iter f l'
