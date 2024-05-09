
-- API based on OCaml's List module:
-- https://ocaml.org/manual/5.1/api/List.html
--
-- Implementation also based on that from OCaml's List module:
-- https://github.com/lucasaiu/ocaml/blob/master/stdlib/list.ml

let length = fn l -> case l of
  | [] -> 0
  | _ :: l -> 1 + length l

let is_empty = fn l -> case l of
  | [] -> True
  | _ -> False

let cons = __cons

let hd = fn l -> case l of
  | [] -> failwith "List.hd"
  | a :: _ -> a

let tl = fn l -> case l of
  | [] -> failwith "List.tl"
  | _ :: l -> l

let nth_opt = fn l n ->
  if n < 0 then invalid_arg "List.nth_opt" else
  (case (l, n) of
    | ([], _) -> None
    | (a :: _, 0) -> Some a
    | _ -> nth_opt (tl l) (n - 1)
  )

let nth = fn l n ->
  case nth_opt l n of 
    | Some x -> x
    | None -> failwith "List.nth"

-- Skip first k elements of the list
let chop = fn k l ->
  if k == 0 then l else
  (case l of 
    | x :: t -> chop (k - 1) t
    | _ -> failwith "List.chop"
  )

-- Get first n elements of a list, or less if the list is shorter.
let take = fn n l -> 
  if n < 0 then invalid_arg "List.take" else
  if n == 0 then [] else
  (case l of
    | [] -> []
    | x :: l' -> x :: take (n - 1) l'
  )

let append = fn a b -> a @ b

let rev_append = fn a b -> case a of 
  | [] -> b
  | x :: a' -> rev_append a' (x :: b)

let rev = fn l -> rev_append l []

let init = fn n f ->
  let aux = fn i n f ->
    if i == n then [] else f i :: aux (i + 1) n f
  in
  aux 0 n f

let flatten = fn l -> case l of 
  | [] -> []
  | x :: l' -> x @ flatten l'

let concat = flatten

-- Comparison

let lists_equal = fn f as bs ->
  case (as, bs) of
    | ([], []) -> True
    | (a :: as', b :: bs') -> f a b && lists_equal f as' bs'
    | _ -> False

-- Iterators

let mapi = fn f l ->
  let aux = fn i f l -> case l of
    | [] -> []
    | x :: l' -> (f i x) :: aux (i + 1) f l'
  in
  aux 0 f l

let iter = fn f l -> case l of
  | [] -> ()
  | x :: l' -> let _ = f x in iter f l'


let iteri = fn f l ->
  let iteri = fn i f l -> case l of
    | [] -> ()
    | a::l -> let _ = f i a in iteri (i + 1) f l
  in
  iteri 0 f l

let fold_left = fn f accu l -> case l of
  | [] -> accu
  | a::l -> fold_left f (f accu a) l

let fold_right = fn f l accu -> case l of 
  | [] -> accu
  | a::l -> f a (fold_right f l accu)


let for_all = fn p l -> case l of
  | [] -> True
  | a :: l -> p a && for_all p l

let exists = fn p l -> case l of
  | [] -> False
  | a :: l -> p a || exists p l


let mem = fn x l -> case l of 
  | [] -> False
  | a :: l' -> a == x || mem x l'


let find = fn p l -> case l of
  | [] -> raise Not_found
  | x :: l -> if p x then x else find p l

let filter = fn p ->
  let find = fn accu l -> case l of
    | [] -> rev accu
    | x :: l -> if p x then find (x :: accu) l else find accu l
  in
  find []

let partition = fn p l ->
  let part = fn yes no l -> case l of
    | [] -> (rev yes, rev no)
    | x :: l -> if p x then part (x :: yes) no l else part yes (x :: no) l
  in
  part [] [] l

let split = fn l -> case l of
  |  [] -> ([], [])
  | (x,y)::l ->
      let (rx, ry) = split l in 
      (x::rx, y::ry)

let combine = fn a b -> case (a, b) of
  | ([], []) -> []
  | (x :: a', y :: b') -> (x, y) :: combine a' b'
  | (_, _) -> invalid_arg "List.combine"

-- Like combine, but stops when shorter list ends.
let zip = fn a b -> case (a, b) of
  | (x :: a', y :: b') -> (x, y) :: zip a' b'
  | _ -> []

-- Sorting

let merge = fn l1 l2 ->
  case (l1, l2) of
  | ([], l2) -> l2
  | (l1, []) -> l1
  | (h1 :: t1, h2 :: t2) ->
      if h1 <= h2
      then h1 :: merge t1 l2
      else h2 :: merge l1 t2

let merge_sort = fn l ->
  let n = length l in
  if n < 2 then l else
  (let k = n / 2 in
  merge (merge_sort (take k l)) (merge_sort (chop k l)))

let sort = merge_sort
