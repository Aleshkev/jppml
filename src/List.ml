
-- API copied from OCaml's List module.

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

-- let rec iteri i f = function
--     [] -> ()
--   | a::l -> f i a; iteri (i + 1) f l

-- let iteri f l = iteri 0 f l

-- let rec fold_left f accu l =
--   match l with
--     [] -> accu
--   | a::l -> fold_left f (f accu a) l

-- let rec fold_right f l accu =
--   match l with
--     [] -> accu
--   | a::l -> f a (fold_right f l accu)

-- let rec map2 f l1 l2 =
--   match (l1, l2) with
--     ([], []) -> []
--   | (a1::l1, a2::l2) -> let r = f a1 a2 in r :: map2 f l1 l2
--   | (_, _) -> invalid_arg "List.map2"

-- let rev_map2 f l1 l2 =
--   let rec rmap2_f accu l1 l2 =
--     match (l1, l2) with
--     | ([], []) -> accu
--     | (a1::l1, a2::l2) -> rmap2_f (f a1 a2 :: accu) l1 l2
--     | (_, _) -> invalid_arg "List.rev_map2"
--   in
--   rmap2_f [] l1 l2
-- ;;

-- let rec iter2 f l1 l2 =
--   match (l1, l2) with
--     ([], []) -> ()
--   | (a1::l1, a2::l2) -> f a1 a2; iter2 f l1 l2
--   | (_, _) -> invalid_arg "List.iter2"

-- let rec fold_left2 f accu l1 l2 =
--   match (l1, l2) with
--     ([], []) -> accu
--   | (a1::l1, a2::l2) -> fold_left2 f (f accu a1 a2) l1 l2
--   | (_, _) -> invalid_arg "List.fold_left2"

-- let rec fold_right2 f l1 l2 accu =
--   match (l1, l2) with
--     ([], []) -> accu
--   | (a1::l1, a2::l2) -> f a1 a2 (fold_right2 f l1 l2 accu)
--   | (_, _) -> invalid_arg "List.fold_right2"

let for_all = fn p l -> case l of
  | [] -> True
  | a :: l -> p a && for_all p l

let exists = fn p l -> case l of
  | [] -> False
  | a :: l -> p a || exists p l

-- let rec for_all2 p l1 l2 =
--   match (l1, l2) with
--     ([], []) -> true
--   | (a1::l1, a2::l2) -> p a1 a2 && for_all2 p l1 l2
--   | (_, _) -> invalid_arg "List.for_all2"

-- let rec exists2 p l1 l2 =
--   match (l1, l2) with
--     ([], []) -> false
--   | (a1::l1, a2::l2) -> p a1 a2 || exists2 p l1 l2
--   | (_, _) -> invalid_arg "List.exists2"

let mem = fn x l -> case l of 
  | [] -> False
  | a :: l' -> a == x || mem x l'

-- let rec assoc x = function
--     [] -> raise Not_found
--   | (a,b)::l -> if compare a x = 0 then b else assoc x l

-- let rec assq x = function
--     [] -> raise Not_found
--   | (a,b)::l -> if a == x then b else assq x l

-- let rec mem_assoc x = function
--   | [] -> false
--   | (a, b) :: l -> compare a x = 0 || mem_assoc x l

-- let rec mem_assq x = function
--   | [] -> false
--   | (a, b) :: l -> a == x || mem_assq x l

-- let rec remove_assoc x = function
--   | [] -> []
--   | (a, b as pair) :: l ->
--       if compare a x = 0 then l else pair :: remove_assoc x l

-- let rec remove_assq x = function
--   | [] -> []
--   | (a, b as pair) :: l -> if a == x then l else pair :: remove_assq x l

let find = fn p l -> case l of
  | [] -> raise Not_found
  | x :: l -> if p x then x else find p l

-- let find_all p =
--   let rec find accu = function
--   | [] -> rev accu
--   | x :: l -> if p x then find (x :: accu) l else find accu l in
--   find []

-- let filter = find_all

let partition = fn p l ->
  let part = fn yes no l -> case l of
    | [] -> (rev yes, rev no)
    | x :: l -> if p x then part (x :: yes) no l else part yes (x :: no) l
  in
  part [] [] l

-- let rec split = function
--     [] -> ([], [])
--   | (x,y)::l ->
--       let (rx, ry) = split l in (x::rx, y::ry)

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

-- let stable_sort cmp l =
--   let rec rev_merge l1 l2 accu =
--     match l1, l2 with
--     | [], l2 -> rev_append l2 accu
--     | l1, [] -> rev_append l1 accu
--     | h1::t1, h2::t2 ->
--         if cmp h1 h2 <= 0
--         then rev_merge t1 l2 (h1::accu)
--         else rev_merge l1 t2 (h2::accu)
--   in
--   let rec rev_merge_rev l1 l2 accu =
--     match l1, l2 with
--     | [], l2 -> rev_append l2 accu
--     | l1, [] -> rev_append l1 accu
--     | h1::t1, h2::t2 ->
--         if cmp h1 h2 > 0
--         then rev_merge_rev t1 l2 (h1::accu)
--         else rev_merge_rev l1 t2 (h2::accu)
--   in
--   let rec sort n l =
--     match n, l with
--     | 2, x1 :: x2 :: _ ->
--        if cmp x1 x2 <= 0 then [x1; x2] else [x2; x1]
--     | 3, x1 :: x2 :: x3 :: _ ->
--        if cmp x1 x2 <= 0 then begin
--          if cmp x2 x3 <= 0 then [x1; x2; x3]
--          else if cmp x1 x3 <= 0 then [x1; x3; x2]
--          else [x3; x1; x2]
--        end else begin
--          if cmp x1 x3 <= 0 then [x2; x1; x3]
--          else if cmp x2 x3 <= 0 then [x2; x3; x1]
--          else [x3; x2; x1]
--        end
--     | n, l ->
--        let n1 = n asr 1 in
--        let n2 = n - n1 in
--        let l2 = chop n1 l in
--        let s1 = rev_sort n1 l in
--        let s2 = rev_sort n2 l2 in
--        rev_merge_rev s1 s2 []
--   and rev_sort n l =
--     match n, l with
--     | 2, x1 :: x2 :: _ ->
--        if cmp x1 x2 > 0 then [x1; x2] else [x2; x1]
--     | 3, x1 :: x2 :: x3 :: _ ->
--        if cmp x1 x2 > 0 then begin
--          if cmp x2 x3 > 0 then [x1; x2; x3]
--          else if cmp x1 x3 > 0 then [x1; x3; x2]
--          else [x3; x1; x2]
--        end else begin
--          if cmp x1 x3 > 0 then [x2; x1; x3]
--          else if cmp x2 x3 > 0 then [x2; x3; x1]
--          else [x3; x2; x1]
--        end
--     | n, l ->
--        let n1 = n asr 1 in
--        let n2 = n - n1 in
--        let l2 = chop n1 l in
--        let s1 = sort n1 l in
--        let s2 = sort n2 l2 in
--        rev_merge s1 s2 []
--   in
--   let len = length l in
--   if len < 2 then l else sort len l
-- ;;

-- let sort = stable_sort;;
-- let fast_sort = stable_sort;;
