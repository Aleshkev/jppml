
-- List pattern matching
let head = fn l -> case l of
  | [] -> failwith "empty >:c"
  | x :: _ -> x


let _ = map assert [
  head [1, 2] == 1,
  head [[], ["s"]] == []
]
