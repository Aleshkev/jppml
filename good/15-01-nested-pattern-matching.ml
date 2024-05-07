-- Example adapted from https://www.cs.cornell.edu/courses/cs3110/2014fa/lectures/5/lec05.pdf

let zip3 = fn lists -> case lists of
  | ([],[],[]) -> []
  | (hd1::tl1,hd2::tl2,hd3::tl3) -> 
      (hd1,hd2,hd3)::zip3(tl1,tl2,tl3)
  | _ -> failwith "list length mismatch"
let unzip3 = fn triples -> case triples of
  | [] -> ([],[],[])
  | (a,b,c)::tl ->
      case unzip3 tl of
        (l1, l2, l3) -> (a::l1,b::l2,c::l3)


let _ = map assert [
  zip3 ([1, 2], ["a", "b"], [True, False]) == [(1, "a", True), (2, "b", False)],
  unzip3 [(1, "a", True), (2, "b", False)] == ([1, 2], ["a", "b"], [True, False])
]
