open List

let fizzbuzz = fn n ->
  let aux = fn x ->
    if mod x 15 == 0 then print_endline "fizzbuzz"
    else if mod x 3 == 0 then print_endline "fizz"
    else if mod x 5 == 0 then print_endline "buzz"
    else ()
  and range = fn a b ->
    init (b - a) (fn x -> a + x)
  in
    iter aux (range 1 (n + 1))

let _ =
  fizzbuzz 0