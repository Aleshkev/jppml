
let x = (map Some [1, 2, 3]) @ [None]

let _ = 
  assert (x == [Some 1, Some 2, Some 3, None])
