
type red_node =
  | Red of blue_node
and blue_node =
  | Blue of red_node
let a = Red b and b = Blue c and c = Red d and d = Blue a
