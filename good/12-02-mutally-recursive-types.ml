
type 'a red_node =
  | Red of 'a blue_node
and 'a blue_node =
  | Blue of 'a red_node
let a = Red b and b = Blue c and c = Red d and d = Blue a
                                               