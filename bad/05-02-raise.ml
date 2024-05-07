
exception Busy of string
let x = raise (Busy "doing Haskell")
