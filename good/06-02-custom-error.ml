
exception Busy of string

-- This would throw an error:
-- raise (Busy "doing Haskell")

let _ =
  assert (__try (fn _x -> raise (Busy "doing Haskell")) == Some "Busy \"doing Haskell\"")
