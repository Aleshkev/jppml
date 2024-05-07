
-- Simple
let x = if 2 + 2 == 4 then "ok" else "err"

-- Nested
let is_even = fn x ->
  if x == 1 then "odd" else
  if x == 2 then "even" else
  "unknown number"

-- Laziness
let x = if 2 + 2 == 4 then "ok" else failwith "err"
