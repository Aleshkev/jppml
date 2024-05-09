

-- This would fail (uncomment to see):
-- let x = 7 / 0

let _ =
  -- We have a special function "__try" (for debugging purposes only) that catches runtime errors.
  assert (__try (fn _x -> ignore (7 / 0)) == Some "Division_by_zero")
