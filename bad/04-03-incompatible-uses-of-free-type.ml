
-- This is ok:
let beta_list = []
let _ = (1 :: beta_list, "s" :: beta_list)

-- But this is not:
let _ =
  let alpha_list = [] in
    (1 :: alpha_list, "s" :: alpha_list)
