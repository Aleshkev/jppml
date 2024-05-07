
open Option

let _ = map assert
[ value (Some "ok") "err" == "ok"
, value None "err" == "err"
, get (Some "ok") == "ok"
]
