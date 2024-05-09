
let x =
  let a = 1 in
    let b = 2 in
      let c = 3 in
        let d = 4 in
          let e = 5 in
            let f = 6 in
              a + b + c + d + e + f

let y =
  let a = 1 and b = 2 and c = 3 in
    let d = 4 and e = 5 and f = 6 in
      a + b + c + d + e + f


let _ = map assert [
  x == 1 + 2 + 3 + 4 + 5 + 6,
  x == y
]
