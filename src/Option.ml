
let value = fn o default -> case o of 
  | Some x -> x
  | None -> default

let get = fn o -> case o of 
  | Some x -> x
  | None -> failwith "Option.get"
