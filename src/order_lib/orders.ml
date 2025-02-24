open Order_sig

(* Int Order module for example *)
module IntOrder : ORDER with type t = int = struct
  type t = int
  let compare x y =
    if x = y then Equal else if x > y then Greater else Lesser
end

module StringOrder : ORDER with type t = string = struct
  type t = string

  let compare x y : order=
    match String.compare x y with
    | 0 -> Equal
    | x when (x > 0) -> Greater
    | x when (x < 0) -> Lesser
    | _ -> failwith "cant"
end