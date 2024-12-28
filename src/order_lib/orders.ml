open Order_sig

(* Int Order module for example *)
module IntOrder : ORDER with type t = int = struct
  type t = int
  let compare x y =
    if x = y then Equal else if x > y then Greater else Lesser
end