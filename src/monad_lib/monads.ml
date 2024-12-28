(* Monad for Options *)
module OptionMonad = struct
  let return x = Some x

  let ( >>= ) opt f =
    match opt with
    | None -> None
    | Some x -> f x
end