type order = 
| Greater 
| Lesser 
| Equal

(* Polymprphic Order module *)
module type ORDER = sig
  type t

  val compare : t -> t -> order
end