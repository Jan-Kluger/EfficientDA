module type TREE = sig
  type key
  type t

  val insert : t -> element:key -> t
  val delete : t -> element:key -> t
  val search : t -> element:key -> key option
  val successor : t -> element:key -> key option
  val predecessor : t -> element:key -> key option
  val min : t -> key option
  val max : t -> key option
end
