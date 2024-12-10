module type TREE = sig
  type 'a t

  val insert : 'a t -> 'a -> 'a t

  val delete : 'a t -> 'a -> 'a t

  val search : 'a t -> int -> 'a

  val successor : 'a t -> 'a -> 'a

  val predecessor : 'a t -> 'a -> 'a

  val min : 'a t -> 'a

  val max : 'a t -> 'a
end
