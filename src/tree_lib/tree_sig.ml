module type TREE = sig
  type 'a t

  val insert : 'a t -> element:'a -> 'a t
  val delete : 'a t -> element:'a -> 'a t
  val search : 'a t -> element:'a -> 'a option
  val successor : 'a t -> element:'a -> 'a option
  val predecessor : 'a t -> element:'a -> 'a option
  val min : 'a t -> 'a
  val max : 'a t -> 'a
end
