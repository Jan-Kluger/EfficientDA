type color = Red | Black

type 'a rb_node =
  | Node of color * 'a rb_node * 'a rb_node
  | Leaf

module RB_TREE : sig
  type 'a t
  val insert : 'a t -> 'a -> 'a t
end = struct
  type 'a t = int

  let insert _ _ = 
    failwith "todo"
end