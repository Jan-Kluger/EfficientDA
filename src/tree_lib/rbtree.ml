type color = Red | Black

type 'a rb_node =
  | Node of color * 'a rb_node * 'a rb_node
  | Leaf

module RbTree : Tree_sig.TREE with type 'a t = 'a rb_node = struct
  type 'a t = 'a rb_node
  
  let insert _ _ = 
    failwith "todo"
  
  let delete _ _ = failwith "TODO"

  let search _ _ = failwith "TODO"

  let successor _ _ = failwith "TODO"

  let predecessor _ _ = failwith "TODO"

  let min _ = failwith "TODO"

  let max _ = failwith "TODO"
end