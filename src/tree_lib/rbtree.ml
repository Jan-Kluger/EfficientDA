[@@@ warning "-32"]

open Order_lib.Orders

type color = Red | Black

type 'a rb_node =
  | Node of color * 'a * 'a rb_node * 'a rb_node
  | Leaf

(* RB Tree implementation *)
module RbTree (Order : Order_lib.Order_sig.ORDER) : Tree_sig.TREE with type 'a t = 'a rb_node = struct
  open Monad_lib.Monads.OptionMonad

  type 'a t = 'a rb_node

  let [@ inline] rec black_height (node : 'a t) : int =
    match node with
    | Node (c, _, l, _) -> if c = Black then (black_height l + 1) else black_height l
    | Leaf -> 1
    
    let rec black_height_check (node : 'a t) : int =
      match node with
      | Node (c, _, l, r) ->
          let left_height = black_height l in
          let right_height = black_height r in
          if left_height <> right_height then
            failwith "Inconsistent black height in the tree";
          if c = Black then left_height + 1 else left_height
      | Leaf -> 1

let insert (tree : 'a t) ~(element : 'a) : 'a t = 
    match tree with
    | Leaf -> Node (Black, element, Leaf, Leaf)
    | Node _ -> failwith "todo" 
  
  let delete (tree : 'a t) ~(element : 'a) = failwith "TODO"

  let search (tree : 'a t) ~(element : 'a) = failwith "TODO"

  let successor (tree : 'a t) ~(element : 'a) = failwith "TODO"

  let predecessor (tree : 'a t) ~(element : 'a) = failwith "TODO"

  let min _ = failwith "TODO"

  let max _ = failwith "TODO"
end

module Int_RBTree = RbTree(IntOrder)

let splice (arr : int array) (idx : int) : int array =
  (Array.sub arr (idx + 1) (Array.length arr - idx - 1)) |> Array.append (Array.sub arr 0 idx)

let testarr = [|0;1;2;3;4;5;6;7;8;9|]

let tres = splice testarr 2