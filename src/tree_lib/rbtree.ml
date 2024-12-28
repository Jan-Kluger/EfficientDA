[@@@ warning "-32"]
open Order_lib.Orders

(* RB Tree implementation *)
module RbTree (Order : Order_lib.Order_sig.ORDER) : Tree_sig.TREE with type key = Order.t = struct
  open Monad_lib.Monads.OptionMonad

  (* Color type for RB - Node *)
  type color = Red | Black

  (* Type for RB - Node *)
  type 'a rb_node =
    | Node of color * 'a rb_node * 'a * 'a rb_node
    | Leaf

  (* Type for key and tree, required by signature *)
  type key = Order.t
  type t = key rb_node

  (* Calculate black height, assuming correct *)
  let [@ inline] rec black_height (node : t) : int =
    match node with
    | Node (c, l,_, _) -> if c = Black then (black_height l + 1) else black_height l
    | Leaf -> 1
    
  (* Check wether or not black height is correct (more for testing) *)
  let rec black_height_check (node : t) : int =
    match node with
    | Node (c,l,_,r) ->
        let left_height = black_height_check l in
        let right_height = black_height_check r in
        if left_height <> right_height then
          failwith "Inconsistent black height in the tree";
        if c = Black then left_height + 1 else left_height
    | Leaf -> 1

  (* Method to rebalance tree (for insertion) *)
  let fix_ins (Node (c,r,v,l) as node : t) : t =
    match r, l with
    | Node (c1,Node (c3,r3,v3,l3),v1,Node (c4,r4,v4,l4)), Node (c2,Node (c5,r5,v5,l5),v2,Node (c6,r6,v6,l6)) -> failwith "todo"
    | _ -> failwith "can't fix"

  (* Simple binary tree insertion for red element *)
  let rec red_ins (tree : t) ~(element : key): t =
    match tree with
    | Leaf -> Node (Red, Leaf, element, Leaf)
    | Node (color, l, v, r) as node ->
        begin match (Order.compare v element) with
        | Equal -> node
        | Lesser ->
            let updated_left = red_ins l ~element in
            Node (color, updated_left, v, r)
        | Greater ->
            let updated_right = red_ins r ~element in
            Node (color, l, v, updated_right)
        end
  
    let [@ inline] insert (tree : t) ~(element : key) : t = 
    (* Insert node at correct position as red *)
    let new_tree = red_ins tree element in
    (* Now rebalance new trww *)
    match new_tree with
    | Leaf -> Node (Black, Leaf, element , Leaf)
    | Node _ -> failwith "todo" 
      
  let delete (tree : t) ~(element : key) = failwith "TODO"

  let search (tree : t) ~(element : key) = failwith "TODO"

  let successor (tree : t) ~(element : key) = failwith "TODO"

  let predecessor (tree : t) ~(element : key) = failwith "TODO"

  let min _ = failwith "TODO"

  let max _ = failwith "TODO"
end

module Int_RBTree = RbTree(IntOrder)

let splice (arr : int array) (idx : int) : int array =
  (Array.sub arr (idx + 1) (Array.length arr - idx - 1)) |> Array.append (Array.sub arr 0 idx)

let testarr = [|0;1;2;3;4;5;6;7;8;9|]

let tres = splice testarr 2