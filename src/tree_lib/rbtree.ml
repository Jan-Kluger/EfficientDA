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
  let fix_ins (Node (c,r,v,l) as _node : t) (z_pos : int) : t =
    begin match r, l with
    | Node (c1,Node (c3,r3,v3,l3),v1,Node (c4,r4,v4,l4)), Node (c2,Node (c5,r5,v5,l5),v2,Node (c6,r6,v6,l6)) -> 
      begin match z_pos with
        | 0 -> failwith "todo"
        | 1 -> failwith "todo"
        | 2 -> failwith "todo"
        | 3 -> failwith "todo"
        | _ -> failwith "pos must be in range [0-3]"
      end
    | _ -> failwith "can't fix"
    end

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
    let new_tree = red_ins tree ~element:element in
    (* Now rebalance new trww *)
    match new_tree with
    | Leaf -> Node (Black, Leaf, element , Leaf)
    | Node _ -> failwith "todo" 
      
  let delete (tree : t) ~(element : key) = failwith "TODO"

  let search (tree : t) ~(element : key) = failwith "TODO"

  let successor (tree : t) ~(element : key) = failwith "TODO"

  let predecessor (tree : t) ~(element : key) = failwith "TODO"

  let rec min (tree : t) = 
    match tree with
    | Node (_, _, v, Leaf) -> Some v
    | Node (_, _, _, r) -> min r
    | Leaf -> None

  let rec max (tree : t) = 
    match tree with
    | Node (_, Leaf, v, _) -> Some v
    | Node (_, l, _, _) -> max l
    | Leaf -> None
end

module Int_RBTree = RbTree(IntOrder)