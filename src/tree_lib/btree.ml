[@@@ warning "-32"]

(* type for B+ tree*)
type 'a bp_node =
  | Internal of 'a array * 'a bp_node array
  | Leaf of ('a * 'a option) array * 'a bp_node option

type 'a bp_tree = {
  root : 'a bp_node;
  k : int;
}

(* Type for orders *)
type order = 
  | Greater 
  | Lesser 
  | Equal

(* Polymprphic Order module *)
module type ORDER = sig
  type t
  val compare : t -> t -> order
end

(* Int Order module for example *)
module IntOrder : ORDER with type t = int = struct
  type t = int
  let compare x y =
    if x = y then Equal else if x > y then Greater else Lesser
end
  
(* Public methods for B+ tree *)
module type TREE = sig
  type key

  type t

  val empty : ?k:int -> t

  val insert : t -> key -> t

  val delete : t -> key -> t

  val search : t -> key -> key

  val successor : t -> key -> key

  val predecessor : t -> key -> key

  val min : t -> key

  val max : t -> key
end

(* Implementation of B+ tree *)
module BPTree (Order : ORDER) : TREE with type key = Order.t = struct

  type key = Order.t

  type t = key bp_tree

  (* Bin search to get rid of redundancy, ppx enabled inline to improve performance *)
  let [@inline] binary_search (arr : key array) (k : key) : int =
    let rec aux low high =
      if low >= high then low
      else
        let mid = (low + high) / 2 in
        match Order.compare k arr.(mid) with
        | Lesser | Equal -> aux low mid
        | Greater -> aux (mid + 1) high
    in
    aux 0 (Array.length arr)

  (* finds correct position in an array and inserts (this os to keep array sorted) *)
  let put_in_pos (arr : key array) (el : key) : key array =
    let pos = binary_search arr el in
    Array.append (Array.sub arr 0 pos) [|el|]
    |> Array.append (Array.sub arr pos (Array.length arr - pos))

  (* find leaf at which position we should insert element *)
  let rec find_leaf (node : key bp_node) (el : key) : key bp_node =
    match node with
    | Leaf _ -> node
    | Internal (keys, children) ->
        let child_index = binary_search keys el in
        find_leaf children.(child_index) el

  let empty ?(k = 2) : t =
    { root = Leaf ([||], None); k = k }

  let insert (tree : t) (element : key) : t =
    let root = tree.root in
    let _leaf_node = find_leaf root element in
    failwith "TODO: insert logic"

  let delete (tree : t) (element : key) : t =
    failwith "TODO"

  let search (tree : t) (element : key) : key =
    failwith "TODO"

  let successor (tree : t) (element : key) : key =
    failwith "TODO"

  let predecessor (tree : t) (element : key) : key =
    failwith "TODO"

  let min (tree : t) : key =
    failwith "TODO"

  let max (tree : t) : key =
    failwith "TODO"

end
