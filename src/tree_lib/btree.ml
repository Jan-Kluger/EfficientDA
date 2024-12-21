[@@@ warning "-32"]

(* type for B+ tree*)
type 'a bp_node =
  | Internal of 'a array * 'a bp_node array              (* Keys and child pointers as arrays *)
  | Leaf of ('a * 'a option) array * 'a bp_node option   (* Data as array and next leaf *)

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
  type 'a t

  val empty : ?k:int -> 'a t

  val insert : 'a t -> 'a -> 'a t

  val delete : 'a t -> 'a -> 'a t

  val search : 'a t -> 'a -> 'a

  val successor : 'a t -> 'a -> 'a

  val predecessor : 'a t -> 'a -> 'a

  val min : 'a t -> 'a

  val max : 'a t -> 'a
end

(* Implementation of B+ tree *)
module BPTree (Order : ORDER) : TREE = struct
  type 'a t = 'a bp_tree

  (* Method for inserting at correct position in a list, let compiler inline for greater performance *)
  let [@inline] [@tail_mod_cons] put_in_pos (arr : 'a array) (el : 'a) : 'a array  =
    let len = Array.length arr in
    let rec binary_search low high =
      if low >= high then low
      else
        let mid = (low + high) / 2 in
        if arr.(mid) < el then binary_search (mid + 1) high
        else binary_search low mid
    in
    let pos = binary_search 0 len in
    Array.append (Array.sub arr 0 pos) [|el|] |> Array.append( Array.sub arr pos (len - pos))

  (* Method to get an empty tree *)
  let empty ?(k = 2) : 'a bp_tree =
    { root = Leaf ([||], None); k = k }
  
  let insert (tree : 'a t) (element : 'a) : 'a t = 
    failwith "todo"

  let delete (tree : 'a t) (element : 'a) = failwith "TODO"

  let search (tree : 'a t) (element : 'a) = failwith "TODO"

  let successor (tree : 'a t) (element : 'a) = failwith "TODO"

  let predecessor (tree : 'a t) (element : 'a) = failwith "TODO"

  let min (tree : 'a t) = failwith "TODO"

  let max (tree : 'a t) = failwith "TODO"
  end