[@@@ warning "-32"]

(* The first top of this file is a bunch of types and other "organizational" modules which i usually would have hidden in other files
 however for this all to run in one file it is necessary*)

(* Monad for Options *)
module OptionMonad = struct
  let return x = Some x

  let ( >>= ) opt f =
    match opt with
    | None -> None
    | Some x -> f x
end

(* type for B+ tree*)
type 'a bp_node =
  | Internal of 'a array * 'a bp_node array
  | Leaf of 'a array * 'a bp_node option

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
(* Labelled element argument to make Pipeline friendly *)
module type TREE = sig
  type key
  type t

  val empty :  unit -> ?k:int -> t
  val insert : t -> element:key -> t
  val delete : t -> element:key -> t
  val search : t -> element:key -> key option
  val successor : t -> element:key -> key option
  val predecessor : t -> element:key -> key option
  val min : t -> key option
  val max : t -> key option
  val print_tree : (key -> string) -> t -> unit
end


(* 
#############################################
#                                           #
#            tree implementation            #
#                                           #
############################################# 
*)

(* Implementation of B+ tree *)
module BPTree (Order : ORDER) : TREE with type key = Order.t = struct
  open OptionMonad

  type key = Order.t

  type t = key bp_tree

  type 'a split_result =
  | NoSplit of 'a bp_node
  | Split of 'a bp_node * 'a * 'a bp_node

  (* Bin search to get rid of redundancy, ppx enabled inline to improve performance *)
  (* Log N *)
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

  (* Binary search that returns an option for use in search and successor, critical for search and seccessor, some redundant code but idc anymore *)
  let [@inline] binary_search_opt (arr : key array) (k : key) : int option =
    let rec aux low high =
      if low >= high then None  (* Key not found *)
      else
        let mid = (low + high) / 2 in
        match Order.compare k arr.(mid) with
        | Equal -> Some mid
        | Lesser -> aux low mid
        | Greater -> aux (mid + 1) high
    in
    aux 0 (Array.length arr)

  (* finds correct position in an array and inserts (this os to keep array sorted) *)
  (* Log N *)
  let put_in_pos (arr : key array) (el : key) : key array =
    let pos = binary_search arr el in
    Array.append (Array.sub arr 0 pos) [|el|]
    |> Array.append (Array.sub arr pos (Array.length arr - pos))

  (* Get empty tree, by default k is set to 2, can be decided by user themselves of course *)
  let empty () ?(k = 2): t =
    { root = Leaf ([||], None); k = k }

  (* Insert helper method *)
  let rec insert_in_node (tree : t) (node : key bp_node) (x : key) : key split_result =
  match node with
    | Leaf (vals, next_opt) ->
        let new_vals = put_in_pos vals x in
        if Array.length new_vals < 2 * tree.k then
          NoSplit (Leaf (new_vals, next_opt))
        else
          let mid = Array.length new_vals / 2 in
          let right_vals = Array.sub new_vals mid (Array.length new_vals - mid) in
          let right_leaf = Leaf (right_vals, next_opt) in
          Split (Leaf (Array.sub new_vals 0 mid, Some right_leaf), right_vals.(0), right_leaf)

  | Internal (keys, children) ->
      let i = binary_search keys x in
      begin match insert_in_node tree children.(i) x with
      | NoSplit updated_child ->
          children.(i) <- updated_child;
          NoSplit (Internal (keys, children))
      | Split (l_node, sep_key, r_node) ->
          let new_keys = put_in_pos keys sep_key in
          let len = Array.length children in

          (* Replace the old child with l_node. *)
          children.(i) <- l_node;
          
          (* Make left and right parts  *)
          let new_children =
            let l_child = Array.sub children 0 (i+1) in
            let r_child = Array.sub children (i+1) (len - (i+1)) in
            Array.concat [ l_child; [| r_node |]; r_child ]
          in
          (* save calculation of keys_length for faster bla. *)
          let keys_length = Array.length new_keys in

          (* Check if new keys call for a split *)
          if keys_length < 2 * tree.k then
            (* no split case *)
            NoSplit (Internal (new_keys, new_children))

          else
            (* split case *)
            (* seperator and middle value *)
            let mid = keys_length / 2 in
            let sep = new_keys.(mid) in

            (* Both look like long expressions, its just left and right half of left keys/values  *)
            let left_int  = Internal (Array.sub new_keys 0 mid, Array.sub new_children 0 (mid+1)) in
            let right_int = Internal (Array.sub new_keys (mid+1) (keys_length - (mid+1)), Array.sub new_children (mid+1) (Array.length new_children - (mid+1))) in
            Split (left_int, sep, right_int)
      end

  (* Inser method, Log N? *)
  let insert (tree : t) ~(element : key) : 'a bp_tree =
    match insert_in_node tree tree.root element with
    | NoSplit updated_root ->
      { tree with root = updated_root }
    | Split (l_node, sep_key, r_node) ->
      let new_children = [| l_node; r_node |] in
      (* new root value if root splits, just the seperator key *)
      { tree with root = Internal ([| sep_key |], new_children) }

  let delete (tree : t) ~(element : key) : t =
    failwith "TODO"

  let search_node (tree : t) ~(element : key) : key bp_node option =
    failwith "TODO"

  (* Normal seach, using monads to avoid nested matchings *)
  let search (tree : t) ~(element : key) : key option =
    search_node tree ~element:element >>= function
    | Leaf (value, _) -> binary_search_opt value element >>= fun idx -> Some value.(idx)
    | _ -> failwith "search must yield leaf"

  (* Go to neighbor *)
  let successor (tree : t) ~(element : key) : key option =
    search_node tree ~element:element >>= function
    | Leaf (vals, neighbor_opt) ->
        binary_search_opt vals element >>= fun idx ->
        if idx < Array.length vals - 1 then
          Some vals.(idx + 1)  (* Successor is the next value in the current array *)
        else
          neighbor_opt >>= begin function
          | Leaf (next_vals, _) ->
              if Array.length next_vals > 0 then Some next_vals.(0) else None
          | _ -> failwith "Neighbor must be a leaf"
          end
    | _ -> failwith "search_node must yield a leaf"
  
  let predecessor (tree : t) ~(element : key) : key option =
      let rec find_min_leaf node =
        match node with
        | Leaf (vals, _) -> Some node  (* Return the smallest leaf node *)
        | Internal (_, children) -> find_min_leaf children.(0)  (* Traverse to the leftmost child *)
      in
    
      search_node tree ~element:element >>= function
      | Leaf (vals, neighbor_opt) ->
          binary_search_opt vals element >>= fun idx ->
          if idx > 0 then
            (* Predecessor is the previous value in the current node *)
            Some vals.(idx - 1)
          else
            (* predecessor is in previous leaf, so we need to go through all leaves *)
            find_min_leaf tree.root >>= fun min_leaf ->
            
            if min_leaf == (Leaf (vals, neighbor_opt)) then
              (* The current leaf is the smallest leaf, no predecessor *)
              None  
            else
              (* Now we find the smallest leaf and go through all leaves, if next leaf is the leaf that our key is in, we know current leaf is predecessor leaf *)
              let rec traverse_leaves current_leaf =
                match current_leaf with
                | Leaf (current_vals, Some next_leaf) ->
                    if next_leaf = (Leaf (vals, neighbor_opt)) then
                      (* We know we are in correct leaf, return last element *)
                      Some current_vals.(Array.length current_vals - 1)
                    else
                      (* go to next leaf *)
                      traverse_leaves next_leaf
                | Leaf (_, None) -> failwith "predecessor leaf cannot be last leaf"  
                | _ -> failwith "internal nodes are never travesed"
                (* this last match case is just to keep typechecker happy *)
              in
              traverse_leaves min_leaf
      | _ -> failwith "search_node must yield a leaf"

  (* Helper function for min/max search, using Monads for more readability (may be an inefficient use of monadsa) *)
  let rec min_max_node (node : key bp_node) (to_index : 'a array -> int option) : key option =
    match node with
    | Leaf (vals, _) ->
        to_index vals >>= fun idx -> Some vals.(idx)
    | Internal (values, children) ->
        to_index values >>= fun idx -> min_max_node children.(idx) to_index

  (* Min method *)
  let min (tree : t) : key option =
    min_max_node tree.root (fun arr ->
        if Array.length arr = 0 then None else Some 0
      )

  (* Max method *)
  let max (tree : t) : key option =
    min_max_node tree.root (fun arr ->
        if Array.length arr = 0 then None else Some (Array.length arr - 1)
      )

    let print_tree (string_of_key : key -> string) (tree : t) : unit =
      let rec print_node (node : 'a bp_node) : unit =
        match node with
        | Leaf (vals, _next_opt) ->
            let contents =
              vals
              |> Array.to_list
              |> List.map string_of_key
              |> String.concat ","
            in
            Printf.printf "Leaf: [%s]\n" contents
    
        | Internal (keys, children) ->
            let contents =
              keys
              |> Array.to_list
              |> List.map string_of_key
              |> String.concat ","
            in
            Printf.printf "Internal: [%s]\n" contents;
            Array.iter print_node children
      in
      print_node tree.root

end

(* 
#############################################
#                                           #
#             testing methods               #
#                                           #
############################################# 
*)

(* BPTree with ints *)
module Int_BPTree = BPTree(IntOrder)

(* Make new BP tree *)
let newTree =
  Int_BPTree.empty ()
  |> Int_BPTree.insert ~element:1
  |> Int_BPTree.insert ~element:2
  |> Int_BPTree.insert ~element:3
  |> Int_BPTree.insert ~element:4
  |> Int_BPTree.insert ~element:5
  |> Int_BPTree.insert ~element:6
  |> Int_BPTree.insert ~element:7
  |> Int_BPTree.insert ~element:8
  |> Int_BPTree.insert ~element:9
  |> Int_BPTree.insert ~element:10
  (* |> Int_BPTree.delete ~element:4
  |> Int_BPTree.delete ~element:2 *)

let min_v = Int_BPTree.min newTree
let max_v = Int_BPTree.max newTree

let () =
  Int_BPTree.print_tree string_of_int newTree;
  print_endline (string_of_int (Option.value ~default:0 min_v))