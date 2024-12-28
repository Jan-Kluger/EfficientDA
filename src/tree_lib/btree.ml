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

  val empty : ?k:int -> unit -> t
(* O log N *)
  val insert : t -> element:key -> t

(* O log N *)
  val delete : t -> element:key -> t
(* O log N *)
  val search : t -> element:key -> key option
(* O 1 *)
  val successor : t -> element:key -> key option
(* O log N *)
  val predecessor : t -> element:key -> key option
(* O log N *)
  val min : t -> key option
(* O log N *)
  val max : t -> key option
(* O N *)
  val print_tree : (key -> string) -> t -> unit
end


(* 
#############################################
#                                           #
#         tree implementation               #
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
  let [@inline] binary_search ?(none_case = false) (arr : key array) (k : key) : int option =
    let rec bs_helper low high none_case =
      if low >= high then if none_case then None else Some low
      else
        let mid = (low + high) / 2 in
        match Order.compare k arr.(mid) with
        | Equal -> Some mid
        | Lesser -> bs_helper low mid none_case
        | Greater -> bs_helper (mid + 1) high none_case
    in
    bs_helper 0 (Array.length arr) none_case

  (* finds correct position in an array and inserts (this os to keep array sorted) *)
  (* Log N *)
  let put_in_pos (arr : key array) (el : key) : key array =
    let pos = Option.get (binary_search arr el) in
      Array.append (Array.sub arr 0 pos) [|el|]
      |> Array.append (Array.sub arr pos (Array.length arr - pos))

  (* Get empty tree, by default k is set to 2, can be decided by user themselves of course *)
  let empty ?(k = 2) (): t =
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
      let i = Option.get (binary_search keys x) in
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

  (* find node in tree and keep track of path to get there *)
  let search_node_and_path (tree : t) ~(element : key) : (key bp_node option * key bp_node list) =
  let rec snap_helper (node : key bp_node) (path : key bp_node list) : (key bp_node option * key bp_node list) =
    match node with
    (* found leaf *)
    | Leaf _ ->
        (Some node, node :: path)

    (* Internal node and we need to keep traversing *)
    | Internal (keys, children) ->
        let idx = Option.get (binary_search keys element) in
        snap_helper children.(idx) (node :: path)
  in
  snap_helper tree.root []

  let delete _ = failwith "todo"
    
  (* Normal seach, using monads to avoid nested matchings *)
  let search (tree : t) ~(element : key) : key option =
    let (leaf_opt, _path) = search_node_and_path tree ~element in
    leaf_opt >>= function
    | Leaf (vals, _) ->
        (* If we have a leaf, binary_search_opt returns (int option). 
           If it’s Some idx, we do Some vals.(idx). If None, everything returns None. *)
        binary_search vals element ~none_case:true >>= fun idx ->
        Some vals.(idx)
    | Internal _ ->
        failwith "search must yield leaf in a B+ tree"

  (* Go to neighbor *)
  let successor (tree : t) ~(element : key) : key option =
    let (leaf_opt, _) = search_node_and_path tree ~element in
    leaf_opt >>= function
    | Leaf (vals, neighbor_opt) ->
        binary_search vals element ~none_case:true >>= fun idx ->
        if idx < Array.length vals - 1 then
          Some vals.(idx + 1)
        else
          neighbor_opt >>= fun neighbor_node ->
          begin match neighbor_node with
          | Leaf (next_vals, _) ->
              if Array.length next_vals > 0 then Some next_vals.(0) else None
          | Internal _ ->
              failwith "Neighbor must be a leaf"
          end
    | Internal _ ->
        failwith "successor must yield a leaf"
  

  (* log N predecessor function, go up until we have a sameller child, then dart right *)
  (* This is 2log N worst case, or also O log N *)
  let predecessor (tree : t) ~(element : key) : key option =
    (* find child to traverse to *)
    let rec find_child (children : key bp_node array) (child : key bp_node) i =
      if i >= Array.length children then None
      else if children.(i) == child then Some i
      else find_child children child (i + 1)
    in
    (* Dart right *)
      let rec rightmost = function
      | Leaf (vals, _) ->
          if Array.length vals = 0 then None
          else Some vals.(Array.length vals - 1)
      | Internal (_, children) ->
          rightmost children.(Array.length children - 1)
    in
    (* go up, if we find a smaller child, call dart reight, elso go up again *)
      let rec up current = function
      | [] -> None
      | parent :: rest ->
        match parent with
        | Leaf _ -> None
        | Internal (_, children) ->
            find_child children current 0 >>= fun idx ->
            if idx > 0 then rightmost children.(idx - 1) else up parent rest
    in
      let (leaf_opt, path) = search_node_and_path tree ~element in
      leaf_opt >>= function
      | Leaf (vals, _) ->
        binary_search vals element ~none_case:true >>= fun idx ->
        if idx > 0 then Some vals.(idx - 1) else up (Leaf (vals, None)) path
      | Internal _ ->
        failwith "data must be in a leaf"

  (* Helper function for min/max search, using Monads for more readability (may be an inefficient use of monadsa) *)
  let rec min_node = function
    | Leaf (vals, _) ->
        if Array.length vals = 0 then None
        else Some vals.(0)
    | Internal (_, children) ->
        min_node children.(0)

  let rec max_node = function
    | Leaf (vals, _) ->
        if Array.length vals = 0 then None
        else Some vals.(Array.length vals - 1)
    | Internal (_, children) ->
        max_node children.(Array.length children - 1)

  (* Min method *)
  let min (tree : t) : key option =
    min_node tree.root

  (* Max method *)
  let max (tree : t) : key option =
    max_node tree.root

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

let min_v = Int_BPTree.min newTree
let max_v = Int_BPTree.max newTree

let () =
  Int_BPTree.print_tree string_of_int newTree;
  print_endline (string_of_int (Option.value ~default:(-1) min_v));
  print_endline (string_of_int (Option.value ~default:(-1) (Int_BPTree.search newTree ~element:4)));
  print_endline (string_of_int (Option.value ~default:(-1) (Int_BPTree.successor newTree ~element:4)));
  print_endline (string_of_int (Option.value ~default:(-1) (Int_BPTree.predecessor newTree ~element:1)));
  print_endline (string_of_int (Option.value ~default:(-1) (Int_BPTree.search newTree ~element:14)));