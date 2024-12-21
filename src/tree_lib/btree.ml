[@@@ warning "-32"]

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
module type TREE = sig
  type key

  type t

  val empty : ?k:int -> unit -> t

  val insert : t -> key -> t

  val delete : t -> key -> t

  val search : t -> key -> key

  val successor : t -> key -> key

  val predecessor : t -> key -> key

  val min : t -> key

  val max : t -> key

  (* experimental *)
  val to_graphviz : t -> string
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

  let empty ?(k = 2) (): t =
    { root = Leaf ([||], None); k = k }

  let insert (tree : t) (element : key) : t =
    let rec insert_in_node (node : key bp_node) : key bp_node =
      (* Find leaf node where to insert *)
      match node with

      (* found leaf to insert at *)
      | Leaf (values, neighbor) ->
          if Array.length values >= tree.k*2 then
          (* split case *)
            failwith "TODO: Leaf splitting"
          else
          (* normal insert case *)
            let new_vals = put_in_pos values element in
            Leaf (new_vals, neighbor)

      (* continue sifitng through internal nodes if leaf not yet found *)
      | Internal (keys, children) ->
          let [@inline] idx = binary_search keys element in
          children.(idx) <- insert_in_node children.(idx);
          Internal (keys, children)
    in
    let new_root = insert_in_node tree.root in
    { tree with root = new_root }

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

  (* AI Generated to_graphviz method for debugging and visualization *)
  let to_graphviz (tree : t) : string =
    (* We'll perform a BFS or DFS, assigning each node a unique ID and
        generating the appropriate DOT lines for:
          - Node labels
          - Edges to children
          - (Optionally) edges between leaf nodes
    *)

    let buffer = Buffer.create 1024 in
    let () = Buffer.add_string buffer "digraph BPTree {\n  node [shape=record];\n\n" in

    (* We'll keep a global counter to give each node an integer ID. *)
    let counter = ref 0 in

    (* Use a Hashtbl to avoid re‐printing the same node multiple times. *)
    let visited = Hashtbl.create 16 in

    (* A small helper to get or assign an ID for a node. *)
    let node_id (node : key bp_node) : int =
      match Hashtbl.find_opt visited node with
      | Some id -> id
      | None ->
          let id = !counter in
          incr counter;
          Hashtbl.add visited node id;
          id
    in

    (* Turn an array of keys into a string joined by "|" for the label. *)
    let keys_to_record (arr : key array) : string =
      if Array.length arr = 0 then ""
      else
        (* For your real code, you might need a function to string_of_key.
            We'll do a naive approach, assuming 'key is int or something easily printed. *)
        String.concat "|" (Array.to_list (Array.map (fun x -> string_of_int (Obj.magic x)) arr))
        (* (Obj.magic usage is a hack if key is not necessarily int—replace with your real
            conversion, e.g. a “show_key” function or something similar.) *)
    in

    (* BFS or DFS approach:
        We'll define a queue, start with [tree.root], enqueue it, then process until empty. *)
    let queue = Queue.create () in
    Queue.push tree.root queue;
    ignore (node_id tree.root);  (* Force assigning an ID to the root. *)

    while not (Queue.is_empty queue) do
      let node = Queue.pop queue in
      let id = node_id node in

      match node with
      | Leaf (values, next_opt) ->
          (* Print a record label of just the keys: e.g. "10|20|30". *)
          let label = keys_to_record values in
          Buffer.add_string buffer (Printf.sprintf "  node%d [label=\"%s\"];\n" id label);

          (* (Optional) If you want to link leaf i to leaf i+1 (the "next" pointer),
              you can add an edge here: *)
          begin match next_opt with
          | Some next_leaf ->
              let nxt_id = node_id next_leaf in
              Buffer.add_string buffer (Printf.sprintf "  node%d -> node%d [color=\"blue\", style=\"dashed\"];\n" id nxt_id);
              (* also enqueue next_leaf if not visited *)
              if not (Hashtbl.mem visited next_leaf) then Queue.push next_leaf queue
          | None -> ()
          end

      | Internal (keys, children) ->
          (* Construct a record label with ports: e.g. "<c0>| key0 |<c1>| key1 |<c2>". *)
          let nkeys = Array.length keys in
          (* We'll create (nkeys+1) child ports and interleave them with the keys. *)
          let parts = ref [] in
          for i = 0 to nkeys - 1 do
            parts := !parts @ [ Printf.sprintf "<c%d> " i; string_of_int (Obj.magic keys.(i)) ];
          done;
          (* Add final port for child nkeys. *)
          parts := !parts @ [ Printf.sprintf "<c%d>" nkeys ];
          let label = String.concat "|" !parts in

          (* Print the node with these record fields. *)
          Buffer.add_string buffer (Printf.sprintf "  node%d [label=\"%s\"];\n" id label);

          (* Then for each child, produce an edge from nodeX:<c_i> -> nodeChild. *)
          Array.iteri (fun i child ->
            let child_id = node_id child in
            Buffer.add_string buffer (Printf.sprintf "  node%d:c%d -> node%d;\n" id i child_id);
            if not (Hashtbl.mem visited child) then Queue.push child queue
          ) children
    done;

    Buffer.add_string buffer "}\n";
    Buffer.contents buffer
  
end


(* BPTree with ints *)
module Int_BPTree = BPTree(IntOrder)

(* Make new BP tree *)
let testTree = Int_BPTree.empty ()
let newTree = Int_BPTree.insert testTree 1
let newTree = Int_BPTree.insert newTree 2
let newTree = Int_BPTree.insert newTree 3

(* Test Graphviz function *)
let testVis = Int_BPTree.to_graphviz newTree

(* Print result *)
let () =
  print_endline testVis