[@@@ warning "-32"]

(* Define exception foredge insertion *)
exception Insert_error of string

open Order_lib.Order_sig

(* Nodes and edges are made to accomodate Max flow/Min cut and Min cost flow problems *)
type 'a node = Node of 
int                 (* Demand of node *)
* 'a                (* Value of node *)

and 'a edge = Edge of 
int                 (* Min capacity *)
* int               (* Max capacity *)
* int               (* Current flow *)
* int               (* Cost *)
* 'a node           (* Destination of edge *)

(* A functor to produce a NodeMap. The map is keyed by nodes whose ordering is defined by comparing the node’s value. *)
module NodeMap (O : ORDER) = Map.Make(struct
  type t = O.t node  (* our node is: Node of int * O.t *)
  let compare (Node (_, v1)) (Node (_, v2)) =
    match O.compare v1 v2 with
    | Greater -> 1
    | Lesser  -> -1
    | Equal   -> 0
end)

(* Now, we define the GRAPH functor. We assume that the node’s value type is O.t. *)
(* The graph type is defined as a map from nodes to their outgoing edge list. *)

module MapGraph (O : ORDER) : sig

  (* Type of the graph, this is used for self referencation *)
  type t

  (* Returns an empty graph *)
  (* Int is estimated amount of nodes in graph, we will use a hash table *)
  val empty : t

  (* Insert node into graph *)
  val insert_node : t -> O.t node -> t

  (* Insert edge to node *)
  val insert_edge : t -> O.t node -> O.t edge -> t

  (* Insert edge to node *)
  (* NOTE: can raise an exception if node doesnt exist *)
  val insert_edge : t -> O.t node -> O.t edge -> t

  (* function to see if node is contained within graph *)
  val contains_node : t -> O.t node -> bool

  (* Build graph from nodes, this is essentially just a repeated insert node, however it may make work easier *)
  val build_graph : t -> O.t node list -> t

  (* Min cut max flow, takes a src and a sink and returns the saturated graph *)
  val minCut_maxFlow : t -> src:O.t node -> sink:O.t node -> t

  (* Computes Min-cost flow, returns mincost graph, min cost can be extrapolated seperatley from the graph *)
  val minCostFlow : t -> src:O.t node -> sink:O.t node -> t


end = struct
(* Implementation of graph module *)


  module NodeMap = NodeMap(O)

  (* Our graph maps a node (of type O.t node) to its outgoing edge list *)
  type t = O.t edge list NodeMap.t

  (* We ignore the estimated size parameter since NM.empty takes no arguments. *)
  let empty = NodeMap.empty

  let insert_node (graph : t) (node : O.t node) : t =
    NodeMap.add node [] graph

  let insert_edge (graph : t) (node : O.t node) (edge : O.t edge) : t =
    (* Make sure edge to be inserted is empty (May change later) *)
    let Edge (_, _, current_flow, _, _) = edge in
    if current_flow <> 0 then raise (Insert_error "Inserted edges must not have any flow")

    else
    (* Check to see if node is in graph *)
    match NodeMap.find_opt node graph with
    (* If yes, edge to node *)
    | Some current_neightbors -> graph |> NodeMap.add node (edge :: current_neightbors)
    (* If not, return old graph *)
    | None -> graph

  let insert_edge_exm (graph : t) (node : O.t node) (edge : O.t edge) : t =
    (* Make sure edge to be inserted is empty (May change later) *)
    let Edge (_, _, current_flow, _, _) = edge in
    if current_flow <> 0 then raise (Insert_error "Inserted edges must not have any flow")

    else
    (* Check to see if node is in graph *)
    match NodeMap.find_opt node graph with
    | Some current_neightbors -> graph |> NodeMap.add node (edge :: current_neightbors)
    (* If not throw exception *)
    | None -> raise (Insert_error "Insertion node not in graph")

  let contains_node (graph : t) (node : O.t node) : bool =
    NodeMap.mem node graph

  let build_graph (graph : t) (nodes : O.t node list) : t =
    List.fold_left insert_node graph nodes

  let minCut_maxFlow (graph : t) ~(src : O.t node) ~(sink : O.t node) : t =

    let find_aug_path ?(path : O.t node list = []) (node : O.t node) (set : int NodeMap.t) : O.t node list =
      failwith "todo"
    in

    failwith "todo"

  let minCostFlow (graph : t) ~(src : O.t node) ~(sink : O.t node) : t =
    failwith "todo"
end