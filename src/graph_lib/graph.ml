[@@@ warning "-32"]

(* Define exception foredge insertion *)
exception Insert_error of string
exception Search_error of string


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
  val minCut_maxFlow_exm : t -> src:O.t node -> sink:O.t node -> t

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

  let minCut_maxFlow_exm (graph : t) ~(src : O.t node) ~(sink : O.t node) : t =

    (* Sub function to find an augmenting path *)
    (* Returns (O.t node list * int) which is the path and bottleneck *)
    let rec find_aug_path ?(path : O.t node list = []) (node : O.t node) (set : int NodeMap.t) ~(bottleneck : int option) : (O.t node list* int) option =

      (* Get the edges of the current node, removing nodes weve already been at and already saturated flows*)
      let edges =
        match (NodeMap.find_opt node graph) with
        | Some e -> List.filter (fun (Edge (_, cap, flow, _, dest)) -> not (NodeMap.mem dest set) || (cap <> flow)) e
        | None -> raise (Search_error "Node references non existant neighbor")
      in

      (* For each neigboring node, keep loking, if we reach sink return some path, if not keep looking, if there are no more edges tovisit, return none *)
      List.fold_left (fun _ (Edge (_, cap, flow, _, dest))  -> 
        let new_bottleneck = 
          match bottleneck with
        | None -> (cap - flow)
        | Some b -> min b (cap - flow)
        in
        if dest = sink then Some (sink :: node :: path, new_bottleneck) else find_aug_path ~path:(node :: path) dest (set |> NodeMap.add node 1) ~bottleneck:(Some new_bottleneck)
        ) None edges
    in

    (* Find augmenting path by calling on set only containing src *)
    let path = Option.value (find_aug_path src (NodeMap.empty |> NodeMap.add src 1) ~bottleneck:None) ~default:([],0) in
    failwith "todo"

  let minCostFlow (graph : t) ~(src : O.t node) ~(sink : O.t node) : t =
    failwith "todo"
end