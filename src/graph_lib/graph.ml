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
  val insert_edge_exm : t -> O.t node -> O.t edge -> t

  (* function to see if node is contained within graph *)
  val contains_node : t -> O.t node -> bool

  (* Build graph from nodes, this is essentially just a repeated insert node, however it may make work easier *)
  val build_graph : t -> O.t node list -> t

  (* Get the current flow running in the graph *)
  val get_flow : t -> src:O.t node -> int

  (* Min cut max flow, takes a src and a sink and returns the saturated graph *)
  val maxFlow_exm : t -> src:O.t node -> sink:O.t node -> t [@depriciated]

  val maxflow : t -> src:O.t node -> sink:O.t node -> t


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

  let get_flow (graph : t) ~(src : O.t node) : int =
    match NodeMap.find_opt src graph with
    | Some e -> List.fold_left (fun acc (Edge (_, _, flow, _, _)) -> acc + flow) 0 e
    | None -> raise (Search_error "src not in graph!")

  let maxFlow_exm (graph : t) ~(src : O.t node) ~(sink : O.t node) : t [@depriciated] =

    (* Sub function to find an augmenting path *)
    (* Returns (O.t node list * int) which is the path and bottleneck *)
    let rec find_aug_path ?(path : O.t node list = []) (node : O.t node) (set : int NodeMap.t) ~(bottleneck : int option) : (O.t node list* int) option =

      (* Get the edges of the current node, removing nodes weve already been at and already saturated flows*)
      let edges =
        match (NodeMap.find_opt node graph) with
        | Some e -> List.filter (fun (Edge (_, cap, flow, _, dest)) -> not (NodeMap.mem dest set) && (cap <> flow)) e
        | None -> raise (Search_error "Node references non existant neighbor")
      in

      (* For each neigboring node, keep loking, if we reach sink return some path, if not keep looking, if there are no more edges tovisit, return none *)
      List.fold_left (fun _ (Edge (_, cap, flow, _, dest))  -> 
        let new_bottleneck = 
          match bottleneck with
          (* If we do not have a bottleneck yet, then we have one now, it is the difference between cap and flow *)
          | None -> (cap - flow)
          (* If we already have a bottleneck, check if our new one is lowe, if so, decrease the bottlneck *)
          | Some b -> min b (cap - flow)
        in

        (* If we have reached our goal, return the path to get there and out bottleneck along the way *)
        if dest = sink then 
          Some (sink :: node :: path, new_bottleneck) 
        else 
        (* If not update bottleneck and keep searching *)
          find_aug_path ~path:(node :: path) dest (set |> NodeMap.add node 1) ~bottleneck:(Some new_bottleneck)
        ) None edges
    in

    (* Find augmenting path by calling on set only containing src *)
    let aug_path = find_aug_path src (NodeMap.empty |> NodeMap.add src 1) ~bottleneck:None in

    (* Check to see if we find any augmenting paths *)
    match aug_path with
    (* If we can find an aumenting path, increase the flow by the bottlneck amount *)
    | Some (path, bottleneck) -> 
      let rec increase_flow (path : O.t node list) (amount : int) (graph_acc : t) : t = 
        match path with
        | f :: t :: xs -> begin
          (* (min_cap, max_cap, old_flow, cost, dest) *)
          let old_edges = NodeMap.find f graph in
          (* Map the edge heading to the next node with increased flow *)
          let new_edges = List.map (fun (Edge (min_cap, max_cap, old_flow, cost, dest) as edge) ->
            if dest = t then Edge (min_cap, max_cap, old_flow + amount, cost, dest) else edge) old_edges
          in

          (* update node with new edges in graph *)
          let new_graph = NodeMap.add f new_edges graph in

          (* recursive call to update next edge *)
          increase_flow (t :: xs) amount new_graph
        end
        (* If we are at the ned of tthe path we have updated all edges, and we can return the new graph *)
        | _ -> graph_acc
      in

      (* Recall to see if there any more augmenting paths *)
      increase_flow (List.rev path) bottleneck graph
    (* If we find no augmenting path, then we return the graph, this means we have saturated all we can *)
    | None -> graph

  let maxflow (graph : t) ~(src : O.t node) ~(sink : O.t node) : t  =
    (* Helper to compute residual capacity of an edge.
       For a forward edge (with nonnegative flow) residual capacity is (max_cap - flow).
       For a reverse edge (with negative flow) we take -flow. *)
    let residual_capacity (Edge (_, max_cap, flow, _, _)) =
      if flow >= 0 then max_cap - flow else -flow
    in
  
    (* Helper to update both forward and reverse edges when augmenting along (u,v) by amount *)
    let update_edge_and_residual graph u v amount =
      (* Update forward edge from u to v *)
      let forward_edges = 
        match NodeMap.find_opt u graph with
        | Some es -> es
        | None -> [] 
      in
      let forward_edges_updated =
        List.map (fun (Edge (min_cap, max_cap, flow, cost, dest) as edge) ->
          if dest = v then Edge (min_cap, max_cap, flow + amount, cost, dest)
          else edge
        ) forward_edges
      in
      let graph = NodeMap.add u forward_edges_updated graph in
  
      (* Update reverse edge from v to u *)
      let reverse_edges =
        match NodeMap.find_opt v graph with
        | Some es -> es
        | None -> [] 
      in
      let (found, reverse_edges_updated) =
        List.fold_left (fun (found, acc) (Edge (min_cap, max_cap, flow, cost, dest) as edge) ->
          if dest = u then
            (true, acc @ [Edge (min_cap, max_cap, flow - amount, cost, dest)])
          else
            (found, acc @ [edge])
        ) (false, []) reverse_edges
      in
      let reverse_edges_final =
        if not found then
          (* Insert a new reverse edge.
             We set min_cap = 0 and max_cap = amount so that the residual capacity (max_cap - (-amount))
             becomes exactly the amount pushed. *)
          reverse_edges_updated @ [Edge (0, amount, -amount, 0, u)]
        else reverse_edges_updated
      in
      NodeMap.add v reverse_edges_final graph
    in
  
    (* The recursive max-flow routine. *)
    let rec max_flow graph =
      (* Recursive function to find an augmenting path in the current graph.
         Note that we pass the current graph as a parameter so that we always use the
         latest residual capacities. *)
      let rec find_aug_path graph ?(path = []) (node : O.t node) (visited : int NodeMap.t) ~(bottleneck : int option)
        : (O.t node list * int) option =
        let edges =
          match NodeMap.find_opt node graph with
          | Some es ->
              List.filter (fun edge ->
                (* Only consider edges with positive residual capacity and whose destination hasn't been visited *)
                (residual_capacity edge > 0) &&
                (let Edge (_, _, _, _, dest) = edge in not (NodeMap.mem dest visited))
              ) es
          | None -> raise (Search_error "Node references non existent neighbor")
        in

        List.fold_left (fun acc edge ->
          match acc with
          | Some _ -> acc  (* If we've already found a valid path, stop exploring further *)
          | None ->
              let current_res = residual_capacity edge in
              let new_bottleneck =
                match bottleneck with
                | None -> current_res
                | Some b -> min b current_res
              in
              let Edge (_, _, _, _, dest) = edge in
              if dest = sink then 
                Some (sink :: node :: path, new_bottleneck)
              else 
                find_aug_path graph ~path:(node :: path) dest (visited |> NodeMap.add node 1) ~bottleneck:(Some new_bottleneck)
        ) None edges
      in
  
      match find_aug_path graph src (NodeMap.empty |> NodeMap.add src 1) ~bottleneck:None with
      | Some (path, bottleneck) ->
          let rec increase_flow path amount graph =
            match path with
            | u :: v :: rest ->
                let graph = update_edge_and_residual graph u v amount in
                increase_flow (v :: rest) amount graph
            | _ -> graph
          in
          let new_graph = increase_flow (List.rev path) bottleneck graph in
          max_flow new_graph  (* Continue until no augmenting path is found *)
      | None -> graph
    in
  
    max_flow graph
  
    

  let minCostFlow (_graph : t) ~(src : O.t node) ~(sink : O.t node) : t =
    let _ = (src,sink) in
    failwith "todo"
end