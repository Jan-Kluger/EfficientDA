open Graph

module  MinCostFlow (G : Graph_sig with type 'a t = 'a node ref list) : sig

  (* Method to compute the max flow *)
  val compute_min_cost_flow : 'a node ref -> 'a node ref -> unit
end = struct

  (* Method to compute max flow on the graph *)
  let rec compute_min_cost_flow (src : 'a node ref) (sink : 'a node ref) : unit =
    failwith "todo"
end