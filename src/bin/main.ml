open Order_lib.Orders
open Graph_lib.Graph

module StringGraph = MapGraph(StringOrder)
let  make_neutral_edge (cap : int) (dest : 'a node) : 'a edge =
  (Edge (0, cap, 0, 0, dest))

(* Make nodes *)
let test_nodes = [Node (0, "src") ; Node (0, "Station 1") ; Node (0, "Station 2") ; Node (0, "Station 3") ; Node (0, "sink")]

(* Make graph with nodes *)
let graph = StringGraph.build_graph StringGraph.empty test_nodes

(* add edges to graph *)
let graph = List.fold_left (fun acc (cap, dest) -> StringGraph.insert_edge acc (Node (0, "src")) (make_neutral_edge cap dest) ) graph [(1, Node (0, "Station 1")); (1, Node (0, "Station 2")); (1, Node (0, "Station 3"))]

(* add adges between stations *)
let graph = StringGraph. insert_edge graph (Node (0, "Station 1")) (make_neutral_edge 1 (Node (0, "Station 2")))
let graph = StringGraph. insert_edge graph (Node (0, "Station 2")) (make_neutral_edge 1 (Node (0, "Station 3")))

(* add edges to sink *)
let graph = StringGraph. insert_edge graph (Node (0, "Station 1")) (make_neutral_edge 1 (Node (0, "sink")))
let graph = StringGraph. insert_edge graph (Node (0, "Station 2")) (make_neutral_edge 2 (Node (0, "sink")))
let graph = StringGraph. insert_edge graph (Node (0, "Station 3")) (make_neutral_edge 3 (Node (0, "sink")))

(****************************)
(*  Graph should look like: *)
(*            s1            *)
(*         /  |  \          *)
(*        1   1   3         *)
(*       /    |    \        *)
(*      s -1- s2 -2-t       *)
(*       \    |    /        *)
(*        1   1   1         *)
(*         \  |  /          *)
(*            s3            *)
(****************************)

(* Build flow *)
let graph_g = StringGraph.maxflow graph ~src:(Node (0, "src")) ~sink:(Node (0, "sink"))

(* Should be 3 *)
let flow_g = StringGraph.get_flow graph_g ~src:(Node (0, "src"))

let () =
  print_endline ("flow in graph: " ^ (string_of_int flow_g))
