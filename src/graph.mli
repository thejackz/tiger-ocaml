type graph

type node

val get_nodes: graph -> node list 

val successor: graph -> node -> node list

val predecessor: graph -> node -> node list 

val adjacent: graph -> node -> node list 

val equal: node -> node -> bool

val new_graph: unit -> graph 

val new_node: graph -> node 

val make_edge: node -> node -> unit 

val remove_edge: node -> node -> unit 

module Table : Map.S with type key = node 

