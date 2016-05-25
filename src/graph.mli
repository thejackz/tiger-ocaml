type graph

type node

val nodes: graph -> node list 

val successor: node -> node list

val predecessor: node -> node list 

val adjacent: node -> node list 

val equal: node -> node -> bool

val new_graph: unit -> graph 

val new_node: graph -> node 

val make_edge: node -> node -> unit 

val remove_edge: node -> node -> unit 

module Table : Map.S with type key = node 

