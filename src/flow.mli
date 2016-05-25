type flowgraph = {
  control:    Graph.graph;
  define:     Temp.temp list Graph.Table.t;
  usage:      Temp.temp list Graph.Table.t;
  is_move:    bool Graph.Table.t

}
