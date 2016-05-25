open Core.Std

type node = {
  id:               int;
  def:              Temp.TempSet.t;
  use:              Temp.TempSet.t;
  is_move:          bool;
  mutable succ:     node list;
  mutable pred:     node list;
  mutable live_in:  Temp.TempSet.t;
  mutable live_out: Temp.TempSet.t;
}

type flowgraph = node list

let singleton = Temp.TempSet.empty 

let node2string node = 
  let set2string set = 
    String.concat ~sep:"," (List.map (Temp.TempSet.elements set) 
                           ~f:(fun elt -> Temp.temp_to_string elt))
  in
  let succid = List.map node.succ ~f:(fun node -> node.id) in 
  let predid = List.map node.pred ~f:(fun node -> node.id) in
