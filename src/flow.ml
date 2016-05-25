open Core.Std
open Assem

(* https://www.cs.cmu.edu/~fp/courses/15411-f09/schedule.html *)

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

let empty = Temp.TempSet.empty 

let singleton = Temp.TempSet.singleton

let node_to_string node = 
  let set_to_string set = 
    String.concat ~sep:"," (List.map (Temp.TempSet.elements set) 
                           ~f:(fun elt -> Temp.temp_to_string elt))
  in
  let succid = List.map node.succ ~f:(fun node -> node.id) in 
  let predid = List.map node.pred ~f:(fun node -> node.id) in
  Printf.sprintf "{\tid = (%d);\npred = (%s);\nsucc = (%s);\ndef = (%s);\nuse = (%s);\n in = (%s);\nout = (%s);})}"
    node.id 
    (String.concat ~sep:", " (List.map string_of_int predid))
    (String.concat ~sep:", " (List.map string_of_int succid))
    (set_to_string node.def)
    (set_to_string node.use)
    (set_to_string node.live_in)
    (set_to_string node.live_out)


let instr_to_graph instrs : flowgraph = 
  let live_in, live_out = empty, empty in
  let rec conv idx instrs nodes map id : node list * (int LabelMap.t) = 
    match instrs with
    | [] -> List.rev nodes, map 
    | OPER (assm, dst, src, jump) :: rest -> 
        { id = id; 
          def = singleton dst;
          use = singleton src;
          is_move = true;
          succ = [];
          pred = [];
          live_in = live_in;
          live_out = live_out; }

