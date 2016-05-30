open Core.Std
open Assem

(* https://www.cs.cmu.edu/~fp/courses/15411-f09/schedule.html *)

type unique = unit ref

type node = {
  id:                 unique;
  define:             Temp.TempSet.t;
  usage:              Temp.TempSet.t;
  mutable live_in:    Temp.TempSet.t;
  mutable live_out:   Temp.TempSet.t;
  mutable succ:       node list;
  mutable prec:       node list;
  is_move:            bool;
}

type cfg = node list


let gen_nodes_n_labmap instrs = 
  let module TempSet = Temp.TempSet in
  let module LabelMap = Temp.LabelMap in
  let last_index = (List.length instrs) - 1 in
  let nodes, label_map, _ = List.fold_right instrs 
    ~init:([], last_index, LabelMap.empty)
    ~f:(fun instr (nodes, index, map) ->
    match instr with
    | LABEL (_, label) ->
        let new_node = {
          id = ref ();
          define = TempSet.empty;
          usage = TempSet.empty;
          live_in = TempSet.empty;
          live_out = TempSet.empty;
          succ = [];
          prec = [];
          is_move = false;
        } in 
        (new_node :: nodes, index - 1, LabelMap.add label index map)
    | MOVE (assem, dst, src) ->
        let new_node = {
          id = ref ();
          define = TempSet.singleton dst;
          usage = TempSet.singleton src;
          live_in = TempSet.empty;
          live_out = TempSet.empty;
          succ = [];
          prec = [];
          is_move = true;
        } in 
        (new_node :: nodes, index - 1, map)
    | OPER (assem, dst_lst, src_lst, next) -> 
        let new_node = {
          id = ref ();
          define = TempSet.of_list dst_lst;
          usage = TempSet.of_list src_lst;
          live_in = TempSet.empty;
          live_out = TempSet.empty;
          succ = [];
          prec = [];
          is_move = false;
        } in 
        (new_node :: nodes, index - 1, map))
  in (nodes, label_map)

let uncover_liveness (instrs : Assem.instr list) = 
  let nodes, label_map = gen_nodes_n_labmap instrs in





        



  
