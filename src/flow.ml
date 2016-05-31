open Core.Std
open Assem

(* https://www.cs.cmu.edu/~fp/courses/15411-f09/schedule.html *)

type unique = unit ref


type node = {
  id:                 int;
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
    ~init:([], LabelMap.empty, last_index)
    ~f:(fun instr (nodes, map, index) ->
    match instr with
    | LABEL (_, label) ->
        let new_node = {
          id = index;
          define = TempSet.empty;
          usage = TempSet.empty;
          live_in = TempSet.empty;
          live_out = TempSet.empty;
          succ = [];
          prec = [];
          is_move = false;
        } in 
        (new_node :: nodes, LabelMap.add label index map, index - 1)
    | MOVE (assem, dst, src) ->
        let new_node = {
          id = index;
          define = TempSet.singleton dst;
          usage = TempSet.singleton src;
          live_in = TempSet.empty;
          live_out = TempSet.empty;
          succ = [];
          prec = [];
          is_move = true;
        } in 
        (new_node :: nodes, map, index - 1)
    | OPER (assem, dst_lst, src_lst, next) -> 
        let new_node = {
          id = index;
          define = TempSet.of_list dst_lst;
          usage = TempSet.of_list src_lst;
          live_in = TempSet.empty;
          live_out = TempSet.empty;
          succ = [];
          prec = [];
          is_move = false;
        } in 
        (new_node :: nodes, map, index - 1))
  in (nodes, label_map)

let get_predecessor nodes = 
  List.fold_left nodes 
  ~init:[]
  ~f:(fun prev node ->
    match node with
    | OPER (assem, dst_lst, src_lst, Some label) ->

  
  )

let uncover_liveness (instrs : Assem.instr list) = 
  (* Convert each instruction to node, and record the position of each label *)
  let nodes, label_map = gen_nodes_n_labmap instrs in
  



  
  






        



  
