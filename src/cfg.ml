open Core.Std
open Assem

(* https://www.cs.cmu.edu/~fp/courses/15411-f09/schedule.html *)

type unique = unit ref


type node = {
  id:                 int;
  stmt:               Assem.instr;
  define:             Temp.TempSet.t;
  usage:              Temp.TempSet.t;
  mutable live_in:    Temp.TempSet.t;
  mutable live_out:   Temp.TempSet.t;
  mutable succ:       node list;
  mutable prec:       node list;
  is_move:            bool;
}

type cfg = node list

let reverse_graph cfg = List.rev cfg


let gen_nodes_n_labmap instrs = 
  let module TempSet = Temp.TempSet in
  let module LabelMap = Temp.LabelMap in
  let last_index = (List.length instrs) - 1 in
  let nodes, label_map, _ = List.fold_right instrs 
    ~init:([], LabelMap.empty, last_index)
    ~f:(fun instr (nodes, map, index) ->
    match instr with
    | LABEL (assem, label) ->
        let new_node = {
          id = index;
          stmt = assem;
          define = TempSet.empty;
          usage = TempSet.empty;
          live_in = TempSet.empty;
          live_out = usage;
          succ = [];
          prec = [];
          is_move = false;
        } in 
        (new_node :: nodes, LabelMap.add label index map, index - 1)
    | MOVE (assem, dst, src) ->
        let new_node = {
          id = index;
          stmt = assem;
          define = TempSet.singleton dst;
          usage = TempSet.singleton src;
          live_in = TempSet.empty;
          live_out = usage;
          succ = [];
          prec = [];
          is_move = true;
        } in 
        (new_node :: nodes, map, index - 1)
    | OPER (assem, dst_lst, src_lst, next) -> 
        let new_node = {
          id = index;
          stmt = assem;
          define = TempSet.of_list dst_lst;
          usage = TempSet.of_list src_lst;
          live_in = TempSet.empty;
          live_out = usage;
          succ = [];
          prec = [];
          is_move = false;
        } in 
        (new_node :: nodes, map, index - 1))
  in (nodes, label_map)

let rec nodes_to_graph label_map nodes = 
  let rec update_node node next_node = 
    match node.stmt with
    | MOVE _ 
    | LABEL _ 
    | OPER (_, _, _, None) -> node.succ <- [next_node]
    | OPER (assem, _, _, Some labs) ->
        node.succ <- List.map labs ~f:(fun lab -> LabelMap.find label_map lab)
  in
  match nodes with
  | [] -> nodes
  | node :: (next_node :: _ as rest) ->
      update_node node next_node;
      nodes_to_graph labelmap rest
  | node :: [] -> 
      update_node node [];
      nodes_to_graph labelmap []

let make_cfg (instrs : Assem.instr list) : cfg = 
  (* Convert each instruction to node, and record the position of each label *)
  let nodes, label_map = gen_nodes_n_labmap instrs in
  nodes_to_graph label_map nodes
  
let is_condition node = 
  match node.stmt with
  | OPER (_, _, _, Some (l1 :: l2 :: [])) -> true
  | OPER (_, _, _, Some (l1 :: l2 :: rest)) -> failwith "CJUMP has more than two branches"
  | _ -> false

let is_jump node = 
  match node.stmt with
  | OPER (_, _, _, Some (l :: [])) -> true
  | _ -> false 

let rec uncover_liveness cfg : cfg = 

  let backward_pass rev_cfg = 
    match rev_cfg with
    | [] -> reverse_graph rev_cfg
    | node :: (pre_node :: _ as rest) ->

  let get_live_out node next_node = 
    
    (* what is live at the next node but is not live at the current node? *)
    let diff = NodeSet.diff next_node.live_out node.live_out in 

    match NodeSet.is_empty node.define with
    | true -> node.live_out <- NodeSet.union node.live_out diff
    | false -> NodeSet.iter diff
        ~f:(fun var_at_next -> match NodeSet.mem node.define var_at_next with
          | true -> ()
          | false -> node.live_out <- NodeSet.add node.live_out var_at_next)
  in
  (* first pass, process node in order *)
  match cfg with
  | [] -> cfg
  | node :: (next_node :: _ as rest) ->
      get_live_out node next_node;
      uncover_liveness rest
  | node :: [] ->
      node.live_out <- [];
      uncover_liveness []
  






        



  
