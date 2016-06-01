open Core.Std
open Assem

(* https://www.cs.cmu.edu/~fp/courses/15411-f09/schedule.html *)
module TempSet = Temp.TempSet 
module LabelMap = Temp.LabelMap

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
} with sexp, compare

module Node = Comparable.Make(struct
  type t = node with sexp, compare
end)

module NodeSet = Node.Set

type cfg = node list

let reverse_graph cfg = 
  List.iter cfg ~f:(fun node -> node.succ, node.prec <- node.prec, node.succ);
  List.rev cfg

let gen_nodes_n_labmap instrs = 
  let last_index = (List.length instrs) - 1 in
  let nodes, label_map, _ = List.fold_right instrs 
    ~init:([], LabelMap.empty, last_index)
    ~f:(fun instr (nodes, map, index) ->
    match instr with
    | LABEL (assem, label) ->
        let new_node = {
          id = index;
          stmt = instr;
          define = TempSet.empty;
          usage = TempSet.empty;
          live_in = TempSet.empty;
          live_out = TempSet.empty;
          succ = NodeSet.empty;
          prec = NodeSet.empty;
          is_move = false;
        } in 
        (new_node :: nodes, LabelMap.add label index map, index - 1)
    | MOVE (assem, dst, src) ->
        let new_node = {
          id = index;
          stmt = instr;
          define = TempSet.singleton dst;
          usage = TempSet.singleton src;
          live_in = TempSet.empty;
          live_out = TempSet.singleton src;
          succ = NodeSet.empty;
          prec = NodeSet.empty;
          is_move = true;
        } in 
        (new_node :: nodes, map, index - 1)
    | OPER (assem, dst_lst, src_lst, next) -> 
        let new_node = {
          id = index;
          stmt = instr;
          define = TempSet.of_list dst_lst;
          usage = TempSet.of_list src_lst;
          live_in = TempSet.empty;
          live_out = TempSet.of_list src_lst;
          succ = NodeSet.empty;
          prec = NodeSet.empty;
          is_move = false;
        } in 
        (new_node :: nodes, map, index - 1))
  in (nodes, label_map)

let rec nodes_to_graph label_map nodes = 
  let rec update_node node next_node = 
    match node.stmt with
    | MOVE _ 
    | LABEL _ 
    | OPER (_, _, _, None) -> 
        node.succ <- NodeSet.add node.succ next_node;
        next_node.prec <- NodeSet.add next_node.prec next_node
    | OPER (assem, _, _, Some labs) ->
        List.iter labs ~f:(fun lab -> let nx = LabelMap.find label_map lab in 
          NodeSet.add node.succ nx;
          NodeSet.add nx.prec node)
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



let rec uncover_livenss cfg ~new_changes : cfg = 

  let update_live_in node next_node = 
    
    (* what is live at the next node but is not live at the current node? *)
    let diff = TempSet.diff next_node.live_in node.live_in in 
    match TempSet.is_empty node.define with
    | true -> node.live_in <- TempSet.union node.live_in diff
    | false -> TempSet.iter diff
        ~f:(fun var_at_next -> match TempSet.mem node.define var_at_next with
          | true -> ()
          | false -> node.live_in <- TempSet.add node.live_in var_at_next)
  in
  (* first pass, process node in order *)
  match cfg with
  | [] -> (match new_changes with
      | true -> uncover_livenss ~new_changes:false 
      | false -> cfg)
  | node :: (next_node :: _ as rest) ->
      update_live_in node next_node;
      uncover_livenss rest
  | node :: [] ->
      node.live_in <- TempSet.empty;
      uncover_livenss []
  











        



  
