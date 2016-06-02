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
}

type cfg = node list

let reverse_graph cfg = List.rev cfg


let node_set_add nodes node = 
  match List.mem ~equal:(fun n1 n2 -> n1.id = n2.id) nodes node with
  | true -> nodes
  | false -> node :: nodes


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
          succ = [];
          prec = [];
          is_move = false;
        } in 
        (new_node :: nodes, LabelMap.add ~key:label ~data:new_node map, index - 1)
    | MOVE (assem, dst, src) ->
        let new_node = {
          id = index;
          stmt = instr;
          define = TempSet.singleton dst;
          usage = TempSet.singleton src;
          live_in = TempSet.empty;
          live_out = TempSet.singleton src;
          succ = [];
          prec = [];
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
    | OPER (_, _, _, None) -> (match next_node with
        | Some n -> 
            node.succ <- node_set_add node.succ n;
            n.prec <- node_set_add node.prec node
        | None -> ())
    | OPER (assem, _, _, Some labs) ->
        List.iter labs ~f:(fun lab -> let nx = LabelMap.find_exn label_map lab in
          node.succ <- node_set_add node.succ nx;
          nx.prec <- node_set_add nx.prec node)
  in
  match nodes with
  | [] -> nodes
  | node :: (next_node :: _ as rest) ->
      update_node node (Some next_node);
      nodes_to_graph label_map rest
  | node :: [] -> 
      update_node node None;
      nodes_to_graph label_map []

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

let rec uncover_liveness cfg = 

  (* process node, if it is updated, return true, else false *)
  let get_live_in node = 

    (* get all live-in vars of current node's successor *)
    let livein_next_nodes =  
      List.map node.succ ~f:(fun node -> TempSet.to_list node.live_in)
      |> List.concat |> TempSet.of_list
    in

    (* all vars that are live in the next nodes but are not live in at the current node *)
    let diff = TempSet.diff node.live_in livein_next_nodes in 

    (* any var that is defined at the current node ? *)
    match TempSet.is_empty node.define with
    | true -> 
        let new_node_live_in = TempSet.union node.live_in diff in
        (match TempSet.diff new_node_live_in node.live_in |> TempSet.is_empty with
        (* no change for node.live, return false, meaning that there is no new update *)
        | true -> false
        (* change detected, update live-in and return true. Another iteration is needed for liveness *)
        | false -> node.live_in <- new_node_live_in; true)
      
    | false -> TempSet.for_all diff
        ~f:(fun var_at_next -> match TempSet.mem node.define var_at_next with
          | true -> false
          | false -> node.live_in <- TempSet.add node.live_in var_at_next; true)
  in
  let change_lst = List.map cfg ~f:(fun node -> get_live_in node) in
  (* is there any new update? *)
  match List.mem change_lst true with
  | true -> uncover_liveness cfg
  | false -> cfg
  
