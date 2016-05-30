open Core.Std
open Assem

(* https://www.cs.cmu.edu/~fp/courses/15411-f09/schedule.html *)
type cfg = {
  control:     Graph.graph;
  define:      Temp.temp list Graph.Table.t; 
  usage:       Temp.temp list Graph.Table.t;
  is_move:     bool Graph.Table.t;
}


let uncover_liveness (instrs : Assem.instr list) = 
  let module Varset = Temp.TempSet in
  let rec helper instrs ~collector = 
    match instrs with
    | [] -> List.rev collector
    | instr :: rest ->
        let pre_liveset = List.hd_exn collector in 
        match instr with
        | OPER (assem, dst_lst, src_lst, next_label) ->
        | MOVE (assem, dst, src) ->
        | LABEL (assem, label) ->
        

  in 
  let rev_instrs = List.rev instrs in
  let hd, rest_instrs = List.hd_exn rev_instrs, List.tl_exn rev_instrs in 
  helper rest_instrs ~collector:[Varset.empty]


        



  
