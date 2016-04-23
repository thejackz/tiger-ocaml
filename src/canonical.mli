(* This function eliminates all the SEQ and ESEQ nodes, 
 * and move CALLs to the top level *)
val linearize : Tree.stm -> Tree.stm list

(* this function eliminates all conditional jumps, 
 * and group them into basic block *)
val basic_blocks : Tree.stm list -> (Tree.stm list list * Temp.label)


(*
 * basic blocks are transformed into traces,
 * in which every CJUMP is immediately followed
 * by its FALSE label
 *)
val trace_schedule : Tree.stm list list -> Temp.label -> Tree.stm list

