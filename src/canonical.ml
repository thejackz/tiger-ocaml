open Core.Std
open Tree

module T = Tree

let linearize stm = 

  let split_pairs pair_lst = 
    List.fold_right pair_lst
    ~init:([], [])
    ~f:(fun (p1, p2) (l1, l2) -> (p1 :: l1, p2 :: l2))
  in

  let rec do_stm stm = 
    match stm with
    | MOVE (TEMP tmp, e)       -> reorder_stm [e] (fun [e] -> MOVE (TEMP tmp, e))
    | MOVE (MEM e1, e2)        -> reorder_stm [e1; e2] (fun [e1; e2] -> MOVE (MEM e1, e2))
    | EXP e                    -> reorder_stm [e] (fun [e] -> EXP e)
    | JUMP (addr, labs)        -> reorder_stm [addr] (fun [addr] -> JUMP (addr, labs))
    | CJUMP (op, e1, e2, t, f) -> reorder_stm [e1; e2] (fun [e1; e2] -> CJUMP (op, e1, e2, t, f))
    | SEQ (s1, s2)             -> SEQ (do_stm s1, do_stm s2)
    | LABEL _                  -> stm
  in

  and do_exp exp = 
    match exp with
    | CONST _             ->
    | NAME _              ->
    | TEMP _              -> EXP (CONST 0), exp
    | BINOP (op, e1, e2)  -> reorder_exp [e1; e2] (fun [e1; e2] -> BINOP (op, e1, e2))
    | MEM e               -> reorder_exp [e] (fun [e] -> MEM e)
    | CALL (func, params) -> 
    | ESEQ (s, e)         -> let s1, e1 = reorder_exp e in SEQ (do_stm s, s1), e
  in

  and reorder_stm subs build =

  in

  and reorder_exp subs build = 



  
  let commute e1 e2 = 
    match e1, e2 with
    | T.EXP (T.CONST _), _ -> true
    | _, T.NAME _ -> true
    | _, T.CONST _ -> true
    | _, _ -> false
  in



  (* main function of linearize *)
  let rec linear stm l = 
    match stm with
    | T.SEQ (a, b) -> linear a (linear b l)
    | _ -> stm :: l
  in linear (do_stm stm, [])

