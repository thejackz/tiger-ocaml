open Core.Std
open Tree

module T = Tree
module LabelMap = Temp.LabelMap

let fail () = failwith "impossible"

let (%) a b = 
  match a, b with
  | EXP (CONST _), b -> b
  | a, EXP (CONST _) -> a
  | _, _ -> SEQ (a, b)

let commute e1 e2 = 
  match e1, e2 with
  | EXP (CONST _), _ -> true
  | _, NAME _ -> true
  | _, CONST _ -> true
  | _, _ -> false

let split_pairs pair_lst = 
  List.fold_right pair_lst
  ~init:([], [])
  ~f:(fun (p1, p2) (l1, l2) -> (p1 :: l1, p2 :: l2))

let rec do_stm stm = 
  match stm with
  | MOVE (TEMP tmp, CALL (e, el)) -> 
      reorder_stm (e :: el) (function 
                             | (e :: el) -> MOVE (TEMP tmp, CALL (e, el))
                             | _         -> fail ())
                            
  | MOVE (TEMP tmp, e) -> 
      reorder_stm [e] (function 
                       | [e] -> MOVE (TEMP tmp, e)
                       | _         -> fail ())
  | MOVE (MEM e1, e2) -> 
      reorder_stm [e1; e2] (function
                            | [e1; e2] -> MOVE (MEM e1, e2)
                            | _        -> fail ())
  | MOVE (ESEQ (s, e), b) -> 
      do_stm (SEQ (s, MOVE (e, b)))
  | EXP (CALL (e, el)) -> 
      reorder_stm (e :: el) (function
                             | (e :: el) -> EXP (CALL (e, el))
                             | _         -> fail ())
  | EXP e -> 
      reorder_stm [e] (function 
                       | [e] -> EXP e
                       | _   -> fail ())
  | JUMP (addr, labs) -> 
      reorder_stm [addr] (function 
                          | [addr] -> JUMP (addr, labs)
                          | _      -> fail ())
  | CJUMP (op, e1, e2, t, f) -> 
      reorder_stm [e1; e2] (function 
                            | [e1; e2] -> CJUMP (op, e1, e2, t, f)
                            | _        -> fail ())
  | SEQ (s1, s2) -> (do_stm s1) % (do_stm s2)
  | _   -> reorder_stm [] (function 
                           | [] -> stm
                           | _  -> fail ())

and do_exp exp = 
  match exp with
  | CONST _             
  | NAME _              
  | TEMP _ -> 
      reorder_exp [] (function
                      | [] -> exp
                      | _  -> fail ())
  | BINOP (op, e1, e2)  -> 
      reorder_exp [e1; e2] (function 
                            | [e1; e2] -> BINOP (op, e1, e2)
                            | _ -> fail ())
  | MEM e -> 
      reorder_exp [e] (function
                       | [e] -> MEM e
                       | _   -> fail ())
  | ESEQ (s, e) -> 
      let stms = do_stm s in
      let stms', e' = do_exp e in
      (stms % stms', e')
  | CALL (e, el) -> 
      reorder_exp (e :: el) (function
                             | (e :: el) -> CALL (e, el)
                             | _         -> fail ())

and reorder es : Tree.stm * Tree.exp list = 
  match es with
  | ((CALL _ as e) :: rest) -> 
      let t = Temp.new_temp () in
      reorder ((ESEQ (MOVE (TEMP t, e), TEMP t)) :: rest)
  | hd :: rest ->
      let stms, e = do_exp hd in
      let stms', el = reorder rest in
      (match commute stms' e with
      | true -> (stms % stms', e :: el)
      | false ->
          let t = Temp.new_temp () in
          (stms % MOVE (TEMP t, e) % stms', TEMP t :: el))
      | [] -> EXP (CONST 0), []

and reorder_stm el build =
  let stms, el' = reorder el in
  stms % (build el')

and reorder_exp el build = 
  let stms, el' = reorder el in
  stms, (build el')

let linearize stm = 
  (* main function of linearize *)
  let rec linear stm l = 
    match stm with
    | T.SEQ (a, b) -> linear a (linear b l)
    | _ -> stm :: l
  in linear (do_stm stm) []

(* --------------------------------------------------------------------------------------------- *)

let basic_blocks stms = 
  let done_lab = Temp.new_label () in 
  let rec blocks stms blist = 
    
    let rec next stms this_block = 
      match stms with
      | JUMP _ as hd :: rest -> end_block rest (hd :: this_block)
      | CJUMP _ as hd :: rest -> end_block rest (hd :: this_block)
      | (LABEL lab) :: _ -> next ((JUMP (NAME lab, [lab])) :: stms) this_block
      | hd :: rest -> next rest (hd :: this_block)
      | [] -> next [JUMP (NAME done_lab, [done_lab])] this_block

    and end_block stms this_block = 
      blocks stms ((List.rev this_block) :: blist)
    in
    
    match stms with
    | LABEL _ as hd :: rest -> next rest [hd]
    | [] -> List.rev blist
    | _ ->  blocks ((LABEL (Temp.new_label ())) :: stms) blist
  in blocks stms [], done_lab

(* --------------------------------------------------------------------------------------------- *)

let enter_block block table = 
  match block with
  | LABEL sym :: _ -> LabelMap.add table ~key:sym ~data:block
  | _ -> table

let rec split_last = function
  | [x] -> ([], x)
  | hd :: tl -> 
      let front, last = split_last tl in
      hd :: front, last
  | [] -> failwith "impossible, basic block should not be empty block"

let get_successor table lab = 
  match LabelMap.find table lab with
  | Some ((_ :: _) as sucessor) -> Some sucessor 
  | _ -> None

let rec trace table block rest_blocks =
  match block with
  | LABEL sym :: _ ->
      
      (* update the table, this marks that lab has been processed *)
      let new_table = LabelMap.add table sym [] in

      (match split_last block with

      | front, JUMP (NAME lab, _) ->
          (match get_successor table lab with
          | Some successor -> front @ trace new_table successor rest_blocks 
          | None -> block @ gen_next new_table rest_blocks)

      | front, CJUMP (op, x, y, t, f) ->
          (match get_successor table t, get_successor table f with
          (* always choose the false label as the next jump point if there is one *)
          | _, Some successor -> front @ trace new_table successor rest_blocks
          | Some successor, _ -> 
              front @ ((CJUMP (T.rev_relop op, x, y, f, t)) :: (trace new_table successor rest_blocks))
          | _ -> 
              let f' = Temp.new_label () in
              front @ [CJUMP (op, x, y, t, f'); LABEL f'; JUMP (NAME f, [f])] 
                @ gen_next new_table rest_blocks)
      
      | front, JUMP _ -> front @ gen_next new_table rest_blocks
          
      | _ -> failwith "should not happend")

  | _ -> failwith "this should not happend"

and gen_next table blocks = 
  match blocks with
  | (LABEL l :: _) as block :: rest ->
      (match LabelMap.find table l with
      | Some (_ :: _) -> trace table block rest   
      | _ -> gen_next table rest)
  | [] -> []
  | _ -> failwith "this is impossible, every block starts with a label"


let trace_schedule (blocks : Tree.stm list list) done_lab = 
  (gen_next (List.fold_right blocks ~init:LabelMap.empty ~f:enter_block) blocks) @ [LABEL done_lab]
