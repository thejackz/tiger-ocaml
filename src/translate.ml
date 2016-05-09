open Core.Std
open Frame
open Tree
open Temp

module F = MISP
module T = Tree
module A = Ast

(* http://www.cs.indiana.edu/~sabry/teaching/compilers/wi97/ *)


let wordsize = F.word_size

let fragments = ref []

let add_frag frag = 
  fragments := frag :: !fragments

type unique = unit ref


(*Every time a new function is declared or in a new let expression, a new level should be created*)
type level = {
  parent    : level option ;
  frame     : F.frame      ;
  cmp       : unique       ;
}

(* Access type define the level and the location (memory or frame) *)
type access = level * F.access

let outermost = {
  parent = None ;
  frame  = F.new_frame (Temp.named_label "main") [] ;
  cmp    = ref () ;
}

let compare_level l1 l2 = phys_equal l1 l2

let new_level parent name formals = 
  (*add one bool at the beginning of formals, this is the static links*)
  let new_frame = F.new_frame name (true :: formals) in
  { 
    parent = parent; 
    frame  = new_frame;
    cmp    = ref ();
  }



(* Process the frame formals of the current level 
 * and attach level to each access of frame's access
 * Note that since the first element of formals is 
 * the static link, so that is discard*)
let formals level : access list = 
  match level.parent with
  | None -> []
  | Some parent ->
      let fm_formals = F.formals level.frame in
      match List.map fm_formals ~f:(fun access -> level, access) with
      | [] -> failwith "formals should not be empty"
      | hd :: tl -> tl


let alloc_local level escape = 
  level, (F.alloc_locals level.frame escape)


type exp = 
  | Ex of Tree.exp
  | Nx of Tree.stm
  | Cx of (Temp.label -> Temp.label -> Tree.stm)


let rec seq = function
  | [a; b] -> T.SEQ (a, b)
  | a :: b :: l -> T.SEQ (seq([a; b]), seq(l))
  | [a] -> a
  | [] -> T.EXP (T.CONST 450)


let unEx exp = 
  match exp with
  | Ex e -> e
  | Nx s -> T.ESEQ (s, T.CONST 0)
  | Cx fn -> 
      let r = Temp.new_temp () in
      let t = Temp.new_label () in
      let f = Temp.new_label () in
      let sequence = seq [T.MOVE (T.TEMP r, T.CONST 1);
                           fn t f ;
                           T.LABEL f;
                           T.MOVE (T.TEMP r, T.CONST 0);
                           T.LABEL t] 
      in
      T.ESEQ (sequence, T.TEMP r)

let unNx exp = 
  match exp with
  | Nx s -> s
  | Ex e -> T.EXP e
  | Cx fn -> T.EXP (unEx (Cx fn))

let unCx exp = 
  match exp with
  | Cx fn -> fn
  | Ex e -> fun t f -> T.CJUMP (T.EQ, e, T.CONST 0, t, f)
  | Nx _ -> failwith "this should not happend"

let get_static_link level = 
  match level.parent with
  | None -> failwith "top level has no static link"
  | Some _ ->
      let formals = F.formals level.frame in
      match List.hd formals with
      | None -> failwith "no static link exist, this is a bug"
      | Some link -> link

let empty = Ex (CONST 0)


let cmp_ast2tree ast =
  match ast with
  | A.Eq  (_, _)       -> T.EQ
  | A.Neq (_, _)       -> T.NE
  | A.Le  (_, _)       -> T.LE
  | A.Lt  (_, _)       -> T.LT 
  | A.Ge  (_, _)       -> T.GE 
  | A.Gt  (_, _)       -> T.GT

let bool_ast2tree ast =
  match ast with
  | A.Or  (_, _)       -> T.OR
  | A.And (_, _)       -> T.AND

let arith_ast2tree ast = 
  match ast with
  | A.Add  (_, _)      -> T.PLUS
  | A.Sub  (_, _)      -> T.MINUS
  | A.Mul  (_, _)      -> T.MUL
  | A.Div  (_, _)      -> T.DIV


let trans_int i =
  Ex (T.CONST i)

let trans_nil () = 
  Ex (T.CONST 0)

let rec trans_id id_access use_level =
  let def_level, f_access = id_access in
  let rec helper def_level check_level fp_ir =
    match compare_level def_level check_level with
    (* true -> it is in the current frame *)
    | true -> Ex (F.calc_texp fp_ir f_access)
    (* false -> it is in the previous frame*)
    | false ->
        let sl_offset = get_static_link use_level in
        match use_level.parent with
        | None -> failwith "variable is undefined, type checker has bug"
        | Some parent ->
            let prev_frame_ir = F.calc_texp fp_ir sl_offset in
            helper def_level parent prev_frame_ir
  in helper def_level use_level (T.TEMP F.fp)

(* Subscript: a[3] -> base (a) + 3 * wordsize *)
let trans_subscript lv_ir index_ir = 
  let lv_exp, index_exp = unEx lv_ir, unEx index_ir in
  Ex T.(MEM (BINOP (PLUS, lv_exp, BINOP (MUL, index_exp, CONST wordsize))))


let trans_fieldexp base_ir id fields = 
  let rec get_offset id fields offset = 
    match fields with
    | [] -> None
    | (sym, ty) :: rest ->
        match id = sym with
        | false -> get_offset id rest (offset + 1)
        | true  -> Some (offset, ty)
  in
  let base_exp = unEx base_ir in
  match get_offset id fields 0 with
  | None -> None
  | Some (offset, ty) ->
      Some (Ex T.(MEM (BINOP (PLUS, base_exp, BINOP (MUL, CONST offset, CONST wordsize)))), ty)


let trans_whileexp breakpoint cond body = 
  let cond_exp, body_exp = unCx cond, unEx body in
  let test_lab, done_lab = Temp.new_label (), Temp.new_label () in
  Nx T.(seq [LABEL test_lab; 
             cond_exp test_lab done_lab; 
             EXP body_exp;
             JUMP (NAME test_lab, [test_lab]);
             LABEL done_lab])
  
let trans_assignment lhs rhs = 
  let lhs_exp, rhs_exp = unEx lhs, unEx rhs in
   Nx T.(MOVE (MEM (lhs_exp), rhs_exp))


let condition_helper test then_e else_e = 
  let true_lab, false_lab = Temp.new_label (), Temp.new_label () in
  let test_stm, then_exp = unCx test, unEx then_e in
  let return_temp = Temp.new_temp () in
  let join_point = Temp.new_label () in 
  match else_e with
  | None -> 
      Ex T.(ESEQ (seq [test_stm true_lab false_lab; 
                       LABEL true_lab;
                       MOVE (TEMP return_temp, then_exp);
                       JUMP (NAME join_point, [join_point]);],
                  TEMP return_temp))
  | Some else_e ->
      let else_exp = unEx else_e in
      Ex T.(ESEQ (seq [test_stm true_lab false_lab; 
                       LABEL true_lab;
                       MOVE (TEMP return_temp, then_exp);
                       JUMP (NAME join_point, [join_point]);
                       LABEL false_lab;
                       MOVE (TEMP return_temp, else_exp);
                       JUMP (NAME join_point, [join_point])],
                  TEMP return_temp))

let trans_ifthenelse test then_e else_e = 
  condition_helper test then_e (Some else_e)

let trans_ifthen test then_e = 
  condition_helper test then_e None

let trans_rec_create fields = 
  let length = List.length fields in
  let field_exps = List.map fields ~f:(fun e -> unEx e) in
  let malloc_call = T.(CALL (NAME F.malloc, [CONST (length * wordsize)])) in
  let r = Temp.new_temp () in
  let head = T.(MOVE (TEMP r, malloc_call)) in
  let rec record_init r fields counter = 
    match fields with
    | [] -> failwith "this should not happend, record init"
    | h :: [] -> T.(MOVE (TEMP r, malloc_call))
    | h :: d ->
        T.(SEQ (MOVE (MEM (BINOP (PLUS, TEMP r, CONST (counter * wordsize))), h),
           record_init r d (counter + 1)))
  in
  let stm = match length with
    | 0 -> failwith "empty record"
    | 1 -> T.(SEQ (head, MOVE (MEM (BINOP (PLUS, TEMP r, CONST wordsize)), List.hd_exn field_exps)))
    | _ -> T.(SEQ (head, record_init r field_exps 0))
  in
  Ex T.(ESEQ (stm, TEMP r))

let trans_seq ir_lst = 
  let rec get_all_but_last (ir_lst : exp list) col = 
    match ir_lst with
    | [] -> failwith "exp seq is empty, this should not pass type checker"
    | h :: [] -> (List.rev col), h
    | h :: r -> get_all_but_last r (h :: col)
  in
  let front, last = get_all_but_last ir_lst [] in
  let sequence = seq (List.map front ~f:(fun e -> unNx e)) in
  Nx T.(SEQ (sequence, unNx last))

let trans_funcall call_level def_level arg_irs = 
  let rec get_enclosing_static_link check_level fp_ir = 
    let sl_offset = get_static_link check_level in
    match compare_level def_level check_level with
    | true -> 
        F.calc_texp fp_ir sl_offset
    | false -> 
        match check_level.parent with
        | None -> failwith "static link not found, there's bug in type checker"
        | Some parent ->
            let prev_frame_ir = F.calc_texp fp_ir sl_offset in
            get_enclosing_static_link parent prev_frame_ir
  in
  let args = List.map arg_irs ~f:(fun arg -> unEx arg) in
  let fname = F.name call_level.frame in
  match call_level.parent with
  (* function is call at top level, no static link, could be runtime function *)
  | None -> Ex T.(CALL (NAME fname, args))
  | Some parent ->
      let cur_frame_ir = T.(TEMP F.fp) in
      let enclosing_frame = get_enclosing_static_link call_level cur_frame_ir in
      Ex T.(CALL (NAME fname, enclosing_frame :: args))

let trans_arrcreate size init = 
  Ex T.(CALL (NAME (F.init_array), [unEx size; unEx init]))

let trans_ariths ast e1 e2 = 
  let binop = arith_ast2tree ast in
  Ex (BINOP (binop, unEx e1, unEx e2))

let trans_boolexp ast e1 e2 = 
  let binop = bool_ast2tree ast in
  Ex (BINOP (binop, unEx e1, unEx e2))

let trans_cmpexp ast e1  e2 = 
  let binop = cmp_ast2tree ast in
  Cx (fun t f ->  T.CJUMP (binop, unEx e1, unEx e2, t, f))

let trans_break label = 
  Nx T.(JUMP (NAME label, [label]))

let trans_string str = 
  let lab = Temp.new_label () in
  add_frag (F.make_fragstring lab str);
  Ex T.(NAME lab)


let trans_letexp init_exps body =
  let inits = List.map init_exps ~f:(fun init -> unNx init) in
  Ex T.(ESEQ (seq inits, unEx body))

  
let proc_entry_exit ~is_procedure level body = 
  let fm = level.frame in
  let body_ir = match is_procedure with
    | true -> T.(SEQ (unNx body, MOVE (TEMP F.rv, CONST 0)))
    | false -> T.(MOVE (TEMP F.rv, unEx body))
  in 
  let stmt = F.proc_entry_exit1 fm body in 
  fragments := F.Proc (stmt, fm) :: !fragments




let get_fragments () = !fragments
