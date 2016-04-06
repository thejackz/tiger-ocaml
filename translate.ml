open Core.Std
open Frame
open Tree
open Temp

module F = MISP
module T = Tree



(*Every time a new function is declared or in a new let expression, a new level should be created*)
type level = {
  parent : level option;
  frame  : F.frame ;
  cmp    : int;
}
  


(* Access type define the level and the location (memory or frame) *)
type access = level * F.access

let level_cmp = ref 0

let inc_levelcmp () = level_cmp := !level_cmp + 1
let dec_levelcmp () = level_cmp := !level_cmp - 1
let get_level_cmp () = !level_cmp



let outermost = {
  parent   = None;
  frame    = F.new_frame (Temp.named_label "main") [];
  cmp      = !level_cmp;
}

let new_level parent name formals = 
  (*add one bool at the beginning of formals, this is the static links*)
  let new_frame = F.new_frame name (true :: formals) in
  let levelcmp = get_level_cmp () in
  inc_levelcmp ();
  {parent = parent; frame = new_frame; cmp = levelcmp}



(* Process the frame formals of the current level 
 * and attach level to each access of frame's access
 * Note that since the first element of formals is 
 * the static link, so that is discard*)
let formals level : access list = 
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
  let formals = F.formals level.frame in
  match List.hd formals with
  | None -> failwith "no static link exist, this is a bug"
  | Some link -> link

let empty = Ex (CONST 0)

let trans_int i =
  failwith ""

let trans_nil () = 
  failwith ""

let rec trans_id (id_access : access) use_level =
  let def_level, f_access = id_access in
  match def_level = use_level with
  (* true -> it is in the current frame *)
  | true -> Ex (F.calc_texp (T.TEMP F.fp) f_access)
  (* false -> it is in the previous frame*)
  | false ->
      let static_link = get_static_link use_level in
      match use_level.parent with
      | None -> failwith "variable is undefined, type checker has bug"
      | Some parent ->
          let previous_ir = unEx (trans_id id_access parent) in
          Ex (F.calc_texp previous_ir static_link)
           


let trans_subscript lv_ir index_ir = 
  let lv_exp, index_exp = unEx lv_ir, unEx index_ir in
  Ex T.(MEM (BINOP (PLUS, lv_exp, index_exp)))


let trans_fieldexp base id fields = 
  failwith ""
  

let trans_forexp index_id breakpoint low high body = 
  failwith ""

let trans_whileexp breakpoint cond body = 
  failwith ""
  
let trans_assignment lhs rhs = 
  failwith ""

let trans_ifthenelse test then_e else_e = 
  failwith ""

let trans_ifthen test then_e = 
  failwith ""

let trans_rec_create elts = 
  failwith ""

let trans_seq ir_lst = 
  failwith ""

let trans_funcall level ir_lst = 
  failwith ""

let trans_arrcreate size init = 
  failwith ""

let trans_ariths ast exp exp = 
  failwith ""

let trans_boolexp ast exp exp = 
  failwith ""

let trans_cmpexp ast exp exp = 
  failwith ""

let trans_break label = 
  failwith ""

let trans_string str = 
  failwith ""

