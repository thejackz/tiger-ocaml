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







let tran_int i =
  failwith ""

let tran_nil () = 
  failwith ""

let tran_string s = 
  failwith ""

let tran_break label = 
  failwith ""

let trans_id access level =
  failwith ""

let trans_subscript lv_ir index = 
  failwith ""

let trans_fieldexp base id fields = 
  failwith ""
  
  



(*let tran_expseq : exp list -> exp*)
(*let tran_negexp : exp -> exp*)
(*let tran_callexp : Temp.label -> exp list -> level -> level -> exp*)
(*let tran_arr : exp -> exp -> exp*)
(*let tran_rec : exp list -> exp*)
(*let trans_if : exp -> exp -> exp option -> exp*)
(*let trans_while : exp -> exp -> Temp.label -> exp*)
(*let trans_assign : exp -> exp -> exp*)
