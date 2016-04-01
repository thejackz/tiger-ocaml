
open Frame
open Tree
open Temp

module F = MISP
module T = Tree



(*Every time a new function is declared or in a new let expression, a new level should be created*)
type level = 
  | Top
  | Level of {frame : F.frame; parent : level; cmp : int}


(* Access type define the level of *)
type access = level * F.access

let level_cmp = ref 0

let inc_levelcmp () = level_cmp := !level_cmp + 1
let dec_levelcmp () = level_cmp := !level_cmp - 1
let get_level_cmp () = !level_cmp



let outermost = Top

let new_level {parent; name; formals} = 
  (*add one bool at the beginning of formals, this is the static links*)
  let new_frame = F.new_frame {name = name; escapes = (true :: formals)} in
  let levelcmp = get_level_cmp () in
  inc_levelcmp ();
  Level (new_frame, parent, levelcmp)


let formals level = 
  match level with
  | Top -> []
  | Level (frame, parent, cmp) as l -> (l, frame.formals)


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







