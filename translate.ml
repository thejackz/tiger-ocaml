
open Frame
open Tree
open Temp

module F = Frame
module T = Tree


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







