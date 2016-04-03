open Tree

(* https://heim.ifi.uio.no/msteffen/download/software/tiger/main033.html *)

(* https://github.com/lijunsong/tiger-compiler-in-ocaml/blob/master/frontend%2Fsemant.ml*)

type level

type access

type exp

(*type new_level_arg = {*)
  (*parent  : level;*)
  (*name    : Temp.label;*)
  (*formals : bool list;*)
(*}*)

val outermost : level

val new_level : level option -> Temp.label -> bool list -> level

val formals : level -> access list

val alloc_local : level -> bool -> access

val unEx : exp -> Tree.exp
val unNx : exp -> Tree.stm
val unCx : exp -> (Temp.label -> Temp.label -> Tree.stm)



(* funtions for translating AST to IR*)

val trans_int : int -> exp
val trans_nil : unit -> exp
val trans_string : string -> exp
val trans_break : Temp.label -> exp


val trans_cmpexp : Ast.cmp_exp -> exp -> exp -> exp
val trans_boolexp : Ast.bool_exp -> exp -> exp -> exp
val trans_ariths : Ast.arith_exp -> exp -> exp -> exp

val trans_id : access -> level -> exp
val trans_subscript : exp -> exp -> exp   
val trans_fieldexp : exp -> Symbol.symbol -> (Symbol.symbol * Datatypes.datatype) list 
                         -> (exp * Datatypes.datatype) option



(*val tran_expseq : exp list -> exp*)
(*val tran_negexp : exp -> exp*)
(*val tran_callexp : Temp.label -> exp list -> level -> level -> exp*)
(*val tran_arr : exp -> exp -> exp*)
(*val tran_rec : exp list -> exp*)
(*val trans_if : exp -> exp -> exp option -> exp*)
(*val trans_while : exp -> exp -> Temp.label -> exp*)
(*val trans_assign : exp -> exp -> exp*)

