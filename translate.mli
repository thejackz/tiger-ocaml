open Frame
open MISP
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


val empty : exp



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


(*val trans_forexp : exp -> Temp.label option -> exp -> exp -> exp -> exp*)
val trans_whileexp : Temp.label option -> exp -> exp -> exp
val trans_assignment : exp -> exp -> exp
val trans_ifthenelse : exp -> exp -> exp -> exp
val trans_ifthen : exp -> exp -> exp
val trans_rec_create : exp list -> exp
val trans_seq : exp list -> exp
val trans_funcall : level -> exp list -> exp
val trans_arrcreate : exp -> exp -> exp
val trans_letexp : exp list -> exp -> exp

(* level -> body -> unit *)
val proc_entry_exit : level -> exp -> unit 

val get_fragments : unit -> frag list
