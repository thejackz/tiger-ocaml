open Datatypes

type ty = Datatypes.datatype

type env_type = 
  | VAR_TYPE  of ty
  | FUNC_TYPE of ty list * ty

(* 
 * environment for type value
 * type a = int
 * a, VAR_TYPE INT will be stored 
 *)
let base_tenv = Symbol.empty

let base_venv = Symbol.empty
