open Datatypes
type ty = Datatypes.datatype

type env_type = 
  | VAR_TYPE  of ty
  | FUNC_TYPE of ty list * ty

val base_tenv : ty Symbol.table 
val base_venv : env_type Symbol.table

