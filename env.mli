open Datatypes
type ty = Datatypes.datatype

type env_type = 
  | VAR_TYPE  of Translate.access * ty
  | FUNC_TYPE of Translate.level * Temp.label * ty list * ty

val base_tenv : ty Symbol.table 
val base_venv : env_type Symbol.table

