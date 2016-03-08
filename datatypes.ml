

type unique = unit ref

type datatype = 
  | INT
  | STRING
  | RECORD of (Symbol.symbol * datatype) list * unique
  | ARRAY of datatype * unique
  | NIL
  | UNIT
  | NAME of Symbol.symbol * datatype option ref


let type_to_string = function
  | INT -> "int"
  | STRING -> "string"
  | RECORD _ -> "RECORD"
  | ARRAY _ -> "ARRAY"
  | NIL -> "NIL"
  | UNIT -> "UNIT"
  | NAME _ -> "NAME"
