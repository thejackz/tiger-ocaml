

type unique = unit ref

type datatype = 
  | INT
  | STRING
  | RECORD of (Symbol.symbol * datatype) list * unique
  | ARRAY of datatype * unique
  | NIL
  | UNIT
  | NAME of Symbol.symbol * datatype option ref
