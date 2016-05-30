type reg = string
type temp = Temp.temp
type label = Temp.label

type instr =
    (* assem * dst * src * jump *)
  | OPER of string * temp list * temp list * label list option

  | LABEL of string * label

  | MOVE of string * temp * temp

(*let format_str : (temp -> string) -> instr -> string*)


