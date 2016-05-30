
(* abstract name for local variables *)
type temp

val new_temp : unit -> temp

val temp_to_string : temp -> string



(*name for the static memory addresses *)
type label = Symbol.symbol

val new_label : unit -> label

val label_to_string : label -> string

(* Return a new label whose assembly language name is the given string *)
val named_label : string -> label

module TempMap : Map.S with type key = temp

module TempSet : Set.S with type elt = temp

module LabelMap : Map.S with type key = label
