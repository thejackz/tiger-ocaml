open Symbol
open Core.Std


type temp = int with sexp, compare

type label = Symbol.symbol with sexp, compare

let temp_count = ref 0
let label_count = ref 0

let new_temp () = 
  let return = !temp_count in
  temp_count := !temp_count + 1;
  return

let temp_to_string t = "temp" ^ (string_of_int t) 

let new_label () : label = 
  let return = !label_count in
  label_count := !label_count + 1;
  Symbol.symbol_of_string ("label" ^ (string_of_int return ))

let label_to_string l = 
  Symbol.name l

let named_label s = 
  Symbol.symbol_of_string s



module Temp = struct
  type t = temp with sexp, compare
end
module TempComp = Comparable.Make(Temp)
module TempSet = TempComp.Set 
module TempMap = TempComp.Map


module Label = struct
  type t = label with sexp, compare
end
module LabelComp = Comparable.Make(Label)
module LabelMap = LabelComp.Map





