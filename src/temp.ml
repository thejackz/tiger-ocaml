open Symbol


type temp = int

type label = Symbol.symbol

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



module TempMap = Map.Make(struct
  type t = temp
  let compare = compare
end)

module TempSet = Set.Make(struct
  type t = temp
  let compare = compare
end)




