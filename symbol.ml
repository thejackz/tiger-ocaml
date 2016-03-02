open Core.Std

module type SYMBOL = sig

  type symbol
  val symbol_of_string: string -> symbol
  val name: symbol -> string

  type 'a table
  val empty: 'a table
  val add: 'a table -> symbol -> 'a -> 'a table
  val lookup: 'a table -> symbol -> 'a option
end


module Symbol : SYMBOL = struct

  type symbol = string * int

  let sym_int = ref (-1)
  let next_sym_int () = 
    sym_int := !sym_int + 1; !sym_int

  let symbol_to_int_table = Hashtbl.create ~hashable:String.hashable ()
  let table = symbol_to_int_table

  let symbol_of_string (str:string) = 
    match Hashtbl.find table str with
    | Some i -> (str, i)
    | None   -> 
        (let next_i = next_sym_int () in
        match Hashtbl.add table ~key:str ~data:next_i with
        | `Duplicate | `Ok -> (str, next_i))

  let name (s, _) = s
  let integer (_, i) = i

  module Symbol_map = Map.Make(
    struct
      type t = int with sexp, compare
    end)

  type 'a table = 'a Symbol_map.t

  let empty = Symbol_map.empty 

  let add table (_, i) value = 
    Symbol_map.add table ~key:i ~data:value

  let lookup table (_, i) = 
    Symbol_map.find table i
end