
type symbol with sexp, compare
val symbol_of_string: string -> symbol
val name: symbol -> string
val make_dummy_sym: unit -> symbol
(*val symbol_compare: symbol -> symbol -> int*)

type 'a table
val empty: 'a table
val add: 'a table -> symbol -> 'a -> 'a table
val lookup: 'a table -> symbol -> 'a option

