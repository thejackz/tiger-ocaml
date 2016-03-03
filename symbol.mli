
type symbol
val symbol_of_string: string -> symbol
val name: symbol -> string

type 'a table
val empty: 'a table
val add: 'a table -> symbol -> 'a -> 'a table
val lookup: 'a table -> symbol -> 'a option
