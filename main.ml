#directory "_build";;
#load "lexer.cmo";;
#load "parser.cmo";;
#load "main.cmo";;

(*let token_to_str = function*)
  (*| EOF -> "eof"*)
  (*| LPAREN   -> "("  | RPAREN    -> ")"*)
  (*| LBRACE   -> "{"  | RBRACE    -> "}"*)
  (*| LBRACKET -> "["  | RBRACKET  -> "]"*)
  (*| PLUS     -> "+"  | MINUS     -> "-"*)
  (*| TIMES    -> "*"  | DIVIDE    -> "/"*)
  (*| EQ       -> "="  | NEQ       -> "<>"*)
  (*| LESS     -> "<"  | LESSEQ    -> "<="*)
  (*| GREATER  -> ">"  | GREATEREQ -> ">="*)
  (*| AMPERSAND -> "&" | PIPE      -> "|"*)
  (*| COLON     -> ":" | SEMICOL   -> ";"*)
  (*| COLONEQ   -> ":="| DOT       -> "." *)
  (*| COMMA     -> "," | ARRAY     -> "array"*)
  (*| BREAK     -> "break"  | DO  -> "do"*)
  (*| ELSE      -> "else" | END -> "end"*)
  (*| FOR       -> "for" | FUNCTION -> "function"*)
  (*| IF        -> "if" | IN -> "in"*)
  (*| LET       -> "let" | NIL -> "nil"*)
  (*| OF        -> "of" | THEN -> "then"*)
  (*| TO        -> "to" | TYPE -> "type"*)
  (*| VAR       -> "var" | WHILE -> "while"*)
  (*| INT i     -> "Int " ^ (string_of_int i )*)
  (*| ID id     -> "ID " ^ id*)
  (*| STRING str -> "STRING " ^ str*)

let lexer s = 
  let lexbuf = Lexing.from_string s in
  let rec helper buf col = 
    let tok = Lexer.read buf in
    if tok <> EOF
    then helper buf (tok :: col)
    else List.rev col
  in helper lexbuf []


let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast


