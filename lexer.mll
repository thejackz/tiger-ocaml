{
open Lexing
open Parser
exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
    pos_lnum = pos.pos_lnum + 1
    }

}

let int = ['0' - '9'] ['0' - '9']*
let newline = '\n' | '\r' | "\r\n"
let whitespace = [' ' '\t']+
let id = ['a' - 'z' 'A' - 'Z' '_'] ['a' - 'z' 'A' - 'Z' '0' - '9' '_']*

rule read = parse
  | whitespace            { read lexbuf }
  | newline               { next_line lexbuf; read lexbuf }
  | '"'                   { read_string (Buffer.create 17) lexbuf }
  | "/*"                  { comment lexbuf; read lexbuf }
  | eof                   { EOF }
  | '('                   { LPAREN }
  | ')'                   { RPAREN }
  | '{'                   { LBRACE }
  | '}'                   { RBRACE }
  | '['                   { LBRACKET }
  | ']'                   { RBRACKET }
  | '+'                   { PLUS }
  | '-'                   { MINUS }
  | '*'                   { TIMES }
  | '/'                   { DIVIDE }
  | '='                   { EQ }
  | "<>"                  { NEQ }
  | "<"                   { LT }
  | "<="                  { LE }
  | '>'                   { GT }
  | ">="                  { GE }
  | '&'                   { AMPERSAND }
  | '|'                   { PIPE }
  | ":="                  { COLONEQ }
  | ';'                   { SEMICOL }
  | ':'                   { COLON }
  | '.'                   { DOT }
  | ','                   { COMMA }
  | "array"               { ARRAY }
  | "break"               { BREAK }
  | "do"                  { DO }
  | "else"                { ELSE }
  | "end"                 { END }
  | "for"                 { FOR }
  | "function"            { FUNCTION }
  | "if"                  { IF }
  | "in"                  { IN }
  | "let"                 { LET }
  | "nil"                 { NIL }
  | "of"                  { OF }
  | "then"                { THEN }
  | "to"                  { TO }
  | "type"                { TYPE }
  | "var"                 { VAR }
  | "while"               { WHILE }
  | int                   { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | id                    { ID (Lexing.lexeme lexbuf) }

and read_string buf =
  parse
  | '"'                   { STRING (Buffer.contents buf) }
  | '\\' '/'              { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\'             { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'              { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'              { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'              { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'              { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'              { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+         { Buffer.add_string buf (Lexing.lexeme lexbuf);
                            read_string buf lexbuf }
  | _                     { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof                   { raise (SyntaxError ("String is not terminated")) }

and comment = parse
  | "*/"                  { () }
  | "/*"                  { comment lexbuf; comment lexbuf }
  | _                     { comment lexbuf }

(*{*)
    (*let main () =*)
      (*let lexbuf = Lexing.from_channel stdin in *)
      (*let rec helper buf = *)
        (*let tok = read buf in*)
        (*print_endline (token_to_str tok);*)
        (*if tok <> EOF*)
        (*then helper buf*)
      (*in helper lexbuf*)
      
    
    (*let () = main ()*)
(*}*)
  
