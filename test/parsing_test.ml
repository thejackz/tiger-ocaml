(*open Error*)
open Printf
open Lexing
open Core.Std
open Analysis



let cur_dir = Sys.getcwd ()

let file_path = cur_dir ^ "/test/samples/test_cases"


let get_full_paths base_path file_lst = 
  List.map file_lst
      ~f:(fun file_str -> base_path ^ "/" ^ file_str)

let all_files = 
  get_full_paths file_path (Sys.ls_dir file_path)

let read_from_file filename = 
  let inx = In_channel.read_all filename in
  let lexbuf = Lexing.from_string inx in
   lexbuf

let print_position lexbuf = 
  let pos = lexbuf.lex_curr_p in
  Printf.printf "Parsing error %s:%d:%d" pos.pos_fname 
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)


let parse_with_error (lexbuf : Lexing.lexbuf) =
  let ast = Parser.prog Lexer.read lexbuf in
  let frags = Analysis.trans_prog ast in 
  frags
  (*with*)
    (*| Lexer.SyntaxError _  -> print_position lexbuf*)
    (*| Parser.Error ->*)
        (*print_position lexbuf;*)
        (*exit (-1)*)

let sample = 
  "let
    var N := 8

    type intArray = array of int

    var row := intArray [N] of 0
    var col := intArray [N] of 0
    var diag2 := intArray [N+N-1] of 0
in 
    3
end
"
let parse filename = 
  let lexbuf = read_from_file filename in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
  parse_with_error lexbuf


(*let test_all () = *)
  (*List.iter all_files *)
    (*~f:(fun filename -> parse filename)*)


let () = parse_with_error (Lexing.from_string sample)

(*let () = test_all ()*)
