(*#directory "_build";;*)
(*#load "lexer.cmo";;*)
(*#load "parser.cmo";;*)
(*#load "main.cmo";;*)

open Lexing
open Core.Std
open Ast
open Datatypes
open Env
open Symbol
open Frame
open Temp
open Tree
(*open Translate*)


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

