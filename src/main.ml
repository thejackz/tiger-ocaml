(*#directory "_build";;*)
(*#load "lexer.cmo";;*)
(*#load "parser.cmo";;*)
(*#load "main.cmo";;*)

open Lexing
open Core.Std
open Ast
open Datatypes
open Env
open Analysis
open Symbol
open Temp
open Tree
open Translate
open Canonical
open Graph
open Assem
open Frame
(*open Codegen*)


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

let type_check ast = 
  Analysis.trans_prog ast




