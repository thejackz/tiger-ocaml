
type exp = 
  | CONST of int
  | NAME of Temp.label
  | TEMP of Temp.temp
  | BINOP of binop * exp * exp
  | MEM of exp
  | CALL of exp * exp list
  | ESEQ of stm * exp

and stm = 
  | MOVE of exp * exp
  | EXP of exp
  | JUMP of exp * Temp.label list
  | CJUMP of relop * exp * exp * Temp.label * Temp.label
  | SEQ of stm * stm
  | LABEL of Temp.label

and binop = 
  | PLUS | MINUS | MUL | DIV
  | AND | OR | LSHIFT | RSHIFT | ARSHIFT | XOR

and relop = 
  | EQ | NE | LT | GT | LE | GE
  | ULT | ULE | UGT | UGE 

let rev_relop = function
  | EQ -> NE
  | NE -> EQ
  | LT -> GT
  | GT -> LT
  | LE -> GE
  | GE -> LE
  | ULT -> ULE
  | ULE -> ULT
  | UGT -> UGE
  | UGE -> UGT

  let relop_to_string = function
  | EQ -> "="
  | NE -> "!="
  | LT -> "<"
  | GT -> ">"
  | LE -> "<="
  | GE -> ">="
  | ULT -> "<"
  | ULE -> "<="
  | UGT -> ">"
  | UGE -> ">="

let binop_to_string = function
  | PLUS    -> "+"
  | MINUS   -> "-"
  | MUL     -> "*"
  | DIV     -> "/"
  | AND     -> "and"
  | OR      -> "or"
  | LSHIFT  -> "<<"
  | RSHIFT  -> ">>"
  | ARSHIFT -> ">>>"
  | XOR     -> "xor"


let rec exp_to_doc e =
  let open Pprint in
  let open Printf in
  match e with
  | CONST n -> text (string_of_int n)
  | NAME l -> text (sprintf "NAME(%s)" (Temp.label_to_string l))
  | TEMP t -> text (Temp.temp_to_string t)
  | BINOP (op, e0, e1) ->
    text "BINOP("
    <-> nest 6 (text (binop_to_string op)
                <-> exp_to_doc e0
                <-> exp_to_doc e1
                <-> text ")")
  | MEM (m) ->
    text "MEM(" <-> nest 4 (exp_to_doc m <-> text ")")
  | CALL (f, args) ->
    text "CALL("
    <-> nest 5 (exp_to_doc f <-> text ","
                <-> line
                <-> concat (text ",") (List.map exp_to_doc args)
                <-> text ")")
  | ESEQ (s, e) ->
    text "ESEQ("
    <-> nest 5 (stmt_to_doc s <-> text ","
                <-> line
                <-> (exp_to_doc e)
                <-> (text ")"))

and stmt_to_doc stmt =
  let open Pprint in
  let open Printf in
  match stmt with
  | MOVE (dst, src) ->
    text "MOVE("
    <-> nest 5 (exp_to_doc dst
                <-> text ", "
                <-> exp_to_doc src
                <-> text ")")
  | EXP e ->
    text "EXP("
    <-> nest 4 (exp_to_doc e)
    <-> nest 4 (text ")")
  | JUMP (e, ls) ->
    text "JUMP("
    <-> nest 5 (exp_to_doc e <-> text ",")
    <-> nest 5 (concat (text "," <-> line)
                  (List.map (fun l ->
                       text (Temp.label_to_string l)) ls))
    <-> nest 5 (text ")")
  | CJUMP (op, e0, e1, t, f) ->
    text "CJUMP("
    <-> nest 6 (
      (text (relop_to_string op) <-> text ",")
      <-> line
      <-> (exp_to_doc e0) <-> text ","
      <-> line
      <-> (exp_to_doc e1) <-> text ","
      <-> line
      <-> (text (Temp.label_to_string t)) <-> text ", "
      <-> (text (Temp.label_to_string f))
      <-> text ")")
  | SEQ (s1, s2) ->
    text "SEQ("
    <-> nest 4 (stmt_to_doc s1 <-> text ","
                <-> line
                <-> stmt_to_doc s2 <-> text ")")
  | LABEL (l) ->
    text (sprintf "LABEL(%s)" (Temp.label_to_string l))

let rec exp_to_string e =
  exp_to_doc e |> Pprint.layout

and stmt_to_string stmt =
  stmt_to_doc stmt |> Pprint.layout
