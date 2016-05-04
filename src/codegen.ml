module type CODEGEN = sig
  module F : Frame.FRAME

  val codegen : F.frame -> Tree.stm -> Assem.instr list 

end 


module MispCodegen : CODEGEN = struct

  module F = Frame.MISP
  module P = Printf
  open Tree
  open Assem

  let instr_list : instr list ref = ref []

  let emit instr = instr_list := instr :: !instr_list

  let codegen = failwith ""


  let rec munch_stm stm : unit = 
    match stm with
    | SEQ (s1, s2) -> munch_stm s1; munch_stm s2
    | EXP e -> munch_exp e; ()


    (* data movement, store to memory.  sw *)
    | MOVE (MEM (BINOP (PLUS, e1, CONST i)), e2) 
    | MOVE (MEM (BINOP (PLUS, CONST i, e1)), e2) ->
        let lhs, rhs = munch_exp e1, munch_exp e2 in
        OPER ((P.sprintf "sw `s0, %d(`s1) " i), 
               [lhs],
               [rhs], None) 
        |> emit
    | MOVE (MEM (CONST i), e) ->
        OPER ((P.sprintf "sw `s0, %d($zero)" i),
              [],
              [munch_exp e], None)
      |> emit
    | MOVE (MEM (e1), e2) ->
        OPER ((P.sprintf "sw `s0, 0(`s1)"),
              [munch_exp e1],
              [munch_exp e2], None)
        |> emit


    (* Write constant into register *)
    | MOVE (TEMP t, CONST i) ->
        OPER ((P.sprintf "li `rd, %d" i),
              [t], [], None)
        |> emit
    | MOVE (TEMP t, e) ->
        OPER ((P.sprintf "move `t0, `t1"),
              [t],
              [munch_exp e], None)
      |> emit

    | LABEL lab -> LABEL (Symbol.name lab ^ ":\n", lab) |> emit

    (* Unconditional jump *)
    | JUMP (NAME lab, _) -> OPER(P.sprintf "b lab", [], [], None) |> emit
    | JUMP (e, _) -> OPER(P.sprintf "jr `r0", [], [munch_stm e], None) |> emit



let trans_binop op ?im ?two_reg = 
  match im = two_reg with
  | true -> failwith "impossible"
  | false ->
      match op, im, two_reg with
      | PLUS, true, false -> "addi"
      | MINUS, true, false -> "subi"
      | PLUS, false, true -> "add"
      | MINUS, false, true -> "sub"
      | PLUS, _, _ 
      | MINUS, _, _ -> failwith "impossible"
      | MUL, _, true -> "mul"
      | DIV, _, true -> "div"
      | MUL, _, _
      | DIV, _, _ -> failwith "impossible"
      | AND, true, false -> "addi"
      | AND, false, true -> "and"
      | AND, _, _ -> failwith "impossible"
      | OR, true, false -> "ori"
      | OR, false, true -> "or"
      | OR, _, _ -> failwith "impossible"
      | LSHIFT, _, _ -> "sll"
      | RSHIFT, _, _ -> "srl"
      | ARSHIFT, _, _ -> "srav"
      | XOR, _, _, -> "xor"


    


  and munch_exp exp : Temp.temp =
    let result gen = let t = Temp.new_temp () in gen t; t in
    match exp with
    | MEM (BINOP (PLUS, e, CONST i))
    | MEM (BINOP (PLUS, CONST i, e)) ->
        result (fun r -> OPER ((P.sprintf "lw `d0, %d(`s0)" i),
                                [r], [munch_exp e], None)
                         |> emit)
    | MEM (CONST i) ->
        result (fun r -> OPER ((P.sprintf "lw `d0, %d($zero)" i),
                                [r], [], None)
                          |> emit)

    | MEM e ->
        result (fun r -> OPER ((P.sprintf "lw `d0, 0(`s0)"))
                                [r], [munch_exp e], None)
                              |> emit)

    | CONST i ->
        result (fun r -> OPER ((P.sprintf "li `d0, %d" i),
                               [r], [], None)
                         |> emit)

    | BINOP (op, e, CONST i) 
    | BINOP (op, CONST i, e) ->
        let oper = trans_binop oper ?im=true in
        result (fun r -> OPER ((P.sprintf "%s `d0, %d(`s0)" oper i),
                                [r], [munch_exp e], None)
                          |> emit)

    | BINOP (op, e1, e2) ->
        let oper = trans_binop oper ?two_reg=true in
        result (fun r -> OPER ((P.sprintf "%s `d0, `s1, `s2" oper),
                               [r], [munch_exp e1; munch_exp e2], None)
                          |> emit)









end
