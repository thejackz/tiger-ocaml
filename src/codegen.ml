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

  let codegen frame stm = failwith ""

  let trans_binop ?im ?two_reg op = 
    match op, im, two_reg with
    | PLUS, Some true, Some false -> "addi"
    | MINUS, Some true, Some false -> "subi"
    | PLUS, Some false, Some true -> "add"
    | MINUS, Some false, Some true -> "sub"
    | MUL, _, Some true -> "mul"
    | DIV, _, Some true -> "div"
    | AND, Some true, Some false -> "addi"
    | AND, Some false, Some true -> "and"
    | OR, Some true, Some false -> "ori"
    | OR, Some false, Some true -> "or"
    | LSHIFT, _, _ -> "sll"
    | RSHIFT, _, _ -> "srl"
    | ARSHIFT, _, _ -> "srav"
    | XOR, _, _ -> "xor"
    | OR, _, _  
    | AND, _, _ 
    | MUL, _, _
    | DIV, _, _ 
    | PLUS, _, _ 
    | MINUS, _, _ -> failwith "impossible"


  let rec munch_stm stm : unit = 
    match stm with
    | SEQ (s1, s2) -> munch_stm s1; munch_stm s2

     | EXP (CALL (NAME lab, args)) ->
        OPER ((P.sprintf "jal %s" (Symbol.name lab)),
              []
          ) 

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
    | JUMP (e, _) -> OPER(P.sprintf "jr `r0", [], [munch_exp e], None) |> emit


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
        result (fun r -> OPER ((P.sprintf "lw `d0, 0(`s0)"),
                                [r], [munch_exp e], None)
                              |> emit)

    | CONST i ->
        result (fun r -> OPER ((P.sprintf "li `d0, %d" i),
                               [r], [], None)
                         |> emit)

    | BINOP (op, e, CONST i) 
    | BINOP (op, CONST i, e) ->
        let oper = trans_binop op ~im:true ~two_reg:false in
        result (fun r -> OPER ((P.sprintf "%s `d0, %d(`s0)" oper i),
                                [r], [munch_exp e], None)
                          |> emit)

    | BINOP (op, e1, e2) ->
        let oper = trans_binop op ~two_reg:true ~im:false in
        result (fun r -> OPER ((P.sprintf "%s `d0, `s1, `s2" oper),
                               [r], [munch_exp e1; munch_exp e2], None)
                          |> emit)

    | TEMP t -> t
    | NAME name ->
        result (fun r -> OPER ((P.sprintf "la `d0, %s" (Symbol.name name)),
                                [r], [], None)
                          |> emit)



  and munch_args i args col = 
    match args with
    | [] -> List.rev col
    | hd :: tl ->
        match (i >= 0 and i <= 3) with
        | true -> 
            let reg = F.get_reg ("$a" ^ (string_of_int i)) in
            (match hd with
            | CONST const -> 
                OPER ((P.sprintf "li $a%d, %d" i const),
                       [reg], [], None)
                |> emit;
                munch_args (i + 1) tl (reg :: col)
            | _ ->
                OPER ((P.sprintf "move $a%d, `s0" i),
                      [reg],
                      [munch_exp e], None)
                |> emit;
                munch_args (i + 1) tl (reg :: col))
        | false ->
            















end
