open Core.Std 

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


  let trans_binop ?im ?two_reg op = 
    match op, im, two_reg with
    | PLUS, Some true, _ -> "addi"
    | MINUS, Some true, _ -> "subi"
    | PLUS, _, Some true -> "add"
    | MINUS, _, Some true -> "sub"
    | MUL, _, Some true -> "mul"
    | DIV, _, Some true -> "div"
    | AND, Some true, _  -> "andi"
    | AND, Some false, Some true -> "and"
    | OR, Some true, _ -> "ori"
    | OR, _, Some true -> "or"
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


  let get_branch0_op op = 
    match op with
    | EQ -> Some "beqz"
    | NE -> Some "bnez"
    | LT -> Some "bltz"
    | LE -> Some "blez"
    | GT -> Some "bgtz"
    | GE -> Some "bgez"
    | _  -> None

  let get_relop op =
    match op with
    | EQ  -> "beq"
    | NE  -> "bne"
    | LT  -> "blt"
    | LE  -> "ble"
    | GT  -> "bgt"
    | GE  -> "bge"
    | ULT -> "bltu"
    | ULE -> "bleu"
    | UGT -> "bgtu"
    | UGE -> "bgeu"


  let rec munch_stm stm : unit = 
    match stm with

    (************************************************************)
    (* Sequence *)
    | SEQ (s1, s2) -> munch_stm s1; munch_stm s2

    (************************************************************)
    (* Expression. Discard returned register *) 
    | EXP e -> ignore (munch_exp e); ()

    (************************************************************)
    (* Unconditional jump *)
    | JUMP (NAME lab, _) -> OPER(P.sprintf "b lab", [], [], Some [lab]) |> emit
    | JUMP (e, labs) -> OPER(P.sprintf "jr `r0", [], [munch_exp e], Some labs) |> emit

    (************************************************************)
    (* Branch condition *)
    | CJUMP (relop, CONST 0, e, b1, b2) -> munch_stm (CJUMP (Tree.rev_relop relop, e, CONST 0, b1, b2))
    | CJUMP (relop, e, CONST 0, b1, b2) ->
        (match get_branch0_op relop with
        | Some op -> OPER (op ^ " `s0, `j0",
                           [], [munch_exp e], Some [b1; b2])
                     |> emit
        | None -> OPER ((get_relop relop) ^ " `s0, `s1, `j0",
                        [], [munch_exp e; F.zero], Some [b1; b2])
                        |> emit)
    | CJUMP (relop, e1, e2, b1, b2) ->
        OPER ((get_relop relop) ^ " `s0, `s1, `j0",
              [], [munch_exp e1; munch_exp e2], Some [b1; b2])
        |> emit

    (************************************************************)
    (* data movement, store to memory.  sw *)
    | MOVE (MEM (BINOP (PLUS, e1, CONST i)), e2) 
    | MOVE (MEM (BINOP (PLUS, CONST i, e1)), e2) ->
        let lhs, rhs = munch_exp e1, munch_exp e2 in
        OPER ((P.sprintf "sw `s0, %d(`s1) " i), 
               [],
               [lhs; rhs], None) 
        |> emit
    | MOVE (MEM (CONST i), e) ->
        OPER ((P.sprintf "sw `s0, %d($zero)" i),
              [],
              [munch_exp e], None)
      |> emit

    (* General to-memory move *)
    | MOVE (MEM (e1), e2) ->
        OPER ("sw `s0, 0(`s1)",
              [],
              [munch_exp e1; munch_exp e2], None)
        |> emit


    (************************************************************)
    (* Write into register MOVE (TEMP t, ... ) *)
    | MOVE (TEMP t, CONST i) ->
        OPER ((P.sprintf "li `d0, %d" i),
              [t], [], None)
        |> emit
    | MOVE (TEMP t, MEM (BINOP (PLUS, e, CONST i)))
    | MOVE (TEMP t, MEM (BINOP (PLUS, CONST i, e)))->
        OPER ((P.sprintf "lw `d0, %d(`s0)" i),
              [t], [munch_exp e], None)
        |> emit
    | MOVE (TEMP t, MEM (BINOP (MINUS, e, CONST i))) ->
        OPER ((P.sprintf "lw `d0, -%d(`s0)" i),
              [t], [munch_exp e], None)
        |> emit
    | MOVE (TEMP t, NAME name) ->
        OPER ((P.sprintf "la `d0, %s" (Symbol.name name)),
                                [t], [], None)
        |> emit
    | MOVE (TEMP t, BINOP (op, CONST i, e)) 
    | MOVE (TEMP t, BINOP (op, e, CONST i)) -> 
        let op' = trans_binop ~im:true op in
        OPER ((P.sprintf "%s `d0, `s0, %d" op' i),
              [t], [munch_exp e], None)
        |> emit
    | MOVE (TEMP t, BINOP (op, e1, e2)) ->
        let op' = trans_binop ~two_reg:true op in
        OPER ((P.sprintf "%s `d0, `s0, `s1" op'),
              [t], [munch_exp e1; munch_exp e2], None)
        |> emit

    (* General to-register move *)
    | MOVE (TEMP t, e) ->
        Assem.MOVE ((P.sprintf "move `t0, `t1"),
              t,
              munch_exp e)
      |> emit

    | MOVE (_, _) -> failwith "MOVE error"

    (************************************************************)
    | LABEL lab -> LABEL (Symbol.name lab ^ ":\n", lab) |> emit

    (************************************************************)
    
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

    | CALL (exp, args) ->

          let src_regs = List.map2_exn args F.arg_regs ~f:(fun _ input_reg -> input_reg) in
          
          (* First save all caller-saved register *)
          let save_temps = List.map F.caller_saved ~f:(fun _ -> Temp.new_temp ()) in
          let save_reg_instrs = List.map2_exn save_temps F.caller_saved
            ~f:(fun temp reg -> Tree.MOVE (TEMP temp, TEMP reg))
          in

          (* save return address register *)
          let ra_temp, rv_temp = Temp.new_temp (), Temp.new_temp () in
          let save_ra_instr = Tree.MOVE (TEMP ra_temp, TEMP F.ra) in
          let save_rv_instr = Tree.MOVE (TEMP rv_temp, TEMP F.rv) in

          (* Push caller-saved register and ra rv, ect *)
          let _ = 
            save_ra_instr :: save_rv_instr :: save_reg_instrs |> Translate.seq |> munch_stm 
          in 

        (* Push input parameters into input registers or on stack *)
        let _, _ = List.fold_left args
          ~init:(0, 1)
          ~f:(fun (rindex, findex) arg ->
                match rindex <= 3 with
                | true -> 
                    let arg_reg = List.nth_exn F.arg_regs rindex in
                    let instr = match arg with 
                      | CONST c -> 
                          OPER (P.sprintf "li `d0, %d" c, [arg_reg], [], None)
                      | _ -> 
                          Assem.MOVE ("move `d0, `s0", arg_reg, munch_exp arg)
                    in emit instr;     
                    (rindex + 1, findex)
                | false ->
                    let offset = findex * F.word_size in
                    OPER (P.sprintf "sw `s0, %d(`d0)" offset, [F.fp], [munch_exp arg], None)
                    |> emit;
                    (rindex, findex + 1))
          in
          codegen_call exp src_regs |> emit;
          F.rv
    | ESEQ _ -> failwith "ESEQ should not appear in code generator"


  and codegen_call exp src_regs = 
    match exp with
    | NAME fname ->
        let dst = F.caller_saved @ F.arg_regs in
        OPER (P.sprintf "jal %s" (Symbol.name fname), 
              F.rv :: dst,
              F.sp :: src_regs, 
              None)
    | _ ->
        let dst = F.caller_saved @ F.arg_regs in
        OPER ("jalr `s0", 
              F.rv :: dst, 
              munch_exp exp :: F.sp :: src_regs, 
              None)

  let codegen frame stm =
    munch_stm stm;
    List.rev !instr_list
end
