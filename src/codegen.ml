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
    | MOVE (MEM (BINOP (PLUS, e1, CONST i)), e2) 
    | MOVE (MEM (BINOP (PLUS, CONST i, e1)), e2) ->
        let lhs, rhs = munch_exp e1, munch_exp e2 in
        OPER ((P.sprintf "sw `s0, %d(`s1) " i), 
               [lhs],
               [rhs], None) 
        |> emit
    | MOVE (MEM (CONST i), e) ->
        OPER ((P.sprintf "sw `s0, %d($r0)" i),
              [],
              [munch_exp e], None)
      |> emit
    | MOVE (MEM (e1), e2) ->
        OPER ((P.sprintf "sw `s0, 0(`s1)"),
              [munch_exp e1],
              [munch_exp e2], None)
        |> emit
    | MOVE (TEMP t, e) ->
        OPER ((P.sprintf "move `t0, `t1"),
              [t],
              [munch_exp e], None)
      |> emit
    | LABEL lab -> LABEL (Symbol.name lab ^ ":\n", lab) |> emit




    


  and munch_exp exp : Temp.temp =
    failwith ""



end
