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


  let rec munch_stm stm = 
    match stm with
    | SEQ (s1, s2) -> munch_stm s1; munch_stm s2

    | MOVE (MEM (BINOP (PLUS, e1, CONST i)), e2) 
    | MOVE (MEM (BINOP (PLUS, CONST i, e1)), e2) ->
        let lhs, rhs = munch_exp e1, munch_exp e2 in
        OPER ((P.sprintf "lw `s1, `s0 + %d " i), 
               [lhs; rhs],
               [],
               None) 
        |> emit

  and munch_exp exp =
    failwith ""



end
