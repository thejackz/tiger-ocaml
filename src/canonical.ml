module T = Tree

let linearize stm = 

  let rec do_stm stm = 

  
  let commute e1 e2 = 
    match e1, e2 with
    | T.EXP (T.CONST _), _ -> true
    | _, T.NAME _ -> true
    | _, T.CONST _ -> true
    | _, _ -> false
  in


  let rec linear stm l = 
    match stm with
    | T.SEQ (a, b) -> linear a (linear b l)
    | _ -> stm :: l
  in linear (do_stm stm, [])

