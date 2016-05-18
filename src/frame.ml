open Temp
open Core.Std
open Tree

module type FRAME = sig


  (* frame is used to describe functions. What is the name? What are the formal parameter?
   * and what are its local variables
   *)
  type frame

  type register

  (*
   *   The access type describe formals and locals that might
   *   be in the frame or in the register
   *)
  type access 

  type frag

  type reg_table

  val word_size : int

  val input_reg_num : int

  val new_frame : Temp.label -> bool list -> frame

  val name : frame -> Temp.label

  val get_formals : frame -> access list

  val alloc_locals : frame -> bool -> access
  
  val calc_texp : Tree.exp -> access -> Tree.exp

  val fp : Temp.temp

  val rv : Temp.temp

  val make_fragstring : Temp.label -> string -> frag

  val make_fragproc : Tree.stm -> frame -> frag

  val malloc : Temp.label

  val init_array : Temp.label

  val external_call : string -> Tree.exp list -> Tree.exp

  val get_reg: reg_table -> register -> Temp.temp

end

module MISP : FRAME = struct

  
  module T = Tree
  

  let loc = ref 0

  let word_size = 4

  type offset = int

  type register = string

  
  module Reg_map = Map.Make(
    struct
      type t = string with sexp, compare
    end)

  type reg_table = Temp.temp Reg_map.t

  let get_reg table reg = failwith ""

  type access = 
    (* offset from the frame pointer *)
    | In_frame of offset
    | In_reg of Temp.temp

  type frame = {
    name            : Temp.label;       (* name of the frame *)
    length          : int ;             (* number of parameter *)
    formals         : access list;      (* for each parameter, are they in mem or reg ?*)
    mutable locals  : access list;      (* local variables *)
  }

  type frag = 
    | Proc   of Tree.stm * frame  (* body and frame *)
    | String of Temp.label * string 

  let make_fragstring lab str = String (lab, str)
  let make_fragproc stm body = Proc (stm, body)

  let word_size = 4

  let malloc = Temp.named_label "malloc"

  let init_array = Temp.named_label "init_array"

  let gen_offset () = 
    let res = !loc * word_size in
    loc := !loc - 1; res


  (* Assumption: first four parameter will be passed into a0 - a3, 
     the rest will be passed on stack.

     Escaped parameters will be copied BELOW frame pointer.
     Non-escaped parameters will be copied to temp registers
  *)
  let new_frame name escapes  = 

    let fs = List.fold_left escapes
      ~init:([], 0)
      ~f:(fun (acc, index) escape -> match escape with
            | true -> (In_frame (index * word_size)) ::  acc, index - 1 
            | false -> (In_reg (Temp.new_temp ())) :: acc, index)
    in
    let l = List.length fs in
    {
      name    = name;
      length  = l;
      formals = List.rev fs;
      locals  = [];
    }


  let name frame = frame.name

  let get_formals frame = frame.formals

  let alloc_locals frame escape = 
    match escape with
    | true   -> In_frame (gen_offset ()) 
    | false  -> In_reg (Temp.new_temp ())


  let registers = [
    "s0"; "s1"; "s2"; "s3"; "s4"; "s5"; "s6"; "s7";
    "t0"; "t1"; "t2"; "t3"; "t4"; "t5"; "t6"; "t7"; "t8"; "t9";
    "a0"; "a1"; "a2"; "a3"; "v0"; "v1";
    "gp"; "fp"; "sp"; "ra"; "at"; "zero"; 
  
  ]

  let sepecial_regs = [
    "zeor"; "gp"; "sp"; "fp"; "ra"; "v0"; "v1"
  ]


  let callee_saved = [
    "s0"; "s1"; "s2"; "s3"; "s4"; "s5"; "s6"; "s7";
  ]

  let caller_saved = [
    "t0"; "t1"; "t2"; "t3"; "t4"; "t5"; "t6"; "t7"; "t8"; "t9";
  ]

  let arg_regs = ["a0"; "a1"; "a2"; "a3"]

  let is_caller_saved reg = 
    List.mem caller_saved reg 

  let is_callee_saved reg = 
    List.mem callee_saved reg

  let input_reg_num = List.length arg_regs
                  
  (**
   *  this function takes an tree.exp and frame.access
   *  the tree.exp is the base address and the frame.access
   *  is the offset if its in the frame
   *  and return another tree.access.
   *)
  let calc_texp base access = 
    match access with
    | In_reg reg -> T.TEMP reg
    | In_frame offset -> T.MEM (T.BINOP (T.PLUS, base, T.CONST offset))

  let fp = failwith "unimplemented"

  let rv = failwith "unimplemented"

  let runtime_funcs = []


  let external_call name args = 
    match List.mem runtime_funcs name with
    | false -> failwith "Undefined function call"
    | true  -> T.(CALL (NAME (named_label name), args))


  let proc_entry_exit1 (frame : frame) body_stm =
    let rec seq = function
      | [a; b] -> T.SEQ (a, b)
      | a :: b :: l -> T.SEQ (T.SEQ (a, b), seq(l))
      | [a] -> a
      | [] -> T.EXP (T.CONST 450)
    in

    (* Move formal parameters to proper positions*)
    let rec view_shift formals ~reg_counter ~frame_counter = 
      List.(rev (fold_left formals
        ~init:([], reg_counter, frame_counter)
        ~f:(fun (acc, reg_counter, frame_counter) formal ->)
          match hd with
          | In_reg reg -> 
              let assembly = MOVE (TEMP (Temp.new_temp ()), get_reg ("a" ^ (string_of_int reg_counter))) in
              assembly :: acc, reg_counter + 1, frame_counter
          | In_frame offset ->
              let assembly = MOVE (MEM (BINOP (PLUS, fp, offset)), BINOP (PLUS, fp, frame_counter * word_size)) in
              assembly :: acc, reg_counter, frame_counter + 1))
    in
    
    let saved_regs = ra :: callee_saved in
    let temps = List.map saved_regs ~f:(fun _ -> Temp.new_temp ()) in
    let regs_save_intrs = List.map2_exn temps saved_regs ~f:(fun temp reg -> MOVE (TEMP temp, TEMP reg)) in 
    let regs_restore_intrs = List.map2_exn saved_regs temps ~f:(fun reg temp -> MOVE (TEMP reg, TEMP temp)) in 
    let body' = [regs_save_intrs, body_stm, regs_restore_intrs] in
    match get_formals formals with
    | [] -> seq body'
    | _ as args -> T.SEQ (view_shift args ~reg_counter:0 ~frame_counter:0 |> seq, body') 





end
