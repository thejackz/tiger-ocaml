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

  val word_size : int

  val input_reg_num : int

  val new_frame : Temp.label -> bool list -> frame

  val name : frame -> Temp.label

  val get_formals : frame -> access list

  val alloc_locals : frame -> bool -> access
  
  val calc_texp : Tree.exp -> access -> Tree.exp

  val caller_saved : Temp.temp list

  val callee_saved : Temp.temp list

  val arg_regs : Temp.temp list

  val fp : Temp.temp 

  val rv : Temp.temp

  val ra : Temp.temp

  val sp : Temp.temp

  val zero : Temp.temp

  val make_fragstring : Temp.label -> string -> frag

  val make_fragproc : Tree.stm -> frame -> frag

  val malloc : Temp.label

  val init_array : Temp.label

  val external_call : string -> Tree.exp list -> Tree.exp

  val get_reg : Temp.temp -> string


end

module MISP : FRAME = struct

  
  module T = Tree
  
  let loc = ref 0

  let word_size = 4

  type offset = int

  type register = string

  let get_reg reg = failwith ""

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

    let fs, _ = List.fold_left escapes
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


  let label_registers label regs = 
    List.mapi regs ~f:(fun i reg -> reg, label ^ (Temp.temp_to_string reg))

  let callee_saved = List.init 8 ~f:(fun i -> Temp.new_temp ())

  let caller_saved = List.init 10 ~f:(fun i -> Temp.new_temp ())

  let arg_regs = List.init 4 ~f:(fun i -> Temp.new_temp ())

  let fp = Temp.new_temp ()

  let ra = Temp.new_temp ()

  let rv = Temp.new_temp ()

  let sp = Temp.new_temp ()

  let zero = Temp.new_temp ()

  let reg_table = List.fold_left 
    ([(fp, "$fp"); (ra, "$ra"); (rv, "$rv"); (sp, "$sp"); (zero, "$zero")] @ 
      label_registers "$s" callee_saved @ 
      label_registers "$t" caller_saved @ 
      label_registers "$a" arg_regs)
    ~init:Temp.TempMap.empty
    ~f:(fun table (temp, str) -> Temp.TempMap.add temp str table)

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


  let runtime_funcs = []


  let external_call name args = 
    match List.mem runtime_funcs name with
    | false -> failwith "Undefined function call"
    | true  -> T.(CALL (NAME (named_label name), args))


  let proc_entry_exit1 frame body =
    let rec seq = function
      | [a; b] -> T.SEQ (a, b)
      | a :: b :: l -> T.SEQ (T.SEQ (a, b), seq(l))
      | [a] -> a
      | [] -> T.EXP (T.CONST 450)
    in

    (* Move formal parameters to proper positions*)
    let view_shift (formals : access list) ~reg_counter ~frame_counter : Tree.stm list = 
      let insts, _, _ = List.fold_left formals
        ~init:([], reg_counter, frame_counter)
        ~f:(fun (acc, reg_counter, frame_counter) formal ->
          match formal with
          | In_reg reg -> 
              let assembly = MOVE (TEMP (Temp.new_temp ()), get_reg ("a" ^ (string_of_int reg_counter))) in
              assembly :: acc, reg_counter + 1, frame_counter
          | In_frame offset ->
              let formal_offset = frame_counter * word_size in
              let assembly = MOVE (MEM (BINOP (PLUS, TEMP fp, CONST offset)), 
                                   BINOP (PLUS, TEMP fp, CONST formal_offset)) 
              in
              assembly :: acc, reg_counter, frame_counter + 1)
      in insts
    in
    
    let saved_regs = List.map ~f:(fun str -> get_reg str) (ra :: callee_saved) in
    let temps = List.map saved_regs ~f:(fun _ -> Temp.new_temp ()) in
    let regs_save_intrs = List.map2_exn temps saved_regs ~f:(fun temp reg -> MOVE (TEMP temp, TEMP reg)) in 
    let regs_restore_intrs = List.map2_exn saved_regs temps ~f:(fun reg temp -> MOVE (TEMP reg, TEMP temp)) in 
    let body' = [seq regs_save_intrs; body; seq regs_restore_intrs] in
    match get_formals frame with
    | [] -> seq body'
    | _ as args -> T.SEQ (view_shift args ~reg_counter:0 ~frame_counter:0 |> seq, seq body') 

  let proc_entry_exit2 frame body = 
    body @ 
      [Assem.OPER ("", [], [zero; rv; ra; sp] , Some [])]





end
