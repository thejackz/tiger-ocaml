open Temp
open Translate
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
  type access = 
    | In_frame of int
    | In_reg of Temp.temp

  type frag

  val word_size : int

  val new_frame : Temp.label -> bool list -> frame

  val name : frame -> Temp.label

  val formals : frame -> access list

  val alloc_locals : frame -> bool -> access
  
  val calc_texp : Tree.exp -> access -> Tree.exp

  val fp : Temp.temp

  val make_fragstring : Temp.label -> string -> frag

  val make_fragproc : Tree.stm -> frame -> frag

  val malloc : Temp.label

end

module MISP : FRAME = struct
  
  module T = Tree

  let loc = ref 0

  let word_size = 4

  type offset = int

  type register = string
  
  type access = 
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

  let gen_offset () = 
    let res = !loc * word_size in
    loc := !loc + 1; res

  let new_frame name escapes  = 
    let fs = List.map escapes
      ~f:(fun t -> if t then (In_frame (gen_offset ())) else In_reg (Temp.new_temp ()))
    in
    let l = List.length fs in
    {
      name    = name;
      length  = l;
      formals = fs;
      locals  = [];
    }

  let name frame = frame.name

  let formals frame = frame.formals

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


end
