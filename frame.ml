open Temp
open Core.Std

module type FRAME = sig


  (* frame is used to describe functions. What is the name? What are the formal parameter?
   * and what are its local variables
   *)
  type frame

  (*
   *   The access type describe formals and locals that might
   *   be in the frame or in the register
   *)
  type access

  type frame_arg = {name: Temp.label; escapes: bool list} 

  val new_frame : frame_arg -> frame

  val name : frame -> Temp.label

  val formals : frame -> access list

  val alloc_locals : frame -> bool -> access

end

module MISP : FRAME = struct

  let loc = ref 0

  let word_size = 4

  type offset = int
  
  type access = 
    | In_frame of offset
    | In_reg of Temp.temp

  type frame_arg = { name: Temp.label; escapes: bool list } 

  type frame = {
    name            : Temp.label;       (* name of the frame *)
    length          : int ;             (* number of parameter *)
    formals         : access list;      (* for each parameter, are they in mem or reg ?*)
    mutable locals  : access list;      (* local variables *)
  }


  let gen_offset () = 
    let res = !loc * word_size in
    loc := !loc + 1; res

  let new_frame frame_arg  = 
    let fs = List.map frame_arg.escapes
      ~f:(fun t -> if t then (In_frame (gen_offset ())) else In_reg (Temp.new_temp ()))
    in
    let l = List.length fs in
    {
      name    = frame_arg.name;
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


end
