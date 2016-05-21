open Env
open Translate
open Datatypes
open Ast
open Symbol
open Core.Std


module D = Datatypes
module S = Symbol
module T = Translate
module E = Env

type venv = Env.env_type Symbol.table
type tenv = ty Symbol.table

type expty = Translate.exp * Datatypes.datatype

let actual_type ty = 
  match ty with
  | NAME (_, {contents = Some t}) -> t
  | _ -> ty

let actual_type_lst type_lst = 
  List.map type_lst
    ~f:(fun ty -> actual_type ty)

let rec trans_exp (cur_level : T.level) (break_to : Temp.label option)(venv : Env.env_type Symbol.table)
                  (tenv : Env.ty Symbol.table) (ast : Ast.exp) : expty = 
  match ast with 
  | Nil                                         -> T.trans_nil (), D.NIL
  | Int i                                       -> T.trans_int i, D.INT
  | String s                                    -> T.trans_string s, D.STRING
  | Break                                       -> trans_break break_to
  | Lvalue lv                                   -> trans_lvalue cur_level break_to venv tenv lv
  | Exp_seq exp_seq                             -> trans_exp_seq cur_level break_to venv tenv exp_seq
  | Negation_exp e                              -> trans_neg_exp cur_level break_to venv tenv e
  | Call_exp (fname, params)                    -> trans_call_exp cur_level break_to venv tenv fname params 
  | Arr_create (type_id, size, init)            -> trans_arr_create cur_level break_to venv tenv type_id size init
  | Rec_create (rec_type, field_lst)            -> trans_rec_create cur_level break_to venv tenv rec_type field_lst
  | Assignment (lhs, rhs_exp)                   -> trans_assignment cur_level break_to venv tenv lhs rhs_exp
  | Ifthenelse (test, then_e, else_e)           -> trans_ifthenelse cur_level break_to venv tenv test then_e else_e
  | Ifthen (test, then_e)                       -> trans_ifthen cur_level break_to venv tenv test then_e
  | Whileexp (cond, do_exp)                     -> trans_whileexp cur_level break_to venv tenv cond do_exp
  | Forexp (id, escape, first, last, do_exp)    -> trans_forexp cur_level break_to venv tenv id first last do_exp
  | Letexp (decls, body)                        -> trans_letexp cur_level break_to venv tenv decls body
  | ArithExp arith_exp                          -> trans_ariths cur_level break_to venv tenv arith_exp
  | BoolExp bool_exp                            -> trans_boolexp cur_level break_to venv tenv bool_exp
  | CmpExp cmp_exp                              -> trans_cmpexp cur_level break_to venv tenv cmp_exp

and trans_break break_to = 
  match break_to with
  | None -> failwith "break is not in a loop"
  | Some label -> T.trans_break label, Datatypes.UNIT

and trans_cmpexp cur_level break_to venv tenv cmp_exp = 
  match cmp_exp with
  | Eq (e1, e2)
  | Neq (e1, e2) ->
       (let e1_ir, e1_ty = trans_exp cur_level break_to venv tenv e1 in
       let e2_ir, e2_ty = trans_exp cur_level break_to venv tenv e2 in
       match e1_ty, e2_ty with
       | D.INT, D.INT 
       | D.RECORD _, D.RECORD _
       | D.ARRAY _, D.ARRAY _ -> T.trans_cmpexp cmp_exp e1_ir e2_ir, D.INT
       | _, _ -> failwith (Printf.sprintf "Incompatible type %s %s for comparison op"
                            (D.type_to_string e1_ty) (D.type_to_string e2_ty)))
  | Le (e1, e2)
  | Lt (e1, e2)
  | Ge (e1, e2)
  | Gt (e1, e2) ->
       (let e1_ir, e1_ty = trans_exp cur_level break_to venv tenv e1 in
       let e2_ir, e2_ty = trans_exp cur_level break_to venv tenv e2 in
       match e1_ty, e2_ty with
       | D.INT, D.INT  -> T.trans_cmpexp cmp_exp e1_ir e2_ir, D.INT
       | _, _ -> failwith (Printf.sprintf "Incompatible type %s %s for comparison op"
                            (D.type_to_string e1_ty) (D.type_to_string e2_ty)))

and trans_boolexp cur_level break_to venv tenv bool_exp = 
  match bool_exp with
  | Or (e1, e2) 
  | And (e1, e2) ->
       (let e1_ir, e1_ty = trans_exp cur_level break_to venv tenv e1 in
       let e2_ir, e2_ty = trans_exp cur_level break_to venv tenv e2 in
       match e1_ty = D.INT, e2_ty = D.INT with
       | true, true -> T.trans_boolexp bool_exp e1_ir e2_ir, D.INT
       | false, true -> failwith (Printf.sprintf "Expect e1 has type int, but get %s instead\n" 
                                    (D.type_to_string e1_ty))
       | true, false -> failwith (Printf.sprintf "Expect e2 has type int, but get %s instead\n" 
                                    (D.type_to_string e2_ty))
       | false, false -> failwith (Printf.sprintf "Expect e1, e2 has type int, but get %s %s instead\n" 
                                    (D.type_to_string e1_ty) (D.type_to_string e2_ty)))



and trans_ariths cur_level break_to venv tenv arith_exp = 
  match arith_exp with
  | Add (e1, e2)  
  | Sub (e1, e2)
  | Mul (e1, e2)
  | Div (e1, e2) ->
       (let e1_ir, e1_ty = trans_exp cur_level break_to venv tenv e1 in
       let e2_ir, e2_ty = trans_exp cur_level break_to venv tenv e2 in
       match e1_ty = D.INT, e2_ty = D.INT with
       | true, true -> T.trans_ariths arith_exp e1_ir e2_ir, D.INT
       | false, true -> failwith (Printf.sprintf "Expect e1 has type int, but get %s instead\n" 
                                    (D.type_to_string e1_ty))
       | true, false -> failwith (Printf.sprintf "Expect e2 has type int, but get %s instead\n" 
                                    (D.type_to_string e2_ty))
       | false, false -> failwith (Printf.sprintf "Expect e1, e2 has type int, but get %s %s instead\n" 
                                    (D.type_to_string e1_ty) (D.type_to_string e2_ty)))


and trans_forexp cur_level break_to venv tenv id first last body_exp = 
  let for_to_while id first last do_exp =
    let limit = Symbol.symbol_of_string "limit" in
    Letexp ([Var_decl (id, None, ref true, first); Var_decl (limit, None, ref true, last)],
             [Whileexp (CmpExp (Lt (Lvalue (Id id), Lvalue (Id limit))), 
                       Exp_seq [do_exp; Assignment ((Id id), (ArithExp (Add (Lvalue (Id id), Int 1))))])])
  in
  let low_ir, low_ty = trans_exp cur_level break_to venv tenv first in
  let high_ir, high_ty = trans_exp cur_level break_to venv tenv last in
  match low_ty, high_ty with
  | D.INT, D.INT ->
      (let while_exp = for_to_while id first last body_exp in
       trans_exp cur_level break_to venv tenv while_exp)
  | _, D.INT -> failwith (Printf.sprintf "Expectig first has type int, but get %s instead\n" 
                                    (D.type_to_string low_ty))
  | D.INT, _ -> failwith (Printf.sprintf "Expectig last has type int, but get %s instead\n" 
                                    (D.type_to_string high_ty))
  | _, _ -> failwith (Printf.sprintf "Expectig first, last has type int, but get %s and %s instead\n" 
                                    (D.type_to_string low_ty) (D.type_to_string high_ty))


and trans_whileexp cur_level break_to venv tenv cond body = 
  let cond_ir, cond_ty = trans_exp cur_level break_to venv tenv cond in
  match cond_ty with
  | D.INT -> 
      let breakpoint = Temp.new_label () in
      let body_ir, body_ty = trans_exp cur_level (Some breakpoint) venv tenv body in
      T.trans_whileexp (Some breakpoint) cond_ir body_ir, D.UNIT
  | _ -> failwith (Printf.sprintf "Expecting int type for cond but get %s instead " (D.type_to_string cond_ty))

and trans_assignment cur_level break_to venv tenv lhs rhs_exp = 
  let lhs_ir, lhs_type = trans_lvalue cur_level break_to venv tenv lhs in
  let rhs_ir, rhs_type = trans_exp cur_level break_to venv tenv rhs_exp in
  match lhs_type = rhs_type with
  | true -> T.trans_assignment lhs_ir rhs_ir, D.UNIT
  | false -> 
      let msg = Printf.sprintf "Lhs has type %s, but rhs has type %s" 
                  (D.type_to_string lhs_type) (D.type_to_string rhs_type)
      in failwith msg

and trans_ifthenelse cur_level break_to venv tenv test then_e else_e = 
  let test_ir, test_ty = trans_exp cur_level break_to venv tenv test in
  let then_ir, then_ty = trans_exp cur_level break_to venv tenv then_e in
  let else_ir, else_ty = trans_exp cur_level break_to venv tenv else_e in
  match test_ty, then_ty = else_ty with
  | D.INT, true -> 
      T.trans_ifthenelse test_ir then_ir else_ir, actual_type then_ty
  | D.INT, false -> failwith (Printf.sprintf "then exp and else exp have different type")
  | _, _ -> failwith (Printf.sprintf "Expecting test exp has type int, but get %s instead\n" 
                        (D.type_to_string test_ty))

and trans_ifthen cur_level break_to venv tenv test then_e = 
  let test_ir, test_ty = trans_exp cur_level break_to venv tenv test in
  let then_ir, then_ty = trans_exp cur_level break_to venv tenv then_e in
  match test_ty with
  | D.INT -> 
      T.trans_ifthen test_ir then_ir, actual_type then_ty
  | _  -> failwith (Printf.sprintf "Expecting test exp has type int, but get %s instead\n" 
                        (D.type_to_string test_ty))

and trans_rec_create cur_level break_to venv tenv rec_type field_lst = 
  match S.lookup tenv rec_type with
  | Some (D.RECORD (fields_ty, _) as record)  -> 
      let ir_lst = List.fold2_exn fields_ty field_lst
        ~init:[]
        ~f:(fun acc (decl_sym, decl_ty) (id, exp) ->
            let elt_ir, actual_ty = trans_exp cur_level break_to venv tenv exp in
            match decl_sym = id, decl_ty = actual_ty with
            | true, true -> elt_ir :: [] 
            | _, _ -> failwith "type mismatch")
      in T.trans_rec_create (List.rev ir_lst), record
  | Some t -> 
      let msg = Printf.sprintf "Expecting array type but get %s" (D.type_to_string t) in 
      failwith msg
  | None ->
      failwith (Printf.sprintf "%s is not defined" (S.name rec_type))

and trans_exp_seq cur_level break_to venv tenv exp_seq = 
  match exp_seq with
  | [] -> T.empty, D.UNIT
  | l  -> 
    let expty_lst = (List.map l ~f:(fun elt -> trans_exp cur_level break_to venv tenv elt )) in
    T.trans_seq (List.map expty_lst ~f:(fun (ir, ty) -> ir)), (actual_type (snd (List.last_exn expty_lst))) 


and trans_neg_exp cur_level break_to venv tenv neg_e = 
  match trans_exp cur_level break_to venv tenv neg_e with
  | ir, D.INT -> ir, D.INT
  | _    -> failwith "negation on non int type"

and trans_call_exp cur_level break_to venv tenv fname params = 
  match S.lookup venv fname with
  | Some (E.FUNC_TYPE (def_level, name, args, return_ty)) -> 
      (let params_irs = List.fold2_exn params args ~init:[] 
          ~f:(fun acc param arg_ty -> 
            let param_ir, param_ty = trans_exp cur_level break_to venv tenv param in
            match param_ty = arg_ty with
            | false -> failwith "functon call: type mismatch"
            | true  -> param_ir :: acc)
      in T.trans_funcall cur_level def_level (List.rev params_irs), return_ty)
  | Some (E.VAR_TYPE _) -> failwith (S.name fname ^ " is not a function")
  | None -> failwith (S.name fname ^ " is undefined")

and trans_arr_create cur_level break_to venv tenv type_id size init = 
  let size_ir, size_type = trans_exp cur_level break_to venv tenv size in
  let init_ir, init_type = trans_exp cur_level break_to venv tenv init in
  match S.lookup tenv type_id, size_type with
  | Some (D.ARRAY (ty, _) as arr), D.INT -> 
      (match ty = init_type with
      | false -> failwith ("Initial type is not compatible with declared type: " ^ D.type_to_string init_type)
      | true  -> T.trans_arrcreate size_ir init_ir, arr)
  | Some t, D.INT -> failwith ("expecting an array type but get " ^ D.type_to_string t)
  | Some t, s_t  -> failwith ("expecting an array type but get " ^ D.type_to_string t ^ " , expecting int but 
                                get " ^ D.type_to_string s_t)
  | None, _ -> failwith ((S.name type_id ) ^ " is undefined")

and trans_lvalue cur_level break_to venv tenv lv : expty = 
  match lv with
  | Id id -> 
      (match S.lookup venv id with
      | Some (E.VAR_TYPE (access, ty)) -> Translate.trans_id access cur_level, ty
      | Some (t) ->
          (failwith "Expecting a variable, but get a function")
      | None   -> (failwith ("Unknow id " ^ (S.name id))))
  | Subscript (lv, exp) -> 
      (let lv_ir, lv_type = trans_lvalue cur_level break_to venv tenv lv in
      let e_ir, exp_type = trans_exp cur_level break_to venv tenv exp in
       match lv_type, exp_type with
       | D.ARRAY (e_type, u), D.INT -> 
           Translate.trans_subscript lv_ir e_ir, (actual_type e_type)
       | _ as t, D.INT -> failwith ("expecting array but get " ^ (D.type_to_string t))
       | D.ARRAY _, t -> failwith ("expecting INT as index but get " ^ (D.type_to_string t))
       | vid, sub -> 
           failwith (("expecting array but get " ^ (D.type_to_string vid)) ^ ", " ^
                     ("expecting INT as index but get " ^ (D.type_to_string sub))))
  | Field_exp (lv, id) ->
      (let lv_ir, lv_type = trans_lvalue cur_level break_to venv tenv lv in
       match lv_type with
       | D.RECORD (fields, _) -> 
           (match Translate.trans_fieldexp lv_ir id fields with
            | None -> failwith "error, field not found"
            | Some expty -> expty  )
      | _  -> failwith "not getting a record type for field access")

and trans_letexp cur_level break_to venv tenv decls body = 
  let (new_venv, new_tenv, init_lst) = trans_decls cur_level break_to venv tenv decls in
  let body_ir, t = trans_exp_seq cur_level break_to new_venv new_tenv body in
  T.trans_letexp init_lst body_ir, t


and process_header cur_level break_to venv tenv decls = 
  List.fold_left decls
  ~init:(venv, tenv)
  ~f:(fun (venv, tenv) decl ->
    match decl with
    | Type_decl (id, ty) -> (venv, S.add tenv id (D.NAME (id, ref None)))
    | Func_decl (fname, params, return, body) ->
        (let param_refs = List.map params ~f:(fun (id, _, _) -> D.NAME (id, ref None)) in
        let flabel = Temp.new_label () in
        let return_ref = match return with
        | Some t -> D.NAME (t, ref None) 
        | None -> 
            let dummy_return = make_dummy_sym () in
            D.NAME (dummy_return, ref None)
        in
        (S.add venv fname (E.FUNC_TYPE (cur_level, flabel, param_refs, return_ref)), tenv))
    | Var_decl (vname, vtype, escape, rhs) ->
        let var_access = T.alloc_local cur_level true in
        (S.add venv vname (E.VAR_TYPE (var_access, (D.NAME (vname, ref None)))), tenv))

and trans_decls cur_level break_to venv tenv decls = 
  let (venv', tenv') = process_header cur_level break_to venv tenv decls in
  List.fold_left decls
    ~init:(venv', tenv', [])
    ~f:(fun (venv, tenv, exp_lst) delc -> trans_decl cur_level break_to venv tenv exp_lst delc)

and trans_decl cur_level break_to venv tenv exp_lst decl =
  match decl with
  | Type_decl (id, ty) -> trans_typedecl cur_level break_to venv tenv exp_lst id ty
  | Func_decl (fname, params, return, body) ->
                          trans_funcdecl cur_level break_to venv tenv exp_lst fname params return body
  | Var_decl (vname, vtype, escape, rhs) ->
                          trans_vardecl cur_level break_to venv tenv exp_lst vname vtype rhs

and trans_ty tenv id ty =  
  let ty_to_datatype tenv = function
  | Type_id type_id -> 
      (match S.lookup tenv id with
      | Some t -> actual_type t
      | None   -> failwith (Printf.sprintf "%s is not defined\n" (S.name id)))
  | Array_ty arr -> 
      (match S.lookup tenv arr with
      | Some t -> ARRAY (t, ref ())
      | None -> failwith (Printf.sprintf "%s is not defined\n" (S.name arr)))
  | Rec_ty fields ->
      let ls = List.map fields ~f:(fun (id, type_id, _) -> 
        match S.lookup tenv type_id with
        | Some t -> (id, t)
        | None   -> failwith (Printf.sprintf "%s is not defined\n" (S.name type_id)))
      in RECORD (ls, ref ())
  in
    (match S.lookup tenv id with
    | Some (D.NAME (id, ref_)) -> 
        (match !ref_ with
        | Some _ -> failwith ("%s is already defined before process, should never happend")
        | None -> ref_ := Some (ty_to_datatype tenv ty))
    | Some _  -> failwith "error"
    | None -> failwith (Printf.sprintf "%s is undefined" (S.name id)));
    tenv


and trans_typedecl cur_level break_to venv tenv exp_lst id ty = 
  let new_tenv = trans_ty tenv id ty in
  (venv, new_tenv, exp_lst)

and trans_funcdecl cur_level break_to venv tenv exp_lst fname params return body = 

  (* walk through params and extra all escapse information *)
  let calculate_escapes params = 
    List.map params ~f:(fun (id, ty, escape) -> !escape)
  in

  (* first verify that all incoming arguments are already defined, return id * type tuple list *)
  let param_idtys = List.map params
    ~f:(fun (id, type_id, _) -> match S.lookup tenv type_id with
        | None -> failwith (Printf.sprintf "%s is undefined" (S.name type_id)) 
        | Some t -> (id, t))
  in 


  (* add all id * type pairs to the current type environment *)
  let tenv_params = List.fold_left param_idtys ~init:tenv
    ~f:(fun acc (id, t) -> S.add acc id t)
  in
  let fname_label = Temp.named_label (S.name fname) in
  let level_formals = calculate_escapes params in
  let new_level = T.new_level (Some cur_level) fname_label level_formals in
  let body_ir, body_type = trans_exp new_level break_to venv tenv_params body in
  let params = List.map param_idtys ~f:(fun (_, t) -> t ) in
  (match S.lookup venv fname with
  | None -> failwith "function does not exist, should never happend"
  | Some (FUNC_TYPE (level, flabel, param_refs, NAME (id, ref_))) ->
      (match List.(length param_refs = length params) with
      | false -> failwith "should not happend, function params length is not fixed"
      | true  ->
        (*handle return type*)
        (match return with
        | None -> ref_ := Some body_type;
        | Some ty -> 
            match S.lookup tenv ty with
            | Some t -> if body_type = t then ref_ := Some t else failwith "body type and return type does not match"
            | None   -> failwith (Printf.sprintf "%s is not defined" (S.name ty)));

        (*handle function parameter*)
        List.iter2_exn param_refs params 
          ~f:(fun param_ref param -> match param_ref with
          | NAME (id, ref_) -> 
            (match !ref_ with
            | Some _ -> failwith "this should not happend"
            | None  -> ref_ := Some param)
          | _ -> failwith "param_ref should not have other type other than NAME"))
  | _ -> failwith "should not happend");

  (* Type check is done at this point, proc_entry_exit will generate assem for functions *)
  T.proc_entry_exit ~is_procedure:true new_level body_ir;
  (venv, tenv, exp_lst)

and trans_vardecl cur_level break_to venv tenv exp_lst vname vtype rhs = 
  let lhs_ir, _ = trans_lvalue cur_level break_to venv tenv (Id vname) in
  match S.lookup venv vname with
  | None -> failwith (Printf.sprintf "%s is undefined, should not happend" (S.name vname))
  | Some (VAR_TYPE (access, (NAME (id, ref_)))) ->
      (let rhs_ir, rhs_ty = trans_exp cur_level break_to venv tenv rhs in
      let new_exp_lst = (T.trans_assignment lhs_ir rhs_ir) :: exp_lst in
      match !ref_ with
      | Some _ -> failwith "this should not happend"
      | None ->  
          match vtype with
          | None -> ref_ := Some rhs_ty; (venv, tenv, new_exp_lst)
          | Some ty ->
            (match S.lookup tenv ty with
            | None -> failwith (Printf.sprintf "%s is not define" (S.name ty))
            | Some var_declty -> 
                match var_declty = rhs_ty with
                | true  -> ref_ := Some rhs_ty; (venv, tenv, new_exp_lst)
                | false -> failwith "var type and rhs does not match"))
  | Some _ -> failwith "this should not happend"

let trans_prog ast = 
  let body, t = trans_exp T.outermost None E.base_venv E.base_tenv ast in
  T.proc_entry_exit ~is_procedure:true T.outermost body;
  T.get_fragments ()
