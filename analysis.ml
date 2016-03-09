open Env
open Translate
open Datatypes
open Ast
open Symbol
open Core.Std

module D = Datatypes
module S = Symbol
module E = Env

type venv = Env.env_type Symbol.table
type tenv = ty Symbol.table

type expty = {
  exp     :  Translate.exp;
  ty      :  Datatypes.datatype;
}

let make_expty exp ty = {
  exp = exp ;
  ty  = ty  ;
}

let actual_type ty = 
  match ty with
  | NAME (_, {contents = Some t}) -> t
  | _ -> ty

let actual_type_lst type_lst = 
  List.map type_lst
    ~f:(fun ty -> actual_type ty)


let rec trans_exp venv tenv ast = 
  match ast with 
  | Nil                                 -> make_expty () NIL
  | Break                               -> make_expty () NIL
  | Int i                               -> make_expty () INT 
  | String s                            -> make_expty () STRING
  | Lvalue lv                           -> trans_lvalue venv tenv lv
  | Exp_seq exp_seq                     -> trans_exp_seq venv tenv exp_seq
  | Negation_exp e                      -> trans_neg_exp venv tenv e
  | Call_exp (fname, params)            -> trans_call_exp venv tenv fname params 
  | Arr_create (type_id, size, init)    -> trans_arr_create venv tenv type_id size init
  | Rec_create (rec_type, field_lst)    -> trans_rec_create venv tenv rec_type field_lst
  | Assignment (lhs, rhs_exp)           -> trans_assignment venv tenv lhs rhs_exp
  | Ifthenelse (test, then_e, else_e)   -> trans_ifthenelse venv tenv test then_e else_e
  | Ifthen (test, then_e)               -> trans_ifthen venv tenv test then_e
  | Whileexp (cond, do_exp)             -> trans_whileexp venv tenv cond do_exp
  | Forexp (id, first, last, do_exp)    -> trans_forexp venv tenv id first last do_exp
  | Letexp (decls, exps)                -> trans_letexp venv tenv decls exps
  | ArithExp arith_exp                  -> trans_ariths venv tenv arith_exp
  | BoolExp bool_exp                    -> trans_boolexp venv tenv bool_exp
  | CmpExp cmp_exp                      -> trans_cmpexp venv tenv cmp_exp

and trans_cmpexp venv tenv cmp_exp = 
  match cmp_exp with
  | Eq (e1, e2)
  | Neq (e1, e2) ->
       (let e1_ty = (trans_exp venv tenv e1).ty in
       let e2_ty = (trans_exp venv tenv e2).ty in
       match e1_ty, e2_ty with
       | D.INT, D.INT 
       | D.RECORD _, D.RECORD _
       | D.ARRAY _, D.ARRAY _ -> make_expty () D.INT
       | _, _ -> failwith (Printf.sprintf "Incompatible type %s %s for comparison op"
                            (D.type_to_string e1_ty) (D.type_to_string e2_ty)))
  | Le (e1, e2)
  | Lt (e1, e2)
  | Ge (e1, e2)
  | Gt (e1, e2) ->
       (let e1_ty = (trans_exp venv tenv e1).ty in
       let e2_ty = (trans_exp venv tenv e2).ty in
       match e1_ty, e2_ty with
       | D.INT, D.INT  -> make_expty () D.INT
       | _, _ -> failwith (Printf.sprintf "Incompatible type %s %s for comparison op"
                            (D.type_to_string e1_ty) (D.type_to_string e2_ty)))

and trans_boolexp venv tenv bool_exp = 
  match bool_exp with
  | Or (e1, e2) 
  | And (e1, e2) ->
       let e1_ty = (trans_exp venv tenv e1).ty in
       let e2_ty = (trans_exp venv tenv e2).ty in
       match e1_ty = D.INT, e2_ty = D.INT with
       | true, true -> make_expty () D.INT
       | false, true -> failwith (Printf.sprintf "Expect e1 has type int, but get %s instead\n" 
                                    (D.type_to_string e1_ty))
       | true, false -> failwith (Printf.sprintf "Expect e2 has type int, but get %s instead\n" 
                                    (D.type_to_string e2_ty))
       | false, false -> failwith (Printf.sprintf "Expect e1, e2 has type int, but get %s %s instead\n" 
                                    (D.type_to_string e1_ty) (D.type_to_string e2_ty))

and trans_ariths venv tenv arith_exp = 
  match arith_exp with
  | Add (e1, e2)  
  | Sub (e1, e2)
  | Mul (e1, e2)
  | Div (e1, e2) ->
      (let e1_ty = (trans_exp venv tenv e1).ty in
       let e2_ty = (trans_exp venv tenv e2).ty in
       match e1_ty = D.INT, e2_ty = D.INT with
       | true, true -> make_expty () D.INT
       | false, true -> failwith (Printf.sprintf "Expect e1 has type int, but get %s instead\n" 
                                    (D.type_to_string e1_ty))
       | true, false -> failwith (Printf.sprintf "Expect e2 has type int, but get %s instead\n" 
                                    (D.type_to_string e2_ty))
       | false, false -> failwith (Printf.sprintf "Expect e1, e2 has type int, but get %s %s instead\n" 
                                    (D.type_to_string e1_ty) (D.type_to_string e2_ty)))

and trans_forexp venv tenv id first last do_exp = 
  let low_ty = (trans_exp venv tenv first).ty in
  let high_ty = (trans_exp venv tenv last).ty in
  let do_ty = (trans_exp venv tenv do_exp).ty in
  match low_ty, high_ty, do_ty with
  | D.INT, D.INT, D.UNIT -> make_expty () D.UNIT
  | _, D.INT, D.UNIT -> failwith (Printf.sprintf "Expectig first has type int, but get %s instead\n" 
                                    (D.type_to_string low_ty))
  | D.INT, _, D.UNIT -> failwith (Printf.sprintf "Expectig last has type int, but get %s instead\n" 
                                    (D.type_to_string high_ty))
  | _, _, D.UNIT -> failwith (Printf.sprintf "Expectig first, last has type int, but get %s and %s instead\n" 
                                    (D.type_to_string low_ty) (D.type_to_string high_ty))
  | D.INT, D.INT, _ -> failwith (Printf.sprintf "Expectig do_exp has type unit, but get %s instead\n" 
                                    (D.type_to_string do_ty ))
  | _, _, _ -> failwith (Printf.sprintf "Expectig first, last has type int, and do_exp has type unit, 
                                          but get %s %s and %s instead\n" 
                                    (D.type_to_string low_ty) (D.type_to_string high_ty) (D.type_to_string do_ty))


and trans_whileexp venv tenv cond do_exp = 
  let cond_ty = (trans_exp venv tenv cond).ty in
  let do_ty = (trans_exp venv tenv do_exp).ty in
  match cond_ty with
  | D.INT -> make_expty () (actual_type do_ty)
  | _ -> failwith (Printf.sprintf "Expecting int type for cond but get %s instead " (D.type_to_string cond_ty))

and trans_assignment venv tenv lhs rhs_exp = 
  let {exp = _; ty = lhs_ty} = trans_lvalue venv tenv lhs in
  let {exp = _; ty = rhs_ty} = trans_exp venv tenv rhs_exp in
  match lhs_ty = rhs_ty with
  | true -> make_expty () UNIT
  | false -> 
      let msg = Printf.sprintf "Lhs has type %s, but rhs has type %s" 
                  (D.type_to_string lhs_ty) (D.type_to_string rhs_ty)
      in failwith msg

and trans_ifthenelse venv tenv test then_e else_e = 
  let test_ty = (trans_exp venv tenv test).ty in
  let then_ty = (trans_exp venv tenv then_e).ty in
  let else_ty = (trans_exp venv tenv else_e).ty in
  match test_ty, then_ty = else_ty with
  | D.INT, true -> make_expty () (actual_type then_ty)
  | D.INT, false -> failwith (Printf.sprintf "then exp and else exp have different type")
  | _, _ -> failwith (Printf.sprintf "Expecting test exp has type int, but get %s instead\n" 
                        (D.type_to_string test_ty))

and trans_ifthen venv tenv test then_e = 
  let test_ty = (trans_exp venv tenv test).ty in
  let then_ty = (trans_exp venv tenv then_e).ty in
  match test_ty with
  | D.INT -> make_expty () (actual_type then_ty)
  | _  -> failwith (Printf.sprintf "Expecting test exp has type int, but get %s instead\n" 
                        (D.type_to_string test_ty))

and trans_rec_create venv tenv rec_type field_lst = 
  match S.lookup tenv rec_type with
  | Some (D.RECORD (fields, _)) -> 
      List.fold2_exn field_lst fields
      ~init:make_expty () UNIT
      ~f:(fun acc (id, exp) (decl_sym, decl_ty) ->
          let {exp = _; ty = actual_ty} = trans_exp venv tenv exp in
          match id = decl_sym, actual_ty = decl_ty with
          | true, true -> acc
          | false, true -> failwith (S.name id ^ " is undefined")
          | true, false -> failwith ""
          | false, false -> failwith "")
  | Some t -> 
      let msg = Printf.sprintf "Expecting array type but get %s" (D.type_to_string t) in 
      failwith msg
  | None ->
      failwith (Printf.sprintf "%s is not defined" (S.name rec_type))

and trans_exp_seq venv tenv exp_seq = 
  match exp_seq with
  | [] -> make_expty () D.UNIT
  | last :: [] ->
    let last_ty = (trans_exp venv tenv last).ty in
    make_expty () (actual_type last_ty)
  | hd :: rest -> ignore (trans_exp venv tenv hd) ; trans_exp_seq venv tenv rest

and trans_neg_exp venv tenv neg_e = 
  match (trans_exp venv tenv neg_e).ty with
  | D.INT -> make_expty () INT
  | _    -> failwith "negation on non int type"

and trans_call_exp venv tenv fname params = 
  match S.lookup venv fname with
  | Some (E.FUNC_TYPE (args, return)) -> 
       (let all_match = List.fold2_exn params args ~init:true 
          ~f:(fun acc param arg -> 
            let {exp = _; ty = e_ty} = trans_exp venv tenv param in
            acc && (e_ty = arg))
        in match all_match with
        | true -> make_expty () return
        | false -> failwith "function call type mismatch")
  | Some (E.VAR_TYPE _) -> failwith (S.name fname ^ " is not a function")
  | None -> failwith (S.name fname ^ " is undefined")

and trans_arr_create venv tenv type_id size init = 
  let {exp = _; ty = size_type} = trans_exp venv tenv size in
  let {exp = _; ty = init_type} = trans_exp venv tenv init in
  match S.lookup tenv type_id, size_type with
  | Some (D.ARRAY (ty, _)), D.INT -> 
      if ty = init_type then make_expty () D.UNIT else 
        failwith ("Initial type is not compatible with declared type: " ^ D.type_to_string init_type)
  | Some t, D.INT -> failwith ("expecting an array type but get " ^ D.type_to_string t)
  | Some t, s_t  -> failwith ("expecting an array type but get " ^ D.type_to_string t ^ " , expecting int but 
                                get " ^ D.type_to_string s_t)
  | None, _ -> failwith ((S.name type_id ) ^ " is undefined")


and trans_lvalue venv tenv lv = 
  match lv with
  | Id id -> 
      (match S.lookup venv id with
      | Some (E.VAR_TYPE ty) -> 
          make_expty () (actual_type ty)
      | Some (E.FUNC_TYPE (params, return)) ->
          (failwith "Expecting a variable, but get a function")
      | None   -> (failwith ("Unknow id " ^ (S.name id))))
  | Subscript (lv, exp) -> 
      (let {exp = _; ty = lv_type} = trans_lvalue venv tenv lv in
      let {exp = _; ty = exp_type} = trans_exp venv tenv exp in
       match lv_type, exp_type with
       | D.ARRAY (e_type, u), D.INT -> make_expty () (actual_type e_type) 
       | _ as t, D.INT -> failwith ("expecting array but get " ^ (D.type_to_string t))
       | D.ARRAY _, t -> failwith ("expecting INT as index but get " ^ (D.type_to_string t))
       | vid, sub -> 
           failwith (("expecting array but get " ^ (D.type_to_string vid)) ^ ", " ^
                     ("expecting INT as index but get " ^ (D.type_to_string sub))))
  | Field_exp (lv, id) ->
      (let {exp = _; ty = lv_type} = trans_lvalue venv tenv lv in
       match lv_type with
       | D.RECORD (fields, _) -> 
           (let field = List.filter fields ~f:(fun (name, ty) -> name = id) in
           match field  with
           | [] -> failwith "no field exist in record"
           | (name, ty) :: []  -> make_expty () (actual_type ty)
           | _   -> failwith "should not happend")
      | _  -> failwith "not getting a record type for field access")

and trans_letexp venv tenv decls exps = 
  let (new_venv, new_tenv) = trans_decls venv tenv decls in
  trans_exp_seq new_venv new_tenv exps

and process_header venv tenv decls = 
  List.fold_left decls
  ~init:(venv, tenv)
  ~f:(fun (venv, tenv) decl ->
    match decl with
    | Type_decl (id, ty) -> (venv, S.add tenv id (D.NAME (id, ref None)))
    | Func_decl (fname, params, return, body) ->
        (let param_refs = List.map params ~f:(fun (id, _) -> D.NAME (id, ref None)) in
        let return_ref = match return with
        | Some t -> D.NAME (t, ref None) 
        | None -> 
            let dummy_return = make_dummy_sym () in
            D.NAME (dummy_return, ref None)
        in
        (S.add venv fname (E.FUNC_TYPE (param_refs, return_ref)), tenv))
    | Var_decl (vname, vtype, rhs) ->
                            (S.add venv vname (E.VAR_TYPE (D.NAME (vname, ref None))), tenv))

and trans_decls venv tenv decls = 
  let (venv', tenv') = process_header venv tenv decls in
  List.fold_left decls
    ~init:(venv', tenv')
    ~f:(fun (venv, tenv) delc -> trans_decl venv tenv delc)

and trans_decl venv tenv decl =
  match decl with
  | Type_decl (id, ty) -> trans_typedecl venv tenv id ty
  | Func_decl (fname, params, return, body) ->
                          trans_funcdecl venv tenv fname params return body
  | Var_decl (vname, vtype, rhs) ->
                          trans_vardecl venv tenv vname vtype rhs

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
      let ls = List.map fields ~f:(fun (id, type_id) -> 
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


and trans_typedecl venv tenv id ty = 
  let new_tenv = trans_ty tenv id ty in
  (venv, new_tenv)

and trans_funcdecl venv tenv fname params return body = 
  let param_idtys = List.map params
    ~f:(fun (id, type_id) -> match S.lookup tenv type_id with
        | None -> failwith (Printf.sprintf "%s is undefined" (S.name type_id)) 
        | Some t -> (id, t))
  in 
  let tenv_params = List.fold_left param_idtys ~init:tenv
    ~f:(fun acc (id, t) -> S.add acc id t)
  in
  let {exp = _; ty = body_type} = trans_exp venv tenv_params body in
  let params = List.map param_idtys ~f:(fun (_, t) -> t ) in
  match S.lookup venv fname with
  | None -> failwith "function does not exist, should never happend"
  | Some (FUNC_TYPE (param_refs, NAME (id, ref_))) ->
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
          | _ -> failwith "param_ref should not have other type other than NAME"));
        (venv, tenv)
  | _ -> failwith "should not happend"


and trans_vardecl venv tenv vname vtype rhs = 
  (match S.lookup venv vname with
  | None -> failwith (Printf.sprintf "%s is undefined, should not happend" (S.name vname))
  | Some (VAR_TYPE (NAME (id, ref_))) ->
      (let {exp = _; ty = rhs_ty} = trans_exp venv tenv rhs in
      match !ref_ with
      | Some _ -> failwith "this should not happend"
      | None ->  
          match vtype with
          | None ->  ref_ := Some rhs_ty
          | Some ty ->
            (match S.lookup tenv ty with
            | None -> failwith (Printf.sprintf "%s is not define" (S.name ty))
            | Some var_declty -> 
                if var_declty = rhs_ty then ref_ := Some rhs_ty else failwith "var type and rhs does not match"))
  | Some _ -> failwith "this should not happend");
  (venv, tenv)



  




                                            


