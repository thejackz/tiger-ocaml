open Symbol

type pos = {
  start_pos : Lexing.position;
  end_pos   : Lexing.position;
}


let make_pos start_pos end_pos = 
  { start_pos; end_pos }


type exp = 
  | Nil             
  | Break           
  | Int             of int
  | String          of string 
  | Lvalue          of lvalue 
  | Exp_seq         of exp list 
  | Negation_exp     of exp
  | Call_exp        of id * exp list 
  | Arr_create      of type_id * exp * exp 
  | Rec_create      of type_id * field_create list
  | Assignment      of lvalue  * exp
  | Ifthenelse      of exp * exp * exp 
  | Ifthen          of exp * exp 
  | Whileexp        of exp * exp
  | Forexp          of id  * exp * exp * exp 
  | Letexp          of decl list * exp list
  | ArithExp        of arith_exp 
  | BoolExp         of bool_exp 
  | CmpExp          of cmp_exp 


and decl = 
  | Type_decl of type_id * ty
  | Func_decl of id * (field_decl list) * return_ty option * exp
  | Var_decl  of id * (type_id option)  * exp
  
and id              = Symbol.symbol
and type_id         = Symbol.symbol
and return_ty       = type_id 
and array_type      = type_id
and field_decl      = id * type_id
and field_create    = id * exp

and ty = 
  | Type_id  of type_id
  | Array_ty of array_type
  | Rec_ty   of field_decl list

and lvalue = 
  | Id        of id 
  | Subscript of lvalue * exp 
  | Field_exp of lvalue * id

and arith_exp =
  | Add of exp * exp
  | Sub of exp * exp
  | Mul of exp * exp
  | Div of exp * exp

and bool_exp =
  | Or of exp * exp
  | And of exp * exp

and cmp_exp =
  | Eq of exp * exp
  | Neq of exp * exp
  | Lt of exp * exp
  | Le of exp * exp
  | Gt of exp * exp
  | Ge of exp * exp



