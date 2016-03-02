%{
open Lexing
open Ast
open Symbol

let make_sym str = Symbol.symbol_of_string str

%}

%token <int> INT
%token <string> ID
%token <string> STRING
%token ARRAY BREAK DO ELSE END FOR
%token FUNCTION IF IN LET NIL OF
%token THEN TO TYPE VAR WHILE
%token LPAREN RPAREN LBRACE RBRACE
%token LBRACKET RBRACKET 
%token COLON SEMICOL COLONEQ
%token DOT COMMA 
%token PLUS MINUS TIMES DIVIDE
%token EQ NEQ GT GE
%token LT LE AMPERSAND PIPE
%token EOF

%nonassoc DO
%nonassoc THEN
%nonassoc ELSE
%nonassoc COLONEQ
%right LBRACKET
%left PIPE
%left AMPERSAND
%nonassoc EQ NEQ LT LE GT GE
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UMINUS

%start <Ast.exp> prog

%%

prog:
  | e = exp EOF { e }
  ;


exp:
  | NIL             { Nil  }
  | BREAK           { Break }
  | i = INT         { Int i  }
  | s = STRING      { String s }
  | lv = lvalue     { Lvalue lv }
  | LPAREN es = exp_seq RPAREN 
                    { match es with
                      | [e] -> e
                      | es  -> Exp_seq es
                    }
  | MINUS e = exp %prec UMINUS 
                    { Negation_exp e }

  | i = ID LPAREN es = exp_list RPAREN                (* function call  *)
                    { Call_exp (make_sym i, es) }

  | i = ID LBRACE fc = field_create_list RBRACE 
                    { Rec_create (make_sym i, fc) }
  | i = ID LBRACKET e1 = exp RBRACKET OF e2 = exp 
                    { Arr_create (make_sym i, e1, e2) } %prec LBRACKET
  | lv = lvalue COLONEQ e = exp
                    { Assignment (lv, e) }
  | IF test = exp THEN then_exp = exp ELSE else_exp = exp
                    { Ifthenelse (test, then_exp, else_exp) }
  | IF test = exp THEN then_exp = exp 
                    { Ifthen (test, then_exp) }
  | WHILE e1 = exp DO e2 = exp 
                    { Whileexp (e1, e2) }
  | FOR i = ID COLONEQ e0 = exp TO e1 = exp DO e2 = exp 
                    { Forexp (make_sym i, e0, e1, e2) }
  | LET decls = decl_list IN es = exp_seq END
                    { Letexp (decls, es)}
  | ae = arith_exp  { ArithExp ae }
  | ce = cmp_exp    { CmpExp ce }
  | be = bool_exp   { BoolExp be }

lvalue:
  | i = ID   { Id (make_sym i) } %prec LBRACKET
  | lv = lvalue DOT i = ID
              { Field_exp (lv, make_sym i) }
  | i = ID LBRACKET e = exp RBRACKET
              { Subscript (Id (make_sym i), e) }
  | lv = lvalue LBRACKET e = exp RBRACKET
              { Subscript (lv, e) }


exp_seq:
  | es = separated_list(SEMICOL, exp) { es }

exp_list:
  | el = separated_list(COMMA, exp) { el }

field_create_list:
  | fc = separated_list(COMMA, field_create) { fc }

field_create:
  | i = ID EQ e = exp  { (make_sym i, e) }



decl_list:
  | dl = nonempty_list(decl) { dl }

decl:
  | TYPE i = ID EQ t = ty         { Type_decl  (make_sym i, t) }
  | VAR i = ID COLONEQ e = exp    { Var_decl (make_sym i, None, e) }
  | VAR i = ID COLON tid = ID COLONEQ e = exp
                                  { Var_decl (make_sym i, Some (make_sym tid), e) }
  | FUNCTION i = ID LPAREN fd = field_decl_list RPAREN EQ e = exp
                                  { Func_decl (make_sym i, fd, None, e) }
  | FUNCTION i = ID LPAREN fd = field_decl_list RPAREN COLON tid = ID EQ e = exp
                                  { Func_decl (make_sym i, fd, Some (make_sym tid), e)}

ty:
  | i = ID                        { Type_id (make_sym i)}
  | ARRAY OF tid = ID             { Array_ty (make_sym tid) }
  | LBRACE fd = field_decl_list RBRACE 
                                  { Rec_ty fd }

field_decl_list:
  | fl = separated_list(COMMA, field_decl)   { fl }

field_decl:
  | i = ID COLON tid = ID  { (make_sym i, make_sym tid) }


arith_exp:
  | e1=exp PLUS  e2=exp  { Add(e1, e2) }
  | e1=exp MINUS e2=exp  { Sub(e1, e2) }
  | e1=exp TIMES e2=exp  { Mul(e1, e2) }
  | e1=exp DIVIDE e2=exp { Div(e1, e2) }
                                  
cmp_exp:
  | e1=exp EQ  e2=exp { Eq(e1, e2) }
  | e1=exp NEQ e2=exp { Neq(e1, e2) }
  | e1=exp LT  e2=exp { Lt(e1, e2) }
  | e1=exp LE  e2=exp { Le(e1, e2) }
  | e1=exp GT  e2=exp { Gt(e1, e2) }
  | e1=exp GE  e2=exp { Ge(e1, e2) }

bool_exp:
  | e1=exp AMPERSAND e2=exp { And(e1, e2) }
  | e1=exp PIPE e2=exp { Or(e1, e2) }






