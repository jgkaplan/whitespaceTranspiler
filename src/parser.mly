(******************************************************************************
   You do not need to modify anything in this file.
 ******************************************************************************)

(* Acknowledgement:  this parser is adapted from the OCaml 4.04 parser
 *  [https://github.com/ocaml/ocaml/blob/trunk/parsing/parser.mly],
 *  written by Xavier Leroy, projet Cristal, INRIA Rocquencourt
 *  and distributed under the GNU Lesser General Public License version 2.1. *)

%{
open Ast

let has_dups lst =
  let open List in
  length lst <> length (sort_uniq Pervasives.compare lst)
%}

%token <string> INT
%token <string> ID STRING
%token <char> CHAR
%token PLUS MINUS TIMES DIV MOD AND OR XOR
       LT LEQ GT GEQ EQUAL NOTEQUAL
       NOT
%token LPAREN RPAREN SEMI ASSIGN LBRACE RBRACE COLON
       COMMA LBRACKET RBRACKET
%token TRUE FALSE
%token IF ELSE
       WHILE DO LOOP BREAK RETURN PRINTC PRINTI
%token FUNCTION
%token EOF

(* The entries commented out below are unnecessary, but indicate
   the correct place in the precedence table for those tokens
   should they ever become necessary. *)

(* %nonassoc IN *)
%nonassoc below_SEMI
%nonassoc SEMI
(* %nonassoc LET *)
(* %nonassoc CATCH *)
/* %nonassoc HANDLE */
/* %nonassoc FINALLY */
/* %nonassoc THEN */
%nonassoc ELSE
%right ASSIGN /* UPDATE */
(* %right ARROW *)
%right OR
%right XOR
%right AND
%left EQUAL NOTEQUAL /* EQUALEQUAL NOTEQUALEQUAL */ LT LEQ GT GEQ
%left PLUS MINUS
%left TIMES DIV MOD
%nonassoc NOT /* TYPEOF DEREF */
(* %nonassoc DOT *)
(* %nonassoc BEGIN FALSE LPAREN TRUE UNDEFINED *)

/* %start <Ast.expr> parse_expression */
/* %start <Ast.statement> parse_statement */
%start <Ast.program> parse_program

%%



/* parse_expression:
  | e = expr; EOF
        { e } */

parse_program:
    | fs = functions; EOF { fs }

/* parse_phrase:
	| e = seq_expr; DOUBLE_SEMI?; EOF
		{ Expr e }
  | d = defn; DOUBLE_SEMI?; EOF
        { Defn d }
  | DOUBLE_SEMI?; EOF
        { raise End_of_file }
	; */

/* defn:
  | LET; x = ID; EQUAL; e = expr
        { make_let_defn x e }
  | LET; REC; f = ID; LPAREN; xs = nonempty_list(ident); RPAREN; EQUAL; e = expr
        { make_let_rec_defn f xs e }
  ; */

/* seq_expr:
  | e = expr; %prec below_SEMI
        { e }
  | e = expr; SEMI
        { e }
  | e = expr; SEMI; s = seq_expr;
        { make_seq e s } */

functions:
    | fs = list(my_function) { fs }

my_function:
    | FUNCTION; f = ident; LPAREN; args = separated_list(COMMA, ident); RPAREN; LBRACE; s = statements; RBRACE
        { (f, args, s) }

statements:
    | s = list(statement) { s }

statement:
    | e = expr; SEMI; { SExpr e }
    | LOOP; e = expr; LBRACE; s = statements; RBRACE { SLoop (e, s) }
    | RETURN; e = expr; SEMI; { SReturn e }
    | PRINTC; e = expr; SEMI; { SPrintC e }
    | PRINTI; e = expr; SEMI; { SPrintI e }
    | IF; LPAREN; e = expr; RPAREN; LBRACE; s = statements; RBRACE { SIf (e, s) }
    | IF; LPAREN; e = expr; RPAREN; LBRACE; s1 = statements; RBRACE; ELSE; LBRACE; s2 = statements; RBRACE { SIfElse (e, s1, s2) }

expr:
  | e = simple_expr
        { e }
  | f = ident; LPAREN; es = separated_list(COMMA, expr); RPAREN
        { EApp (f, es) }
  | uop = unop; e = expr
        { EUop (uop, e) }
  | v = ident; ASSIGN; e = expr
        { EAssign (v, e) }
  | e1 = expr; bop = binop; e2 = expr
        { EBop (e1, bop, e2) }
/*
  | e1 = simple_expr; LBRACKET e2 = expr; RBRACKET; UPDATE; e3 = expr
        { make_binop BopUpdate (make_get_field e1 e2) e3 }
  | e1 = expr; AND; e2 = expr
		{ make_and e1 e2 }
  | e1 = expr; OR; e2 = expr
		{ make_or e1 e2 }
    | e1 = expr; XOR; e2 = expr
    		{ make_xor e1 e2 }
  | IF; e1 = seq_expr; THEN; e2 = expr; ELSE; e3 = expr
        { make_if e1 e2 e3 }
  | IF; e1 = seq_expr; THEN; e2 = expr
        { make_if_partial e1 e2 }
  | LET; x = ID; EQUAL; e1 = expr; IN; e2 = seq_expr
		{ make_let x e1 e2 }
  | LET; REC; f = ID; LPAREN; xs = nonempty_list(ident); RPAREN; EQUAL; e1 = expr; IN; e2 = seq_expr
		{ make_let_rec f xs e1 e2 }
  | TRY; e1 = seq_expr; CATCH; x = ID; HANDLE; e2 = seq_expr
        { make_try e1 x e2 }
  | TRY; e1 = seq_expr; CATCH; x = ID; HANDLE; e2 = seq_expr; FINALLY; e3 = seq_expr
        { make_try_finally e1 x e2 e3 }
  | THROW; e = simple_expr
        { make_throw e}
  | REF; e = simple_expr
        { make_ref e }
  | FUN; LPAREN; xs = nonempty_list(ident); RPAREN; ARROW; e = seq_expr
        { if has_dups xs
          then $syntaxerror (* duplicate argument names *)
          else make_fun xs e }
  | WHILE; e1 = seq_expr; DO; e2 = seq_expr; DONE
        { make_while e1 e2 }
  | DELETE e1 = simple_expr; LBRACKET; e2 = expr; RBRACKET
        { make_delete_field e1 e2 }
  | DELETE e = simple_expr; DOT; x = ident
        { make_delete_field e (make_string x) } */
	;

simple_expr:
  | x = ident
        { EVar x }
  | LPAREN; e = expr; RPAREN
        { e }
  | s = INT
		{ EInteger (false, s) }
  | c = CHAR
        { EChar c }
  | s = STRING
		{ EString s }
  | TRUE
		{ EBool true }
  | FALSE
		{ EBool false }
  /* | LBRACE; fields = separated_list(COMMA, field_bind); RBRACE
        { if fields |> List.map fst |> has_dups
          then $syntaxerror (* duplicate fields *)
          else make_object fields } */
  /* | e1 = simple_expr; LBRACKET e2 = expr; RBRACKET
        { EArrayAccess (e1, e2) } */
  /* | e1 = simple_expr; DOT; x = ident
        { make_get_field e1 (make_string x) } */

/* field_bind:
  | f = STRING; COLON; e = expr
        { (f, e) } */

ident:
  | x = ID
        { x }

%inline unop:
  | MINUS { UopMinus }
  | NOT { UopNot }

%inline binop:
  | PLUS { BopPlus }
  | MINUS { BopMinus }
  | TIMES { BopTimes }
  | DIV { BopDiv }
  | MOD { BopMod }
  | LT { BopLt }
  | LEQ { BopLeq }
  | GT { BopGt }
  | GEQ { BopGeq }
  | EQUAL { BopEq }
  | NOTEQUAL { BopNeq }
  ;
