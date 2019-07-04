
(* The type of tokens. *)

type token = 
  | WHILE
  | TRUE
  | TIMES
  | STRING of (string)
  | SEMI
  | RPAREN
  | RBRACKET
  | RBRACE
  | PRINT
  | PLUS
  | OR
  | NOTEQUAL
  | NOT
  | MOD
  | MINUS
  | LT
  | LPAREN
  | LOOP
  | LEQ
  | LBRACKET
  | LBRACE
  | INT of (string)
  | IF
  | ID of (string)
  | GT
  | GEQ
  | FUNCTION
  | FALSE
  | EQUAL
  | EOF
  | ELSE
  | DO
  | DIV
  | COMMA
  | COLON
  | BREAK
  | ASSIGN
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val parse_statements: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.statements)

val parse_expression: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.expr)
