type id = string

module IdMap = Map.Make(String)


type unop =
  | UopMinus
  | UopNot

type binop =
  | BopPlus
  | BopMinus
  | BopTimes
  | BopDiv
  | BopMod
  | BopLt
  | BopLeq
  | BopGt
  | BopGeq
  | BopEq
  | BopNeq

type expr =
  | EInteger of string | EBool of bool | EChar of char
  | EString of string
  (* | EArray of id *)
  | EVar of id
  (* | EArrayAccess of expr * expr *)
  | EUop of unop * expr
  | EApp of id * expr list
  | EAssign of id * expr

type statement =
  | SLoop of expr * statements
  | SExpr of expr
  | SFunction of id * id list * statements
  | SIf of expr * statement
  | SIfElse of expr * statement * statement
  | SReturn of expr

and statements = statement list

let print_uop u = match u with
  | UopMinus -> print_string "-"
  | UopNot -> print_string "!"

let rec print_expr e = match e with
  | EInteger (b, s) -> print_string (if b then "-"^s else s)
  | EString s -> print_string ("\""^s^"\"")
  | EBool b -> print_string (if b then "true" else "false")
  | EVar id -> print_string id
  | EUop (u, ex) -> print_uop u; print_expr ex
  | EApp (id, ex) -> print_string (id^"("); List.iter (fun e' -> print_expr e'; print_string ",") ex; print_string ")"

let rec print_statement =
  let rec repeat n = if n = 0 then () else (print_string "\t"; repeat (n-1))
  in let tabcount = ref 0
  in let print_ts s = repeat !tabcount; print_string s
  in function
  | SLoop (e, s) -> print_ts "loop "; print_expr e; print_endline "{"; incr tabcount; List.iter print_statement s; decr tabcount; print_ts "}\n"
  | SExpr e -> repeat !tabcount; print_expr e; print_endline ";"
  | SFunction (id, args, s) -> print_ts (id^"("); List.iter (fun arg -> print_string (arg^",")) args; print_endline ") {";
      incr tabcount; List.iter print_statement s; decr tabcount; print_ts "}\n"
  (* module StringMap = Map.Make(String)
  (* [EInteger s] represents and integer. s is a string representation of the magnitude of the integer
   *)
  type expr =
    | EInteger of bool * string | EBool of bool | EString of string | EUndefined
    | EVar of id
    | ELet of id * expr * expr | ELetRec of id * id list * expr * expr
    | EFun of id list * int * expr
    | EApp of expr * expr list
    | EIf of expr * expr * expr
    | ESequence of expr * expr
    | EWhile of expr * expr
    | EUop of unop * expr | EBop of binop * expr * expr
    | EAnd of expr * expr | EOr of expr * expr
    | ERef of expr
    | EThrow of expr
    | ETry of expr * id * expr | ETryFinally of expr * id * expr * expr
     | EObject of expr StringMap.t | EGetField of expr * expr | EDeleteField of expr * expr *)
