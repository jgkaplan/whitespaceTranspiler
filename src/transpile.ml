open Ast
open WhitespaceAst
open Builtins

exception UnboundVariable of string

(* 5; should push 5 to the stack and then pop it. Or just not execute at all *)
(* Update: should probably execute, because fname(); is also a valid expression *)
(* So therefore print() should probably return something. maybe 0 *)
let next_label : unit -> stinrg =
  let counter = ref 0
  in fun () ->
    let toReturn = !counter
    in incr counter; string_of_int toReturn

let alloc : int -> string =
  let counter = ref 0
  in fun amount ->
    let toReturn = !counter
    in counter := counter + amount; string_of_int toReturn

let functionMap = ref IdMap.empty

let addFunc key = functionMap := (IdMap.add key (next_label ()) !functionMap)

let getFunc key = IdMap.find_opt key !functionMap

let rec compile_expression e varmap = match e with
  | EInteger s -> [Stack (Push s)]
  | EVar v -> begin
      match IdMap.find_opt v varmap with
      | None -> raise UnboundVariable v
      | Some s -> [Stack (Push ); Heap Retrieve]
    end

let rec compile_statement = function
  | SExpr e -> (compile_expression e) ++ [Stack Discard]
  | SReturn e -> (compile_expression e) ++ [Flow Return]
  | SIf (e, s) -> begin
      let l = next_label ()
      in (compile_expression e) ++ [Flow (JumpZero l)] ++ (List.map compile_statement s) ++ [Flow (Mark l)]
    end
  | SIfElse (e, s1, s2) -> begin
      let l = next_label ()
      in let l1 = next_label ()
      in (compile_expression e)
         ++ [Flow (JumpZero l)]
         ++ (List.map compile_statement s1)
         ++ [Flow (Jump l1); Flow (Mark l)]
         ++ (List.map compile_statement s2)
         ++ [Flow (Mark l1)]
    end
  | SLoop (e, s) ->

let compile_statements statements =

let compile_program statements =
