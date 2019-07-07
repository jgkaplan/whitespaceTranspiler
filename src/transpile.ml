open Ast
open WhitespaceAst
open Builtins

exception UnboundVariable of id
exception UndefinedFunction of id

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

let addFunc key = functionMap := (IdMap.add key (next_label ()) !functionMap)

let getFunc key = IdMap.find_opt key !functionMap

let rec compile_expression e varmap = match e with
  | EInteger s -> [Stack (Push s)]
  | EBool b -> if b then [Stack (Push "1")] else [Stack (Push "0")]
  | EChar c -> [Stack (Push (c |> Char.code |> string_of_int))]
  (* | EVar v -> begin
      match IdMap.find_opt v varmap with
      | None -> raise UnboundVariable v
      | Some s -> [Stack (Push s); Heap Retrieve]
     end
  *)

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
  | SLoop (e, s) -> begin
      let l = next_label ()
      in (compile_expression e) ++
    end


let compile_statements functionMap statements = failwith "unimplemented"

let compile_function functionMap (name, args, statements) =
  let label = match IdMap.find_opt name functionMap with
    | None -> raise UndefinedFunction name
    | Some l -> l
  in let compiled_s = compile_statements functionMap statements
  in let forceReturn = match List.rev compiled_s with
      | Flow Return :: xs -> compiled_s
      | xs -> List.rev (Flow Return :: xs)
  in let mainFix = List.map (fun s -> begin
        match s with
        | Flow Return -> if name = "main" then Flow Terminate else Flow Return
        | x -> x
      end) forceReturn
  in (Flow (Mark l)) :: mainFix

let compile_program fs =
  let functionMap = List.fold_left
      (fun acc (name, _, _) ->
         IdMap.add name (if name = "main" then "0" else next_label ()) acc) IdMap.empty) fs
  in let compiled = List.map (compile_function functionMap) fs
  in (Flow (Call "0")) :: (List.flatten compiled)
