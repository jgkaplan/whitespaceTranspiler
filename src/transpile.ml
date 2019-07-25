open Ast
open WhitespaceAst
(* open Builtins *)

exception UnboundVariable of id
exception UndefinedFunction of id

(* 5; should push 5 to the stack and then pop it. Or just not execute at all *)
(* Update: should probably execute, because fname(); is also a valid expression *)
(* So therefore print() should probably return something. maybe 0 *)
let next_label : unit -> string =
  let counter = ref 1
  in fun () ->
    let toReturn = !counter
    in incr counter; string_of_int toReturn

let alloc : int -> string =
  let counter = ref 0
  in fun amount ->
    let toReturn = !counter
    in counter := !counter + amount; string_of_int toReturn

(* let addFunc key = functionMap := (IdMap.add key (next_label ()) !functionMap) *)

(* let getFunc key = IdMap.find_opt key !functionMap *)

let rec compile_expression functionMap varmap = function
  | EInteger (b, s) -> [Stack (Push (b,s))]
  | EBool b -> if b then [Stack (Push (false,"1"))] else [Stack (Push (false,"0"))]
  | EChar c -> [Stack (Push (false, c |> Char.code |> string_of_int))]
  | EString s -> failwith "Unimplemented" (* TODO *)
  | EUop (u, e') -> begin
      match u with
      | UopMinus -> (compile_expression functionMap varmap e') @ [Stack (Push (true, "1")); Arithmetic Mul]
      | UopNot -> (compile_expression functionMap varmap e') @ [Stack (Push (false, "1")); Arithmetic Sub; Stack Duplicate; Arithmetic Mul]
    end
  | EVar v -> begin
      match IdMap.find_opt v !varmap with
      | None -> raise (UnboundVariable v)
      | Some s -> [Stack (Push (false, "0")); Heap Retrieve; Stack (Push (false, s)); Arithmetic Add; Heap Retrieve]
    end
  | EAssign (id, e) -> begin
      let ce = compile_expression functionMap varmap e
      in let l =
        begin
          match IdMap.find_opt id !varmap with
          | None -> let l = IdMap.cardinal !varmap + 1 in varmap := IdMap.add id (string_of_int l) !varmap; string_of_int l (* Add it *)
          | Some l -> l (* update it *)
        end
      in
      ce
      @ [Stack Duplicate; Stack (Push (false, "0")); Heap Retrieve; Stack (Push (false, l)); Arithmetic Add]
      @ [Stack Swap; Heap Store]
    end
  | EApp (id, es) -> begin
      match IdMap.find_opt id functionMap with
      | None -> raise (UndefinedFunction id)
      | Some l ->
        [Stack (Push (false, "0")); Stack Duplicate; Heap Retrieve]
        @ [Stack (Push (false, !varmap |> IdMap.cardinal |> (+) 1 |> string_of_int ))]
        @ [Arithmetic Add; Stack Duplicate; Stack (Copy "2"); Heap Retrieve; Heap Store; Heap Store]
          @ List.flatten (List.map (compile_expression functionMap varmap) es) @ [Flow (Call l)]
    end
  | EBop (e1, b, e2) -> begin
      match b with
      | BopPlus ->
        compile_expression functionMap varmap e1
        @ compile_expression functionMap varmap e2
        @ [Arithmetic Add]
      | BopMinus ->
        compile_expression functionMap varmap e1
        @ compile_expression functionMap varmap e2
        @ [Arithmetic Sub]
      | BopTimes ->
        compile_expression functionMap varmap e1
        @ compile_expression functionMap varmap e2
        @ [Arithmetic Mul]
      | BopDiv ->
        compile_expression functionMap varmap e1
        @ compile_expression functionMap varmap e2
        @ [Arithmetic Div]
      | BopMod ->
        compile_expression functionMap varmap e1
        @ compile_expression functionMap varmap e2
        @ [Arithmetic Mod]
      | BopLt -> let neg = next_label ()
        in let after = next_label ()
        in compile_expression functionMap varmap e1
           @ compile_expression functionMap varmap e2
           @ [Arithmetic Sub; Flow (JumpNeg neg); Stack (Push (false, "0")); Flow (Jump after); Flow (Mark neg); Stack (Push (false, "1")); Flow (Mark after)]
      | BopLeq -> let neg = next_label ()
        in let after = next_label ()
        in compile_expression functionMap varmap e2
           @ compile_expression functionMap varmap e1
           @ [Arithmetic Sub; Flow (JumpNeg neg); Stack (Push (false, "1")); Flow (Jump after); Flow (Mark neg); Stack (Push (false, "0")); Flow (Mark after)]
      | BopGt -> let neg = next_label ()
        in let after = next_label ()
        in compile_expression functionMap varmap e2
           @ compile_expression functionMap varmap e1
           @ [Arithmetic Sub; Flow (JumpNeg neg); Stack (Push (false, "0")); Flow (Jump after); Flow (Mark neg); Stack (Push (false, "1")); Flow (Mark after)]
      | BopGeq -> let neg = next_label ()
        in let after = next_label ()
        in compile_expression functionMap varmap e1
           @ compile_expression functionMap varmap e2
           @ [Arithmetic Sub; Flow (JumpNeg neg); Stack (Push (false, "1")); Flow (Jump after); Flow (Mark neg); Stack (Push (false, "0")); Flow (Mark after)]
      | BopEq -> let eq = next_label ()
        in let after = next_label ()
        in compile_expression functionMap varmap e1
           @ compile_expression functionMap varmap e2
           @ [Arithmetic Sub; Flow (JumpZero eq); Stack (Push (false, "0")); Flow (Jump after); Flow (Mark eq); Stack (Push (false, "1")); Flow (Mark after)]
      | BopNeq -> let eq = next_label ()
        in let after = next_label ()
        in compile_expression functionMap varmap e1
           @ compile_expression functionMap varmap e2
           @ [Arithmetic Sub; Flow (JumpZero eq); Stack (Push (false, "1")); Flow (Jump after); Flow (Mark eq); Stack (Push (false, "0")); Flow (Mark after)]
      | BopAnd ->
        compile_expression functionMap varmap e1
        @ compile_expression functionMap varmap e2
        @ [Arithmetic Mul]
      | BopOr ->
        compile_expression functionMap varmap e1
        @ [Stack (Push (false, "1")); Arithmetic Sub; Stack Duplicate; Arithmetic Mul] (* NOT *)
        @ compile_expression functionMap varmap e2
        @ [Stack (Push (false, "1")); Arithmetic Sub; Stack Duplicate; Arithmetic Mul] (* NOT *)
        @ [Arithmetic Mul]
        @ [Stack (Push (false, "1")); Arithmetic Sub; Stack Duplicate; Arithmetic Mul] (* NOT *)
      | BopXor ->
        compile_expression functionMap varmap e1
        @ compile_expression functionMap varmap e2
        @ [Arithmetic Sub; Stack Duplicate; Arithmetic Mul]
    end


let rec compile_statement functionMap varmap = function
  | SExpr e -> (compile_expression functionMap varmap e) @ [Stack Discard]
  | SPrintC e -> (compile_expression functionMap varmap e) @ [IO OutputC]
  | SPrintI e -> (compile_expression functionMap varmap e) @ [IO OutputI]
  | SReturn e ->
    (compile_expression functionMap varmap e)
    @ [Stack (Push (false, "0")); Heap Retrieve; Heap Retrieve; Stack (Push (false, "0")); Stack Swap; Heap Store]
    @ [Flow Return]
  | SIf (e, s) -> begin
      let l = next_label ()
      in (compile_expression functionMap varmap e) @ [Flow (JumpZero l)] @ (compile_statements functionMap varmap s) @ [Flow (Mark l)]
    end
  | SIfElse (e, s1, s2) -> begin
      let l = next_label ()
      in let l1 = next_label ()
      in (compile_expression functionMap varmap e)
         @ [Flow (JumpZero l)]
         @ (compile_statements functionMap varmap s1)
         @ [Flow (Jump l1); Flow (Mark l)]
         @ (compile_statements functionMap varmap s2)
         @ [Flow (Mark l1)]
    end
  | SLoop (e, s) -> failwith "unimplemented"
    (* begin
      let l = next_label ()
      in (compile_expression functionMap varmap e) @ compile_statements (* TODO *)
    end *)


and compile_statements functionMap oldVarmap statements =
  let varmap = ref (!oldVarmap)
  in List.fold_left (fun acc s ->
      acc @ (compile_statement functionMap varmap s)
    ) [] statements

(* TODO force functions to return a value *)
let compile_function functionMap (name, args, statements) =
  let label = match IdMap.find_opt name functionMap with
    | None -> raise (UndefinedFunction name)
    | Some l -> l
  in let varmap = ref IdMap.empty
  in let () = List.iteri (fun i arg -> varmap := IdMap.add arg (string_of_int (i + 1)) !varmap) args
  in let returnFix = match List.rev statements with
      | SReturn _ :: _ -> statements
      | xs -> List.rev (SReturn (EInteger (false, "0")) :: xs)
  in let compiled_s = compile_statements functionMap varmap returnFix
  in (Flow (Mark label))
     :: List.flatten ((List.map
           (fun arg ->
              [Stack (Push (false, "0")); Heap Retrieve; Stack (Push (false, IdMap.find arg !varmap)); Arithmetic Add; Stack Swap; Heap Store]
           ) (List.rev args)
        ))
     @ compiled_s



let compile_program fs =
  let functionMap = List.fold_left
      (fun acc (name, _, _) ->
         IdMap.add name (if name = "main" then "0" else next_label ()) acc) IdMap.empty fs
in let compiled = List.map (compile_function functionMap) fs
in Stack (Push (false, "0"))
   :: Stack (Push (false, "1"))
   :: Stack Duplicate
   :: Stack Duplicate
   :: Heap Store
   :: Heap Store
   :: (Flow (Call "0"))
   :: (Flow Terminate)
   :: (List.flatten compiled)
