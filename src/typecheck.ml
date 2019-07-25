open Ast

type typ = TInt | TBool | TChar | TString | TArr

exception ImproperType of string

type functionParams = typ list * typ

let rec check_exp expr (varstore : typ IdMap.t ref) (functionmap : functionParams IdMap.t) = match expr with
  | EInteger _ -> TInt
  | EString _ -> TString
  | EBool _ -> TBool
  | EChar _ -> TChar
  | EVar x -> begin
      match IdMap.find_opt x !varstore with
      | None -> raise (ImproperType "Unbound variable: " ^ x)
      | Some t -> t
    end
  | EUop (uop, e) -> begin
      match (uop, check_exp e) with
      | UopMinus, TInt -> TInt
      | UopNot, TBool -> TBool
      | _ -> raise (ImproperType "Bad unary operator")
    end
  | EApp (id, es) -> begin
      match IdMap.find_opt id functionmap with
      | None -> raise (ImproperType "Unknown function: " ^ id)
      | Some (args, t) ->
        if map (fun e -> check_expr e varstore functionmap) es = args
        then t
        else raise (ImproperType "Invalid arguments to function " ^ id)
    end
  | EAssign (id, e) ->
      let t = check_exp e
      in varstore := IdMap.add id t !varstore; t
  | EBop (e1, bop, e2) -> begin
      match (e1, e2) with
      | TInt, TInt -> begin
          match bop with
          | BopPlus | BopMinus | BopTimes | BopDiv | BopMod -> TInt
          | BopLt | BopLeq | BopGt | BopGeq | BopEq | BopNeq -> TBool
          | _ -> raise (ImproperType "Bad binary operation")
        end
      | TChar, TChar | TString, TString -> begin
          match bop with
          | BopEq | BopNeq -> TBool
          | _ -> raise (ImproperType "Bad binary operation")
        end
      | TBool, TBool -> begin
          match bop with
          | BopEq | BopNeq | BopAnd | BopOr | BopXor -> TBool
          | _ -> raise (ImproperType "Bad binary operation")
        end
      | _ -> -> raise (ImproperType "Bad binary operation")
    end
