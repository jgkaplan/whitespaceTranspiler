type EType = TInt | TBool | TChar | TString | TArr

exception ImproperType;

let rec check expr = match expr with
  | EInteger _ -> TInt
  | EString _ -> TString
  | EBool _ -> TBool
  | EChar _ -> TChar
  | EUop (uop, e) -> begin
      match (uop, check e) with
      | UopMinus, TInt -> TInt
      | UopNot, TBool -> TBool
      | _ -> raise ImproperType
    end
  | EApp (id, es) -> 
