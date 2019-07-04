type token = Space | Tab | Newline

type label = string

type program = expression list

type expression =
  | Stack of stackE
  | Arithmetic of arithE
  | Heap of heapE
  | Flow of flowE
  | IO of ioE

type stackE =
  | Push of string (* Push int to top of stack *)
  | Duplicate (* Duplicate top item of stack *)
  | Copy of string (* Copy nth item to top of stack *)
  | Swap (* Swap top two items *)
  | Discard (* Discard top item of stack *)
  | Slide of string (* Slide n items off stack, keeping top item *)

type arithE =
  | Add
  | Sub
  | Mul
  | Div
  | Mod

type heapE =
  | Store
  | Retrieve

type flowE =
  | Mark of label
  | Call of label
  | Jump of label
  | JumpZero of label
  | JumpNeg of label
  | Return
  | Terminate

type ioE =
  | OutputC
  | OutputI
  | InputC
  | InputI

      (*
https://stackoverflow.com/questions/29957418/how-to-convert-char-list-to-string-in-ocaml

  def divByTwo(s):
      new_s = ''
      add = 0

      for ch in s:
          new_dgt = (ord(ch) - ord('0')) // 2 + add
          new_s = '%s%d' % (new_s, new_dgt)
          add = oddsToOne(ch) * 5

      if new_s != '0' and new_s.startswith('0'):
          new_s = new_s[1:]

      return new_s
*)
let strIntToTokens : string -> token list =
  let isOdd s =
    match String.get s (String.length s - 1) with
    | '1' | '3' | '5' | '7' | '9' -> true
    | _ -> false
  in let rec divByTwo s acc add =
    if s = ""
    then String.concat "" (List.map (String.make 1) acc)
    else
      let ch = String.get s 0
      in let n = (Char.code ch - Char.code '0') / 2 + add
      in let new_s = acc @ [Char.chr (n + Char.code '0')]
      in let new_s' = match new_s with
          | '0'::[] -> ['0']
          | '0'::r -> r
          | r -> r
      in divByTwo (String.sub s 1 ((String.length s)-1)) new_s' (if ch = '1' || ch = '3' || ch = '5' || ch = '7' || ch = '9' then 5 else 0)
  in let rec toBin s acc =
    if s = "0" then acc else toBin (divByTwo s [] 0) ((if isOdd s then Tab else Space) :: acc)
  in fun s -> if s = "0" then [Space; Space; Newline] else Space :: (toBin s [Newline])


let intToTokens : int -> token list = fun n ->
  let first = if n >= 0 then Space else Tab
  in let rec intToBin a acc =
       if a = 0
       then acc
       else
         let bit = if a land 1 = 1 then Tab else Space
         in intToBin (a asr 1) (bit::acc)
  in first :: (intToBin (abs n) [Newline])

let stackToTokens = function
  | Push i -> Space :: (strIntToTokens i)
  | Duplicate -> [Newline; Space]
  | Copy i -> Tab :: Space :: (strIntToTokens i)
  | Swap -> [Newline; Tab]
  | Discard -> [Newline; Newline]
  | Slide i -> Tab :: Newline :: (strIntToTokens i)

let arithToTokens = function
  | Add -> [Space; Space]
  | Sub -> [Space; Tab]
  | Mul -> [Space; Newline]
  | Div -> [Tab; Space]
  | Mod -> [Tab; Tab]

let heapToTokens = function
  | Store -> [Space]
  | Retrieve -> [Tab]

let flowToTokens = function
  | Mark l -> Space :: Space :: (strIntToTokens l)
  | Call l -> Space :: Tab :: (strIntToTokens l)
  | Jump l -> Space :: Newline :: (strIntToTokens l)
  | JumpZero l -> Tab :: Space :: (strIntToTokens l)
  | JumpNeg l -> Tab :: Tab :: (strIntToTokens l)
  | Return -> [Tab; Newline]
  | Terminate -> [Newline; Newline]

let ioToTokens = function
  | OutputC -> [Space; Space]
  | OutputI -> [Space; Tab]
  | InputC -> [Tab; Space]
  | InputI -> [Tab; Tab]

let eToTokens : expression -> token list = function
  | Stack s -> Space :: (stackToTokens s)
  | Arithmetic a -> Tab :: Space :: (arithToTokens a)
  | Heap h -> Tab :: Tab :: (heapToTokens h)
  | Flow f -> Newline :: (flowToTokens f)
  | IO io -> Tab :: Newline :: (ioToTokens io)

let tokenToString : token -> string = function
  | Space -> " "
  | Tab -> "\t"
  | Newline -> "\n"

let eToString e = e |> toTokens |> map tokenToString |> String.concat ""

let pToString p = map eToString p |> String.concat ""
