open whitespaecAst

module FunctionMap = Map.Make(String)

let print_char = [IO OutputC]

let print_int = [IO OutputI]

let read_char = [IO InputC]

let read_int = [IO InputI]

(* let length = [] *)

let builtinLabels = ["print"]

(* print string

   push (startingAddress of string)
   call label_0


label_0:
  dup
  retrieve
  dup
  jz label_1
  printc
  push 1
  add
  jmp label_0
label_1:
   drop
   drop
   ret
*)


(* read string

   push (startingAddress of where you want to put string)
   call label_1


  label_1:
      dup
      dup
      readc
      retrieve
      dup
      push 10
      sub
      jz label_5
      drop
      push 1
      add
      jmp label_1
  label_5:
      drop
      push 1
      add
      push 0
      store
      ret
*)

(* print newline
  label_2:
    push 10
    push 13
    printc
    printc
*)
