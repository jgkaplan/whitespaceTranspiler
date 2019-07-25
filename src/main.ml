open Ast
open Transpile
open WhitespaceAst

let usage_message = "Whitespace Transpiler transpiles a human-readable programming language into whitespace. \n\tUsage: ./wst <inputfile>"

open Parse

let compile_file filename =
  let output_name = (Filename.remove_extension filename) ^ ".ws"
  in let inchannel = open_in filename
  in let lexbuf = Lexing.from_channel inchannel
  in let parsedAST = parse_program lexbuf
(* in let () = List.iter print_statement parsedAST *)
  in let compiledAST = compile_program parsedAST
  in let output = pToString compiledAST
  in let outchannel = open_out output_name
  in output_string outchannel output; flush outchannel; close_in inchannel; close_out outchannel

let () =
  Arg.parse [] compile_file usage_message
