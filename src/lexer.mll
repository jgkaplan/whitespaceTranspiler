(* Acknowledgement:  the lexing code for strings, integers, identifiers
 *  and comments is adapted from the OCaml 4.04 lexer
 *  [https://github.com/ocaml/ocaml/blob/trunk/parsing/lexer.mll],
 *  written by Xavier Leroy, projet Cristal, INRIA Rocquencourt
 *  and distributed under the GNU Lesser General Public License version 2.1.
 *
 * This code is a modified version of the Cornell CS3110 Spring 2018 A4 release code.
 *)


{
open Lexing
open Parser

exception Error

let comment_depth = ref 0

(******************************************************************)
(* Helper functions for lexing strings *)
(******************************************************************)

let string_buffer = Buffer.create 256
let reset_string_buffer () = Buffer.reset string_buffer
let get_stored_string () = Buffer.contents string_buffer

let store_string_char c = Buffer.add_char string_buffer c
let store_string s = Buffer.add_string string_buffer s
let store_lexeme lexbuf = store_string (Lexing.lexeme lexbuf)

let store_escaped_char lexbuf c = store_string_char c

let hex_digit_value d = (* assert (d in '0'..'9' 'a'..'f' 'A'..'F') *)
  let d = Char.code d in
  if d >= 97 then d - 87 else
  if d >= 65 then d - 55 else
  d - 48

let hex_num_value lexbuf ~first ~last =
  let rec loop acc i = match i > last with
  | true -> acc
  | false ->
      let value = hex_digit_value (Lexing.lexeme_char lexbuf i) in
      loop (16 * acc + value) (i + 1)
  in
  loop 0 first

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let char_for_decimal_code lexbuf i =
  let c = 100 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
           10 * (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) +
                (Char.code(Lexing.lexeme_char lexbuf (i+2)) - 48) in
  if (c < 0 || c > 255)
    then raise Error
    else Char.chr c

let char_for_octal_code lexbuf i =
  let c = 64 * (Char.code(Lexing.lexeme_char lexbuf i) - 48) +
           8 * (Char.code(Lexing.lexeme_char lexbuf (i+1)) - 48) +
               (Char.code(Lexing.lexeme_char lexbuf (i+2)) - 48) in
  Char.chr c

let char_for_hexadecimal_code lexbuf i =
  let byte = hex_num_value lexbuf ~first:i ~last:(i+1) in
  Char.chr byte

}

(******************************************************************)
(* Lexer body *)
(******************************************************************)

let newline = ('\013'* '\010')
let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z']
let identchar = ['A'-'Z' 'a'-'z' '_' '0'-'9']
let id = (lowercase | '_') identchar*

let decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*
let hex_digit =
  ['0'-'9' 'A'-'F' 'a'-'f']
let hex_literal =
  '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*
let oct_literal =
  '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
let bin_literal =
  '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*
let int_literal =
  decimal_literal | hex_literal | oct_literal | bin_literal

  rule token = parse
    | blank+
          { token lexbuf }
    | ['\n']
          { new_line lexbuf; token lexbuf }
    | int_literal
          { INT (Lexing.lexeme lexbuf) }
    | "\""
          { reset_string_buffer();
            let string_start = lexbuf.lex_start_p in
            string lexbuf;
            lexbuf.lex_start_p <- string_start;
            STRING (get_stored_string()) }
    | "/*"
          { incr comment_depth;
            comment lexbuf;
            token lexbuf }
    | ";"
          { SEMI }
    | "!"
          { NOT }
    | "&&"
          { AND }
    | "||"
          { OR }
    | "+"
          { PLUS }
    | "-"
          { MINUS }
    | "*"
          { TIMES }
    | "/"
          { DIV }
    | "%"
          { MOD }
    | "<"
          { LT }
    | "<="
          { LEQ }
    | ">"
          { GT }
    | ">="
          { GEQ }
    | "("
          { LPAREN }
    | ")"
          { RPAREN }
    | "=="
          { EQUAL }
    | "!="
          { NOTEQUAL }
    | "true"
          { TRUE }
    | "false"
          { FALSE }
    | "if"
          { IF }
    | "else"
          { ELSE }
    | ":="
          { ASSIGN }
    | "function"
          { FUNCTION }
    | "while"
          { WHILE }
    | "do"
          { DO }
    | "loop"
          { LOOP }
    | "{"
          { LBRACE }
    | "}"
          { RBRACE }
    | ":"
          { COLON }
    | ","
          { COMMA }
    | "["
          { LBRACKET }
    | "]"
          { RBRACKET }
    | id
          { ID (Lexing.lexeme lexbuf) }
    | eof
          { EOF }
    | "--"
          { raise Error (* to prevent expressions like [--1] *) }
    | _
          { raise Error }

  and comment = parse
    | "/*"
          { incr comment_depth;
            comment lexbuf }
    | "*/"
          { decr comment_depth;
            let d = !comment_depth in
            if d = 0 then ()
            else if d > 0 then comment lexbuf
            else assert false
            }
    | eof
          { raise Error }
    | newline
          { new_line lexbuf;
            comment lexbuf }
    | _
          { comment lexbuf }

  and string = parse
    | '\"'
          { () }
    | '\\' newline ([' ' '\t'] * as space)
          { new_line lexbuf;
            let pos = lexbuf.lex_curr_p in
            lexbuf.lex_curr_p <- { pos with
              pos_bol = pos.pos_cnum - (String.length space)
            };
            string lexbuf }
    | '\\' ['\\' '\'' '\"' 'n' 't' 'b' 'r' ' ']
          { store_escaped_char lexbuf
                               (char_for_backslash(Lexing.lexeme_char lexbuf 1));
            string lexbuf }
    | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
          { store_escaped_char lexbuf (char_for_decimal_code lexbuf 1);
             string lexbuf }
    | '\\' 'o' ['0'-'3'] ['0'-'7'] ['0'-'7']
          { store_escaped_char lexbuf (char_for_octal_code lexbuf 2);
            string lexbuf }
    | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']
          { store_escaped_char lexbuf (char_for_hexadecimal_code lexbuf 2);
            string lexbuf }
    | '\\' _
          { raise Error }
    | newline
          { new_line lexbuf;
            store_lexeme lexbuf;
            string lexbuf }
    | eof
          { raise Error }
    | _
          { store_string_char(Lexing.lexeme_char lexbuf 0);
            string lexbuf }
