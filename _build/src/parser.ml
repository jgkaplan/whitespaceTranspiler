
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | WHILE
    | TRUE
    | TIMES
    | STRING of (
# 19 "src/parser.mly"
       (string)
# 14 "src/parser.ml"
  )
    | SEMI
    | RPAREN
    | RBRACKET
    | RBRACE
    | PRINT
    | PLUS
    | OR
    | NOTEQUAL
    | NOT
    | MOD
    | MINUS
    | LT
    | LPAREN
    | LOOP
    | LEQ
    | LBRACKET
    | LBRACE
    | INT of (
# 18 "src/parser.mly"
       (string)
# 36 "src/parser.ml"
  )
    | IF
    | ID of (
# 19 "src/parser.mly"
       (string)
# 42 "src/parser.ml"
  )
    | GT
    | GEQ
    | FUNCTION
    | FALSE
    | EQUAL
    | EOF
    | ELSE
    | DO
    | DIV
    | COMMA
    | COLON
    | BREAK
    | ASSIGN
    | AND
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState53
  | MenhirState49
  | MenhirState42
  | MenhirState38
  | MenhirState36
  | MenhirState33
  | MenhirState32
  | MenhirState28
  | MenhirState26
  | MenhirState25
  | MenhirState16
  | MenhirState11
  | MenhirState5
  | MenhirState4
  | MenhirState3
  | MenhirState0

# 10 "src/parser.mly"
  
open Ast

let has_dups lst =
  let open List in
  length lst <> length (sort_uniq Pervasives.compare lst)

# 99 "src/parser.ml"

let rec _menhir_goto_list_innerStatement_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.statements) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState32 | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (s : (Ast.statements)) = _v in
        let _v : (Ast.statements) = 
# 103 "src/parser.mly"
                               ( s )
# 111 "src/parser.ml"
         in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        (match _menhir_s with
        | MenhirState38 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let (((_menhir_stack, _menhir_s), _, (e : (Ast.expr))), _, (s : (Ast.statements))) = _menhir_stack in
                let _5 = () in
                let _3 = () in
                let _1 = () in
                let _v : (Ast.statement) = 
# 107 "src/parser.mly"
                                                          ( SLoop (e, s) )
# 131 "src/parser.ml"
                 in
                _menhir_goto_innerStatement _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | MenhirState32 ->
            let _menhir_stack = Obj.magic _menhir_stack in
            assert (not _menhir_env._menhir_error);
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | RBRACE ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _menhir_stack = Obj.magic _menhir_stack in
                let ((((_menhir_stack, _menhir_s), _, (f : (Ast.id))), _, (xs0 : (Ast.id list))), _, (s : (Ast.statements))) = _menhir_stack in
                let _8 = () in
                let _6 = () in
                let _5 = () in
                let _3 = () in
                let _1 = () in
                let _v : (Ast.statement) = let args =
                  let xs = xs0 in
                  
# 220 "/Users/joshuakaplan/.opam/4.06.1/lib/menhir/standard.mly"
    ( xs )
# 160 "src/parser.ml"
                  
                in
                
# 100 "src/parser.mly"
        ( SFunction (f, args, s) )
# 166 "src/parser.ml"
                 in
                let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
                let _menhir_stack = Obj.magic _menhir_stack in
                assert (not _menhir_env._menhir_error);
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | FUNCTION ->
                    _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState53
                | EOF ->
                    _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack) MenhirState53
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState53)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            _menhir_fail ())
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.statements)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ast.statement))) = _menhir_stack in
        let _v : (Ast.statements) = 
# 201 "/Users/joshuakaplan/.opam/4.06.1/lib/menhir/standard.mly"
    ( x :: xs )
# 197 "src/parser.ml"
         in
        _menhir_goto_list_innerStatement_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_innerStatement : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.statement) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | INT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | LOOP ->
        _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | MINUS ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | NOT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | PRINT ->
        _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | STRING _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState42 _v
    | TRUE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | RBRACE ->
        _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack) MenhirState42
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState42

and _menhir_goto_separated_nonempty_list_COMMA_expr_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ast.expr list)) = _v in
        let _v : (Ast.expr list) = 
# 144 "/Users/joshuakaplan/.opam/4.06.1/lib/menhir/standard.mly"
    ( x )
# 247 "src/parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.expr list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ast.expr))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.expr list) = 
# 231 "/Users/joshuakaplan/.opam/4.06.1/lib/menhir/standard.mly"
    ( x :: xs )
# 259 "src/parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_reduce11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.statements) = 
# 199 "/Users/joshuakaplan/.opam/4.06.1/lib/menhir/standard.mly"
    ( [] )
# 270 "src/parser.ml"
     in
    _menhir_goto_list_innerStatement_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run33 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | INT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | MINUS ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | NOT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | STRING _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState33 _v
    | TRUE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState33
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState33

and _menhir_run36 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | INT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | MINUS ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | NOT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | STRING _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
    | TRUE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState36
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState16 | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | ID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
            | INT _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | MINUS ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | NOT ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | STRING _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState16 _v
            | TRUE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState16
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState16)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.expr))) = _menhir_stack in
            let _v : (Ast.expr list) = 
# 229 "/Users/joshuakaplan/.opam/4.06.1/lib/menhir/standard.mly"
    ( [ x ] )
# 368 "src/parser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_expr_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.expr) = 
# 157 "src/parser.mly"
        ( e )
# 392 "src/parser.ml"
             in
            _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
        let _10 = () in
        let _v : (Ast.expr) = let uop =
          let _1 = _10 in
          
# 184 "src/parser.mly"
          ( UopMinus )
# 411 "src/parser.ml"
          
        in
        
# 116 "src/parser.mly"
        ( EUop (uop, e) )
# 417 "src/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
        let _10 = () in
        let _v : (Ast.expr) = let uop =
          let _1 = _10 in
          
# 185 "src/parser.mly"
        ( UopNot )
# 430 "src/parser.ml"
          
        in
        
# 116 "src/parser.mly"
        ( EUop (uop, e) )
# 436 "src/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 56 "src/parser.mly"
       (Ast.expr)
# 452 "src/parser.ml"
            ) = 
# 66 "src/parser.mly"
        ( e )
# 456 "src/parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (
# 56 "src/parser.mly"
       (Ast.expr)
# 463 "src/parser.ml"
            )) = _v in
            Obj.magic _1
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let ((_menhir_stack, _menhir_s), _, (e : (Ast.expr))) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : (Ast.statement) = 
# 108 "src/parser.mly"
                             ( SPrint e )
# 487 "src/parser.ml"
             in
            _menhir_goto_innerStatement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | ID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
            | INT _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
            | LOOP ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | MINUS ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | NOT ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | PRINT ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | STRING _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
            | TRUE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | RBRACE ->
                _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState32 | MenhirState38 | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (e : (Ast.expr))) = _menhir_stack in
            let _2 = () in
            let _v : (Ast.statement) = 
# 106 "src/parser.mly"
                      ( SExpr e )
# 552 "src/parser.ml"
             in
            _menhir_goto_innerStatement _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_separated_nonempty_list_COMMA_ident_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.id list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (x : (Ast.id list)) = _v in
        let _v : (Ast.id list) = 
# 144 "/Users/joshuakaplan/.opam/4.06.1/lib/menhir/standard.mly"
    ( x )
# 579 "src/parser.ml"
         in
        _menhir_goto_loption_separated_nonempty_list_COMMA_ident__ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.id list)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ast.id))) = _menhir_stack in
        let _2 = () in
        let _v : (Ast.id list) = 
# 231 "/Users/joshuakaplan/.opam/4.06.1/lib/menhir/standard.mly"
    ( x :: xs )
# 591 "src/parser.ml"
         in
        _menhir_goto_separated_nonempty_list_COMMA_ident_ _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        _menhir_fail ()

and _menhir_goto_loption_separated_nonempty_list_COMMA_ident__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.id list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LBRACE ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | ID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
            | INT _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
            | LOOP ->
                _menhir_run36 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | MINUS ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | NOT ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | PRINT ->
                _menhir_run33 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | STRING _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
            | TRUE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | RBRACE ->
                _menhir_reduce11 _menhir_env (Obj.magic _menhir_stack) MenhirState32
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr list) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let _menhir_stack = Obj.magic _menhir_stack in
    assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | RPAREN ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _menhir_stack = Obj.magic _menhir_stack in
        let ((_menhir_stack, _menhir_s, (f : (Ast.id))), _, (xs0 : (Ast.expr list))) = _menhir_stack in
        let _4 = () in
        let _2 = () in
        let _v : (Ast.expr) = let es =
          let xs = xs0 in
          
# 220 "/Users/joshuakaplan/.opam/4.06.1/lib/menhir/standard.mly"
    ( xs )
# 672 "src/parser.ml"
          
        in
        
# 114 "src/parser.mly"
        ( EApp (f, es) )
# 678 "src/parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_goto_simple_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.expr) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (e : (Ast.expr)) = _v in
    let _v : (Ast.expr) = 
# 112 "src/parser.mly"
        ( e )
# 696 "src/parser.ml"
     in
    _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_list_statement_ : _menhir_env -> 'ttv_tail -> _menhir_state -> (Ast.statements) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (xs : (Ast.statements)) = _v in
        let (_menhir_stack, _menhir_s, (x : (Ast.statement))) = _menhir_stack in
        let _v : (Ast.statements) = 
# 201 "/Users/joshuakaplan/.opam/4.06.1/lib/menhir/standard.mly"
    ( x :: xs )
# 711 "src/parser.ml"
         in
        _menhir_goto_list_statement_ _menhir_env _menhir_stack _menhir_s _v
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_stack = Obj.magic _menhir_stack in
        let (s : (Ast.statements)) = _v in
        let _v : (Ast.statements) = 
# 96 "src/parser.mly"
                          ( s )
# 721 "src/parser.ml"
         in
        let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | EOF ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (s : (Ast.statements))) = _menhir_stack in
            let _2 = () in
            let _v : (
# 58 "src/parser.mly"
       (Ast.statements)
# 736 "src/parser.ml"
            ) = 
# 69 "src/parser.mly"
                          ( s )
# 740 "src/parser.ml"
             in
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_1 : (
# 58 "src/parser.mly"
       (Ast.statements)
# 747 "src/parser.ml"
            )) = _v in
            Obj.magic _1
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.expr) = 
# 163 "src/parser.mly"
  ( EBool true )
# 767 "src/parser.ml"
     in
    _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 19 "src/parser.mly"
       (string)
# 774 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (s : (
# 19 "src/parser.mly"
       (string)
# 782 "src/parser.ml"
    )) = _v in
    let _v : (Ast.expr) = 
# 161 "src/parser.mly"
  ( EString s )
# 787 "src/parser.ml"
     in
    _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | INT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | MINUS ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | NOT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | STRING _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState3 _v
    | TRUE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState3
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState3

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | INT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | MINUS ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | NOT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | STRING _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | TRUE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | INT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | MINUS ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | NOT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | STRING _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | TRUE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 18 "src/parser.mly"
       (string)
# 875 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (s : (
# 18 "src/parser.mly"
       (string)
# 883 "src/parser.ml"
    )) = _v in
    let _v : (Ast.expr) = 
# 159 "src/parser.mly"
  ( EInteger (false, s) )
# 888 "src/parser.ml"
     in
    _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 19 "src/parser.mly"
       (string)
# 895 "src/parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let (x : (
# 19 "src/parser.mly"
       (string)
# 903 "src/parser.ml"
    )) = _v in
    let _v : (Ast.id) = 
# 181 "src/parser.mly"
        ( x )
# 908 "src/parser.ml"
     in
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState32 | MenhirState38 | MenhirState42 | MenhirState36 | MenhirState33 | MenhirState0 | MenhirState3 | MenhirState4 | MenhirState16 | MenhirState11 | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | FALSE ->
                _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState11
            | ID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
            | INT _v ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
            | LPAREN ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState11
            | MINUS ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState11
            | NOT ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState11
            | STRING _v ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState11 _v
            | TRUE ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState11
            | RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState11 in
                let _v : (Ast.expr list) = 
# 142 "/Users/joshuakaplan/.opam/4.06.1/lib/menhir/standard.mly"
    ( [] )
# 944 "src/parser.ml"
                 in
                _menhir_goto_loption_separated_nonempty_list_COMMA_expr__ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState11)
        | COMMA | EOF | LBRACE | RPAREN | SEMI ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.id))) = _menhir_stack in
            let _v : (Ast.expr) = 
# 155 "src/parser.mly"
        ( EVar x )
# 957 "src/parser.ml"
             in
            _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | LPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState28 _v
            | RPAREN ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_s = MenhirState28 in
                let _v : (Ast.id list) = 
# 142 "/Users/joshuakaplan/.opam/4.06.1/lib/menhir/standard.mly"
    ( [] )
# 984 "src/parser.ml"
                 in
                _menhir_goto_loption_separated_nonempty_list_COMMA_ident__ _menhir_env _menhir_stack _menhir_s _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState28)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState49 | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | COMMA ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | ID _v ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState49 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState49)
        | RPAREN ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, (x : (Ast.id))) = _menhir_stack in
            let _v : (Ast.id list) = 
# 229 "/Users/joshuakaplan/.opam/4.06.1/lib/menhir/standard.mly"
    ( [ x ] )
# 1019 "src/parser.ml"
             in
            _menhir_goto_separated_nonempty_list_COMMA_ident_ _menhir_env _menhir_stack _menhir_s _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | _ ->
        _menhir_fail ()

and _menhir_run8 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _menhir_stack = Obj.magic _menhir_stack in
    let _1 = () in
    let _v : (Ast.expr) = 
# 165 "src/parser.mly"
  ( EBool false )
# 1039 "src/parser.ml"
     in
    _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState53 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState49 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState42 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState38 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState36 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState33 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState32 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState28 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState26 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState25 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState16 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState11 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState5 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState4 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState3 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s
    | MenhirState0 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_reduce13 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Ast.statements) = 
# 199 "/Users/joshuakaplan/.opam/4.06.1/lib/menhir/standard.mly"
    ( [] )
# 1114 "src/parser.ml"
     in
    _menhir_goto_list_statement_ _menhir_env _menhir_stack _menhir_s _v

and _menhir_run26 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState26 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState26

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and _menhir_init : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> _menhir_env =
  fun lexer lexbuf ->
    let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and parse_expression : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 56 "src/parser.mly"
       (Ast.expr)
# 1156 "src/parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = _menhir_init lexer lexbuf in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FALSE ->
        _menhir_run8 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | ID _v ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | INT _v ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | LPAREN ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | MINUS ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NOT ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | STRING _v ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | TRUE ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0)

and parse_statements : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 58 "src/parser.mly"
       (Ast.statements)
# 1188 "src/parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env = _menhir_init lexer lexbuf in
    Obj.magic (let _menhir_stack = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | FUNCTION ->
        _menhir_run26 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | EOF ->
        _menhir_reduce13 _menhir_env (Obj.magic _menhir_stack) MenhirState25
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState25)

# 233 "/Users/joshuakaplan/.opam/4.06.1/lib/menhir/standard.mly"
  

# 1208 "src/parser.ml"
