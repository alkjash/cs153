type token =
  | INT of (int)
  | VAR of (Ast.var)
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | EQ
  | NEQ
  | LT
  | LTE
  | GT
  | GTE
  | NOT
  | AND
  | OR
  | ASSIGN
  | IF
  | ELSE
  | WHILE
  | FOR
  | LBRACE
  | RBRACE
  | LPAREN
  | RPAREN
  | WHITESPACE
  | COMMENT
  | SEMI
  | RETURN
  | EOF

open Parsing;;
# 4 "parse.mly"
open Ast
open Lexing
(* use this to get the line number for the n'th token *)
let rhs n =
  let pos = Parsing.rhs_start_pos n in
  pos.pos_lnum
let parse_error s =
  let pos = Parsing.symbol_end_pos () in
  let l = pos.pos_lnum in
  print_string ("line "^(string_of_int l)^": "^s^"\n") 
# 45 "parse.ml"
let yytransl_const = [|
  259 (* PLUS *);
  260 (* MINUS *);
  261 (* STAR *);
  262 (* SLASH *);
  263 (* EQ *);
  264 (* NEQ *);
  265 (* LT *);
  266 (* LTE *);
  267 (* GT *);
  268 (* GTE *);
  269 (* NOT *);
  270 (* AND *);
  271 (* OR *);
  272 (* ASSIGN *);
  273 (* IF *);
  274 (* ELSE *);
  275 (* WHILE *);
  276 (* FOR *);
  277 (* LBRACE *);
  278 (* RBRACE *);
  279 (* LPAREN *);
  280 (* RPAREN *);
  281 (* WHITESPACE *);
  282 (* COMMENT *);
  283 (* SEMI *);
  284 (* RETURN *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* VAR *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\004\000\004\000\005\000\005\000\005\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\007\000\007\000\
\008\000\008\000\008\000\008\000\009\000\009\000\009\000\010\000\
\010\000\010\000\000\000"

let yylen = "\002\000\
\002\000\001\000\002\000\003\000\002\000\005\000\007\000\005\000\
\009\000\003\000\001\000\003\000\001\000\003\000\003\000\001\000\
\003\000\003\000\003\000\003\000\003\000\003\000\001\000\002\000\
\001\000\003\000\002\000\003\000\001\000\003\000\003\000\001\000\
\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\032\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\035\000\000\000\000\000\000\000\
\011\000\000\000\000\000\023\000\000\000\000\000\000\000\033\000\
\027\000\024\000\000\000\000\000\000\000\000\000\000\000\000\000\
\001\000\003\000\005\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\012\000\
\000\000\000\000\000\000\004\000\034\000\010\000\014\000\015\000\
\017\000\018\000\019\000\020\000\021\000\022\000\028\000\026\000\
\031\000\030\000\000\000\000\000\000\000\000\000\008\000\000\000\
\000\000\000\000\007\000\000\000\000\000\009\000"

let yydgoto = "\002\000\
\013\000\014\000\015\000\016\000\017\000\018\000\019\000\020\000\
\021\000\022\000"

let yysindex = "\012\000\
\006\255\000\000\000\000\242\254\010\255\076\255\248\254\001\255\
\005\255\006\255\080\255\080\255\000\000\038\000\006\255\014\255\
\000\000\003\255\048\255\000\000\032\255\085\255\080\255\000\000\
\000\000\000\000\080\255\080\255\080\255\052\255\037\255\056\255\
\000\000\000\000\000\000\076\255\076\255\076\255\076\255\076\255\
\076\255\076\255\076\255\010\255\010\255\029\255\029\255\000\000\
\055\255\078\255\093\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\006\255\006\255\080\255\104\255\000\000\096\255\
\006\255\080\255\000\000\101\255\006\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\039\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\003\000\000\000\
\000\000\238\254\086\255\000\000\097\255\061\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\001\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\022\000\019\000\249\255\058\000\076\000\120\000\251\255\
\051\000\000\000"

let yytablesize = 285
let yytable = "\025\000\
\006\000\023\000\002\000\031\000\032\000\013\000\003\000\004\000\
\013\000\005\000\003\000\024\000\001\000\005\000\027\000\048\000\
\036\000\037\000\006\000\049\000\050\000\051\000\007\000\028\000\
\008\000\009\000\010\000\029\000\011\000\003\000\024\000\030\000\
\011\000\012\000\044\000\045\000\034\000\033\000\063\000\064\000\
\035\000\033\000\033\000\033\000\033\000\033\000\033\000\033\000\
\033\000\033\000\033\000\011\000\033\000\033\000\038\000\039\000\
\040\000\041\000\042\000\043\000\053\000\072\000\033\000\029\000\
\029\000\033\000\076\000\029\000\029\000\029\000\029\000\029\000\
\029\000\052\000\029\000\029\000\003\000\024\000\067\000\005\000\
\003\000\004\000\054\000\005\000\029\000\070\000\071\000\029\000\
\006\000\046\000\047\000\075\000\006\000\055\000\056\000\078\000\
\065\000\066\000\011\000\016\000\016\000\068\000\011\000\025\000\
\025\000\025\000\025\000\025\000\025\000\016\000\025\000\025\000\
\016\000\057\000\058\000\059\000\060\000\061\000\062\000\069\000\
\025\000\073\000\074\000\025\000\077\000\026\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\006\000\006\000\000\000\006\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\006\000\000\000\000\000\
\000\000\006\000\000\000\006\000\006\000\006\000\006\000\006\000\
\002\000\000\000\000\000\000\000\006\000"

let yycheck = "\005\000\
\000\000\016\001\000\000\011\000\012\000\024\001\001\001\002\001\
\027\001\004\001\001\001\002\001\001\000\004\001\023\001\023\000\
\014\001\015\001\013\001\027\000\028\000\029\000\017\001\023\001\
\019\001\020\001\021\001\023\001\023\001\001\001\002\001\010\000\
\023\001\028\001\003\001\004\001\015\000\000\000\044\000\045\000\
\027\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\012\001\023\001\014\001\015\001\007\001\008\001\
\009\001\010\001\011\001\012\001\024\001\069\000\024\001\003\001\
\004\001\027\001\074\000\007\001\008\001\009\001\010\001\011\001\
\012\001\022\001\014\001\015\001\001\001\002\001\024\001\004\001\
\001\001\002\001\027\001\004\001\024\001\067\000\068\000\027\001\
\013\001\005\001\006\001\073\000\013\001\036\000\037\000\077\000\
\046\000\047\000\023\001\014\001\015\001\024\001\023\001\007\001\
\008\001\009\001\010\001\011\001\012\001\024\001\014\001\015\001\
\027\001\038\000\039\000\040\000\041\000\042\000\043\000\027\001\
\024\001\018\001\027\001\027\001\024\001\006\000\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\001\001\002\001\255\255\004\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\013\001\255\255\255\255\
\255\255\017\001\255\255\019\001\020\001\021\001\022\001\023\001\
\022\001\255\255\255\255\255\255\028\001"

let yynames_const = "\
  PLUS\000\
  MINUS\000\
  STAR\000\
  SLASH\000\
  EQ\000\
  NEQ\000\
  LT\000\
  LTE\000\
  GT\000\
  GTE\000\
  NOT\000\
  AND\000\
  OR\000\
  ASSIGN\000\
  IF\000\
  ELSE\000\
  WHILE\000\
  FOR\000\
  LBRACE\000\
  RBRACE\000\
  LPAREN\000\
  RPAREN\000\
  WHITESPACE\000\
  COMMENT\000\
  SEMI\000\
  RETURN\000\
  EOF\000\
  "

let yynames_block = "\
  INT\000\
  VAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.stmt) in
    Obj.repr(
# 56 "parse.mly"
               ( _1 )
# 258 "parse.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.stmt) in
    Obj.repr(
# 60 "parse.mly"
            ( _1 )
# 265 "parse.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.stmt) in
    Obj.repr(
# 61 "parse.mly"
               ( (Seq(_1, _2), snd _1) )
# 273 "parse.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.stmt) in
    Obj.repr(
# 65 "parse.mly"
                      ( (fst _2, rhs 1) )
# 280 "parse.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.exp) in
    Obj.repr(
# 66 "parse.mly"
              ( (Exp _1, snd _1) )
# 287 "parse.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Ast.stmt) in
    Obj.repr(
# 68 "parse.mly"
      ( (If(_3, _5, (skip, snd _5)), rhs 1) )
# 295 "parse.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : Ast.exp) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.stmt) in
    Obj.repr(
# 70 "parse.mly"
      ( (If(_3, _5, _7), rhs 1) )
# 304 "parse.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Ast.stmt) in
    Obj.repr(
# 72 "parse.mly"
      ( (While(_3, _5), rhs 1) )
# 312 "parse.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : Ast.exp) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : Ast.exp) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : Ast.stmt) in
    Obj.repr(
# 74 "parse.mly"
      ( (For(_3, _5, _7, _9), rhs 1) )
# 322 "parse.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.exp) in
    Obj.repr(
# 75 "parse.mly"
                   ( (Return(_2), rhs 1) )
# 329 "parse.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 79 "parse.mly"
            ( _1 )
# 336 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.var) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 80 "parse.mly"
                   ( (Assign(_1, _3), rhs 1) )
# 344 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 84 "parse.mly"
            ( _1 )
# 351 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 85 "parse.mly"
                  ( (And(_1, _3), snd _1) )
# 359 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 86 "parse.mly"
                  ( (Or(_1, _3), snd _1) )
# 367 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 90 "parse.mly"
            ( _1 )
# 374 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 91 "parse.mly"
                  ( (Binop(_1, Eq, _3), snd _1) )
# 382 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 92 "parse.mly"
                  ( (Binop(_1, Neq, _3), snd _1) )
# 390 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 93 "parse.mly"
                  ( (Binop(_1, Lt, _3), snd _1) )
# 398 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 94 "parse.mly"
                  ( (Binop(_1, Lte, _3), snd _1) )
# 406 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 95 "parse.mly"
                  ( (Binop(_1, Gt, _3), snd _1) )
# 414 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 96 "parse.mly"
                  ( (Binop(_1, Gte, _3), snd _1) )
# 422 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 100 "parse.mly"
            ( _1 )
# 429 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 101 "parse.mly"
               ( (Not(_2), rhs 1) )
# 436 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 105 "parse.mly"
            ( _1 )
# 443 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 106 "parse.mly"
                    ( (Binop(_1, Minus, _3), snd _1) )
# 451 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 107 "parse.mly"
               ( (Binop((Int 0, rhs 1), Minus, _2), rhs 1) )
# 458 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 108 "parse.mly"
                  ( (Binop(_1, Plus, _3), snd _1) )
# 466 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 112 "parse.mly"
       ( _1 )
# 473 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 113 "parse.mly"
                    ( (Binop(_1, Div, _3), snd _1) )
# 481 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Ast.exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Ast.exp) in
    Obj.repr(
# 114 "parse.mly"
                  ( (Binop(_1, Times, _3), snd _1) )
# 489 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 118 "parse.mly"
           ( (Int(_1), rhs 1) )
# 496 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.var) in
    Obj.repr(
# 119 "parse.mly"
           ( (Var(_1), rhs 1) )
# 503 "parse.ml"
               : Ast.exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Ast.exp) in
    Obj.repr(
# 120 "parse.mly"
                     ( _2 )
# 510 "parse.ml"
               : Ast.exp))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)
