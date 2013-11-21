type token =
  | TRUE
  | FALSE
  | HD
  | TL
  | FST
  | SND
  | ISNIL
  | PLUS
  | MINUS
  | TIMES
  | DIV
  | LPAREN
  | RPAREN
  | IF
  | THEN
  | ELSE
  | EQUALS
  | LT
  | COMMA
  | SEMI
  | TILDE
  | NIL
  | LBRACKET
  | RBRACKET
  | CONS
  | DARROW
  | LET
  | IN
  | END
  | FN
  | VAL
  | EOF
  | INT of (int)
  | ID of (string)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Mlish_ast.exp
