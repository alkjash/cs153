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

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.program
