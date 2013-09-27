(* Lexer for Fish *)

(* You need to add new definition to build the
 * appropriate terminals to feed to parse.mly.
 *)

{
open Parse
open Lexing

let incr_lineno lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with
    pos_lnum = pos.pos_lnum + 1;
    pos_bol = pos.pos_cnum;
  }
}

(* definition section *)
let cr='\013'
let nl='\010'
let eol=(cr nl|nl|cr)
let ws=('\012'|'\t'|' ')*
let digit=['0'-'9']
let character=['a'-'z' 'A'-'Z']
let identifier=character (character|'_'|digit)*


(* rules section *)
rule lexer = parse
| eol { incr_lineno lexbuf; lexer lexbuf } 
| ws+ { lexer lexbuf }
| digit+ { INT(int_of_string(Lexing.lexeme lexbuf)) }
| identifier        { VAR (Lexing.lexeme lexbuf) }
| "/*"                      { comment lexbuf }
| "+"		                { PLUS }
| "-"		                { MINUS }
| "*"		                { STAR }
| "/"		                { SLASH }
| "="                       { ASSIGN }
| ";"                       { SEMI }
| "("                       { LPAREN }
| ")"                       { RPAREN }
| "!" 						{ NOT }
| "=="						{ EQ }
| "!="						{ NEQ }
| "<"						{ LT }
| "<="						{ LTE }
| ">"						{ GT }
| ">="						{ GTE }
| "&&"						{ AND }
| "||"						{ OR }
| "{"						{ LBRACE }
| "}"						{ RBRACE }
| eof		                { EOF }

and comment = parse
  | "*/"                        { lexer lexbuf }
  | eof                         { raise (Failure "missing comment terminator") }
  | _                           { comment lexbuf }

