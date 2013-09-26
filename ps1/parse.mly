/* Parser for Fish --- TODO */

%{
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
%}

/* Tells us which non-terminal to start the grammar with. */
%start program

/* This specifies the non-terminals of the grammar and specifies the
 * types of the values they build. Don't forget to add any new non-
 * terminals here.
 */
%type <Ast.program> program
%type <Ast.stmt> stmt astmt
%type <Ast.exp> exp fexp eexp dexp cexp bexp aexp

/* The %token directive gives a definition of all of the terminals
 * (i.e., tokens) in the grammar. This will be used to generate the
 * tokens definition used by the lexer. So this is effectively the
 * interface between the lexer and the parser --- the lexer must
 * build values using this datatype constructor to pass to the parser.
 * You will need to augment this with your own tokens...
 */
%token <int> INT 
%token <Ast.var> VAR
%token PLUS MINUS STAR SLASH EQ NEQ LT LTE GT GTE
%token NOT AND OR ASSIGN IF ELSE WHILE FOR
%token LBRACE RBRACE LPAREN RPAREN WHITESPACE COMMENT SEMI RETURN EOF

/* Here's where the real grammar starts -- you'll need to add 
 * more rules here... Do not remove the 2%'s!! */
%%

/* Line numbers not parsed right now */

program:
  stmt EOF { $1 }
;

stmt:
  astmt { $1 } 
;

astmt:
   LBRACE astmt RBRACE { (Ast.skip, 0) }
;

exp:
  /* empty */ { (Int 0, 0) }
;

fexp:
  /* empty */ { (Int 0, 0) }
;

eexp:
  /* empty */ { (Int 0, 0) }
;

dexp:
  /* empty */ { (Int 0, 0) }
;

cexp:
  /* empty */ { (Int 0, 0) }
;

bexp:
  /* empty */ { (Int 0, 0) }
;

aexp:
  /* empty */ { (Int 0, 0) }
;
