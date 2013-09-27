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

%left ASSIGN
%left NOT AND OR
%left EQ NEQ LT LTE GT GTE
%left PLUS MINUS
%left MULTIPLY DIVIDE

%nonassoc IFX
%nonassoc ELSE

/* Here's where the real grammar starts -- you'll need to add 
 * more rules here... Do not remove the 2%'s!! */
%%

/* Line numbers not parsed right now */

program:
  stmt EOF 				{ $1 }
;

stmt:
  astmt 				{ $1 } 
| astmt stmt			{ (Seq($1, $2), snd $1) }
;

astmt:
  LBRACE stmt RBRACE 	{ (fst $2, rhs 1) }
| exp SEMI				{ (Exp $1, snd $1) }
| IF LPAREN exp RPAREN astmt %prec IFX
						{ (If($3, $5, (skip, snd $5)), rhs 1) }
| IF LPAREN exp RPAREN astmt ELSE astmt
						{ (If($3, $5, $7), rhs 1) }
| WHILE LPAREN exp RPAREN astmt
						{ (While($3, $5), rhs 1) }
| FOR LPAREN exp SEMI exp SEMI exp RPAREN astmt
						{ (For($3, $5, $7, $9), rhs 1) }
| RETURN exp SEMI		{ (Return($2), rhs 1) }
;

exp:
  fexp 					{ $1 }
| VAR ASSIGN exp 		{ (Assign($1, $3), rhs 1) }
;

fexp:
  eexp 					{ $1 }
| eexp AND fexp 		{ (And($1, $3), snd $1) }
| eexp OR fexp 			{ (Or($1, $3), snd $1) }
;

eexp:
  dexp 					{ $1 }
| dexp EQ eexp 			{ (Binop($1, Eq, $3), snd $1) }
| dexp NEQ eexp 		{ (Binop($1, Neq, $3), snd $1) }
| dexp LT eexp 			{ (Binop($1, Lt, $3), snd $1) }
| dexp LTE eexp 		{ (Binop($1, Lte, $3), snd $1) }
| dexp GT eexp 			{ (Binop($1, Gt, $3), snd $1) }
| dexp GTE eexp 		{ (Binop($1, Gte, $3), snd $1) }
;

dexp:
  cexp 					{ $1 }
| NOT dexp 				{ (Not($2), rhs 1) }
;

cexp:
  bexp 					{ $1 }
| bexp MINUS cexp 		{ (Binop($1, Minus, $3), snd $1) }
| MINUS cexp			{ (Binop((Int 0, rhs 1), Minus, $2), rhs 1) }
| bexp PLUS cexp		{ (Binop($1, Plus, $3), snd $1) }
;

bexp:
  aexp { $1 }
| aexp SLASH bexp 		{ (Binop($1, Div, $3), snd $1) }
| aexp STAR bexp		{ (Binop($1, Times, $3), snd $1) }
;

aexp:
  INT 					{ (Int($1), rhs 1) }
| VAR 					{ (Var($1), rhs 1) }
| LPAREN exp RPAREN 	{ $2 }
;
