(* This file should be extended to implement the Fish parser using the 
 * parsing combinator library, and the combinator-based lexer. *)
open Lcombinators.GenericParsing
open Comblexer
open Ast

exception TODO
exception FatalError

let dummy_pos : pos = 0

(*
let rec make_exp_parser (():unit) : (token, exp) parser =
  let int_parser = satisfy_opt (function INT i -> Some (Int i, dummy_pos) | _ -> None) in
  let sub_parser = seq (satisfy (fun t -> t == LPAREN), 
    lazy_seq (lazy (make_exp_parser ()), lazy (satisfy (fun t -> t == RPAREN)))) in 
  let sub_exp_parser = map (fun (_, (e, _)) -> e) sub_parser in
  let first_parser = alt (int_parser, sub_exp_parser) in
  let rest_parser = seq (first_parser, make_binop_rest ()) in
  let binop_parser = map (fun (e1, (op, e2)) -> (Binop (e1, op, e2), dummy_pos)) rest_parser in
  alts [binop_parser; first_parser]
and make_binop_rest (():unit) : (token, (binop * exp)) parser =
  let binop_op_parser = satisfy_opt (function 
    PLUS -> Some Plus | MINUS -> Some Minus | 
	STAR -> Some Times | SLASH -> Some Div | EQ -> Some Eq | _ -> None) in
  lazy_seq (lazy binop_op_parser, lazy (make_exp_parser ()))

*)

(*
(* Constructs a statement parser, which either matches a return statement or
   a normal statement, which we construct using make_exp_parser *)
let rec make_stmt_parser (():unit) : (token, stmt) parser =
  let return_parser = seq (satisfy (fun t -> t == RETURN), lazy_seq (lazy (make_exp_parser ()), 
    lazy (satisfy (fun t -> t == SEMI)))) in
  let return_stmt_parser = map (fun (_, (e, _)) -> ((Return e), dummy_pos)) return_parser in
  let exp_stmt_parser = map (fun e -> (Exp e, dummy_pos)) (make_exp_parser ()) in
  alts [return_stmt_parser; exp_stmt_parser]
*)

(* New combinator function: matches a specified token and consumes it *)
let tok (t : token) : (token, token) parser = satisfy (fun h -> t = h) 

(* Constructs an expression parser *)
(* Expressions factor as:
   aexp = Int | Var | ( exp )
   bexp = aexp | aexp / bexp
   cexp = bexp | bexp * cexp
   dexp = cexp | cexp - dexp
   eexp = dexp | dexp + eexp
   fexp = eexp | !fexp
   gexp = fexp | fexp (== , != , < , <= , > , >=) gexp
   hexp = gexp | gexp && hexp | gexp || hexp
   exp = hexp | Var = exp

   Extra levels of refactoring were added to make arithmetic bind completely correctly.
   *)

(* aexp = Int | Var | ( exp ) *)
let rec make_aexp_parser (() : unit) : (token, exp) parser =
  let int_parser = satisfy (fun h -> h = INT(_)) in
  let int_exp_parser = map (fun _(i) -> (Int(i), dummy_pos)) int_parser in
  let var_parser = satisfy (fun h -> h = VAR(_)) in
  let var_exp_parser = map (fun _(v) -> (Var(v), dummy_pos)) var_parser in
  let paren_parser = lazy_seq (lazy (tok LPAREN),
	lazy_seq (lazy (make_exp_parser ()), lazy (tok RPAREN))) in
  let paren_exp_parser = map (fun (_, (e, _)) -> (e, dummy_pos)) in
  alts [int_exp_parser; var_exp_parser; paren_exp_parser]

(* bexp = aexp | aexp / bexp *)
let rec make_bexp_parser (() : unit) : (token, exp) parser = 
  let slash_parser = lazy_seq (lazy (make_aexp_parser ()), 
	lazy_seq (lazy (tok SLASH), lazy (make_bexp_parser ()))) in
  let div_exp_parser = map (fun (e1, (_, e2)) -> (Binop(e1, Div, e2), dummy_pos)) slash_parser in
  alt (make_aexp_parser (), div_exp_parser)

(* cexp = bexp | bexp * cexp *)
let rec make_cexp_parser (() : unit) : (token, exp) parser = 
  let star_parser = lazy_seq (lazy (make_bexp_parser ()), 
	lazy_seq (lazy (tok STAR), lazy (make_cexp_parser ()))) in
  let times_exp_parser = map (fun (e1, (_, e2)) -> (Binop(e1, Times, e2), dummy_pos)) star_parser in
  alt (make_bexp_parser (), times_exp_parser)

(* dexp = cexp | cexp - dexp *)
let rec make_dexp_parser (() : unit) : (token, exp) parser = 
  let minus_parser = lazy_seq (lazy (make_cexp_parser ()), 
	lazy_seq (lazy (tok MINUS), lazy (make_dexp_parser ()))) in
  let minus_exp_parser = map (fun (e1, (_, e2)) -> (Binop(e1, Minus, e2), dummy_pos)) minus_parser in
  alt (make_cexp_parser (), minus_exp_parser)

(* eexp = dexp | dexp + eexp *)
let rec make_eexp_parser (() : unit) : (token, exp) parser = 
  let plus_parser = lazy_seq (lazy (make_dexp_parser ()), 
	lazy_seq (lazy (tok PLUS), lazy (make_eexp_parser ()))) in
  let plus_exp_parser = map (fun (e1, (_, e2)) -> (Binop(e1, Plus, e2), dummy_pos)) plus_parser in
  alt (make_dexp_parser (), plus_exp_parser)

(* fexp = eexp | !fexp *)
let rec make_fexp_parser (() : unit) : (token, exp) parser = 
  let not_parser = lazy_seq (lazy (tok NOT), 
	lazy (make_fexp_parser ())) in
  let not_exp_parser = map (fun (_, e) -> (Not(e), dummy_pos)) not_parser in
  alt (make_eexp_parser (), not_exp_parser)

(* gexp = fexp | fexp (== , != , < , <= , > , >=) gexp *)
let rec make_gexp_parser (() : unit) : (token, exp) parser = 
  let compare_parser = lazy_seq (lazy (make_fexp_paddrser ()), 
	lazy_seq (lazy (alt (tok EQ, tok NEQ, tok LT, tok TLE, tok GT, tok GTE)), 
	lazy (make_gexp_parser ()))) in
  let compare_exp_parser = map (fun (e1, (op, e2)) -> match op with
      EQ -> (Binop(e1, Eq, e2), dummy_pos)
    | NEQ -> (Binop(e1, Neq, e2), dummy_pos)
    | LT -> (Binop(e1, Lt, e2), dummy_pos)
    | LTE -> (Binop(e1, Lte, e2), dummy_pos)
    | GT -> (Binop(e1, Gt, e2), dummy_pos)
    | GTE -> (Binop(e1, Gte, e2), dummy_pos)
    | _ -> raise FatalError) in
  alt (make_fexp_parser (), compare_exp_parser)

(* hexp = gexp | gexp && hexp | gexp || hexp *)
let rec make_hexp_parser (() : unit) : (token, exp) parser = 
  let andor_parser = lazy_seq (lazy (make_gexp_paddrser ()), 
	lazy_seq (lazy (alt (tok AND, tok OR)), lazy (make_hexp_parser ()))) in
  let andor_exp_parser = map (fun (e1, (op, e2)) -> match op with
      AND -> (And(e1, e2), dummy_pos)
    | OR -> (Or(e1, e2), dummy_pos)
    | _ -> raise FatalError) in
  alt (make_gexp_parser (), andor_exp_parser)

(* exp = hexp | Var = exp *)
let rec make_exp_parser (() : unit) : (token, exp) parser = 
  let assign_parser = lazy_seq (lazy (satisfy (fun t -> t = VAR(_))), 
	lazy_seq (lazy (tok ASSIGN), lazy (make_exp_parser ()))) in
  let assign_exp_parser = map (fun (_(v), (_, e)) -> (Assign(v, e), dummy_pos)) assign_parser in
  alt (make_hexp_parser (), assign_exp_parser)

(* Parses a statement of the form Exp, If-Else, While, For, Return, { stmt } 
   Does not match the empty list to guarantee that make_stmt_parser doesn't go into
   infinite loop *)
let rec make_astmt_parser (() : unit) : (token, stmt) parser =
  raise TODO

(* Make parser at the statement level: parses a sequence of astmts into Seq(astmt, stmt) 
   Adds in a skip at the end of the program after parsing the EOF *)
let rec make_stmt_parser (() : unit) : (token, stmt) parser =
  (* Parse recursively for >= 1 astmt *)
  let stmt_parser = lazy_seq (lazy (make_astmt_parser ()), lazy (make_stmt_parser ())) in
  let mult_stmt_parser = map (fun (a, b) -> (Seq (a, b), dummy_pos)) stmt_parser in	
  (* Parse no statements *)
  let eof_parser = const_map (skip, dummy_pos) (tok EOF) in
	alts [mult_stmt_parser; eof_parser]

(* Constructs parser using make_stmt_parser, computes it on a list of tokens, and
   returns some complete parse matching the token list if it exists *)
let parse (ts : token list) : program = 
  let program_parser = make_stmt_parser () in
  match run (program_parser ts) with
   | Some stmt -> stmt
   | None -> failwith "parse error"
