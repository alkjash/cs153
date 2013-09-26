(* This file should be extended to implement the Fish parser using the 
 * parsing combinator library, and the combinator-based lexer. *)
open Lcombinators.GenericParsing
open Comblexer
open Ast

exception TODO
exception FatalError

let dummy_pos : pos = 0
let dummy_stmt : stmt = (skip, dummy_pos)

(* New combinator functions *)
(* [tok t] matches and consumes a token t *)
let tok (t : token) : (token, token) parser = satisfy (fun h -> t = h) 

(* Constructs an expression parser *)
(* Expressions factor as:
   aexp = Int | Var | ( exp )
   bexp = aexp | aexp / bexp | aexp * bexp
   cexp = bexp | bexp - cexp | - cexp | bexp + cexp
   dexp = cexp | !dexp
   eexp = dexp | dexp (== , != , < , <= , > , >=) eexp
   fexp = eexp | eexp && fexp | eexp || fexp
   exp = fexp | Var = exp

   Extra levels of refactoring were added to make arithmetic bind completely correctly.
   *)

(* aexp = Int | Var | ( exp ) *)
let rec make_aexp_parser (() : unit) : (token, exp) parser =
  let int_parser = satisfy (fun h -> match h with
      INT(_) -> true
    | _ -> false) in
  let int_exp_parser = map (fun h -> match h with
	  INT(i) -> (Int(i), dummy_pos)
	| _ -> raise FatalError) int_parser in
  let var_parser = satisfy (fun h -> match h with 
	  VAR(_) -> true
    | _ -> false) in
  let var_exp_parser = map (fun h -> match h with
	  VAR(v) -> (Var(v), dummy_pos)
    | _ -> raise FatalError) var_parser in
  let paren_parser = seq (tok LPAREN,
	lazy_seq (lazy (make_exp_parser ()), lazy (tok RPAREN))) in
  let paren_exp_parser = map (fun (_, (e, _)) -> e) paren_parser in
  alts [int_exp_parser; var_exp_parser; paren_exp_parser]

(* bexp = aexp | aexp / bexp | aexp * bexp *)
and make_bexp_parser (() : unit) : (token, exp) parser = 
  let slash_parser = lazy_seq (lazy (make_aexp_parser ()), 
	lazy (lazy_seq (lazy (tok SLASH), lazy (make_bexp_parser ())))) in
  let div_exp_parser = map (fun (e1, (_, e2)) -> (Binop(e1, Div, e2), dummy_pos)) slash_parser in
  let star_parser = lazy_seq (lazy (make_aexp_parser ()), 
	lazy (lazy_seq (lazy (tok STAR), lazy (make_bexp_parser ())))) in
  let times_exp_parser = map (fun (e1, (_, e2)) -> (Binop(e1, Times, e2), dummy_pos)) star_parser in
  alts [make_aexp_parser (); div_exp_parser; times_exp_parser]

(* cexp = bexp | bexp - cexp | - cexp | bexp + cexp *)
and make_cexp_parser (() : unit) : (token, exp) parser = 
  let minus_parser = lazy_seq (lazy (opt (make_bexp_parser ())), 
	lazy (lazy_seq (lazy (tok MINUS), lazy (make_cexp_parser ())))) in
  let minus_exp_parser = map (fun (e1, (_, e2)) ->  match e1 with
	  Some e -> (Binop(e, Minus, e2), dummy_pos)
	| None -> (Binop((Int 0, dummy_pos), Minus, e2), dummy_pos)) minus_parser in
  let plus_parser = lazy_seq (lazy (make_bexp_parser ()), 
	lazy (lazy_seq (lazy (tok PLUS), lazy (make_cexp_parser ())))) in
  let plus_exp_parser = map (fun (e1, (_, e2)) -> (Binop(e1, Plus, e2), dummy_pos)) plus_parser in
  alts [make_bexp_parser (); minus_exp_parser; plus_exp_parser]

(* dexp = cexp | !dexp *)
and make_dexp_parser (() : unit) : (token, exp) parser = 
  let not_parser = lazy_seq (lazy (tok NOT), lazy (make_dexp_parser ())) in
  let not_exp_parser = map (fun (_, e) -> (Not(e), dummy_pos)) not_parser in
  alt (make_cexp_parser (), not_exp_parser)

(* eexp = dexp | dexp (== , != , < , <= , > , >=) eexp *)
and make_eexp_parser (() : unit) : (token, exp) parser = 
  let compare_parser = lazy_seq (lazy (make_dexp_parser ()), 
	lazy (lazy_seq (lazy (alts [tok EQ; tok NEQ; tok LT; tok LTE; tok GT; tok GTE]),
	lazy (make_eexp_parser ())))) in
  let compare_exp_parser = map (fun (e1, (op, e2)) -> match op with
      EQ -> (Binop(e1, Eq, e2), dummy_pos)
    | NEQ -> (Binop(e1, Neq, e2), dummy_pos)
    | LT -> (Binop(e1, Lt, e2), dummy_pos)
    | LTE -> (Binop(e1, Lte, e2), dummy_pos)
    | GT -> (Binop(e1, Gt, e2), dummy_pos)
    | GTE -> (Binop(e1, Gte, e2), dummy_pos)
    | _ -> raise FatalError) compare_parser in
  alt (make_dexp_parser (), compare_exp_parser)

(* fexp = eexp | eexp && fexp | eexp || fexp *)
and make_fexp_parser (() : unit) : (token, exp) parser = 
  let andor_parser = lazy_seq (lazy (make_eexp_parser ()), 
	lazy (lazy_seq (lazy (alt (tok AND, tok OR)), lazy (make_fexp_parser ())))) in
  let andor_exp_parser = map (fun (e1, (op, e2)) -> match op with
      AND -> (And(e1, e2), dummy_pos)
    | OR -> (Or(e1, e2), dummy_pos)
    | _ -> raise FatalError) andor_parser in
  alt (make_eexp_parser (), andor_exp_parser)

(* exp = fexp | Var = exp *)
and make_exp_parser (() : unit) : (token, exp) parser = 
  let assign_parser = lazy_seq (lazy (satisfy (fun t -> match t with
	  VAR(_) -> true
	| _ -> false)), 
	lazy (lazy_seq (lazy (tok ASSIGN), lazy (make_exp_parser ())))) in
  let assign_exp_parser = map (fun h -> match h with
	  (VAR(v), (_, e)) -> (Assign(v, e), dummy_pos)
	| _ -> raise FatalError) assign_parser in
  alt (make_fexp_parser (), assign_exp_parser)

(* Parses a statement of the form Exp, If-Else, While, For, Return, { stmt } 
   Does not match the empty list to guarantee that make_stmt_parser doesn't go into
   infinite loop *)
and make_astmt_parser (():unit) : (token, stmt) parser =
  let exp_parser = lazy_seq (lazy (make_exp_parser ()), lazy (tok SEMI)) in
  let exp_astmt_parser = map (fun (e, _) -> (Exp (e), dummy_pos)) exp_parser in
  let braces_parser = lazy_seq (lazy (tok LBRACE), 
	lazy (lazy_seq (lazy (make_stmt_parser ()), lazy (tok RBRACE)))) in
  let braces_astmt_parser = 
	map (fun (_, (s, _)) -> s) braces_parser in
  let if_parser = 
    lazy_seq (lazy (tok IF), lazy (lazy_seq (lazy (tok LPAREN), 
	lazy (lazy_seq (lazy (make_exp_parser ()), 
	lazy (lazy_seq (lazy (tok RPAREN), lazy(make_astmt_parser ())))))))) in
  let if_astmt_parser = 
	map (fun (_, (_, (e, (_, s)))) -> (If (e,s,dummy_stmt), dummy_pos)) if_parser in
  let if_else_parser =
    lazy_seq (lazy (tok IF), lazy (lazy_seq (lazy (tok LPAREN), 
	lazy (lazy_seq (lazy (make_exp_parser ()), lazy (lazy_seq (lazy (tok RPAREN),
    lazy (lazy_seq (lazy (make_astmt_parser ()), 
	lazy (lazy_seq (lazy (tok ELSE), lazy (make_astmt_parser ())))))))))))) in
  let if_else_astmt_parser = 
	map (fun (_, (_, (e, (_, (s1, (_, s2)))))) -> ((If (e,s1,s2)), dummy_pos)) if_else_parser in
  let while_parser =
    lazy_seq (lazy (tok WHILE), lazy (lazy_seq (lazy (tok LPAREN),
          lazy (lazy_seq (lazy (make_exp_parser ()), lazy (lazy_seq (lazy (tok RPAREN), lazy (
          make_astmt_parser ())))))))) in
  let while_astmt_parser = map (fun (_, (_, (e, (_, s)))) -> (While (e,s), dummy_pos)) while_parser in
  let for_parser =
    lazy_seq (lazy (tok FOR), lazy (lazy_seq (lazy (tok LPAREN),
          lazy (lazy_seq (lazy (opt (make_exp_parser ())), lazy (lazy_seq (lazy (tok SEMI), 
          lazy (lazy_seq (lazy (make_exp_parser ()), lazy (lazy_seq (lazy (tok SEMI),
          lazy (lazy_seq (lazy (opt (make_exp_parser ())), lazy (lazy_seq (lazy (tok RPAREN),
          lazy (make_astmt_parser ())))))))))))))))) in
  let for_astmt_parser = map (fun (_, (_, (e1, (_, (e2, (_, (e3, (_, s)))))))) ->
	let e1 = match e1 with 
	  Some x -> x 
	| _ -> (Int 0, dummy_pos) in
	let e3 = match e3 with 
	  Some x -> x 
	| _ -> (Int 0, dummy_pos) in
    (For (e1,e2,e3,s), dummy_pos)) for_parser in
  let return_parser = seq (tok RETURN, lazy_seq (lazy (make_exp_parser ()), lazy (
    tok SEMI))) in
  let return_stmt_parser = map (fun (_, (e, _)) -> ((Return e), dummy_pos)) return_parser in
  alts [exp_astmt_parser; braces_astmt_parser; if_astmt_parser; if_else_astmt_parser; 
	while_astmt_parser; for_astmt_parser; return_stmt_parser]

(* Make parser at the statement level: parses a sequence of astmts into Seq(astmt, stmt) *)
and make_stmt_parser (() : unit) : (token, stmt) parser =
  (* Parse recursively for >= 1 astmt *)
  let stmt_parser = lazy_seq (lazy (make_astmt_parser ()), lazy (make_stmt_parser ())) in
  let mult_stmt_parser = map (fun (a, b) -> (Seq (a, b), dummy_pos)) stmt_parser in	
	map (fun s -> match s with
	  Some st -> st
	| None -> dummy_stmt) (opt mult_stmt_parser)

(* Constructs parser using make_stmt_parser, computes it on a list of tokens, and
   returns some complete parse matching the token list if it exists *)
let parse (ts : token list) : program = 
  let program_parser = make_stmt_parser () in
  match run (program_parser ts) with
   | Some stmt -> stmt
   | None -> failwith "parse error"
