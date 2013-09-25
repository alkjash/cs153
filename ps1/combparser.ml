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
    ls (lazy (make_exp_parser ()), lazy (satisfy (fun t -> t == RPAREN)))) in 
  let sub_exp_parser = map (fun (_, (e, _)) -> e) sub_parser in
  let first_parser = alt (int_parser, sub_exp_parser) in
  let rest_parser = seq (first_parser, make_binop_rest ()) in
  let binop_parser = map (fun (e1, (op, e2)) -> (Binop (e1, op, e2), dummy_pos)) rest_parser in
  alts [binop_parser; first_parser]
and make_binop_rest (():unit) : (token, (binop * exp)) parser =
  let binop_op_parser = satisfy_opt (function 
    PLUS -> Some Plus | MINUS -> Some Minus | 
	STAR -> Some Times | SLASH -> Some Div | EQ -> Some Eq | _ -> None) in
  ls (lazy binop_op_parser, lazy (make_exp_parser ()))

*)

(*
(* Constructs a statement parser, which either matches a return statement or
   a normal statement, which we construct using make_exp_parser *)
let rec make_stmt_parser (():unit) : (token, stmt) parser =
  let return_parser = seq (satisfy (fun t -> t == RETURN), ls (lazy (make_exp_parser ()), 
    lazy (satisfy (fun t -> t == SEMI)))) in
  let return_stmt_parser = map (fun (_, (e, _)) -> ((Return e), dummy_pos)) return_parser in
  let exp_stmt_parser = map (fun e -> (Exp e, dummy_pos)) (make_exp_parser ()) in
  alts [return_stmt_parser; exp_stmt_parser]
*)

(* New combinator function: matches a specified token and consumes it *)
let tok (t : token) : (token, token) parser = satisfy (fun h -> t = h) 

(* Shorthand for ls (lazy p1, lazy p2) *)
let ls ((p1 : ('c, 'a) parser), (p2 : ('c, 'b) parser)) : ('c, 'a * 'b) parser =
  lazy_seq (lazy p1, lazy p2)

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
  let paren_parser = ls (tok LPAREN,
	ls (make_exp_parser (), tok RPAREN)) in
  let paren_exp_parser = map (fun (_, (e, _)) -> e) paren_parser in
  alts [int_exp_parser; var_exp_parser; paren_exp_parser]

(* bexp = aexp | aexp / bexp *)
and make_bexp_parser (() : unit) : (token, exp) parser = 
  let slash_parser = ls (make_aexp_parser (), 
	ls (tok SLASH, make_bexp_parser ())) in
  let div_exp_parser = map (fun (e1, (_, e2)) -> (Binop(e1, Div, e2), dummy_pos)) slash_parser in
  alt (make_aexp_parser (), div_exp_parser)

(* cexp = bexp | bexp * cexp *)
and make_cexp_parser (() : unit) : (token, exp) parser = 
  let star_parser = ls (make_bexp_parser (), 
	ls (tok STAR, make_cexp_parser ())) in
  let times_exp_parser = map (fun (e1, (_, e2)) -> (Binop(e1, Times, e2), dummy_pos)) star_parser in
  alt (make_bexp_parser (), times_exp_parser)

(* dexp = cexp | cexp - dexp *)
and make_dexp_parser (() : unit) : (token, exp) parser = 
  let minus_parser = ls (make_cexp_parser (), 
	ls (tok MINUS, make_dexp_parser ())) in
  let minus_exp_parser = map (fun (e1, (_, e2)) -> (Binop(e1, Minus, e2), dummy_pos)) minus_parser in
  alt (make_cexp_parser (), minus_exp_parser)

(* eexp = dexp | dexp + eexp *)
and make_eexp_parser (() : unit) : (token, exp) parser = 
  let plus_parser = ls (make_dexp_parser (), 
	ls (tok PLUS, make_eexp_parser ())) in
  let plus_exp_parser = map (fun (e1, (_, e2)) -> (Binop(e1, Plus, e2), dummy_pos)) plus_parser in
  alt (make_dexp_parser (), plus_exp_parser)

(* fexp = eexp | !fexp *)
and make_fexp_parser (() : unit) : (token, exp) parser = 
  let not_parser = ls (tok NOT, make_fexp_parser ()) in
  let not_exp_parser = map (fun (_, e) -> (Not(e), dummy_pos)) not_parser in
  alt (make_eexp_parser (), not_exp_parser)

(* gexp = fexp | fexp (== , != , < , <= , > , >=) gexp *)
and make_gexp_parser (() : unit) : (token, exp) parser = 
  let compare_parser = ls (make_fexp_parser (), 
	ls (alts [tok EQ; tok NEQ; tok LT; tok LTE; tok GT; tok GTE], make_gexp_parser ())) in
  let compare_exp_parser = map (fun (e1, (op, e2)) -> match op with
      EQ -> (Binop(e1, Eq, e2), dummy_pos)
    | NEQ -> (Binop(e1, Neq, e2), dummy_pos)
    | LT -> (Binop(e1, Lt, e2), dummy_pos)
    | LTE -> (Binop(e1, Lte, e2), dummy_pos)
    | GT -> (Binop(e1, Gt, e2), dummy_pos)
    | GTE -> (Binop(e1, Gte, e2), dummy_pos)
    | _ -> raise FatalError) compare_parser in
  alt (make_fexp_parser (), compare_exp_parser)

(* hexp = gexp | gexp && hexp | gexp || hexp *)
and make_hexp_parser (() : unit) : (token, exp) parser = 
  let andor_parser = ls (make_gexp_parser (), 
	ls (alt (tok AND, tok OR), make_hexp_parser ())) in
  let andor_exp_parser = map (fun (e1, (op, e2)) -> match op with
      AND -> (And(e1, e2), dummy_pos)
    | OR -> (Or(e1, e2), dummy_pos)
    | _ -> raise FatalError) andor_parser in
  alt (make_gexp_parser (), andor_exp_parser)

(* exp = hexp | Var = exp *)
and make_exp_parser (() : unit) : (token, exp) parser = 
  let assign_parser = ls (satisfy (fun t -> match t with
	  VAR(_) -> true
	| _ -> false), 
	ls (tok ASSIGN, make_exp_parser ())) in
  let assign_exp_parser = map (fun h -> match h with
	  (VAR(v), (_, e)) -> (Assign(v, e), dummy_pos)
	| _ -> raise FatalError) assign_parser in
  alt (make_hexp_parser (), assign_exp_parser)

(* Parses a statement of the form Exp, If-Else, While, For, Return, { stmt } 
   Does not match the empty list to guarantee that make_stmt_parser doesn't go into
   infinite loop *)
and make_astmt_parser (():unit) : (token, stmt) parser =
  let exp_parser = raise TODO in
  let braces_parser = raise TODO in
  let if_parser =
    ls (tok IF, ls (tok LPAREN, ls (make_exp_parser (), ls (tok RPAREN, make_stmt_parser ())))) in
  let if_astmt_parser = 
	map (fun (_, (_, (e, (_, s)))) -> (If (e,s,(skip, dummy_pos)), dummy_pos)) if_parser in
  let if_else_parser =
    ls (tok IF, ls (tok LPAREN, ls (make_exp_parser (), ls (tok RPAREN,
          ls (make_stmt_parser (), ls (tok ELSE, make_stmt_parser ())))))) in
  let if_else_astmt_parser = 
	map (fun (_, (_, (e, (_, (s1, (_, s2)))))) -> ((If (e,s1,s2)), dummy_pos)) if_else_parser in
  let while_parser =
    ls (tok WHILE, ls (tok LPAREN,
          ls (make_exp_parser (), ls (tok RPAREN,
          ls (make_stmt_parser (), tok SEMI))))) in
  let while_astmt_parser = map (fun (_, (_, (e, (_, (s, _))))) -> ((While (e,s)), dummy_pos)) while_parser in
  let for_parser =
    ls (tok FOR, ls (tok LPAREN,
          ls (make_exp_parser (), ls (tok SEMI, 
          ls (make_exp_parser (), ls (tok SEMI,
          ls (make_exp_parser (), ls (tok RPAREN,
          ls (make_stmt_parser (), tok SEMI))))))))) in
  let for_astmt_parser = map (fun (_, (_, (e1, (_, (e2, (_, (e3, (_, (s, _))))))))) ->
    ((For (e1,e2,e3,s)), dummy_pos)) for_parser in
  (* what to do for empty while & for statements? *)
  let return_parser = seq (tok RETURN, ls (make_exp_parser (), 
    tok SEMI)) in
  let return_stmt_parser = map (fun (_, (e, _)) -> ((Return e), dummy_pos)) return_parser in
  alts [exp_parser; braces_parser; if_astmt_parser; if_else_astmt_parser; 
	while_astmt_parser; for_astmt_parser; return_stmt_parser]

(* Make parser at the statement level: parses a sequence of astmts into Seq(astmt, stmt) 
   Adds in a skip at the end of the program after parsing the EOF *)
and make_stmt_parser (() : unit) : (token, stmt) parser =
  (* Parse recursively for >= 1 astmt *)
  let stmt_parser = ls (make_astmt_parser (), make_stmt_parser ()) in
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
