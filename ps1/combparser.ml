(* This file should be extended to implement the Fish parser using the 
 * parsing combinator library, and the combinator-based lexer. *)
open Lcombinators.GenericParsing
open Comblexer
open Ast

exception TODO

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

(* Constructs a statement parser, which either matches a return statement or
   a normal statement, which we construct using make_exp_parser *)
let rec make_stmt_parser (():unit) : (token, stmt) parser =
  let return_parser = seq (satisfy (fun t -> t == RETURN), lazy_seq (lazy (make_exp_parser ()), 
    lazy (satisfy (fun t -> t == SEMI)))) in
  let return_stmt_parser = map (fun (_, (e, _)) -> ((Return e), dummy_pos)) return_parser in
  let exp_stmt_parser = map (fun e -> (Exp e, dummy_pos)) (make_exp_parser ()) in
  alts [return_stmt_parser; exp_stmt_parser]
*)

(* Parses a statement of the form Exp, If-Else, While, For, Return, { stmt } *)
let rec make_astmt_parser (():unit) : (token, stmt) parser =
  let if_parser =
    list[satisfy (fun t -> t == IF), satisfy (fun t -> t == LPAREN),
          lazy_seq (lazy (make_exp_parser ()), (lazy_seq (lazy (satisfy (fun t -> t == RPAREN)),
          lazy (make_stmt_parser ()))))] in
  let if_astmt_parser = map (fun (_, (_, (e, (_, s)))) -> ((If (e,s,skip)), dummy_pos)) if_parser in
  let if_else_parser =
    list[satisfy (fun t -> t == IF), satisfy (fun t -> t == LPAREN),
          lazy_seq (lazy (make_exp_parser ()), (lazy_seq (lazy (satisfy (fun t -> t == RPAREN)),
          (lazy_seq (lazy (make_stmt_parser ()), (lazy_seq (lazy (satisfy (fun t -> t == ELSE)),
          (lazy (make_stmt_parser ())))))))))] in
  let if_else_astmt_parser = map (fun (_, (_, (e, (_, (s1, (_, s2)))))) -> ((If (e,s,s2)), dummy_pos)) if_else_parser in
  let while_parser =
    list[satisfy (fun t -> t == WHILE), satisfy (fun t -> t == LPAREN),
          lazy_seq (lazy (make_exp_parser ()), (lazy_seq (lazy (satisfy (fun t -> t == RPAREN)),
          (lazy_seq (lazy (make_stmt_parser ()), lazy (satisfy (fun t -> t == SEMI)))))))] in
  let while_astmt_parser = map (fun (_, (_, (e, (_, (s, _))))) -> ((While (e,s)), dummy_pos)) while_parser in
  let for_parser =
    list[satisfy (fun t -> t == FOR), satisfy (fun t -> t == LPAREN),
          lazy_seq (lazy (make_exp_parser ()), (lazy_seq (lazy (satisfy (fun t -> t == SEMI)), 
          (lazy_seq (lazy (make_exp_parser ()), (lazy_seq (lazy (satisfy (fun t -> t == SEMI)),
          (lazy_seq (lazy make_exp_parser (), (lazy_seq (lazy (satisfy (fun t -> t == RPAREN)),
          (lazy_seq (lazy (make_stmt_parser ()), lazy (satisfy (fun t -> t == SEMI)))))))))))))))] in
  let for_astmt_parser = map (fun (_, (_, (e1, (_, (e2, (_, (e3, (_, (s, _))))))))) ->
    ((For (e1,e2,e3,s)), dummy_pos)) for_parser in
  (* what to do for empty while & for statements? *)
  let return_parser = seq (satisfy (fun t -> t == RETURN), lazy_seq (lazy (make_exp_parser ()), 
    lazy (satisfy (fun t -> t == SEMI)))) in
  let return_stmt_parser = map (fun (_, (e, _)) -> ((Return e), dummy_pos)) return_parser in
  alts [if_parser; if_else_parser; while_parser; for_parser; return_parser]



(* Make parser at the statement level: parses a sequence of astmts into Seq(astmt, stmt) *)
let rec make_stmt_parser (():unit) : (token, stmt) parser =
  let stmt_parser = lazy_seq (lazy (make_astmt_parser ()), lazy (opt (make_stmt_parser ()))) in
  let mult_stmt_parser = map (fun (a, b) -> match b with 
	  Some c -> Seq (a, c)
	| None -> a) stmt_parser

(* Constructs parser using make_stmt_parser, computes it on a list of tokens, and
   returns some complete parse matching the token list if it exists *)
let parse(ts:token list) : program = 
  let program_parser = make_stmt_parser () in
  match run (program_parser ts) with
   | Some stmt -> stmt
   | None -> failwith "parse error"
