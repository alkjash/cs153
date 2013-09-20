open Lcombinators.GenericParsing
open Lcombinators.CharParsing
open Ast

(* the datatype for tokens -- you will need to augment these *)
type token = 
    INT of int 
  | VAR of Ast.var
  | PLUS | MINUS | STAR | SLASH
  | EQ | NEQ | LT | LTE | GT | GTE
  | NOT | AND | OR
  | ASSIGN
  | IF | ELSE | WHILE | FOR
  | LBRACE | RBRACE
  | LPAREN | RPAREN
  | WHITESPACE | COMMENT 
  | SEMI
  | RETURN
  | EOF

(* removes WHITESPACE and COMMENT tokens from a token list *)
let remove_whitespace (ts: token list) : token list =
  let p = fun a t -> match t with (WHITESPACE | COMMENT) -> a | _ -> t::a in
  List.rev (List.fold_left p [] ts)

(* the tokenize function -- should convert a list of characters to a list of 
 * Fish tokens using the combinators. *)
let rec tokenize(cs:char list) : token list = 
  let int_parser = map (fun i -> INT i) integer in
  
  let plus_parser = const_map PLUS (c '+') in
  let minus_parser = const_map MINUS (c '-')  in
  let star_parser = const_map STAR (c '*')  in
  let slash_parser = const_map SLASH (c '/')  in

  (* Helper for constructing parsers for <, <=, etc.
	 Constructs parser that matches either x followed by =, returning y, or
	 Else matches x and returns z *)
  let helper (x: char) (y: token) (z: token) = map (fun pair -> match pair with
	(x, Some '=') -> y
	| _ -> z)
	(seq (c x, opt (c '='))) in
  let eq_parser = helper '=' EQ ASSIGN in
  let not_parser = helper '!' NEQ NOT in
  let less_parser = helper '<' LTE LT in
  let greater_parser = helper '>' GTE GT in

  let and_parser = const_map AND (str "&&") in
  let or_parser = const_map OR (str "||") in

  let keywords = [("if", IF), ("else", ELSE), ("while", WHILE), ("for", FOR)] in
  let str_parser = map (fun s -> 
	try List.assoc s keywords
	with Not_found -> VAR s) identifier in

  let lbrace_parser = const_map LBRACE (c '{') in
  let rbrace_parser = const_map RBRACE (c '}') in

  let lparen_parser = const_map LPAREN (c '(') in
  let rparen_parser = const_map RPAREN (c ')') in

  let ws_parser = const_map WHITESPACE white in
  let comment_parser = const_map COMMENT comment in

  let semi_parser = const_map SEMI (c ';') in

  let return_parser = const_map RETURN (str "return") in

  let eof_parser = const_map EOF eof in

  let all_tokens = [int_parser; ws_parser; comment_parser; 
    plus_parser; minus_parser; star_parser; slash_parser;
	eq_parser; not_parser; less_parser; greater_parser;
	and_parser; or_parser; lbrace_parser; rbrace_parser;
	str_parser; lparen_parser; rparen_parser; return_parser; semi_parser] in
  let p = seq (star (alts all_tokens), eof_parser) in
  match run (p cs) with
   | Some (tokens, EOF) -> remove_whitespace tokens
   | _ -> failwith "lex error"
