open Cfg
open Cfg_ast

let tcfg (s : string) =
	Cish_parse.program Cish_lex.lexer (Lexing.from_channel (open_in s))

let tblocks (s : string) =
	let cfg = tcfg s in
	fn2blocks (List.hd cfg)

let tig (s : string) =
	let cfg = tcfg s in
	List.fold_left print_interference_graph () cfg 
