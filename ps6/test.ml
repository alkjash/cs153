open Mlish

let tom_direct (f : string) =
	let ch = open_in f in
	let prog = Ml_parse.program Ml_lex.lexer (Lexing.from_channel ch) in
	let sc = Mlish_compile.compile_exp prog in
	Monadic.tomonadic sc
