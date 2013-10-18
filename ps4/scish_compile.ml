(* TODO:  your job is to map ScishAst expressions to CishAst functions. 
   The file sample_input.scish shows a sample Scish expression and the
   file sample_output.cish shows the output I get from my compiler.
   You will want to do your own test cases...
 *)

open Scish_ast
open Cish_ast

exception TODO

(* Generate fresh labels *)
let label_counter = ref 0
let new_int() = (label_counter := (!label_counter) + 1; !label_counter)
let new_func() = "F" ^ (string_of_int (new_int()))

(********************** Compile *****************************)
(* Compiles an expression e into a function of name f along with a set of functions
   that f depends on (via Lambda), which are returned as a list of functions headed
   by f *)
let rec compile_func (e : Scish_ast.exp) (name : Cish_ast.var) 
	(arg : Scish_ast.var option) : Cish_ast.func list =
	let flist = [] in (* List of Lambda functions constructed along the way *)
	
	(* Compile the body of f, simultaneously calling compile_func each time
	   we use Lambda to create a new unnamed function, and adding it to flist *)
	let body = match e with
	  Scish_ast.Int(i) -> raise TODO
	| Scish_ast.Var(v) -> raise TODO
	| PrimApp(op, el) -> raise TODO
	| Lambda(f, e) -> raise TODO
	| App(f, e) -> raise TODO
	| Scish_ast.If(e1, e2, e3) -> raise TODO in

	let f = Fn({name = name; args = 
		(match arg with
		  None -> []
		| Some v -> [v]); body = body; pos = 0}) in
	f :: flist

(* compile_exp takes a Scish expression and compiles it into a Cish program
   Explicitly, it calls compile_func to compile e into the main procedure
   and all the procedures it needs *)
let compile_exp (e:Scish_ast.exp) : Cish_ast.program =
	compile_func e "main" None
