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

(********************** Environment *************************)
(* Code for lookup in an environment, which is a linked list of variable values *)
(* /* Helper function for definitions below -- returns
 * the nth item in the environment linked list. */
lookup(env,n) {
  while (n != 0) {
    env = *(env + 4) ; /* get the tail of the env */
    n = n - 1;
  }
  return ( *(env + 0)) ; /* return the head of the env */
} *)
let lookup() =
	let inner = (Seq((Exp (Assign("env", 
		(Load (Binop((Cish_ast.Var "env", 0), Plus, (Cish_ast.Int 4, 0)), 0), 0)), 0), 0), 
		(Exp (Assign("n",
		(Binop((Cish_ast.Var "n", 0), Minus, (Cish_ast.Int 1, 0)), 0)), 0), 0)), 0) in
	let loop = (While((Binop((Cish_ast.Var "n", 0), Neq, (Cish_ast.Int 0, 0)), 
		0), inner), 0) in
	let body = (Seq(loop, (Return ((Load (Cish_ast.Var "env", 0)), 0), 0)), 0) in
	let fsig = {name = "FLOOKUP"; args = ["env"; "n"]; body = body; pos = 0} in
	Fn fsig

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
	| Lambda(v, e) -> raise TODO
	| App(e1, e2) -> raise TODO
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
	(* Reverse the order of the functions so that functions are declared before
	   functions depending on them *)
	List.rev (compile_func e "main" None)
