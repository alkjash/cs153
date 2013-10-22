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
let new_var() = "T" ^ (string_of_int (new_int()))
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

(* The list of helper functions is constructed as side effects of compiling the main function *)
let flist = ref []

(* Compiles Int(i), PrimApp(p, el), If(e1, e2, e3), App(e1, e2), Lambda(v, e); 
	calls compile_applyexp and compile_func to compile the last *)
let rec compile_aexp (e : Scish_ast.exp) : Cish_ast.stmt = 
	match e with
	  Scish_ast.Int(i) -> raise TODO 
	| PrimApp(p, el) -> match p with
		  Plus -> 
		| Minus ->
		| Times ->
		| Div ->
		| Cons ->
		| Fst ->
		| Snd ->
		| Eq ->
		| Lt ->
	| Scish_ast.If(e1, e2, e3) -> raise TODO 
	| App(e1, e2) -> 
		(* First compile e1, which has to be a lambda function, and add it into the flist *)
		let newf = compile_func e1 (new_func()) (Some v) in
		let _ = (flist := newf :: (flist)) in
		(* Next write code for calling e1, given that we have the function newf which takes an
		   environment linked list *)
		let Lambda(v, e3) = e1 in
		compile_applyexp e2 v
	| Lambda(v, e) ->
		(* Compile the function and then compute and return a closure *)
		let newf = compile_func e1 (new_func()) (Some v) in
		let _ = (flist := newf :: (!flist)) in

(* Compiles an "apply" expression, writing code to evaluate an expression where a given variable is
   substituted in by the apply *)
and compile_applyexp (e : Scish_ast.exp) (v : Scish_ast.var) : Cish_ast.stmt =
	raise TODO

and compile_func (e : Scish_ast.exp) (name : Cish_ast.var) 
	(arg : Scish_ast.var option) : Cish_ast.func =
	(* First define a variable "result" which stores all the 
	   temporary calculation values at each step, then compile the expression e into a stmt *)
	let body = (Let "result" (Int 0, 0) (compile_aexp e), 0) in
	let f = Fn({name = name; args = 
		(match arg with
		  None -> []
		| Some v -> [v]); body = body; pos = 0})

(* compile_exp takes a Scish expression and compiles it into a Cish program
   Explicitly, it calls compile_func to compile e into the main procedure
   and all the procedures it needs *)
let compile_exp (e:Scish_ast.exp) : Cish_ast.program =
	(* Reverse the order of the functions so that functions are declared before
	   functions depending on them *)
	lookup() :: List.rev ((compile_func e "main" None) :: (!flist))
