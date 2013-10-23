(* TODO:  your job is to map ScishAst expressions to CishAst functions. 
   The file sample_input.scish shows a sample Scish expression and the
   file sample_output.cish shows the output I get from my compiler.
   You will want to do your own test cases...
 *)

open Scish_ast
open Cish_ast

exception TODO
exception FatalError

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

(* Make a sequence out of a list of stmts *)
let rec make_Seq (l : Cish_ast.stmt list) : Cish_ast.stmt =
	match l with
	  [] -> (skip, 0)
	| h :: t -> (Seq(h, make_Seq(t)), 0)

(* Write code to construct a pair out of two variables and store it in result *)
let make_pair (v1 : Cish_ast.var) (v2 : Cish_ast.var) : Cish_ast.stmt =
	(* In case either of v1, v2 is result, move these to temps first *)
	let t1 = new_var() in
	let t2 = new_var() in
	let assign = make_Seq [(Exp (Assign(t1, (Cish_ast.Var v1, 0)), 0), 0); 
				(Exp (Assign(t2, (Cish_ast.Var v2, 0)), 0), 0)] in
	(* malloc space for the pair and store the values *)
	make_Seq [assign; (Exp (Assign("result", (Malloc (Int(8), 0), 0)), 0), 0);
		(Exp (Store((Cish_ast.Var "result", 0), (Cish_ast.Var t1, 0)), 0), 0);
		(Exp (Store((Binop((Cish_ast.Var "result", 0), Plus, (Cish_ast.Int(4), 0)), 0), 
			(Cish_ast.Var t1, 0)), 0), 0)]

(* Compiles Int(i), PrimApp(p, el), If(e1, e2, e3), App(e1, e2), Lambda(v, e); 
	calls compile_func to compile the last *)
let rec compile_aexp (e : Scish_ast.exp) (args : Scish_ast.var list) : Cish_ast.stmt = 
	match e with
	  Scish_ast.Int(i) -> raise TODO 
	| PrimApp(p, el) -> (match p with
		(* Pairs stored as pair of pointers to objects: *t is the first element, *(t+4) is second *)
		  Fst -> 
			(* compile el into code which puts its value in result, 
				then store the first element of that in result *)
			make_Seq [compile_aexp (List.hd el) var_l;
				(Exp(Assign("result", (Load (Cish_ast.Var "result", 0), 0)), 0), 0)]
		| Snd ->
			(* compile el into code which puts its value in result, 
				then store the second element of that in result *)
			make_Seq [compile_aexp (List.hd el); 
				(Exp(Assign("result", (Load
				(Binop((Cish_ast.Var "result", 0), Plus, (Int(4), 0)), 0), 0)), 0), 0)]
		| _ -> if List.length el <> 2 then raise FatalError else
			let e1 = List.hd el in
			let e2 = List.hd (List.tl el) in
			let temp = new_var() in
			(* Generate the last piece of code first: Assume value of e1 stored in temp,
				value of e2 stored in result; apply p to them and store in result *)
			let compute = match p with
			  Scish_ast.Plus -> (Exp(Assign("result", 
				(Binop((Cish_ast.Var "result", 0), Cish_ast.Plus, (Cish_ast.Var temp, 0)), 0)), 0), 0)
			| Scish_ast.Minus -> (Exp(Assign("result", 
				(Binop((Cish_ast.Var "result", 0), Cish_ast.Minus, (Cish_ast.Var temp, 0)), 0)), 0), 0)
			| Scish_ast.Times -> (Exp(Assign("result", 
				(Binop((Cish_ast.Var "result", 0), Cish_ast.Times, (Cish_ast.Var temp, 0)), 0)), 0), 0)
			| Scish_ast.Div -> (Exp(Assign("result", 
				(Binop((Cish_ast.Var "result", 0), Cish_ast.Div, (Cish_ast.Var temp, 0)), 0)), 0), 0)
			| Scish_ast.Eq -> (Exp(Assign("result", 
				(Binop((Cish_ast.Var "result", 0), Cish_ast.Eq, (Cish_ast.Var temp, 0)), 0)), 0), 0)
			| Scish_ast.Lt -> (Exp(Assign("result", 
				(Binop((Cish_ast.Var "result", 0), Cish_ast.Lt, (Cish_ast.Var temp, 0)), 0)), 0), 0)
			| Cons -> make_pair temp "result" 
			| _ -> raise FatalError in
 
			(* Compile e1, load its result into a temp, then compile e2; all this before applying
			   the primop *)
			let store_temp = (Let (temp, (Cish_ast.Var "result", 0), 
				make_Seq [compile_aexp e2; compute]), 0) in
			make_Seq [compile_aexp e1; store_temp])
			
	| Scish_ast.If(e1, e2, e3) -> raise TODO 
	| App(e1, e2) -> 
		((* First compile e1, which has to be a lambda function, and add it into the flist *)
		let fname = new_func() in
		let newf = compile_func e1 fname (Some "env") in
		let _ = (flist := newf :: (!flist)) in
		(* Next write code for calling e1, given that we have the function newf which takes an
		   environment linked list *)
		let temp = new_var() in
		let call = make_pair fname temp in
		match e1 with
		  Lambda (v, e3) -> make_Seq [call; compile_aexp e2]
		| _ -> raise FatalError)
	| Lambda(v, e1) ->
		(* Compile the function and then compute and return a closure (a pair func, env) *)
		let fname = new_func() in
		let newf = compile_func e1 fname v::args in
		let _ = (flist := newf :: (!flist)) in
		(* Set result = (fname, env), where env is currently just 0 *)
		(* let temp = new_var() in
		let store_temp = (Let (temp, ) *)
		make_Seq [newf; make_pair fname "env"]
	| _ -> raise FatalError

and compile_func (e : Scish_ast.exp) (name : Cish_ast.var) 
	(args : Scish_ast.var list) : Cish_ast.func =
	(* First define a variable "result" which stores all the 
	   temporary calculation values at each step, then compile the expression e into a stmt *)
	let body = (Let ("result", (Int 0, 0), (compile_aexp e args)), 0) in
	Fn({name = name; args = args; body = body; pos = 0})

(* compile_exp takes a Scish expression and compiles it into a Cish program
   Explicitly, it calls compile_func to compile e into the main procedure
   and all the procedures it needs *)
let compile_exp (e:Scish_ast.exp) : Cish_ast.program =
	(* Reverse the order of the functions so that functions are declared before
	   functions depending on them *)
	lookup() :: List.rev ((compile_func e "main" None) :: (!flist))
