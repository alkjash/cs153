module ML = Mlish_ast
module S = Scish_ast

exception TODO
exception FatalError
exception Unbound

(* New variables S.var to remove shadowing concerns *)
let vc = ref 0

let new_var() = (vc := !vc + 1; "VAR" ^ (string_of_int !vc))

(***************** Environment *******************************)
type exp_env = (ML.var * S.exp) list

(* Add a new var/fun to exp_env, possibly shadowing another *)
let insert (en : exp_env) (x : ML.var) (se : S.exp) : exp_env =
	(x, se) :: en

let lookup (en : exp_env) (x : ML.var) : S.exp =
	match (List.filter (fun y -> (fst y) = x) en) with
	  [] -> raise Unbound
	| h::t -> snd h

(***************** Compilation *******************************)

let rec compile_primexp (en : exp_env) (e : ML.rexp) : S.exp =
	match e with
	  ML.PrimApp (p, el) ->
	((* First compile all the arguments *)
	let el = List.map (fun y -> compile_rexp en y) el in
	match p with
	  ML.Int i  -> S.Int i
	| ML.Char c -> S.Int (int_of_char c)
	| ML.Bool b -> (if b then S.Int 1 else S.Int 0) 
	| ML.Unit   -> S.Int 0 (* unit value -- () *)
	| ML.Plus   -> S.PrimApp(S.Plus, el) (* add two ints *)
	| ML.Minus  -> S.PrimApp(S.Minus, el) (* subtract two ints *)
	| ML.Times  -> S.PrimApp(S.Times, el) (* multiply two ints *)
	| ML.Div    -> S.PrimApp(S.Div, el) (* divide two ints *)
	| ML.Eq     -> S.PrimApp(S.Eq, el) (* compare two ints for equality *)
	| ML.Lt     -> S.PrimApp(S.Lt, el) (* compare two ints for inequality *)
	| ML.Pair   -> S.PrimApp(S.Cons, el) (* create a pair from two values *)
	| ML.Fst    -> S.PrimApp(S.Fst, el) (* fetch the 1st component of a pair *)
	| ML.Snd    -> S.PrimApp(S.Snd, el) (* fetch the 2nd component of a pair *)
	| ML.Nil    -> S.Int 0 (* the empty list *)
	| ML.Cons   -> S.PrimApp(S.Cons, el) (* create a list from two values *)
	| ML.IsNil  -> (* determine whether a list is Nil *)
		S.If (List.hd el, S.Int 0, S.Int 1)
	| ML.Hd     -> S.PrimApp(S.Fst, el) (* fetch the head of a list *)
	| ML.Tl     -> S.PrimApp(S.Snd, el) (* fetch the tail of a list *)
	)
	| _ -> raise FatalError

and compile_rexp (en : exp_env) (e : ML.exp) : S.exp =
	let (e, _) = e in
	match e with
	(* Instead of using sLet, directly substitute the expression of x into every
		instance of x *)
	  ML.Var x -> lookup en x
	| ML.PrimApp _ -> compile_primexp en e
	| ML.Fn (x, e) -> 
		let v = new_var() in
		S.Lambda (v, (compile_rexp (insert en x (S.Var v)) e))
		(* if e1 is a function name 'f', are we sure that it becomes ML.Var f?*)
	| ML.App (e1, e2) -> S.App ((compile_rexp en e1), (compile_rexp en e2))
	| ML.If (e1, e2, e3) -> 
		S.If ((compile_rexp en e1), (compile_rexp en e2), (compile_rexp en e3))
	| ML.Let (x, e1, e2) -> let se = compile_rexp en e1 in
		compile_rexp (insert en x se) e2

let compile_exp (e : ML.exp) : S.exp =
	compile_rexp [] e
