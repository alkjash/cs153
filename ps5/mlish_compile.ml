module ML = Mlish_ast
module S = Scish_ast

exception TODO
exception FatalError

(***************** Environment *******************************)
type exp_env = (ML.var * S.exp) list

(* Add a new var/fun to exp_env, possibly shadowing another *)
let insert (en : exp_env) (x : ML.var) (se : S.exp) : exp_env =
	(x, re) :: en

let lookup (en : exp_env) (x : ML.var) : S.exp =
	match (List.filter (fun y -> (fst y) = x) en) with
	  [] -> type_error ("Unbound variable or function" ^ x)
	| h::t -> snd h

(***************** Compilation *******************************)

let rec compile_primexp (en : exp_env) (e : ML.rexp) : S.exp =
	match e with
	  ML.PrimApp (p, el) ->
	(match p with
	  ML.Int i  -> S.Int i
	| ML.Bool b -> (if b then S.Int 1 else S.Int 0) 
	| ML.Unit   -> S.Var("unit") (* unit value -- () *)
	| ML.Plus   -> S.PrimApp(Plus, el) (* add two ints *)
	| ML.Minus  -> S.PrimApp(Minus, el) (* subtract two ints *)
	| ML.Times  -> S.PrimApp(Times, el) (* multiply two ints *)
	| ML.Div    -> S.PrimApp(Div, el) (* divide two ints *)
	| ML.Eq     -> S.PrimApp(Eq, el) (* compare two ints for equality *)
	| ML.Lt     -> S.PrimApp(Lt, el) (* compare two ints for inequality *)
	| ML.Pair   -> S.PrimApp(Cons, el) (* create a pair from two values *)
	| ML.Fst    -> S.PrimApp(Fst, el) (* fetch the 1st component of a pair *)
	| ML.Snd    -> S.PrimApp(Snd, el) (* fetch the 2nd component of a pair *)
	| ML.Nil    -> S.Var("nil") (* the empty list *)
	| ML.Cons   -> S.PrimApp(Cons, el) (* create a list from two values *)
	| ML.IsNil  -> if el == [] then S.Int 1 else S.Int 0 (* determine whether a list is Nil *)
	| ML.Hd     -> S.PrimApp(Fst, el) (* fetch the head of a list *)
	| ML.Tl     -> S.PrimApp(Snd, el) (* fetch the tail of a list *)
	)
	| _ -> raise FatalError

and compile_exp (en : exp_env) (e : ML.exp) : S.exp =
	match e with
	  ML.Var x -> lookup en x
	| ML.PrimApp _ -> let (re,_) = e in compile_primexp en re
	| ML.Fn (x, e) -> S.Lambda (x, (compile_exp en e)
		(* if e1 is a function name 'f', are we sure that it becomes ML.Var f?*)
	| ML.App (e1, e2) -> S.App ((compile_exp en e1), (compile_exp en e2))
	| ML.If (e1, e2, e3) -> 
		S.If ((compile_exp e1), (compile_exp en e2), (compile_exp en e3))
	| ML.Let (x, e1, e2) -> let se = compile_exp en e1 in
		S.sLet x se (compile_exp (insert en x se) e2)
