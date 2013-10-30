module ML = Mlish_ast
module S = Scish_ast

exception TODO
exception FatalError

let rec compile_primexp (e : ML.rexp) : S.exp =
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

and compile_exp ((e,_):ML.exp) : S.exp =
	match e with
	  ML.Var x -> S.Var x
	| ML.PrimApp _ -> compile_primexp e
	| ML.Fn (x, e) -> S.Lambda (x, (compile_exp e))
	| ML.App (e1, e2) -> S.App ((compile_exp e1), (compile_exp e2))
	| ML.If (e1, e2, e3) -> 
		S.If ((compile_exp e1), (compile_exp e2), (compile_exp e3))
	| ML.Let (x, e1, e2) ->
		S.sLet x (compile_exp e1) (compile_exp e2)
