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
	| ML.Unit   -> raise TODO (* unit value -- () *)
	| ML.Plus   -> raise TODO (* add two ints *)
	| ML.Minus  -> raise TODO (* subtract two ints *)
	| ML.Times  -> raise TODO (* multiply two ints *)
	| ML.Div    -> raise TODO (* divide two ints *)
	| ML.Eq     -> raise TODO (* compare two ints for equality *)
	| ML.Lt     -> raise TODO (* compare two ints for inequality *)
	| ML.Pair   -> raise TODO (* create a pair from two values *)
	| ML.Fst    -> raise TODO (* fetch the 1st component of a pair *)
	| ML.Snd    -> raise TODO (* fetch the 2nd component of a pair *)
	| ML.Nil    -> raise TODO (* the empty list *)
	| ML.Cons   -> raise TODO (* create a list from two values *)
	| ML.IsNil  -> raise TODO (* determine whether a list is Nil *)
	| ML.Hd     -> raise TODO (* fetch the head of a list *)
	| ML.Tl     -> raise TODO (* fetch the tail of a list *)
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
