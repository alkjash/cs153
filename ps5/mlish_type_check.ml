module ML = Mlish_ast

exception TODO
exception FatalError
exception TypeError
let type_error(s:string) = (print_string s; raise TypeError)

(***************** Environment *******************************)
type env = (Mlish_ast.var * Mlish_ast.tipe_scheme) list

(* Add a new variable to env, possibly shadowing another *)
let extend (en : env) (x : Mlish_ast.var) (ts : Mlish_ast.tipe_scheme) : env =
	(x, ts) :: en

(* Lookup variable in env, if it doesn't exist raise type error unbound *)
let lookup (en : env) (x : Mlish_ast.var) : Mlish_ast.tipe_scheme =
	match (List.filter (fun y -> (fst y) = x) en) with
	  [] -> type_error ("Unbound variable" ^ x)
	| h::t -> snd h

(***************** Type-Checking ******************************)

(* Checking primitives *)
let rec type_check_prim (en : env) (r : ML.rexp) : ML.tipe =
	match r with
	  ML.PrimApp(p, el) -> 
		(match p with
		  ML.Int _ -> ML.Int_t
		| ML.Bool _ -> ML.Bool_t
		| ML.Unit -> ML.Unit_t
		| ML.Plus | ML.Minus | ML.Times | ML.Div | ML.Eq | ML.Lt -> 
			raise TODO
		| ML.Pair ->
			match el with
			  h::t -> ML.Pair_t(type_check_exp en h,type_check_exp en t)
			| _ -> type_error ("Pair not valid")
		| ML.Fst -> 
			match el with
			  h::_ -> type_check_exp en h
			| _ -> type_error ("Pair not valid")
		| ML.Snd ->
			match el with
			  _::t -> type_check_exp en t
		| ML.Nil -> ML.Tvar_t("nil") (* ??? *)
		| ML.Cons -> raise TODO
		| ML.IsNil -> raise TODO
		| ML.Hd | ML.Tl -> raise TODO)
	| _ -> raise FatalError

(* type_check_exp returns the tipe of the given expression if it typechecks
   internally; otherwise it raises TypeError *)
and type_check_exp (en : env) (e : ML.exp) : ML.tipe = 
	let (r, _) = e in
	match r with
	  ML.Var x -> let ML.Forall(_,t) = lookup en x in t
	| ML.PrimApp (_, _) -> type_check_prim en r
	| ML.Fn (x, e) -> ML.Fn_t(type_check_exp en (ML.Var x),type_check_exp en e)
	| ML.App (e1, e2) -> 
		match (type_check_exp en e1, type_check_exp en e2) with
		| ML.Fn_t(t1,_), t ->
			if (t1 != t) then type_error ("Function expected type" ^t1^"but received"^t)
		| _, _ -> type_error ("Cannot be applied")
	| ML.If (e1, e2, e3) -> 
		(let (t1,t2,t3) = (type_check_exp en e1,type_check_exp en e2,type_check_exp en e3) in
			match t1 with
			| Bool_t ->
				if (t2 != t3) then type_error ("Incompatible types: if-else") else t2
			| _ -> type_error ("Non-boolean value following if statement")
	| ML.Let (x, e1, e2) -> raise TODO
