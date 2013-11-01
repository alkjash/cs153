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

(* Guess: creates an undetermined guess for a currently unknown type variable *)
let guess() = 
	ML.Guess_t (ref None)

(* Substitute: Given a list subs of pairs (tvar, tipe) and an incomplete tipe (i.e. one with
	indeterminate tvars), substitute the tipes in *)
let rec substitute (t : ML.tipe) (subs : (ML.tvar * ML.tipe) list) : tipe =
	(* Helper: find the value of tv in subs *)
	let find_tipe tv =
		snd (List.hd (List.filter (fun p -> ((fst p) = tv)) subs)) in
	match t with
	  ML.Tvar_t tv -> find_tipe tv
	| ML.Int_t -> ML.Int_t
	| ML.Bool_t -> ML.Bool_t
	| ML.Unit_t -> ML.Unit_t
	| ML.Fn_t (x, y) -> ML.Fn_t (substitute x subs, substitute y subs)
	| ML.Pair_t (x, y) -> ML.Pair_t (substitute x subs, substitute y subs)
	| ML.List_t t2 -> ML.List_t (substitute t2 subs)
	| ML.Guess_t (ref None) -> ML.Guess_t (ref None)
	| ML.Guess_t (ref Some t2) -> ML.Guess_t (ref Some (substitute t2 subs))

(* Instantiate: substitute guesses for all the unknown variables *)
let instantiate (ts : ML.tipe_scheme) : tipe =
	let Forall(vs, t) = ts in
	let vs_and_ts = map (fun a -> (a, guess()) vs in
	substitute t vs_and_ts

(* Unify: check if t1 and t2 can be of the same tipe; improve guesses so that
   they are the same tipe at the end *)
let rec unify (t1 : ML.tipe) (t2 : ML.tipe) : bool =
	match (t1, t2) with
	  (t, t) -> true
	| (ML.Guess_t (ref(Some t3)), _) -> unify t3 t2
	| (ML.Guess_t (r as (ref None)), _) -> (r := t2; true)
	| (_, ML.Guess_t(_)) -> unify t2 t1
	| (ML.Fn_t(a1, b1), ML.Fn_t(a2, b2)) 
		-> (unify a1 a2) && (unify b1 b2)
	| ML.Pair_t(a1, b1), ML.Pair_t(a2, b2) 
		-> (unify a1 a2) && (unify b1 b2)
	| _ -> false

(* Checking primitive MLish expressions *)
let rec type_check_prim (en : env) (r : ML.rexp) : ML.tipe =
	match r with
	  ML.PrimApp(p, el) -> 
		(match p with
		  ML.Int _ -> 
			if List.length el = 0 then ML.Int_t else type_error()
		| ML.Bool _ -> 
			if List.length el = 0 then ML.Bool_t else type_error()
		| ML.Unit -> ML.Unit_t
			if List.length el = 0 then ML.Unit_t else type_error()
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
	  ML.Var x -> 
		instantiate (lookup en x)
	| ML.PrimApp (_, _) -> 
		type_check_prim r
	| ML.Fn (x, e) -> 
		let g = guess() in
		ML.Fn_t (g, type_check_exp (extend en x Forall([], g)) e)
	| ML.App (e1, e2) -> 
		let (t1, t2, t) = (type_check_exp en e1, type_check_exp en e2, guess()) in
		if unify t1 Fn_t(t2, t) then t else 
			type_error "Function expected type doesn't match received type" ^ 
	| ML.If (e1, e2, e3) -> 
		if unify (type_check_exp en e1) Bool_t then
			let (t2, t3) = (type_check_exp en e2, type_check_exp en e3) in
			if unify t2 t3 then t2 else type_error "Incompatible types: if-else" 
		else 
			type_error "Non-boolean value following if statement" 
	| ML.Let (x, e1, e2) -> raise TODO
