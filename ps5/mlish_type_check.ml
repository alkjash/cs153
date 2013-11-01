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

(* Generalize: construct a tipe scheme defining a variable, substituting tvars into
	all guesses *)
let generalize (en : env) (t : tipe) : tipe_scheme =
	let guesses_of (t1 : tipe) : 
	let t_gs = guesses_of_tipe t in
	let env_list_gs =
		map (fun (x,s) -> guesses_of s) e in
	let env_gs = foldl union empty env_list_gs
	let diff = minus t_gs env_gs in
	let gs_vs =
		map (fun g -> (g,freshvar())) diff in
	let tc = subst_guess(gs_vs,t) in
		Forall(map snd gs_vs, tc)

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
		(let el = map (fun x -> type_check_exp en x) el in
		match p with
		  ML.Int _ -> 
			if List.length el = 0 then ML.Int_t else type_error "Int takes no arguments"
		| ML.Bool _ -> 
			if List.length el = 0 then ML.Bool_t else type_error "Bool takes no arguments"
		| ML.Unit -> ML.Unit_t
			if List.length el = 0 then ML.Unit_t else type_error "Unit takes no arguments"
		| ML.Plus | ML.Minus | ML.Times | ML.Div | ML.Eq | ML.Lt -> 
			if (List.length el = 2) then
				if (List.length (List.filter (fun x -> unify x ML.Int_t) el) = 2) then
					ML.Int_t
				else type_error "Arithmetic operations only take int"
			else type_error "Arithmetic operations take two arguments"
		| ML.Pair ->
			if List.length el <> 2 then
				type_error "Pair takes two arguments"
			else
				ML.Pair_t (List.hd el, List.hd (List.tl t))
		| ML.Fst -> 
			match el with
			  [h] -> let (t1, t2, t3) = (h, guess(), guess()) in
				if unify t1 (ML.Pair_t (t2, t3)) then t2 else
				type_error "Fst must take a pair"
			| _ -> type_error "Fst takes one argument"
		| ML.Snd ->
			match el with
			  [h] -> let (t1, t2, t3) = (h, guess(), guess()) in
				if unify t1 (ML.Pair_t (t2, t3)) then t3 else
				type_error "Snd must take a pair"
			| _ -> type_error "Snd takes one argument"
		| ML.Nil -> 
			if List.length el = 0 then ML.List_t (guess()) (* Empty list of undetermined tipe *)
			else type_error "Nil list takes no arguments"
		| ML.Cons ->
			if List.length el <> 2 then
				type_error "Pair takes two arguments"
			else let (t1, t2) = (List.hd el, List.hd (List.tl el)) in
				if unify (ML.List_t t1) t2 then t2
				else type_error "List head type doesn't match elements of tail"
		| ML.IsNil -> 
			if List.length el = 1 then
				if unify (List.hd el) (ML.List_t (guess())) then
					Bool_t
				else type_error "IsNil's argument is not a list"
			else type_error "IsNil takes one argument"
		| ML.Hd ->
			if List.length el = 1 then
				let g = guess() in
				if unify (List.hd el) (ML.List_t g) then
					g
				else type_error "Hd's argument is not a list"
			else type_error "Hd takes one argument"
		| ML.Tl ->
			if List.length el = 1 then
				let g = guess() in
				if unify (List.hd el) (ML.List_t g) then
					ML.List_t g
				else type_error "Tl's argument is not a list"
			else type_error "Tl takes one argument")
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
	| ML.Let (x, e1, e2) ->
		let s = generalize en (type_check_exp en e1) in
		type_check_exp (extend en x s) e2
