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
let rec type_check_prim (r : ML.rexp) : ML.tipe =
	match r with
	  ML.PrimApp(p, el) -> 
		(match p with
		  ML.Int _ -> ML.Int_t
		| ML.Bool _ -> ML.Bool_t
		| ML.Unit -> ML.Unit_t
		| ML.Plus | ML.Minus | ML.Times | ML.Div | ML.Eq | ML.Lt -> 
			raise TODO
		| ML.Pair -> raise TODO
		| ML.Fst -> raise TODO
		| ML.Snd -> raise TODO
		| ML.Nil -> raise TODO
		| ML.Cons -> raise TODO
		| ML.IsNil -> raise TODO
		| ML.Hd | ML.Tl -> raise TODO)
	| _ -> raise FatalError

(* type_check_exp returns the tipe of the given expression if it typechecks
   internally; otherwise it raises TypeError *)
and type_check_exp (e : ML.exp) : ML.tipe = 
	let (r, _) = e in
	match r with
	  ML.Var x -> raise TODO
	| ML.PrimApp (_, _) -> type_check_prim r
	| ML.Fn (x, e) -> raise TODO
	| ML.App (e1, e2) -> raise TODO
	| ML.If (e1, e2, e3) -> raise TODO
	| ML.Let (x, e1, e2) -> raise TODO
