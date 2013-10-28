open Mlish_ast

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
	| h::t -> Snd h

(***************** Type-Checking ******************************)

(* Checking primitives *)
let rec type_check_prim (r : Mlish_ast.rexp) : tipe =
	match r with
	  PrimApp(p, el) -> 
		(match p with
		  Int _ -> Int_t
		| Bool _ -> Bool_t
		| Unit -> Unit_t
		| Plus | Minus | Times | Div | Eq | Lt -> 
			raise TODO
		| Pair -> raise TODO
		| Fst -> raise TODO
		| Snd -> raise TODO
		| Nil -> raise TODO
		| Cons -> raise TODO
		| IsNil -> raise TODO
		| Hd | Tl -> raise TODO)
	| _ -> raise FatalError

(* type_check_exp returns the tipe of the given expression if it typechecks
   internally; otherwise it raises TypeError *)
and type_check_exp (e : Mlish_ast.exp) : tipe = 
	let (r, _) = e in
	match r with
	  Var x -> raise TODO
	| PrimApp (_, _) -> type_check_prim e
	| Fn (x, e) -> raise TODO
	| App (e1, e2) -> raise TODO
	| If (e1, e2, e3) -> raise TODO
	| Let (x, e1, e2) -> raise TODO
