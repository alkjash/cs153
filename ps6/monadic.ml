(* Defines our Monadic Intermediate Form, conversion of Scish AST to
 * Monadic Form, and various optimizations on Monadic Form. *)
module S = Scish_ast
type var = string

exception TODO
exception FatalError
exception EXTRA_CREDIT

(* operands -- pure and small *)
type operand = Var of var | Int of int

(* values -- pure, but possibly large *)
type value = 
  Op of operand
| PrimApp of S.primop * (operand list)
| Lambda of var * exp

(* expressions -- possibly impure, control flow *)
and exp = 
  Return of operand
| LetVal of var * value * exp
| LetCall of var * operand * operand * exp
| LetIf of var * operand * exp * exp * exp

(* convert a monadic expression to a string *)
let exp2string (e:exp) : string = 
    let o2s = function
      (Var x) -> x
    | (Int i) -> string_of_int i in
    let rec e2s (tab:string) (e:exp) = 
        match e with
          Return w -> tab ^ (o2s w) ^ "\n"
        | LetCall(x,w1,w2,e) -> 
            tab ^"let "^x^" = "^(o2s w1)^"("^(o2s w2)^")\n"^(e2s tab e)
        | LetIf(x,w,e1,e2,e) -> 
            tab^"let "^x^" = if "^(o2s w)^" then\n"^
            (e2s (tab^"           ") e1)^
                  tab^"         else\n"^
            (e2s (tab^"           ") e2)^
            (e2s tab e)
        | LetVal(x,Op w,e) -> "let "^x^" = "^(o2s w)^"\n"^(e2s tab e)
        | LetVal(x,PrimApp(p,ws),e) -> 
            tab^"let "^x^" = "^(S.primop2string p)^"("^
            (String.concat "," (List.map o2s ws))^")\n"^(e2s tab e)
        | LetVal(x,Lambda(y,e1),e2) -> 
            tab^"fun "^x^"("^y^") =\n"^(e2s (tab^"     ") e1)^(e2s tab e2) in
    e2s "" e

(* used to generate fresh variables *)
let counter = ref 0
let fresh_var() = 
    let c = !counter in
    counter := c+1; "x"^(string_of_int c)

(* naive var->var environments *)
exception UnboundVariable of string
let empty_env (x:var):var = (print_string ("unbound variable "^x); 
                             raise (UnboundVariable x))
let extend (env:var->var) (x:var) (y:var) =
    fun z -> if z = x then y else env z

(* convert an expression e to monadic form:
 *   env is used to rename variables on the fly so they are unique.
 *   k is used as a continuation as explained below:
 * 
 * Conceptually, each Scish expression compiles down to a
 * monadic expression of the form Let(x1,v1,...,Let(xn,vn),Return v).
 * That is, it's a sequence of let-bindings followed by a return
 * of an operand.
 *
 * Consider what happens when we have a compound expression
 * such as e1+e2.  If we had nested lets, then we could write:
 *    Let(x1,compile e1,
 *    Let(x2,compile e2,
 *    LetVal(x3,PrimApp(Plus,[Var x1,Var x2]),
 *    Return (Var x3))))
 * but we don't have nested lets.  In general, we'll have to
 * take the let-expressions and operand for e1 and flatten it
 * out using the equation:
 *
 *  (let x = (let y = a in b) in c) == let y = a in let x = b in c
 *
 * Similarly, we'll have to flatten out the translation of e2.
 * But all of this flattening amounts to appending lists of 
 * let-declarations.  If we're not careful, we end up with a 
 * quadractic algorithm (the same problem we had with lowering to MIPS.)
 *
 * To solve this problem, we use a clever trick which allows us
 * to flatten on-the-fly.  The trick is to parameterize the 
 * translation with a function, which when given an operand,
 * generates "the rest of the translation".  This extra parameter,
 * called a continuation, is essentially a functional accumulator.
 *
 * Consider the case for tom App(e1,e2) Return
 * Imagine that the monadic form of the expressions is
 *    e1 == Let x1=a1 in...Let xn=an in Return w1
 *    e2 == Let y1=b1 in...Let ym=bm in Return w2
 * Then we should get as output something like:
 *   Let x1=a1...in Let xn=an in 
 *    Let y1=b1 ...in Let ym=bm in Let z=w1(w2) in Return z
 *
 * Following the definitions, we have:
 *   tom (App(e1,e2)) Return = tom e1 k1
 *     where k1 = (fn w1-> tom e2 (fn w2 -> Let z=w1(w2) in Return z))
 *   tom e1 k1 = 
 *     Let x1=a1 in ... Let xn=an in k1(w1)    (by assumption)
 *
 *   k1(w1) = tom e2 (fn w2 -> Let z=w1(w2) in Return z)
 *          = Let y1=b1 in ... Let ym=bm in Let z=w1(w2) in Return z
 * So tom e1 k1 = 
 *     Let x1=a1 in ... Let xn=an in 
 *       Let y1=b1 in ...Let ym=bm in Let z=w1(w2) in Return z
 *)
let rec tom (e : S.exp) (env:var->var) (k : operand -> exp) : exp = 
    match e with
      S.Var x -> k (Var (env x))
    | S.Int i -> k (Int i)
    | S.App(S.Lambda(x,e1),e2) -> (* same as let x = e2 in e1 *)
        let x' = fresh_var() in
        tom e2 env (fun w1 -> LetVal(x',Op w1,tom e1 (extend env x x') k))
    | S.App(e1,e2) -> 
        let x = fresh_var() in
        tom e1 env (fun w1 -> tom e2 env (fun w2 -> LetCall(x,w1,w2,k(Var x))))
    | S.Lambda(x,e) -> 
        let x' = fresh_var() in
        let f = fresh_var() in
        LetVal(f,Lambda(x',tom e (extend env x x') (fun x -> Return x)),k(Var f))
    | S.PrimApp(p,es) -> 
        let x = fresh_var() in
        toms es [] env (fun ws -> LetVal(x,PrimApp(p,ws),k(Var x)))
    | S.If(e1,e2,e3) -> 
        let x = fresh_var() in
        tom e1 env 
              (fun w -> LetIf(x,w,tom e2 env (fun x -> Return x),
                              tom e3 env (fun x -> Return x),k(Var x)))
and toms (es : S.exp list) (accum: operand list) 
           (env : var->var) (k: operand list -> exp) = 
    match es with
      [] -> k(List.rev accum)
    | e::rest -> tom e env (fun w -> toms rest (w::accum) env k)

let tomonadic (e:S.exp) : exp = tom e empty_env (fun x -> Return x)

(* a flag used to track when an optimization makes a reduction *)
let changed : bool ref = ref true
let change x = (changed := true; x)

(* naive 'a -> 'b option environments *)
let empty_env x = None
let extend env x w = fun y -> if y = x then Some w else env y

(* operand propagation -- LetVal(x,Op w,e) --> e[w/x] -- just like notes. *)
let rec cprop_exp (env : var -> operand option) (e:exp) = 
    match e with
      Return w -> Return (cprop_oper env w)
    | LetVal(x,Op w,e) -> 
        change(cprop_exp (extend env x (cprop_oper env w)) e)
    | LetVal(x,PrimApp(p,ws),e) -> 
        LetVal(x,PrimApp(p,List.map (cprop_oper env) ws),cprop_exp env e)
    | LetVal(x,Lambda(y,e1),e2) -> 
        LetVal(x,Lambda(y,cprop_exp env e1),cprop_exp env e2)
    | LetCall(x,w1,w2,e) -> 
        LetCall(x,cprop_oper env w1, cprop_oper env w2,cprop_exp env e)
    | LetIf(x,w,e1,e2,e) -> 
        LetIf(x,cprop_oper env w,cprop_exp env e1,cprop_exp env e2,
              cprop_exp env e)
and cprop_oper (env : var -> operand option) (w:operand) = 
    match w with
      Var x -> (match env x with None -> w | Some w' -> w')
    | Int _ -> w

let cprop e = cprop_exp empty_env e

(* common sub-value elimination -- as in the slides *)
let rec cse (e : exp) : exp = 
	cse1 empty_env e

and cse1 (env : value -> var option) (e : exp) : exp =
  match e with
  | Return w -> Return w
  | LetVal(x,v,e) ->
    (match env v with
      | None -> LetVal(x, cse_val env v, cse1 (extend env v x) e)
      | Some y -> change (LetVal(x, Op (Var y), cse1 env e)))
  | LetCall(x, f, w, e) -> LetCall(x, f, w, cse1 env e)
  | LetIf(x, w, e1, e2, e) ->
      LetIf(x, w, cse1 env e1, cse1 env e2, cse1 env e)
and cse_val (env : value -> var option) (v : value) : value =
  match v with
  | Lambda(x, e) -> Lambda(x, cse1 env e)
  | v -> v

(* constant folding
 * Apply primitive operations which can be evaluated. e.g. fst (1,2) = 1
 *)

(* Passes through code calling cfold_val on each value that appears *)
let rec cfold (e : exp) : exp =
	cfold1 e empty_env

(* Keep an environment consisting of all the pairs currently defined for the sake of folding fst and snd *)
and cfold1 (e : exp) (en: var -> value option) : exp =
	match e with
	  Return _ -> e
	| LetVal (x, v, e) ->
		let w = cfold_val v en in
		let (w, en) = (match w with 
		  			Some (Op _ as w) | Some (Lambda _ as w) -> (w, en)
				  | Some w -> (w, extend en x w)
				  | None -> (v, en)) in
		LetVal (x, w, cfold1 e en)
	| LetCall (x, o1, o2, e) -> 
		LetCall (x, o1, o2, cfold1 e en)
	| LetIf (x, o1, e1, e2, e3) -> 
		(* Don't update en with new pairs added in e1, e2, since these aren't necessarily the values we want *)
		LetIf (x, o1, cfold1 e1 en, cfold1 e2 en, cfold1 e3 en)

(* Apply primitive operations in monadic values
 * Returns folded value (to replace original v with) if it is different; returns the value if it is a cons,
 * and returns None if no folding to do *)
and cfold_val (v: value) (en : var -> value option) : (value option) =
	match v with
	  Op _ -> None 
	| PrimApp (s_op, l) ->
		(match s_op with
		(* Arithmetic and comparison operators: just do it if both constants *)
		  S.Plus | S.Minus | S.Times | S.Div 
		| S.Eq | S.Lt ->
			(match l with
		  	  [Int a; Int b] -> 
				Some (match s_op with
				  S.Plus -> change (Op (Int (a + b)))
				| S.Minus -> change (Op (Int (a - b)))
				| S.Times -> change (Op (Int (a * b)))
				| S.Div -> change (Op (Int (a / b)))
				| S.Eq -> change (Op (Int (if (a == b) then 1 else 0)))
				| S.Lt -> change (Op (Int (if (a < b) then 1 else 0)))
				| _ -> raise FatalError)	
			| [Int 1; Var x] ->
				(match s_op with
				  S.Times -> change (Some (Op (Var x)))  (* Fold 1 * x = x *)
				| _ -> None)
			| [Int 0; Var x] ->
				(match s_op with
				   S.Plus -> change (Some (Op (Var x)))  (* Fold 0 + x = x *)
				 | _ -> None)
			| [Var x; Int 0] ->
				(match s_op with
				   S.Minus -> change (Some (Op (Var x))) (* Fold x - 0 = x *)
				 | S.Plus -> change (Some (Op (Var x)))  (* Fold x + 0 = x *)
				 | _ -> None)
			| [Var x; Int 1] ->
				(match s_op with
				   S.Div -> change (Some (Op (Var x)))   (* Fold x / 1 = x *)
				 | S.Times -> change (Some (Op (Var x))) (* Fold x * 1 = x *)
				 | _ -> None)
			| _ -> None)
		(* When v is a cons, return it to be added to env for future folding of pairs *)
		| S.Cons -> Some v
		(* Fold a fst/snd (cons) by looking it up in the env, if it is there *)
		| S.Fst | S.Snd ->
			(match l with
			  [Var x] -> (match en x with
							(* If found in environment fold the fst/snd *)
						    Some w -> (match w with
							            PrimApp (S.Cons, l) -> 
											change (Some (Op (if s_op = S.Fst then List.hd l
											else List.hd (List.tl l))))
									  | _ -> raise FatalError)
							(* Otherwise do nothing *)
						  | None -> None)
			| _ -> raise FatalError))
	| Lambda (x, e) ->
		(* Nothing actually happened to en; just defining a function *)
		Some (Lambda (x, cfold1 e en))

(* To support a somewhat more efficient form of dead-code elimination and
 * inlining, we first construct a table saying how many times each variable 
 * is used, and how many times each function is called.
 * This table is then used to reduce LetVal(x,v,e) to e when x is used
 * zero times, and to reduce LetVal(x,Lambda(y,e),...,LetCall(z,x,w,e2)...)
 * to (...LetVal(y,Op w,(Let(z,e,e2)))...) when x is used once and 
 * that use is a call site.
 *)
(* type cnt_table = (var, {uses:int ref,calls:int ref}) Hashtbl.hash_table *)
type entry = { uses : int ref; calls: int ref }
exception NotFound
let new_cnt_table() = 
    Hashtbl.create 101
let def (t) (x:var) = 
    Hashtbl.add t x {uses=ref 0;calls=ref 0}
let inc r = (r := !r + 1)
let use (t) (x:var) = 
    inc ((Hashtbl.find t x).uses)
let call (t) (x:var) = 
    inc ((Hashtbl.find t x).calls)
let get_uses (t) (x:var) : int = !((Hashtbl.find t x).uses)
let get_calls (t) (x:var) : int = !((Hashtbl.find t x).calls)

let count_table (e:exp) = 
    let table = new_cnt_table() in
    let def = def table in
    let use = use table in
    let call = call table in
    let rec occ_e e = 
      match e with
        Return w -> occ_o w
      | LetVal(x,v,e) -> (def x; occ_v v; occ_e e)
      | LetCall(x,Var f,w2,e) -> 
         (def x; use f; call f; occ_o w2; occ_e e)
      | LetCall(x,w1,w2,e) -> 
         (def x; occ_o w1; occ_o w2; occ_e e)
      | LetIf(x,w,e1,e2,e) -> (def x; occ_o w; occ_e e1; occ_e e2;
                               occ_e e)
    and occ_v v = 
      match v with
        Op w -> occ_o w
      | PrimApp(_,ws) ->  List.iter occ_o ws
      | Lambda(x,e) -> (def x; occ_e e)
        
    and occ_o oper = 
      match oper with
        Var x -> use x
      | Int _ -> () in
    occ_e e; table

(* dead code elimination *)
let rec dce (e : exp) : exp =
	(* Compute use/call table exactly once for the entire expression *)
	let table = count_table e in
	dce1 e table

and dce1 e ct =
  match e with
  | Return w -> Return w
  | LetVal(x,v,e) ->
      (match v with
      | Lambda (x1, e1) ->
        if (get_calls ct x = 0) && (get_uses ct x = 0) then change (dce1 e ct)
        else LetVal(x, Lambda (x1, dce1 e1 ct), dce1 e ct)
      | _ ->
        if get_uses ct x = 0 then change (dce1 e ct)
        else LetVal(x,v,dce1 e ct))
  | LetCall(x, f, w, e) ->
      LetCall(x, f, w, dce1 e ct)
  | LetIf(x, w, e1, e2, e) ->
      LetIf(x, w, dce1 e1 ct, dce1 e2 ct, dce1 e ct)

(* (1) inline functions 
 * (2) reduce LetIf expressions when the value being tested is a constant.
 * 
 * In both cases, we are forced to re-flatten out what would otherwise
 * be nested let-expressions.  Therefore, we use the "splice" helper
 * function to splice the two expressions together.   In particular,
 * splice x e1 e2 is equivalent to flattening out let x=e1 in e2.
 * Note, however, that in the case of inlining where the threshold is
 * above 1, we can end up duplicating the body of a function.  We must
 * restore the invariant that no bound variable is duplicated by renaming
 * each bound variable in the copy (else other optimizations will break
 * due to variable capture.)  
 *)
let splice x e1 e2 = 
    let rec splice_exp final (env : var -> operand option) (e:exp) = 
      match e with
        Return w -> 
          if final then LetVal(x,Op (cprop_oper env w),e2)
          else Return(cprop_oper env w)
      | LetVal(y,v,e) -> 
          let y' = fresh_var() in
          LetVal(y',loop_value env v,
                splice_exp final (extend env y (Var y')) e)
      | LetCall(y,w1,w2,e) -> 
          let y' = fresh_var() in
          LetCall(y',cprop_oper env w1,cprop_oper env w2,
                  splice_exp final (extend env y (Var y')) e)
      | LetIf(y,w,e1,e2,e) -> 
          let y' = fresh_var() in
          LetIf(y',cprop_oper env w, splice_exp false env e1,
                splice_exp false env e2, 
                splice_exp final (extend env y (Var y')) e)
    and loop_value env v = 
      match v with
        Op w -> Op (cprop_oper env w)
      | Lambda(y,e) -> 
          let y' = fresh_var() in 
          Lambda(y',splice_exp false (extend env y (Var y')) e)
      | PrimApp(p,ws) -> PrimApp(p,List.map (cprop_oper env) ws) in
  splice_exp true empty_env e1

let always_inline_thresh (e : exp) : bool = true  (** Always inline **)
let never_inline_thresh  (e : exp) : bool = false (** Never inline  **)

(* return true if the expression e is smaller than i, i.e. it has fewer
 * than i constructors
 *)
let rec constructor_counter (n : int) (e : exp) : int =
  match e with
  | Return op -> n + 1
  | LetVal(_, v, e) -> constructor_counter (n + 2 + (cc_val v)) e
  | LetCall(_, _, _, e) -> constructor_counter (n + 4) e
  | LetIf(_, _, e1, e2, e) -> (constructor_counter (n + 3) e) + (constructor_counter 0 e1)
      + (constructor_counter 0 e2)

and cc_val (v : value) : int =
	match v with
	  Op _ -> 1
	| Lambda (_, e) ->
		2 + (constructor_counter 0 e)
	| PrimApp (_, l) ->
		1 + (List.length l)

let size_inline_thresh (i : int) (e : exp) : bool =
  (constructor_counter 0 e) < i

(* inlining 
 * only inline the expression e if (inline_threshold e) return true
 * or if the function only called once
 *)
let rec inline (inline_threshold: exp -> bool) (e:exp) : exp =
  (* if (inline_threshold e) then *)
  let table = count_table e in
  inline_e inline_threshold e (Var "") table (Op (Var ""))
  (* let table = count_table e in
  inline_e e table "" *)

and inline_e it e name t fcn =
  let ie e n fcn = inline_e it e n t fcn in
  match e with
  | Return _ -> e
  | LetVal (f, Lambda(x, e1), e2) ->
	  (* Regardless: inline internally to the function *)
	  let e1 = ie e1 name fcn in
	  (* If called at most once or below threshold, inline *)
      if get_calls t f = 1 || it e1 then
		(let e2 = ie e2 name fcn in
        LetVal(f, Lambda(x, e1), ie e2 (Var f) (Lambda(x, e1))))
      else LetVal(f, Lambda(x, e1), ie e2 name fcn)
  | LetVal (x, v, e) ->
      LetVal(x, v, ie e name fcn)
  | LetCall (y, f, w, e2) ->
      if name = f then 
        (match fcn with 
		   Lambda(x, e1) ->
        		change (splice y (LetVal(x, Op w, e1)) (ie e2 name fcn))
		 | _ -> raise FatalError)
      else
        LetCall(y, f, w, ie e2 name fcn)
  | LetIf(v, op, e, e1, e2) ->
      LetIf(v, op, ie e name fcn, ie e1 name fcn, ie e2 name fcn)


(* reduction of conditions
 * - Optimize conditionals based on contextual information, e.g.
 *   if (x < 1) then if (x < 2) then X else Y else Z =-> 
 *     if (x < 1) then X else Z
 *   (since x < 1 implies x < 2)
 * - This is similar to constant folding + logic programming
 *)

(* Helper function that decides if two monadic expressions are identical
   Doesn't do very much: just checks if they are identical up to some arithmetic order
   and the variables defined internally are defined in the same way;
   keeps an environment which maps new variables of e1 to new variables of e2 if and only
   if they are the same *)

let rec mon_eq (en : var -> var option) (e1 : exp) (e2 : exp) : bool =
	(* Two variables are the same if they are actually the same or 
	if they are defined identically *)
	let eq x y = (x = y || en x = Some y) in
	(* Ignore the match cases where we apply the same function to same ints or
	If with w1 = w2 are ints - these get folded out by other optimizations *)
	match (e1, e2) with
	  (Return (Int x1), Return (Int x2)) -> (x1 = x2)
	| (Return (Var x1), Return (Var x2)) -> (eq x1 x2)
	| (LetVal (x1, v1, e1'), LetVal (x2, v2, e2')) ->
		(mon_val_eq en v1 v2) && (mon_eq (extend en x1 x2) e1' e2')
	| (LetCall (x1, Var f1, Var w1, e1'), LetCall (x2, Var f2, Var w2, e2')) ->
		(eq f1 f2) && (eq w1 w2) && (mon_eq (extend en x1 x2) e1' e2')
	| (LetIf (x1, Var w1, e11, e12, e13), LetIf (x2, Var w2, e21, e22, e23)) ->
		(eq x1 x2) && (eq w1 w2) && (mon_eq en e11 e21) && (mon_eq en e12 e22) &&
		(mon_eq (extend en x1 x2) e13 e23)
	| _ -> false

and mon_val_eq (en : var -> var option) (v1 : value) (v2 : value) : bool =
	let eq x y = match (x, y) with
	  (Int a, Int b) -> a = b
	| (Var x1, Var y1) -> (x1 = y1 || en x1 = Some y1) 
	| _ -> false in
	match (v1, v2) with
	| (Op x1, Op x2) -> eq x1 x2
	| (PrimApp (o1, l1), PrimApp (o2, l2)) ->
		(o1 = o2) && (match o1 with
		  S.Plus | S.Times | S.Eq -> (* Commutative *)
			(match (l1, l2) with
			   ([a1; b1], [a2; b2]) ->
					((eq a1 a2) && (eq b1 b2)) || ((eq a1 b2) && (eq b1 a2))
			 | _ -> false)
		| S.Minus | S.Div | S.Cons | S.Lt -> (* Noncommutative *)
			(match (l1, l2) with
			   ([a1; b1], [a2; b2]) ->
					((eq a1 a2) && (eq b1 b2))
			 | _ -> false)
		| S.Fst | S.Snd -> (* Single Argument *)
			(match (l1, l2) with
			   ([a1], [a2]) ->
					(eq a1 a2)
			 | _ -> false))
	| (Lambda (x1, e1), Lambda (x2, e2)) -> mon_eq (extend en x1 x2) e1 e2
	| _ -> false

let rec redtest (e : exp) : exp =
	redtest1 e

and redtest1 (e : exp) : exp =
	match e with
	  Return _ -> e
	| LetVal (x, v, e) -> LetVal (x, redtest_val v, redtest1 e)
	| LetCall (x, f, w, e) -> LetCall(x, f, w, redtest1 e)
	| LetIf (x, w, e1, e2, e3) ->
		(* If e1 and e2 are identical then skip the checking w and just letval one of them *)
		if mon_eq empty_env e1 e2 then change (splice x (redtest1 e1) (redtest1 e3))
		else
		((* Check if w is a constant; if so forget about doing the if *)
		match w with
		  Int i -> if i <> 0 then change (splice x (redtest1 e1) (redtest1 e3))
			else change (splice x (redtest1 e2) (redtest1 e3))
		| _ -> LetIf (x, w, redtest1 e1, redtest1 e2, redtest1 e3))

and redtest_val (v : value) : value = 
	match v with
	  Lambda (x, e) -> Lambda (x, redtest1 e)
	| _ -> v

(* optimize the code by repeatedly performing optimization passes until
 * there is no change. *)
let optimize inline_threshold e = 
    let opt = fun x -> dce (cprop (redtest (cse (cfold ((inline inline_threshold) x))))) in
    let rec loop (i:int) (e:exp) : exp = 
      (if (!changed) then 
        let _ = changed := false in
        let e' = opt e in
        let _ = print_string ("\nAfter "^(string_of_int i)^" rounds:\n") in
        let _ = print_string (exp2string e') in
        loop (i+1) e'
      else e) in
    changed := true;
    loop 1 e

