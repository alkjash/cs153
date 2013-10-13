(* Compile Cish AST to MIPS AST *)
open Mips
open Ast

exception IMPLEMENT_ME

type result = { code : Mips.inst list;
                data : Mips.label list }

(* generate fresh labels *)
let label_counter = ref 0
let new_int() = (label_counter := (!label_counter) + 1; !label_counter)
let new_label() = "L" ^ (string_of_int (new_int()))

(************ Environment and variable map declarations ****************)

(* varmap: immutable association list of vars and offsets
	For scopes we implement the following addition: in varmap we add
	in a new variable each time we have a "let v = exp in stmt" type statement,
	and then at the end of that let we would pop the variable v back off the head of
	the varmap *)
type varmap = (Ast.var * int) list

let empty_varmap = [] 

let vm_insert (vm : varmap) (v : Ast.var) (offset : int) : varmap =
	("MOO" ^ v, offset) :: vm 

let vm_delete (vm : varmap) : varmap =
	List.tl vm

(*
let vm_newscope (vm : varmap) : varmap =
	("BRACE", 0) :: vm

let rec vm_endscope (vm : varmap) : varmap = 
	match vm with
	  ("BRACE", 0) :: t -> t
	| _ :: t -> vm_endscope t
	| [] -> []
*)

(* Returns -1 if not found, otherwise positive offset *)
let rec vm_lookup (vm : varmap) (v : Ast.var) : int =
	let var = "MOO" ^ v in
	match vm with
	  [] -> -1
	| h :: t -> (fun (v2, o) -> if v2 = var then o else
		lookup_var t v) h

(* Creates a new temporary variable and cons it to the head of varmap *)
let new_temp (vm : varmap) : varmap = 
	let v = "T" ^ (string_of_int (new_int())) in
	v :: vm

type env = {varmap : varmap; epilogue : Mips.label}

(************ Helpers **************************************************)

(* push and pop: wrappers for pushing and popping vars on and off of the stack *)
(* push writes assembly code down that pushes the value of register r onto the current
   sp and updating sp *)
let push (r : Mips.reg) : Mips.inst list =
	[Sw(r,R29,zero); Sub(R29,R29,4)]

(* pop writes assembly code down that pops off the top of sp into register r and decrements
   sp *)
let pop (r : Mips.reg) : Mips.inst list =
	[Lw(r,R29,zero); Add(R29,R29,4)]

(************ Statement-Level Compilation ******************************)
(* Essentially the same as Fish statements except treat variables differently
   and have to deal with internal scope changes and function calls
   Every function now takes and returns an env to keep varmap updated, and know
   where to return to (epilogue) in case of Return statement *)

let zero = Word32.fromInt 0

(* The only guarantee we make of an expression is that R3 is stored with its value after it is 
   finished evaluating. It may or may not clobber R4 in the process.
   Compile_exp doesn't handle scope changes or Lets, so we don't have to worry about
   passing back a modified varmap *)
let rec compile_exp ((e , _) : Ast.exp) (en : env) : inst list =
	let vm = en.varmap in
    match e with
      Int j -> [Li(R3, Word32.fromInt j)]

    | Var x -> 
	let offset = vm_lookup vm x in
	[Add(R3, R30, Immed(Word32.fromInt (-offset))); Lw(R3, R3, zero)]

    | Binop(e1, op, e2) -> 
    (* Push value of e1 onto stack *)
    (compile_exp e1) @ (push R3)
    (* Pop e1 into R4, meanwhile return value of e2 is in R3 *)
    @ (compile_exp e2) @ (pop R4)
    @ (match op with
    	  Plus -> [Add(R3, R4, Reg R3)]
    	| Minus -> [Sub(R3, R4, R3)]
    	| Times -> [Mul(R3, R4, R3)]
    	| Div -> [Mips.Div(R3, R4, R3)]
    	| Eq -> [Mips.Seq(R3, R4, R3)]
    	| Neq -> [Sne(R3, R4, R3)]
    	| Lt -> [Slt(R3, R4, R3)]
    	| Lte -> [Sle(R3, R4, R3)]
    	| Gt -> [Sgt(R3, R4, R3)]
    	| Gte -> [Sge(R3, R4, R3)]
    )

    | Not(e) -> 
	(compile_exp e) @ [Mips.Seq(R3, R3, R0)] (* Set R3 to 1 if zero, zero otherwise *)

    | And(e1, e2) -> 
    let l = new_label() in
    (* If e1 = 0 jump directly to end, otherwise push it to stack *)
    (compile_exp e1) @ [Beq(R3, R0, l)] @ (push R3)
    (* Recover e1 from stack into R4, meanwhile return value of e2 is in R3 *)
    @ (compile_exp e2) @ (pop R4)
    (* Store if R4 is nonzero in R4, then store if R3 is nonzero in R3, then
      bitwise and the result *)
    @ [Label l; Sne(R4, R4, R0); Sne(R3, R3, R0); Mips.And(R3, R3, Reg(R4))]

    | Or(e1, e2) -> 
    let l = new_label() in
    (* If e1 != 0 jump directly to end, otherwise push it to stack *)
    (compile_exp e1) @ [Bne(R3, R0, l)] @ (push R3) 
    (* Recover e1 from stack into R4, meanwhile return value of e2 is in R3 *)
    @ (compile_exp e2) @ (pop R4)
    (* Store if R4 is nonzero in R4, then store if R3 is nonzero in R3, then
      bitwise or the result *)
    @ [Label l; Sne(R4, R4, R0); Sne(R3, R3, R0); Mips.Or(R3, R3, Reg(R4))]

    | Assign(x, e) -> 
	let offset = vm_lookup vm x in
	(compile_exp e) @ [Add(R4, R30, Immed(Word32.fromInt (-offset))); Sw(R3, R4, zero)] 

	| Call (f, arglist) ->
	raise IMPLEMENT_ME

let rec compile_stmt ((s,_):Ast.stmt) (en : env) : (inst list * env) = 
    match s with
    | Exp(e) -> 
		(compile_exp e en, en)
    | Seq(s1,s2) -> 
		let (ilist1, en) = compile_stmt s1 en in
		let (ilist2, en) = compile_stmt s2 en in
		((ilist1 @ ilist2), en)
    | If(e,s1,s2) ->
        let else_1 = new_label() in
        let end_l = new_label() in
		let ilist1 = compile_exp e en in
		let (ilist2, en) = compile_stmt s1 en in
		let (ilist3, en) = compile_stmt s2 en in
		((ilist1 @ [Beq(R3, R0, else_1)] @
        ilist2 @ [J end_l; Label else_1] @
        ilist3 @ [Label(end_l)]), en)
    | While(e,s) ->
        let test_l = new_label() in
        let top_l = new_label() in
		let (ilist, en) = compile_stmt s en in
        (([J test_l; Label top_l] @
        ilist @ [Label test_l] @
        (compile_exp e en) @
        [Bne(R3,R0,top_l)]), en)
    (* Rewrite For as a While loop *)
    | For(e1,e2,e3,s) ->
        compile_stmt (Seq((Exp e1, 0),(While(e2,(Seq(s,(Exp e3,0)),0)), 0)), 0) en
	| Let(v, e, s) ->
		raise IMPLEMENT_ME
    (* Store the result of R3 into R2 for return, then jump to epilogue *)
    | Return(e) -> (((compile_exp e en) @ [Mips.Add(R2, R3, Reg(R0)); Jr(epilogue)]), en) 



(************ Procedure-Level Compilation ******************************)
(* Stack Frame setup for prologue:
	--- Frame pointer ---
	arg0
	arg1
	...
	argn
	Caller frame pointer
	Return address
	--- Stack pointer ---

	If n < 4 then there are at least 4 anyways. The first four arg spaces are left empty
	and it's up to the callee to store them there, which we always do for consistency.
*)

(* make_env(): create environment for a function, passing through the code once:
   Association list (var * int) which keeps track of where temporary variables are
   with respect to frame pointer. int is the number of words from the frame pointer
   where the base of var starts. *)
let rec args_list (args : var list) : env =
  | h::t -> (h,R29-R30) @ make_env t
  | _ -> []

let rec make_env (fsig : Ast.funcsig) : env =
	args_list fsig.args

(* Set up stack frame for new procedure;  *)
let frame_start (fsig : Ast.funcsig) : Mips.inst list =
	let nargs = List.length fsig.args in
	(push R30) @ 

(* Clean up before return *)
let frame_end (fsig : Ast.funcsig) : Mips.inst list =
	let nargs = List.length fsig.args in
	(push R30) @ 

(* Actually compile a function: first calls frame_start to set up stack frame, then compiles
   body of procedure, then makes epilogue for return *)
let compile_func (f : Ast.func) : Mips.inst list =
	let Fn(fsig) = f in
	let env = make_env fsig in
	let head = Label ("FUNC" ^ fsig.name) :: (frame_start fsig) in 
	

(* TODO: More subfunctions needed: frame_end, frame_save *)

(************* Compile *************************************************)
(*
(* sets of variables -- Ocaml Set and Set.S *)
module VarSet = Set.Make(struct
                           type t = Ast.var
                           let compare = String.compare
                         end)

(* a table of variables that we need for the code segment *)
let variables : VarSet.t ref = ref (VarSet.empty)

(* generate a fresh temporary variable and store it in the variables set. *)
let rec new_temp() = 
    let t = "T" ^ (string_of_int (new_int())) in
    (* make sure we don't already have a variable with the same name! *)
    if VarSet.mem t (!variables) then new_temp()
    else (variables := VarSet.add t (!variables); t)


(* Helper to add a variable to the variables Set; Prepends "MOO" to variable names
 * to avoid conflicts with MIPs codes *)
let var_add (s : string) : unit =
  ignore (variables := VarSet.add ("MOO" ^ s) (!variables))

(* Helper to find all the variables in an expression and add
 * them to the set variables *)
let rec collect_vars_exp (e : Ast.exp) : unit =
  match fst e with
    Int(_) -> ()
  | Var(x) -> var_add x
  | Binop(e1, _, e2) -> (collect_vars_exp e1; collect_vars_exp e2)
    | Not(e) -> collect_vars_exp e
  | And(e1, e2) -> (collect_vars_exp e1; collect_vars_exp e2)
  | Or(e1, e2) -> (collect_vars_exp e1; collect_vars_exp e2)
  | Assign(x, e) -> (var_add x; collect_vars_exp e) 

(* find all of the variables in a program and add them to
 * the set variables *)
let rec collect_vars (p : Ast.program) : unit = 
  match fst p with
    Exp(e) -> collect_vars_exp e
  | Seq(s1, s2) -> (collect_vars s1; collect_vars s2)
  | If(e, s1, s2) -> (collect_vars_exp e; collect_vars s1; collect_vars s2)
  | While(e, s) -> (collect_vars_exp e; collect_vars s)
  | For(e1, e2, e3, s) -> (collect_vars_exp e1; collect_vars_exp e2; 
    collect_vars_exp e3; collect_vars s)
  | Return(e) -> collect_vars_exp e

(* compiles a Fish statement down to a list of MIPS instructions.
 * Note that a "Return" is accomplished by placing the resulting
 * value in R2 and then doing a Jr R31.
 *)

let zero = Word32.fromInt 0

(* The only guarantee we make of an expression is that R3 is stored with its value after it is 
   finished evaluating. It may or may not clobber R4 in the process *)
let rec compile_exp ((e , _) : Ast.exp) : inst list =
    match e with
      Int j -> [Li(R3, Word32.fromInt j)]

    | Var x -> [La(R3, "MOO" ^ x); Lw(R3, R3, zero)]

    | Binop(e1, op, e2) -> 
  (let t = new_temp() in 
  (* Store return value of e1 into t *)
  (compile_exp e1) @ [La(R4, t); Sw(R3, R4, zero)] 
  (* Recover e1 from t into R4, meanwhile return value of e2 is in R3 *)
  @ (compile_exp e2) @ [La(R4, t); Lw(R4, R4, zero)]
  @ (match op with
      Plus -> [Add(R3, R4, Reg R3)]
    | Minus -> [Sub(R3, R4, R3)]
    | Times -> [Mul(R3, R4, R3)]
    | Div -> [Mips.Div(R3, R4, R3)]
    | Eq -> [Mips.Seq(R3, R4, R3)]
    | Neq -> [Sne(R3, R4, R3)]
    | Lt -> [Slt(R3, R4, R3)]
    | Lte -> [Sle(R3, R4, R3)]
    | Gt -> [Sgt(R3, R4, R3)]
    | Gte -> [Sge(R3, R4, R3)]
  ))

    | Not(e) -> (compile_exp e) @ [Mips.Seq(R3, R3, R0)] (* Set R3 to 1 if zero, zero otherwise *)

    | And(e1, e2) -> 
  (let t = new_temp() in 
  let l = new_label() in
  (* If e1 = 0 jump directly to end, otherwise store return value of e1 into t *)
  (compile_exp e1) @ [Beq(R3, R0, l); La(R4, t); Sw(R3, R4, zero)] 
  (* Recover e1 from t into R4, meanwhile return value of e2 is in R3 *)
  @ (compile_exp e2) @ [La(R4, t); Lw(R4, R4, zero)]
  (* Store if R4 is nonzero in R4, then store if R3 is nonzero in R3, then
    bitwise and the result *)
  @ [Label l; Sne(R4, R4, R0); Sne(R3, R3, R0); Mips.And(R3, R3, Reg(R4))])

    | Or(e1, e2) -> 
  (let t = new_temp() in 
  let l = new_label() in
  (* If e1 != 0 jump directly to end, otherwise store return value of e1 into t *)
  (compile_exp e1) @ [Bne(R3, R0, l); La(R4, t); Sw(R3, R4, zero)] 
  (* Recover e1 from t into R4, meanwhile return value of e2 is in R3 *)
  @ (compile_exp e2) @ [La(R4, t); Lw(R4, R4, zero)]
  (* Store if R4 is nonzero in R4, then store if R3 is nonzero in R3, then
    bitwise or the result *)
  @ [Label l; Sne(R4, R4, R0); Sne(R3, R3, R0); Mips.Or(R3, R3, Reg(R4))])

    | Assign(x, e) -> (compile_exp e) @ [La(R4, "MOO" ^ x); Sw(R3, R4, zero)] 

let rec compile_stmt ((s,_):Ast.stmt) : inst list = 
    match s with
    | Exp(e) -> compile_exp e
    | Seq(s1,s2) -> (compile_stmt s1) @ (compile_stmt s2)
    | If(e,s1,s2) ->
        (let else_1 = new_label() in
        let end_l = new_label() in
        (compile_exp e) @ [Beq(R3, R0, else_1)] @
        (compile_stmt s1) @ [J end_l; Label else_1] @
        (compile_stmt s2) @ [Label(end_l)])
    | While(e,s) ->
        (let test_l = new_label() in
        let top_l = new_label() in
        [J test_l; Label top_l] @
        (compile_stmt s) @
        [Label test_l] @
        (compile_exp e) @
        [Bne(R3,R0,top_l)])
  (* Rewrite For as a While loop *)
    | For(e1,e2,e3,s) ->
        compile_stmt((Seq((Exp e1, 0),(While(e2,(Seq(s,(Exp e3,0)),0)), 0)), 0)) 
  (* Store the result of R3 into R2 for return *)
    | Return(e) -> (compile_exp e) @ [Mips.Add(R2, R3, Reg(R0)); Jr(R31)]
*)

(* compiles Cish AST down to MIPS instructions; Cish has no global variables *)
let compile (p : Ast.program) : result = 
	let rec compile_flist (flist : Ast.func list) : Mips.inst list =
	match p with
	  [] -> []
	| h::t -> (compile_func h) @ (compile_flist t) in
	{ code = (Jal("main") :: (compile_flist p)); data = [] }

let result2string (res : result) : string = 
    let code = res.code in
    let data = res.data in
    let strs = List.map (fun x -> (Mips.inst2string x) ^ "\n") code in
    let vaR8decl x = x ^ ":\t.word 0\n" in
    let readfile f =
      let stream = open_in f in
      let size = in_channel_length stream in
      let text = String.create size in
      let _ = really_input stream text 0 size in
		  let _ = close_in stream in 
      text in
	  let debugcode = readfile "print.asm" in
	    "\t.text\n" ^
	    "\t.align\t2\n" ^
	    "\t.globl main\n" ^
	    (String.concat "" strs) ^
	    "\n\n" ^
	    "\t.data\n" ^
	    "\t.align 0\n"^
	    (String.concat "" (List.map vaR8decl data)) ^
	    "\n" ^
	    debugcode
