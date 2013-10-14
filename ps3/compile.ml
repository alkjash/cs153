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

(* Insert a new 4-word variable which has offset equal to 4 more than the last one *)
let vm_insert (vm : varmap) (v : Ast.var) : varmap =
	let offset = match vm with
	  h::t -> (snd h) + 4
	| [] -> 4 in
	(v, offset) :: vm 

let vm_delete (vm : varmap) : varmap =
	List.tl vm

(* Returns -1 if not found, otherwise positive offset *)
let rec vm_lookup (vm : varmap) (v : Ast.var) : int =
	match vm with
	  [] -> -1
	| h :: t -> (fun (v2, o) -> if v2 = v then o else
		vm_lookup t v) h

type env = {varmap : varmap; epilogue : Mips.label}

(************ Helpers **************************************************)

let zero = Word32.fromInt 0

(* push and pop: wrappers for pushing and popping vars on and off of the stack *)
(* push writes assembly code down that pushes the value of register r onto the current
   sp and updating sp *)
let push (r : Mips.reg) : Mips.inst list =
	[Add(R29,R29, Immed(Word32.fromInt (-4))); Sw(r, R29, zero)]

(* pop writes assembly code down that pops off the top of sp into register r and decrements
   sp *)
let pop (r : Mips.reg) : Mips.inst list =
	[Lw(r, R29, zero); Add(R29, R29, Immed(Word32.fromInt 4))]

(************ Statement-Level Compilation ******************************)
(* Essentially the same as Fish statements except treat variables differently
   and have to deal with internal scope changes and function calls
   Every function now takes and returns an env to keep varmap updated, and know
   where to return to (epilogue) in case of Return statement *)

(* The only guarantee we make of an expression is that R3 is stored with its value after it is 
   finished evaluating. It may or may not clobber R4 in the process.
   Compile_exp doesn't handle scope changes or Lets, so we don't have to worry about
   passing back a modified varmap 
   Because our code is written in such a way that any expression is free to clobber any
   registers except R3, we don't need to worry about storing caller registers before
   performing a function call *)
let rec compile_exp ((e , _) : Ast.exp) (en : env) : inst list =
	let vm = en.varmap in
    match e with
      Int j -> [Li(R3, Word32.fromInt j)]

    | Var x -> 
	let offset = vm_lookup vm x in
	[Add(R3, R30, Immed(Word32.fromInt (-offset))); Lw(R3, R3, zero)]

    | Binop(e1, op, e2) -> 
    (* Push value of e1 onto stack *)
    (compile_exp e1 en) @ (push R3)
    (* Pop e1 into R4, meanwhile return value of e2 is in R3 *)
    @ (compile_exp e2 en) @ (pop R4)
    @ (match op with
    	  Plus -> [Add(R3, R4, Reg R3)]
    	| Minus -> [Sub(R3, R4, R3)]
    	| Times -> [Mul(R3, R4, R3)]
    	| Div -> [Mips.Div(R3, R4, R3)]
    	| Eq -> [Mips.Seq(R3, R4, R3)]
    	| Neq -> [Sne(R3, R4, R3)]
    	| Lt -> [Slt(R3, R4, Reg R3)]
    	| Lte -> [Sle(R3, R4, R3)]
    	| Gt -> [Sgt(R3, R4, R3)]
    	| Gte -> [Sge(R3, R4, R3)]
    )

    | Not(e) -> 
	(compile_exp e en) @ [Mips.Seq(R3, R3, R0)] (* Set R3 to 1 if zero, zero otherwise *)

    | And(e1, e2) -> 
    let l = new_label() in
    (* If e1 = 0 jump directly to end, otherwise push it to stack *)
    (compile_exp e1 en) @ [Beq(R3, R0, l)] @ (push R3)
    (* Recover e1 from stack into R4, meanwhile return value of e2 is in R3 *)
    @ (compile_exp e2 en) @ (pop R4)
    (* Store if R4 is nonzero in R4, then store if R3 is nonzero in R3, then
      bitwise and the result *)
    @ [Label l; Sne(R4, R4, R0); Sne(R3, R3, R0); Mips.And(R3, R3, Reg(R4))]

    | Or(e1, e2) -> 
    let l = new_label() in
    (* If e1 != 0 jump directly to end, otherwise push it to stack *)
    (compile_exp e1 en) @ [Bne(R3, R0, l)] @ (push R3) 
    (* Recover e1 from stack into R4, meanwhile return value of e2 is in R3 *)
    @ (compile_exp e2 en) @ (pop R4)
    (* Store if R4 is nonzero in R4, then store if R3 is nonzero in R3, then
      bitwise or the result *)
    @ [Label l; Sne(R4, R4, R0); Sne(R3, R3, R0); Mips.Or(R3, R3, Reg(R4))]

    | Assign(x, e) -> 
	let offset = vm_lookup vm x in
	(compile_exp e en) @ [Add(R4, R30, Immed(Word32.fromInt (-offset))); Sw(R3, R4, zero)] 

	| Call (f, arglist) ->
	(* Pushes the arguments for a function to the stack before calling it; moves stack pointer back
   	to before these arguments so that the function knows that the nargs words after the current
   	stack pointer are its arguments;
   	Technically, we should be putting the first four into registers but we make this simplification
   	which can be thought of
   	as just having the caller save them onto the stack for the callee instead of letting the
   	callee save them, as it would probably do anyways. *)
	(* Compile each argument expression individually and push value onto stack *)
	let ilistlist = List.map (fun e -> (compile_exp e en) @ (push R3)) arglist in
	(* Concatenate expression code all together *)
	let ilist = List.fold_right (fun a b -> a @ b) ilistlist [] in
	(* Shift sp to before arguments *)
	ilist @ [Add(R29, R29, Immed(Word32.fromInt (4 * (List.length arglist)))); 
	(* After return store value of R2 (f's return value) into R3 *)
	Jal("FUNC" ^ f); Add(R3, R2, Immed(zero))]

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
		(* Create a new sub-environment en2 with extra variable to execute s in *)
		((compile_exp e en) @ (push R3) @
		(let vm2 = vm_insert en.varmap v in 
		let en2 = {varmap = vm2; epilogue = en.epilogue} in 
		fst (compile_stmt s en2)) @ (pop R3), en) (* Store the value computed for v into R3 *)
    (* Store the result of R3 into R2 for return, then jump to epilogue;
	   We store it in R16 as well solely for debugging purposes *)
    | Return(e) -> (((compile_exp e en) @ [Mips.Add(R2, R3, Reg(R0)); 
			Mips.Add(R16, R3, Reg(R0)); J(en.epilogue)]), en) 

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

	If n < 4 then there are at least 4 anyways. In the MIPS calling convention, the first 
	four arg spaces are left empty and it's up to the callee to store them there, 
	but for simplicity we have the caller do that for them.
*)

(* make_env(): create environment for a function, just adds the arguments to the varmap
   and creates an epilogue label *)
let make_env (fsig : Ast.funcsig) : env =
	let rec add_args alist =
		match alist with
		  [] -> empty_varmap
		| h::t -> 
			let vm = add_args t in
			vm_insert vm h in
	let ep = new_label() in
	{varmap = add_args fsig.args; epilogue = ep}

(* Prologue *)
let frame_start (fsig : Ast.funcsig) (e : env) : (Mips.inst list * env) =
	let nargs = List.length fsig.args in
	let vm = e.varmap in
	let vm = vm_insert vm "CALLER_FRAME_POINTER" in
	let vm = vm_insert vm "RETURN_ADDRESS" in
	(* Save old frame pointer R30 into stack, make old stack pointer new frame pointer, shift
       stack pointer down the size of nargs (and two more by the pushes) words *)
	let ilist = 
		[Add(R3, R30, Reg R0); Add(R30, R29, Reg R0); Li(R4, Word32.fromInt(nargs * 4));
		  Sub(R29, R29, R4)] in
	(ilist @ (push R3) @ (push R31), {varmap = vm; epilogue = e.epilogue}) 

(* Epilogue *)
let frame_end (fsig : Ast.funcsig) (e : env) : Mips.inst list =
	let caller_fp = vm_lookup e.varmap "CALLER_FRAME_POINTER" in
	let ra = vm_lookup e.varmap "RETURN_ADDRESS" in
	(* Load correct return address *)
	[Add(R3, R30, Immed(Word32.fromInt (-ra))); Lw(R31, R3, zero);
	(* Move sp up to fp *)	
	Add(R29, R30, Reg R0);
	(* Move fp up to old fp and return *)
	Add(R3, R30, Immed(Word32.fromInt (-caller_fp))); Lw(R30, R3, zero); Jr R31]

(* Actually compile a function: first calls frame_start to set up stack frame, then compiles
   body of procedure, then makes epilogue for return *)
let compile_func (f : Ast.func) : Mips.inst list =
	let Fn(fsig) = f in
	let env = make_env fsig in
	let (ilist, env) = frame_start fsig env in
	let name = if fsig.name = "main" then "main" else "FUNC" ^ fsig.name in
	let prologue = (Label name) :: ilist in 
	let (body, _) = compile_stmt fsig.body env in
	let ilist2 = frame_end fsig env in
	let epilogue = (Label env.epilogue) :: ilist2 in
	prologue @ body @ epilogue
	
(************* Compile *************************************************)
(* compiles Cish AST down to MIPS instructions; Cish has no global variables *)
let compile (p : Ast.program) : result = 
	let code = List.fold_right (fun a b -> a @ b) (List.map compile_func p) [] in
	{ code = (Jal("main") :: code); data = [] }

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
