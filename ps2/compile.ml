(* Compile Fish AST to MIPS AST *)
open Mips
open Ast

exception IMPLEMENT_ME

type result = { code : Mips.inst list;
                data : Mips.label list }

(* generate fresh labels *)
let label_counter = ref 0
let new_int() = (label_counter := (!label_counter) + 1; !label_counter)
let new_label() = "L" ^ (string_of_int (new_int()))

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

(* reset internal state *)
let reset() = (label_counter := 0; variables := VarSet.empty)

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

(* The only guarantee we make of an expression is that R2 is stored with its value after it is 
   finished evaluating. It may or may not clobber all the other registers in the process *)
let rec compile_exp ((e , _) : Ast.exp) : inst list =
    match e with
      Int j -> [Li(R2, Word32.fromInt j)]

    | Var x -> [La(R2, x); Lw(R2, R2, zero)]

    | Binop(e1, op, e2) -> 
	(let t = new_temp() in 
	(* Store return value of e1 into t *)
	(compile_exp e1) @ [La(R3, t); Sw(R2, R3, zero)] 
	(* Recover e1 from t into R3, meanwhile return value of e2 is in R2 *)
	@ (compile_exp e2) @ [La(R3, t); Lw(R3, R3, zero)]
	@ (match op with
		  Plus -> [Add(R2, R3, Reg R2)]
		| Minus -> [Sub(R2, R3, R2)]
		| Times -> [Mul(R2, R3, R2)]
		| Div -> [Mips.Div(R2, R3, R2)]
		| Eq -> [Mips.Seq(R2, R3, R2)]
		| Neq -> [Sne(R2, R3, R2)]
		| Lt -> [Slt(R2, R3, R2)]
		| Lte -> [Sle(R2, R3, R2)]
		| Gt -> [Sgt(R2, R3, R2)]
		| Gte -> [Sge(R2, R3, R2)]
	))

    | Not(e) -> (compile_exp e) @ [Mips.Seq(R2, R2, R0)] (* Set R2 to 1 if zero, zero otherwise *)

    | And(e1, e2) -> 
	(let t = new_temp() in 
	let l = new_label() in
	(* If e1 = 0 jump directly to end, otherwise store return value of e1 into t *)
	(compile_exp e1) @ [Beq(R2, R0, l); La(R3, t); Sw(R2, R3, zero)] 
	(* Recover e1 from t into R3, meanwhile return value of e2 is in R2 *)
	@ (compile_exp e2) @ [La(R3, t); Lw(R3, R3, zero)]
	(* Store if R3 is nonzero in R3, then store if R2 is nonzero in R2, then
		bitwise and the result *)
	@ [Label l; Sne(R3, R3, R0); Sne(R2, R2, R0); Mips.And(R2, R2, Reg(R3))])

    | Or(e1, e2) -> 
	(let t = new_temp() in 
	let l = new_label() in
	(* If e1 != 0 jump directly to end, otherwise store return value of e1 into t *)
	(compile_exp e1) @ [Bne(R2, R0, l); La(R3, t); Sw(R2, R3, zero)] 
	(* Recover e1 from t into R3, meanwhile return value of e2 is in R2 *)
	@ (compile_exp e2) @ [La(R3, t); Lw(R3, R3, zero)]
	(* Store if R3 is nonzero in R3, then store if R2 is nonzero in R2, then
		bitwise or the result *)
	@ [Label l; Sne(R3, R3, R0); Sne(R2, R2, R0); Mips.Or(R2, R2, Reg(R3))])

    | Assign(x, e) -> (compile_exp e) @ [La(R3, x); Sw(R2, R3, zero)] 

let rec compile_stmt ((s,_):Ast.stmt) : inst list = 
    match s with
    | Exp(e) -> compile_exp e
    | Seq(s1,s2) -> (compile_stmt s1) @ (compile_stmt s2)
    | If(e,s1,s2) ->
        (let else_1 = new_label() in
        let end_l = new_label() in
        (compile_exp e) @ [Beq(R2, R0, else_1)] @
        (compile_stmt s1) @ [J end_l; Label else_1] @
        (compile_stmt s2) @ [Label(end_l)])
    | While(e,s) ->
        (let test_l = new_label() in
        let top_l = new_label() in
        [J test_l; Label top_l] @
        (compile_stmt s) @
        [Label test_l] @
        (compile_exp e) @
        [Bne(R2,R0,top_l)])
    | For(e1,e2,e3,s) ->
        compile_stmt((Seq((Exp e1, 0),(While(e2,(Seq(s,(Exp e3,0)),0)), 0)), 0)) 
    | Return(e) -> (compile_exp e) @ [Jr(R31)]

(* compiles Fish AST down to MIPS instructions and a list of global vars *)
let compile (p : Ast.program) : result = 
    let _ = reset() in
    let _ = collect_vars(p) in
    let insts = (Label "main") :: (compile_stmt p) in
    { code = insts; data = VarSet.elements (!variables) }

(* converts the output of the compiler to a big string which can be 
 * dumped into a file, assembled, and run within the SPIM simulator
 * (hopefully). *)
let result2string ({code;data}:result) : string = 
    let strs = List.map (fun x -> (Mips.inst2string x) ^ "\n") code in
    let var2decl x = x ^ ":\t.word 0\n" in
    "\t.text\n" ^
    "\t.align\t2\n" ^
    "\t.globl main\n" ^
    (String.concat "" strs) ^
    "\n\n" ^
    "\t.data\n" ^
    "\t.align 0\n"^
    (String.concat "" (List.map var2decl data)) ^
    "\n"

