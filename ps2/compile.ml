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

(* Compile statement example code 
let rec stmt2mips(s:stmt):inst list =
match s with
| Exp e ->
exp2mips e
| Seq(s1,s2) ->
(stmt2mips s1) @ (stmt2mips s2)
| If(e,s1,s2) ->
(let else_l = new_label() in
let end_l = new_label() in
(exp2mips e) @ [Beq(R2,R0,else_l)] @
(stmt2mips s1) @ [J end_l,Label else_l] @
(stmt2mips s2) @ [Label end_l])
| While(e,s) ->
(let test_l = new_label() in
let top_l = new_label() in
[J test_l, Label top_l] @
(stmt2mips s) @
[Label test_l] @
(exp2mips e) @
[Bne(R2,R0,top_l)])
| For(e1,e2,e3,s) ->
stmt2mips(Seq(Exp e1,While(e2,Seq(s,Exp e3)))) *)

let rec compile_stmt ((s,_):Ast.stmt) : inst list = 
    match s with
    | Exp(e) -> compile_exp e
    | Seq(s1,s2) -> raise IMPLEMENT_ME
    | If(e,s1,s2) -> 
    | While(e,s) ->
    | For(e1,e2,e3,s) -> 
    | Return(e) ->

(* Compile Expression example code 
 let rec exp2mips(e:exp):inst list =
match e with
| Int j -> [Li(R2, Word32.fromInt j)]
| Var x -> [La(R2,x), Lw(R2,R2,zero)]
| Binop(e1,b,e2) ->
(let t = new_temp()in
(exp2mips e1) @ [La(R1,t), Sw(R2,R1,zero)]
@(exp2mips e2) @ [La(R1,t), Lw(R1,R1,zero)]
@(match b with
Plus -> [Add(R2,R2,Reg R1)]
| ... -> ...))
| Assign(x,e) => [exp2mips e] @
[La(R1,x), Sw(R2,R1,zero)] *)

let rec compile_exp ((e,_):Ast.exp) : inst list =
    match e with
    | Int j -> raise IMPLEMENT_ME
    | Var x -> raise IMPLEMENT_ME
    | Binop(e1, _, e2) ->
    | Not(e) -> 
    | And(e1, e2) -> 
    | Or(e1, e2) -> 
    | Assign(x, e) ->  
    
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

