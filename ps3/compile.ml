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

(* varmap: immutable association list of vars and offsets *)
type varmap = (Ast.var * int) list
(* TODO:
   Define empty_varmap(), insert_var(), lookup_var() *)

type env = {varmap : varmap}

(************ Helper functions *****************************************)
(* make_env(): create environment for a function, passing through the code once:
   Association list (var * int) which keeps track of where temporary variables are
   with respect to frame pointer. int is the number of words from the frame pointer
   where the base of var starts. *)
let rec make_env (f : Ast.func) : env =
	raise IMPLEMENT_ME

(* push and pop: wrappers for pushing and popping temps off of the stack *)
(* push writes assembly code down that pushes the value of register r onto the current
   sp and updating sp *)
let push (r : Mips.reg) : Mips.inst list =
	raise IMPLEMENT_ME

(* pop writes assembly code down that pops off the top of sp into register r and decrements
   sp *)
let pop (r : Mips.reg) : Mips.inst list =
	raise IMPLEMENT_ME

(* Set up stack frame for new procedure; calls make_env to set up local variables first *)
let frame_start (f : Ast.func) : Mips.inst list =
	raise IMPLEMENT_ME

(* Actually compile a function: first calls frame_start to set up stack frame, then compiles
   body of procedure, then makes epilogue for return *)
let compile_func (f : Ast.func) : Mips.inst list =
	raise IMPLEMENT_ME

(* TODO: More subfunctions needed: frame_end, frame_save *)

(************* Compile *************************************************)
let rec compile (p : Ast.program) : result =
    raise IMPLEMENT_ME

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
