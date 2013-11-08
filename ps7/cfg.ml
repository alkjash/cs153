open Cfg_ast
module C = Cish_ast
exception Implement_Me
exception FatalError

(*******************************************************************)
(* PS7 TODO:  interference graph construction *)

(* an interference graph maps a variable x to the set of variables that
 * y such that x and y are live at the same point in time.  It's up to
 * you how you want to represent the graph.  
 * Implemented as a function which maps x to the list of variables 
 * live at some same point with it  *)
type interfere_graph = var -> var list

(* Representation of pred and succ relations between blocks *)
type block_graph = label -> label list 
let extend_bg g b l =
	if List.mem l (g b) then g 
	else (fun b' -> if b' = b then l :: (g b') else g b')
let empty_bg b = []

(* Make whole fnc global for easy lookup *)
let fnc = ref []

(* Match labels with blocks and vice-versa *)
(* label-to-block *)
let ltb (l : label) : block =
	let f = (!fnc) in
	match List.filter (fun b -> List.hd b = Label l) f with
	  [b] -> b
	| _ -> raise FatalError

(* block-to-label *)
let btl (b : block) : label =
	match List.hd b with
	  Label l -> l
	| _ -> raise FatalError

(* Helper function that adds in all the out-edges of a block to the pred graph *)
let calc_out (pred : block_graph) (b : block) : block_graph = 
	let rec tail b =
		match b with
		  [] -> raise FatalError
		| [i] -> i
		| _ :: t -> tail t in
	match tail b with
	  Jump l -> 
	| If (_, _, _, l1, l2) ->
	| Return _ -> pred
	| _ -> raise FatalError

(* For each block, calculate all its predecessor and successor blocks *)
let calc_block_graph (f : func) : (block_graph * block_graph) =
	let pred = empty_bg in
	List.foldr (
	let succ = empty_bg in
	raise Implement_Me

(* general type for storing LiveIn and LiveOut - takes as input
 * block and inst index within that block and outputs a list of vars *)
type env = block -> int -> var list
let extend_env e b i x =
	(b,i,x) :: e
let empty_env b i = []

(* given a function (i.e., list of basic blocks), construct the
 * interference graph for that function.  This will require that
 * you build a dataflow analysis for calculating what set of variables
 * are live-in and live-out for each program point. *)
let build_interfere_graph (f : func) : interfere_graph =
	(* Update fnc to f; used globally throughout rest of calculation *)
	let _ = (fnc := f) in
	(* Construct graph of pred's and succ's between blocks *)
	let (pred, succ) = calc_block_graph f in
	(* Calculate Live-In and Live-Out sets of each program instruction recursively *)
	(* Calculate Interference Graph by running through instructions and adding all
	   common live-in variables *)
    raise Implement_Me

(* given an interference graph, generate a string representing it *)
let str_of_interfere_graph (g : interfere_graph) : string =
    raise Implement_Me


(*******************************************************************)
(* PS8 TODO:  graph-coloring, coalescing register assignment *)
(* You will need to build a mapping from variables to MIPS registers
   using the ideas behind the graph-coloring register allocation
   heuristics described in class.  This may involve spilling some
   of the variables into memory, so be sure to adjust the prelude
   of the function so that you allocate enough space on the stack
   to store any spilled variables.  The output should be a CFG
   function that doesn't use any variables (except for function
   names.)
*)
let reg_alloc (f : func) : func = 
    raise Implement_Me

(* Finally, translate the ouptut of reg_alloc to Mips instructions *)
let cfg_to_mips (f : func ) : Mips.inst list = 
    raise Implement_Me



(*******************************************************************)
(* Command-Line Interface for printing CFG. You probably will not 
    need to modify this for PS7, but will definitely need to for 
    PS8. Feel free to add additional command-line options as you
    see fit (e.g. -printmips, -evalmips, -printcfg, etc...). 
    Please make sure to document any changes you make.
*)
let parse_file() =
  let argv = Sys.argv in
  let _ = 
    if Array.length argv != 2
    then (prerr_string ("usage: " ^ argv.(0) ^ " [file-to-parse]\n");
    exit 1) in
  let ch = open_in argv.(1) in
  Cish_parse.program Cish_lex.lexer (Lexing.from_channel ch)

let parse_stdin() = 
  Cish_parse.program Cish_lex.lexer (Lexing.from_channel stdin)

let print_interference_graph (():unit) (f : C.func) : unit =
  let graph = build_interfere_graph (fn2blocks f) in
  Printf.printf "%s\n%s\n\n" (C.fn2string f) (str_of_interfere_graph graph)

let _ =
  let prog = parse_file() in
  List.fold_left print_interference_graph () prog


