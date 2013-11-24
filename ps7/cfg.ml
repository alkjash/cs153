open Cfg_ast
open Set
module C = Cish_ast
exception Implement_Me
exception FatalError

(*******************************************************************)
(* PS7 TODO:  interference graph construction *)

(* We use sets instead of lists to avoid duplication and get easy union/intersection *)
module VS = Set.Make(String)

(* an interference graph maps a variable x to the set of variables that
 * y such that x and y are live at the same point in time.  It's up to
 * you how you want to represent the graph.  
 * Implemented as a function which maps x to the list of variables 
 * live at some same point with it  *)
type interfere_graph = var -> VS.t
let extend_ig ig x y =
	if VS.mem y (ig x) then ig
	else (fun z -> if z = x then VS.add y (ig x) else ig z)
let empty_ig x = VS.empty

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

(* Representation of pred and succ relations between blocks *)
type block_graph = block -> block list 
let extend_bg g b1 b2 =
	if List.mem b2 (g b1) then g 
	else (fun b -> if b = b1 then b2 :: (g b) else g b)
let empty_bg b = []

(* Helper function that adds in all the out-edges of a block to the pred graph *)
let calc_out (pred : block_graph) (b : block) : block_graph = 
	let rec tail b =
		match b with
		  [] -> raise FatalError
		| [i] -> i
		| _ :: t -> tail t in
	match tail b with
	  Jump l -> extend_bg pred (ltb l) b
	| If (_, _, _, l1, l2) -> extend_bg (extend_bg pred (ltb l1) b) (ltb l2) b
	| Return _ -> pred
	| _ -> raise FatalError

(* For each block, calculate all its predecessor blocks *)
let calc_block_graph (f : func) : block_graph =
	List.fold_left calc_out empty_bg f

(* general type for storing list of vars associated with instructions - takes as input
 * block and inst index within that block and outputs a list of vars *)
type env = block -> int -> VS.t
let extend_env e b i x =
	let l = e b i in
	if VS.mem x l then e
	else (fun b' i' -> if (b' = b && i' = i) then VS.add x (e b i) else e b' i') 
let update_env e b i s =
	fun b' i' -> if (b' = b && i' = i) then s else e b' i'
let empty_env b i = VS.empty

let get_vars_gen (ins : inst) : VS.t =
  match ins with
  | Move (_, Var x) | Load (_, Var x, _) | Call (Var x) ->
	VS.singleton x
  | Arith (_, x, _, y) | Store (x, _, y) | If (x, _, y, _, _) ->
	(match x, y with
	  (Var a, Var b) -> VS.add b (VS.singleton a)
	| (_, Var b) -> VS.singleton b
	| (Var a, _) -> VS.singleton a
	| _ -> VS.empty)
  | _ -> VS.empty

let get_vars_kill (ins : inst) : VS.t =
  match ins with
  | Move (Var x, _) -> VS.singleton x
  | Arith (Var x,_,_,_)-> VS.singleton x
  | Load (Var x, _, _) -> VS.singleton x
  | _ -> VS.empty

let rec calc_vars_b insts gen kill b i : (env * env) =
  match insts with
  | h::t -> calc_vars_b t (update_env gen b i (get_vars_gen h)) (update_env kill b i (get_vars_kill h)) b (i+1)
  | _ -> (gen,kill)

(* Do one iteration of propagating Live-out sets of each instruction backwards;
   if at beginning of a block, propagate to all predecessors *)
let calc_live (livein : env) (liveout : env) (gen : env) (kill : env) (pred : block_graph) 
				: (env * env) =
	(* Reverse the instructions in each block so we can just propagate liveness 
	   forwards instead of back *)
	let f = (List.map List.rev !fnc) in
	(* Within each block, propagate livein through the reversed list *)
	let rec cl_block (li, lo) b i =
		match b with
		  [] -> (li, lo)
		| [h] -> (* Propagate to all pred blocks *)
			raise Implement_Me
		| h1::h2::t ->
			let out = li b i in
			if out = lo b (i-1) then (li, lo)
			else 
				let lo = update_env lo b (i-1) out in
				let li = raise Implement_Me in (li, lo) in
	List.fold_left (fun (li, lo) b -> cl_block (li, lo) b (List.length b)) (livein, liveout) f

(* given a function (i.e., list of basic blocks), construct the
 * interference graph for that function.  This will require that
 * you build a dataflow analysis for calculating what set of variables
 * are live-in and live-out for each program point. *)
let build_interfere_graph (f : func) : interfere_graph =
	(* Update fnc to f; used globally throughout rest of calculation *)
	let _ = (fnc := f) in
	(* Construct graph of pred's and succ's between blocks *)
	let pred = calc_block_graph f in
	(* Calculate Gen's and kills of each instruction *)
	let (gen, kill) = raise Implement_Me in
	(* Calculate Live-In and Live-Out sets of each program instruction recursively *)
	let rec liveloop li lo =
		let (newli, newlo) = calc_live li lo gen kill pred in
		if (newli, newlo) = (li, lo) then (li, lo)
		else liveloop newli newlo in
	let livein liveout = liveloop gen empty_env in
	(* Calculate Interference Graph by running through instructions and adding all
	   common live-in variables *)
	let add_interfere ig b i = 
	(* Take all pairs of vars in liveout b i and add them to ig *)
    raise Implement_Me in
	let rec add_interfere_block ig b =
	raise Implement_Me in
	List.fold_left add_interfere_block empty_ig f

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


