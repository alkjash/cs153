open Cfg_ast
open Set
module C = Cish_ast
exception Implement_Me
exception FatalError

(*******************************************************************)
(* PS7 TODO:  interference graph construction *)

(******************* New Type Definitions **************************)

(* We use sets instead of lists to avoid duplication and get easy union/intersection *)
module VS = Set.Make(String)

(* an interference graph maps a variable x to the set of variables that
 * y such that x and y are live at the same point in time.  It's up to
 * you how you want to represent the graph.  
 * Implemented as a list of edges x -- y *)
type interfere_graph = (var * var) list
let extend_ig (ig : interfere_graph) (x : var) (y : var) : interfere_graph =
	if List.mem (x, y) ig || List.mem (y, x) ig then ig
	else (x, y) :: ig
let empty_ig = []

(* Representation of pred and succ relations between blocks *)
type block_graph = block -> block list 
let extend_bg g b1 b2 =
	if List.mem b2 (g b1) then g 
	else (fun b -> if b = b1 then b2 :: (g b) else g b)
let empty_bg b = []

(* general type for storing set of vars associated with instructions - takes as input
 * block and inst index within that block and outputs a set of vars *)
type env = block -> int -> VS.t
let extend_env e b i x =
	let l = e b i in
	if VS.mem x l then e
	else (fun b' i' -> if (b' = b && i' = i) then VS.add x (e b i) else e b' i') 
let update_env e b i s =
	fun b' i' -> if (b' = b && i' = i) then s else e b' i'
let rec eq_env f e1 e2 b i =
	if not (VS.equal (e1 b i) (e2 b i)) then false
	else if i = List.length b then
		match f with
		  [] -> true
		| h::t -> eq_env t e1 e2 h 1
	else
		eq_env f e1 e2 b (i+1)
	
let empty_env b i = VS.empty

(******************** Helper functions and definitions **************)
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
	  Jump l -> extend_bg pred (ltb l) b
	| If (_, _, _, l1, l2) -> extend_bg (extend_bg pred (ltb l1) b) (ltb l2) b
	| Return -> pred
	| _ -> raise FatalError

(* For each block, calculate all its predecessor blocks *)
let calc_block_graph (f : func) : block_graph =
	List.fold_left calc_out empty_bg f

(* Calculate gen(ins) *)
let get_vars_gen (ins : inst) : VS.t =
  match ins with
  | Move (_, Var x) | Load (_, Var x, _) ->
	VS.singleton x
  | Arith (_, x, _, y) | Store (x, _, y) | If (x, _, y, _, _) ->
	(match x, y with
	  (Var a, Var b) -> VS.add b (VS.singleton a)
	| (_, Var b) -> VS.singleton b
	| (Var a, _) -> VS.singleton a
	| _ -> VS.empty)
  | _ -> VS.empty

(* Calculate kill(ins) *)
let get_vars_kill (ins : inst) : VS.t =
  match ins with
  | Move (Var x, _) -> VS.singleton x
  | Arith (Var x,_,_,_)-> VS.singleton x
  | Load (Var x, _, _) -> VS.singleton x
  | _ -> VS.empty

(* Recurse through a block and update gen and kill for this block *)
let rec calc_vars_b insts gen kill i b : (env * env) =
  match insts with
  | h::t -> calc_vars_b t (update_env gen b i (get_vars_gen h)) (update_env kill b i (get_vars_kill h)) (i+1) b
  | [] -> (gen,kill)

let rec calc_gen_kill (f : func) (e2 : (env * env)) : (env * env) =
  match f with
  | h::t -> 
    let gen,kill = e2 in
    calc_gen_kill t (calc_vars_b h gen kill 0 h)
  | [] -> e2

(* Do one iteration of propagating Live sets of each instruction backwards;
   if at beginning of a block, propagate to all predecessors *)
let calc_live (livein : env) (liveout : env) (gen : env) (kill : env) (pred : block_graph) 
				: (env * env) =
	(* Reverse the instructions in each block so we can just propagate liveness 
	   forwards instead of back *)
	let f = (List.map List.rev !fnc) in

	(* Helper to propagate current livein of a given b, i to a pred instruction b', i' *)
	let prop li lo b i b' i' =
		let newlo = VS.union (lo b' i') (li b i) in
		if VS.equal newlo (lo b' i') then (* Do nothing, no change *)
			(li, lo)
		else (* Update lo and update li in accordance to that, taking into account gen's and kill's *)
			let lo = update_env lo b' i' newlo in
			let k = kill b' i' in
			let newli = VS.union (gen b' i') (VS.filter (fun e -> not (VS.mem e k)) newlo) in
			let li = update_env li b' i' newli in
			(li, lo) in

	(* Within each block, propagate livein through the reversed list *)
	let rec prop_block (li, lo) b i =
		match i with
		  0 -> (li, lo)
		| 1 -> 
			(* From the first instruction, propagate to the last instructions of all pred blocks *)
			let l = List.map (fun b -> (b, List.length b)) (pred b) in
			List.fold_left (fun (li, lo) (b', i') -> prop li lo b i b' i') (li, lo) l
		| _ -> 
			(* Propagate to only predecessor of an instruction within a block *)
			let (li, lo) = prop li lo b i b (i-1) in
			prop_block (li, lo) b (i-1) in

	List.fold_left (fun (li, lo) b -> prop_block (li, lo) b (List.length b)) (livein, liveout) f

(*********************** Main Wrapper for all computations **********************)

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
	let (gen, kill) = calc_gen_kill f (empty_env, empty_env) in
  
	(* Calculate Live-In and Live-Out sets of each program instruction recursively *)
	let rec liveloop li lo =
		let (newli, newlo) = calc_live li lo gen kill pred in
		if (eq_env f newli li (List.hd f) 1) && (eq_env f newlo lo (List.hd f) 1) then (li, lo)
		else liveloop newli newlo in
	let (livein, liveout) = liveloop gen empty_env in

	(* Calculate Interference Graph by running through instructions and adding all
	   common live-out variables *)
	(* Take all pairs of vars in liveout b i and add them to ig *)
	let add_interfere ig b i = 
		let _ = print_int (List.length (VS.elements (gen b i))) in
		let vs = liveout b i in
		let _ = print_int (List.length (VS.elements vs)) in
		(* Fold over all pairs of distinct x, y in vs: insert (x -- y) into ig *)	
		VS.fold (fun x ig -> (VS.fold (fun y ig -> if x <> y then extend_ig ig x y else ig) vs ig)) vs ig in
	(* Iterate over a block *)
	let add_interfere_block ig b =
		let rec h ig b i =
			if i > 0 then
				h (add_interfere ig b i) b (i-1)
			else ig in
		h ig b (List.length b) in
	(* Iterate over the whole function *)
	List.fold_left add_interfere_block empty_ig f

(* given an interference graph, generate a string representing it *)
let str_of_interfere_graph (g : interfere_graph) : string =
	let ret = "graph interfere_graph {" in
	let add_edge str x y = str ^ "\n\t" ^ x ^ " -- " ^ y ^ ";\n" in
	(List.fold_left (fun str (x, y) -> add_edge str x y) ret g) ^ "}\n"

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

(*
let _ =
  let prog = parse_file() in
  List.fold_left print_interference_graph () prog
*)

