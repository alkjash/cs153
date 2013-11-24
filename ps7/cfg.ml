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
let extend_ig ig x y =
	if List.mem y (ig x) then ig
	else (fun z -> if z = x then y :: (ig x) else ig z)
let empty_ig x = []

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
	|inl Return _ -> pred
	| _ -> raise FatalError

(* For each block, calculate all its predecessor blocks *)
let calc_block_graph (f : func) : block_graph =
	List.fold_left calc_out empty_bg f

(* general type for storing list of vars associated with instructions - takes as input
 * block and inst index within that block and outputs a list of vars *)
type env = block -> int -> var list
let extend_env e b i x =
	let l = e b i in
	if List.mem x l then e
	else (fun b' i' -> if (b' = b && i' = i) then x :: (e b i) else e b' i') 
let update_env e b i l =
	fun b' i' -> if (b' = b && i' = i) then l else e b' i'
let empty_env b i = []

let get_vars_gen ins : var list =
  match ins with
  | Label l -> []
  | Move (x,y) ->
    match x,y with
    | (_,Var b) -> [b]
    | _ -> []
  | Arith (x,y,_,z)->
    match x,y,z with
    | (_,Var b,Var c) -> [b;c]
    | (_,Var b,_) -> [b]
    | (_,_,Var c) -> [c]
    | _ -> []
  | Load _ -> []
  | Store _ -> []
  | Call _ -> []
  | Jump _ -> []
  | Return -> []

let get_vars_kill ins : var list =
  match ins with
  | Label l -> []
  | Move (x,_) -> [x]
  | Arith (x,y,_,z)->
    match x,y,z with
    | (Var a,_,_) -> [a]
    | _ -> []
  | Load x -> [x]
  | Store _ -> []
  | Call x -> [x]
  | Jump _ -> []
  | Return -> []

let calc_vars_b insts gen kill b i : (env * env) =
  match insts with
  | h::t -> calc_vars_b t (extend_env gen b i (get_vars_gen h)) (extend_env kill b i (get_vars_kill h)) b i+1
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
				let li = 
			
	List.fold_left cl_block (li, lo) f


(* Initialize LiveIn[L] := Gen[L].
initialize LiveOut[L] := { }.
loop until no change {
for each L:
Out := LiveIn[L 1 ] ∪ ... ∪ LiveIn[L n ]
where succ[L] = {L 1 ,...,L n }
if Out == LiveOut[L] then continue to next block.
LiveOut[L] := Out.
LiveIn[L] := Gen[L] ∪ (LiveOut[L] - Kill[L]).
} *)	

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
		let newli, newlo = calc_live li lo gen kill in
		if (newli, newlo) = (li, lo) then li lo
		else liveloop newli newlo in
	let livein liveout = liveloop gen empty_env
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


