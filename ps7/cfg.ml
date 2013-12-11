open Cfg_ast
open Set
module C = Cish_ast
module M = Mips
exception Implement_Me
exception FatalError

(*******************************************************************)
(* PS7 TODO:  interference graph construction *)

(******************* NOTES *****************************************)
(* Our interference graph is implemented as a list of undirected edges;
   however we plan to probably change to adjacency lists for the next
   problem set; the switch is not difficult *)

(* We do not implement interference between variables and registers -
   It may be more efficient to spill out callee-saved registers and
   use them for register allocation, but we don't do this now; we only
   use registers that are definitely available for clobbering *)

(* Registers 2 3 and 4 are reserved to simplify matters; we reserve R2 for
   return values and 3 and 4 for storing immediates for operations where
   immediates are allowed in cfg but not in mips 
   Register fp (R30) is reserved for spilling (this will probably be used frequently
   in any case), and R3 and R4 are additionally used as temps for spilling; this allows
   us to forego starting from scratch every time we spill a variable *)

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
	let f = !fnc in
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

	(* Within each block, propagate livein through the list backwards *)
	let rec prop_block (li, lo) b i =
		match i with
		  0 -> raise FatalError
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
		(* If li and lo haven't changed, stop looping *)
		if (eq_env f newli li (List.hd f) 1) && (eq_env f newlo lo (List.hd f) 1) then (li, lo)
		else liveloop newli newlo in
	let (livein, liveout) = liveloop gen empty_env in

	(* Calculate Interference Graph by running through instructions and adding all
	   common live-out variables *)
	(* Take all pairs of vars in liveout b i and add them to ig *)
	let add_interfere ig b i = 
		let vs = liveout b i in
		(* Fold over all pairs of distinct x, y in vs: insert (x -- y) into ig *)	
		let ig = VS.fold (fun x ig -> 
			(VS.fold (fun y ig -> if x <> y then extend_ig ig x y else ig) vs ig)) vs ig in
		(* If the current instruction defines a (possibly dead) variable, 
		   make it interfere with all live-out variables different from itself;
		   this takes care of 0-length live ranges *)
		match VS.elements (kill b i) with
		  [x] -> VS.fold (fun y ig -> if x <> y then extend_ig ig x y else ig) vs ig
		| _ -> ig in
		
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
	let add_edge str x y = str ^ "\n\t" ^ x ^ " -- " ^ y ^ ";" in
	(List.fold_left (fun str (x, y) -> add_edge str x y) ret g) ^ "\n}\n"

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

(*********************** Graph Data structures **************************)
(* Adjacency-list form of interference graph: more useful for graph-coloring *)
type iga = (var * (var list)) list
let extend_iga (g : iga) (x : var) (y : var) : iga =
	match List.filter (fun p -> fst p = x) g with
	  [(x, l)] -> 
		if List.mem y l then g
		(* clumsy *)
		else List.map (fun p -> if (fst p = x) then (fst p, y :: (snd p)) else p) g
	| [] -> (x, [y]) :: g
	| _ -> raise FatalError

let lookup_iga (g : iga) (x : var) : var list =
	match List.filter (fun p -> fst p = x) g with
	  [(x, l)] -> l
	| [] -> []
	| _ -> raise FatalError
let empty_iga = []

(* Map from variables to registers created by graph coloring *)
type regmap = var -> (M.reg option)
let extend_rm rm x r =
	fun y -> if y = x then r else rm y
let empty_rm x = None

(* Convert interference_graph to iga *)
let ig_to_iga (ig : interfere_graph) : iga =
	let add_edge (g : iga) (e : var * var) =
		let (x, y) = e in
		extend_iga (extend_iga g x y) y x in
	List.fold_left add_edge empty_iga ig

(* Adj-list of each block *)
let rec block_move_graph (b : block) (g : iga) (ig : interfere_graph) : interfere_graph =
	match b with
	  h::t ->
		(match h with
		  Move (Var x, Var y) -> 
			if List.mem y (lookup_iga g x) then
				block_move_graph t g ig
			else 
				block_move_graph t g (extend_ig ig x y)
		| _ -> block_move_graph t g ig)
	| [] -> ig

(* Compute moves: for coalescing *)
let rec build_move_graph (f : func) (g : iga) (ig : interfere_graph) : interfere_graph =
	match f with
	| h::t -> (block_move_graph h g empty_ig) @ (build_move_graph t g ig)
	| _ -> ig

(**************************** Data structures for register allocation ***********)

(* A global environment for specific markings on nodes:
 * Each node x is marked None, Some x, or Some y, if it is not move-related, move-related,
 * and move-related and coalesced to y *)
(* type mark = (var * var option) list *)
let cur_mark = ref []
(* Function to initialize a variable into mark *)
let mark_init (x : var) : unit =
	match List.filter (fun y -> fst y = x) (!cur_mark) with
	  [] -> (cur_mark := (x, None) :: (!cur_mark))
	| _ -> ()
(* Mark as move-related *)
let mark_move (x : var) : unit =
	cur_mark := List.map (fun (a1, a2) -> if a1 = x then (a1, Some x) else 
		(a1, a2)) (!cur_mark)
(* Mark x as coalesced with y *)
let mark_coalesce (x : var) (y : var) : unit =
	cur_mark := List.map (fun (a1, a2) -> if a1 = x then (a1, Some y) else 
		(a1, a2)) (!cur_mark)
(* Unmark all move operations that weren't already coalesced *)
let mark_unmove_all() =
	cur_mark := List.map (fun (a1, a2) -> if a2 = Some a1 then (a1, None)
		else (a1, a2)) (!cur_mark)
(* Lookup a variable *)
let mark_lookup (x : var) : var option =
	match List.filter (fun y -> fst y = x) (!cur_mark) with
	  [(x, p)] -> p
	| _ -> raise FatalError
	
(* Keep a global stack represented as a list of variables; add to it whenever simplify succeeds *)
let cur_stack = ref []

(************************* Coalescing register allocation ************)
(* Number of registers allowed = one more than highest degree that can be simplified *)
let n_colors = 24

(* Color-to-register map *)
let ctr (i : int) : M.reg =
	match i with
	  1 -> M.R5   | 2 -> M.R6   | 3 -> M.R7   | 4 -> M.R8   | 5 -> M.R9
	| 6 -> M.R10  | 7 -> M.R11  | 8 -> M.R12  | 9 -> M.R13  | 10 -> M.R14
	| 11 -> M.R15 | 12 -> M.R16 | 13 -> M.R17 | 14 -> M.R18 | 15 -> M.R19
	| 16 -> M.R20 | 17 -> M.R21 | 18 -> M.R22 | 19 -> M.R23 | 20 -> M.R24
	| 21 -> M.R25 | 22 -> M.R28 | 23 -> M.R29 | 24 -> M.R31
	| _ -> M.R0

(* Available colors *)
let colors = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20; 21; 22;
	23; 24]

(* Remove a node *)
let simplify (g : iga) (node : var) : iga =
	List.map (fun a -> let (va,vla) = a in (va, List.filter (fun n -> n <> node) vla))
		(List.filter (fun x -> fst x <> node) g)

(* One iteration of simplifying low-degree nodes as much as possible *)
let rec simplify_loop (g : iga) : iga =
	(* Compute the set of nodes *)
	let vl = List.map fst g in
	(* Filter out move-related nodes *)
	let vl' = List.filter (fun x -> mark_lookup x = None) vl in
	
	(* No more -> done *)
	if vl' = [] then g else

	(* Find smallest degree node that's not move-related *)
	let node = (List.fold_left (fun a b ->
		let vla = lookup_iga g a in
		let vlb = lookup_iga g b in
		if List.length(vlb) < List.length(vla) then b else a) (List.hd vl') (List.tl vl')) in
	
	(* If low-degree, then simplify and loop, else end loop *)
	if List.length (lookup_iga g node) < n_colors then
		let _ = (cur_stack := node :: (!cur_stack)) in
		simplify_loop (simplify g node)
	else
		g

(* Coalesce one move v1 := v2; we use George's strategy: coalesce v1 into v2 if 
 * every high-degree neighbor of v1 in the interference graph is already a neighbor of
 * v2. *)
let coalesce (g : iga) (e : var * var) : iga =
	let (v1, v2) = e in
	let (vl1, vl2) = (lookup_iga g v1, lookup_iga g v2) in

	(* If one of the nodes is not in g (due to already being coalesced) fail *)
	if vl1 = [] || vl2 = [] then
		g
	else

	(* Filter out low-degree neighbors of v1 *)
	let vl1' = List.filter (fun v -> List.length (lookup_iga g v) < n_colors) vl1 in

	(* If vl1' is a subset of vl2, then go ahead and coalesce; otherwise fail *)
	if List.map (fun v -> not (List.mem v vl2)) vl1' = [] then
		let _ = mark_coalesce v1 v2 in
		(* Remove v1 from g *)
		let g = simplify g v1 in
		(* Insert the neighbors of v1 as neighbors of the new v2, the coalesced version *)
		List.fold_left (fun g v -> extend_iga (extend_iga g v2 v) v v2) g vl1
	else
		g
	
(* Coalesce moves to expose more possibilities for simplification *)
let coalesce_loop (g : iga) (mg : interfere_graph) : (iga * interfere_graph) =
	let g = List.fold_left coalesce g mg in
	(* After coalescing, recompute mg *)
	let mg = raise Implement_Me in
	(g, mg)

(* Find a low-degree node that is move-related and freeze all move-related edges coming off it *)
let freeze (g : iga) (mg : interfere_graph) : (iga * interfere_graph) =
	let vl = List.map fst g in
	let check_node v =
		(List.length (lookup_iga g v) < n_colors) && (mark_lookup v = Some v) in

	match List.filter check_node vl with
	  [] -> (g, mg) (* Failed to find a good node *)
	| node :: _ -> (* Freeze this node *)
		(* Unmark everything, remove the edges related to node from mg, then remark the remainder *)
		let _ = mark_unmove_all() in
		let mg = List.filter (fun (v1, v2) -> v1 <> node && v2 <> node) mg in
		let _ = List.map (fun (x, y) -> (mark_move x; mark_move y)) mg in
		(g, mg)
	
(* Find the highest-degree node to remove and mark as potential spill *)
let spill (g : iga) (mg : interfere_graph) : (iga * interfere_graph) =
	(* Compute the set of nodes *)
	let vl = List.map fst g in
	
	(* No more -> done *)
	if vl = [] then (g, mg) else

	(* Find largest degree node *)
	let node = (List.fold_left (fun a b ->
		let vla = lookup_iga g a in
		let vlb = lookup_iga g b in
		if List.length(vlb) > List.length(vla) then b else a) (List.hd vl) (List.tl vl)) in
	
	(* Push it onto stack and remove it from the graph *)
	let _ = (cur_stack := node :: (!cur_stack)) in
	let _ = mark_init node in
	let g = simplify g node in
	let mg = List.filter (fun (x, y) -> x <> node && y <> node) mg in
	(g, mg)

(* Select: pop variables off stack and color, based on the original interference graph *)
let select (g : iga) : var option * regmap =
	(* Coloring will be a function taking variables to the integers 1-n_colors *)
	let coloring = fun x -> 0 in
	let rec color stack coloring =
		match stack with
		  h::t -> 
			let vl = lookup_iga g h in
			let badcolors = List.map coloring vl in
			let goodcolors = List.filter (fun c -> List.mem c badcolors) colors in
			if goodcolors = [] then (Some h, coloring) 
			else let c = List.hd goodcolors in
				color t (fun x -> if x = h then c else coloring x)
		| [] -> (None, coloring) in
	let (v, coloring) = color (!cur_stack) coloring in
	(* map through color-to-register translation *)
	(v, fun i -> Some (ctr (coloring i)))

(* Global variable: number of spills; this will be compiled into how much the stack pointer
 * needs to be shifted down *)
let n_spill = ref 0

let t1 = Reg(M.R3)
let t2 = Reg(M.R4)

(* Apply a spill on x to a single instruction *)
let apply_spill_inst (i : inst) (x : var) (offset : int) : inst list =
	match i with
		(* When defining x, instead load into temp (reserved R3 or R4) and store to stack *)
	  Move (Var x, o) ->
		[Move (t1, o); Store(fp, offset, t1)]
	| Arith (Var x, o1, aop, o2) ->
		[Arith (t1, o1, aop, o2); Store(fp, offset, t1)]
	| Load (Var x, o, j) ->
		[Load (t1, o, j); Store(fp, offset, t1)]
	| Move (o, Var x) ->
		[Load(t1, fp, offset); Move(o, t1)]
	| Arith (o1, Var x, aop, o2) ->
		[Load(t1, fp, offset); Arith (o1, t1, aop, o2)]
	| Arith (o1, o2, aop, Var x) ->
		[Load(t2, fp, offset); Arith (o1, o2, aop, t2)]
	| Load (o, Var x, j) ->
		[Load(t1, fp, offset); Load (o, t1, j)]
	| Store (Var x, j, o) ->
		[Load(t1, fp, offset); Store (t1, j, o)]
	| Store (o, j, Var x) ->
		[Load(t2, fp, offset); Store (o, j, t2)]
	| If (Var x, cop, o, l1, l2) ->
		[Load(t1, fp, offset); If (t1, cop, o, l1, l2)]
	| If (o, cop, Var x, l1, l2) ->
		[Load(t2, fp, offset); If (o, cop, t2, l1, l2)]
	| _ -> [i]

(* Apply a spill on variable x to f *)
let apply_spill (f : func) (x : var) : func =
	let _ = (n_spill := (!n_spill) + 1) in
	let offset = 4 * !n_spill in
	(* Apply spill to everything three times, since the same variable can show up up to 
	three times in the same cfg instruction *)
	let f = List.map (fun b -> List.fold_left (fun l i -> (apply_spill_inst i x offset) @ l) [] b) f in
	let f = List.map (fun b -> List.fold_left (fun l i -> (apply_spill_inst i x offset) @ l) [] b) f in
	List.map (fun b -> List.fold_left (fun l i -> (apply_spill_inst i x offset) @ l) [] b) f

(* Apply a regmap to a single instruction *)
let apply_rm_inst (i : inst) (rm : regmap) =
	let orm o =
		match o with
		  Var v -> 
			(match rm v with
			  Some w -> Reg w
			| None -> raise FatalError)
		| _ -> o in

	match i with
	  Move (o1, o2) ->
		Move (orm o1, orm o2)
	| Arith (o1, o2, aop, o3) ->
		Arith (orm o1, orm o2, aop, orm o3)
	| Load (o1, o2, j) ->
		Load (orm o1, orm o2, j)
	| Store (o1, j, o2) ->
		Store (orm o1, j, orm o2)
	| If (o1, cop, o2, l1, l2) ->
		If (orm o1, cop, orm o2, l1, l2)
	| _ -> i

(* Replace all variables in f by rm of it *)
let apply_rm (f : func) (rm : regmap) =
	List.map (fun b -> List.map (fun i -> apply_rm_inst i rm) b) f

(* Insert prologue and epilogue instructions to extend stack for spilled variables *)
let add_stack (f : func) =
	let total = (!n_spill) * 4 in
	let prologue = List.hd f in
	let h :: t = prologue in
	let new_prologue =
		h :: (Arith (sp, sp, Sub, Int total) :: t) in
	let epilogue =
		[Label 
	

(* Build an interference graph for f, color it, and then convert all variables to the registers
   they are attached to *)
let reg_alloc (f : func) : func = 
	(* Initialize everything for the algorithm, since will be running for multiple functions *)
	let _ = (n_spill := 0) in
	let _ = (cur_stack := []) in
	let _ = (cur_mark := []) in
	(* Build *)
	let ig = build_interfere_graph f in
	let orig = ig_to_iga ig in
	let g = orig in
	let mg = build_move_graph f orig empty_ig in

	(* Compute list of variables that appear in graph: note, we assume all variables interfere
	   with at least one other, which is always true in this implementation of fn2blocks *)
	let vl = List.map fst g in
	(* Initialize mark by marking all vl as no-spill *)
	let _ = List.map (fun x -> mark_init x) vl in
	(* Then mark all move-related nodes *)
	let _ = List.map (fun (x, y) -> (mark_move x; mark_move y)) mg in

	(* Iteration of simplify/coalesce *)
	let rec loop3 g mg =
		(* Simplify - try to push as many non-move-related low-degree nodes onto stack as possible *)
		let g = simplify_loop g in
		(* Coalesce - coalescing nodes via George's conservative strategy *)
		let (h, mh) = coalesce_loop g mg in
		(* If coalesce failed then end this layer and move on to freeze *)
		if h = g then (g, mg) 
		else loop3 h mh in

	(* Iteration of simplify/coalesce/freeze *)
	let rec loop2 g mg =
		let (g, mg) = loop3 g mg in
		(* Freeze - freeze one move-related edge *)
		let (h, mh) = freeze g mg in
		(* If no freeze occurred, stop *)
		if (h, mh) = (g, mg) then
			(g, mg)
		else
			loop2 h mh in

	(* Run simplify/coalesce/freeze, then spill, then repeat *)
	let rec loop g mg =
		(* Check if done *)
		if g = [] then (g, mg) else 
		let (g, mg) = loop2 g mg in
		(* Spill - pick the highest-degree vertex to mark as potential spill *)
		let (g, mg) = spill g mg in
		loop g mg in

	(* Run the whole loop *)
	let _ = loop g mg in

	(* Repeatedly spill and remove nodes until coloring is possible; then finish *)
	(* No need to recompute entire reg_alloc body because we reserve extra registers
	   for the sole purpose of these spilling operations *)
	let rec select_loop f g =
		(* Select - start popping off stack and coloring/spilling *)
		let (v, rm) = select g in
		match v with
		  Some v1 -> (* v1 got spilled; remove it from stack and rewrite f for the spill and recompute *)
			let _ = (cur_stack := List.filter (fun x -> x != v1) (!cur_stack)) in
			select_loop (apply_spill f v1) (simplify g v1)
		| None -> (* No spills; go ahead and replace every variable in f with rm of it *)
			apply_rm f rm in
	add_stack (select_loop f orig)

(*************************** Post-regalloc compilation ************)
(* Compile one block down to mips *)
let rec compile_block (b : block) : M.inst list =
	match b with
	  h::t -> (match h with
		  Label l -> [M.Label l]
		| Move (Reg x, Reg y) -> [M.Add(x, y, M.Reg M.R0)]
		| Arith (Reg x, Reg y, op, Reg z) ->
			(match op with
			  Plus ->  [M.Add(x, y, M.Reg z)]
			| Minus -> [M.Sub(x, y, z)]
			| Times -> [M.Mul(x, y, z)]
			| Div ->   [M.Div(x, y, z)])
		| Arith (Reg x, Reg y, Plus, Int z)
		| Arith (Reg x, Int z, Plus, Reg y) ->
			[M.Add(x, y, M.Immed(Word32.fromInt z))]
		| Load (Reg x, Reg y, i) ->
			[M.Lw(x, y, Word32.fromInt i)]
		| Store (Reg x, i, Reg y) ->
			[M.Sw(x, y, Word32.fromInt i)]
		| Call (Lab f) ->
			[M.J(f)]
		| If(Reg x, cop, Reg y, l1, l2) ->
			(match cop with
			  Eq ->  [M.Beq(x, y, l1); M.J(l2)]
			| Neq -> [M.Bne(x, y, l1); M.J(l2)]
			| Lt ->  [M.Blt(x, y, l1); M.J(l2)]
			| Lte -> [M.Ble(x, y, l1); M.J(l2)]
			| Gt ->  [M.Bgt(x, y, l1); M.J(l2)]
			| Gte -> [M.Bge(x, y, l1); M.J(l2)])
		| Return -> [M.Jr(M.R31)]
		| _ -> raise FatalError) @ (compile_block t)
	| [] -> []

(* Compile cfg down to mips, given it has no variables anymore *)
let rec compile_cfg (f : func) : M.inst list =
	match f with
	  h::t -> (compile_block h) @ (compile_cfg t)
	| [] -> []

(* Finally, translate the output of reg_alloc to Mips instructions *)
let cfg_to_mips (f : func) : M.inst list = 
	compile_cfg (reg_alloc f)

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

(* let _ =
  let prog = parse_file() in
  List.fold_left print_interference_graph () prog
 *)
