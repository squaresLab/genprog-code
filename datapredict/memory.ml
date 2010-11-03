open Batteries
open Map
open Cil
open Utils
open Globals
open Invariant

module type Layout = 
sig
  type t

  (* these functions are the only ones that deal with the memory layout
   * exactly. The others take layout ids! *)

  val empty_layout : unit -> t
  val add_to_layout : t -> string -> memV -> t
(*  val save_layout : t -> int*)
  val get_layout : int -> t

  (* functions to manage, evaluate, etc layouts *)
  val get_vars : int -> string list
  val eval : int -> Invariant.predicate -> bool
  val var_value : int -> string -> memV
  val print_layout : int -> unit
end

let layout_map : (memV StringMap.t, int) Hashtbl.t ref = ref (hcreate 100)
let rev_map : (int, memV StringMap.t) Hashtbl.t ref = ref (hcreate 100)

module Layout =
struct
  (* a layout is one mapping from variable names to values. *)
  (* memory_layout maps string names to memV values *)

  type t = memV StringMap.t

  let empty_layout () = StringMap.empty 

  let add_to_layout (one_layout : t) (var : string) (memvalue : memV) : t = 
    if StringMap.mem var one_layout then 
      failwith "The layout has a value for this var already in Layout.add_to_layout.";
    StringMap.add var memvalue one_layout
		
  (* evaluate predicates given a particular memory mapping *) 

  let get_layout (id : int) : t = hfind !rev_map id

  let get_vars (id : int) : string list =
	let layout = get_layout id in
	  StringMap.fold
		(fun varname ->
		   fun varval ->
			 fun names ->
			   varname :: names) layout []

  let rec eval (id : int) (pred : Invariant.predicate) : bool = 
	let mem = get_layout id in
(*	  print_layout mem;*)
      match pred with
		CilExp(exp) ->
		  begin
			let rec get_vars e =
			  match e with
			  | Lval(Var(vi),offset) ->
				  let name = vi.vname in 
					StringMap.find name mem
			  | CastE(_,e) -> get_vars e
			  | Const(c) ->
				  begin
					match c with 
					| CInt64(i,_,_) -> Int(Int64.to_int(i))
					| CStr(s) -> failwith "eval on something not implemented"
					| CWStr(iis) ->  failwith "eval on something not implemented"
					| CChr(c) ->  failwith "eval on something not implemented"
					| CReal(f,_,_) -> Float(f)
					| CEnum(e,s,einfo) ->  failwith "eval on something not implemented"
				  end
			  | _ -> 
				  let exp_str = Pretty.sprint 80 (d_exp () e) in
					pprintf "doing a getvars on: %s\n" exp_str; flush stdout;
					failwith "eval get vars on a varinfo memory, which also makes no sense"
			in
			  match exp with 
				BinOp(b,e1,e2,t) -> begin
				  let val1 = get_vars e1 in
				  let val2 = get_vars e2 in
				  let bfuni = 
					match b with
					  Gt -> fun one -> fun two -> one > two
					| Lt -> fun one -> fun two -> one < two
					| Eq -> fun one -> fun two -> one == two 
					| Ge -> fun one -> fun two -> one >= two
					| Le -> fun one -> fun two -> one <= two
					| Ne -> fun one -> fun two -> not (one == two)
					| _ -> failwith "Unrecognizable binop"
				  in
				  let bfunf = 
					match b with
					  Gt -> fun one -> fun two -> one > two
					| Lt -> fun one -> fun two -> one < two
					| Eq -> fun one -> fun two -> one == two 
					| Ge -> fun one -> fun two -> one >= two
					| Le -> fun one -> fun two -> one <= two
					| Ne -> fun one -> fun two -> not (one == two)
					| _ -> failwith "Unrecognizable binop"
				  in
					match val1, val2 with
					  Int(i1), Int(i2) -> bfuni i1 i2
					| Int(i1), Float(f2) -> bfunf (float_of_int i1) f2
					| Float(f1), Float(f2) -> bfunf f1 f2
					| Float(f1), Int(i2) -> bfunf f1 (float_of_int i2) 
				end
			  | _ -> failwith "eval on an exp we didn't create"
		  end
      | _ -> failwith "Eval on a non-cil exp predicate, which makes no sense."


  (* debug *)
  let print_layout lid = 
	let layout = get_layout lid in
	pprintf "Printing layout: \n"; flush stdout;
	StringMap.iter
	  (fun vname ->
		 fun vval ->
		   pprintf "%s -> %s\n" vname (string_of_mval vval)) layout;
	pprintf "Done printing layout\n"; flush stdout

  let var_value layout var =
	let mem = get_layout layout in 
	  StringMap.find var mem
end

module type Memory = 
sig
  type t

  val new_state_mem : unit -> t
  val add_layout : t -> int -> int -> int -> t
  val eval_pred_on_run : t -> int -> Invariant.predicate -> int * int
end

module Memory =
struct 
(* memory *never deals with actual layouts *)
(* Layout's accessor methods only take layout ids! *)

  type t = {
	run_layout_to_counts : ((int * int), IntSet.t) Hashtbl.t ;
	run_to_layouts : (int, IntSet.t) Hashtbl.t ;
	in_scope : StrSet.t ;
  }

  let new_state_mem () = 
	{ run_layout_to_counts = hcreate 10 ;
	  run_to_layouts = hcreate 10 ;
	  in_scope = StrSet.empty ;
	}

  let print_in_scope mem = 
	StrSet.iter (fun s -> pprintf "%s, " s) mem.in_scope;
	pprintf "\n"; flush stdout

  let in_scope mem pred = 
    let vars = pred_vars pred in
	  lfoldl (fun accum -> 
			   fun v -> 
				 if StrSet.mem v mem.in_scope then accum
				 else false) true vars

  let add_layout (mem : t) (run : int) (count : int) (layout : int) : t = 
	let mem' = 
	  if (hlen mem.run_layout_to_counts) == 0 then begin
		let vars = Layout.get_vars layout in
		let scope = lfoldl
		  (fun elt ->
			 fun varset ->
			   StrSet.add varset elt)
		  StrSet.empty vars 
		in
		  {mem with in_scope = scope }
	  end else mem
	in
	let set = 
	  ht_find mem'.run_layout_to_counts (run,layout) (fun x -> IntSet.empty) in
	  hrep mem'.run_layout_to_counts (run,layout) (IntSet.add count set);
	  let set = ht_find mem'.run_to_layouts run (fun x -> IntSet.empty) in
		hrep mem'.run_to_layouts run (IntSet.add layout set);
		mem'

  let memory_on_runs mem run = 
	IntSet.elements (ht_find mem.run_to_layouts run (fun x -> IntSet.empty))

  let eval_pred_on_run mem run pred =
	pprintf "in memory on run %d, predicting %s\n" run (d_pred pred); flush stdout;
	pprintf "in scope at this point: "; print_in_scope mem; flush stdout;
	if not (in_scope mem pred) then (0,0)
	else begin
	  let all_layouts = memory_on_runs mem run in
		pprintf "Num all layouts: %d\n" (llen all_layouts); flush stdout;
		lfoldl
		  (fun (num_true,num_false) ->
			 (fun (layout : int) ->
				let count = 
				  IntSet.cardinal (hfind mem.run_layout_to_counts (run,layout)) 
				in
				  pprintf "Saw this layout %d times\n" count; flush stdout;
				  if Layout.eval layout pred then begin
					pprintf "This predicate was true in this layout, new t/f: %d, %d\n\n" (num_true + count) num_false;
					flush stdout;
					(num_true + count, num_false)
				  end
				  else begin
					pprintf "This predicate was false in this layout, new t/f: %d, %d\n\n" num_true (num_false + count);
					flush stdout;
					(num_true, num_false + count)
				  end
				  ))
		  (0,0) all_layouts 
	end
end

module LessStrictMemory =
struct 

  (* map varname * values -> counts *)
  type t =  ((string * memV), IntSet.t) Hashtbl.t

  let new_state_mem () = hcreate 5

  let print_in_scope mem =
	let varset : StrSet.t = 
	  hfold (fun (s, _) ->
			   (fun _ ->
				  (fun accum -> StrSet.add s accum))) mem (StrSet.empty) in
	  StrSet.iter 
		(fun s -> pprintf "%s, " s) 
		varset;
	  pprintf "\n"; flush stdout

  let in_scope mem pred = failwith "Fix me\n"
(*    let vars = pred_vars pred in
	  lfoldl (fun accum -> 
				(fun v -> 
				   if hmem mem.mem v then accum else false)) 
		true vars*)

  let add_varval mem count varname varval =
	let countset = ht_find mem (varname, varval) (fun () -> IntSet.empty) in
	  hrep mem (varname, varval) (IntSet.add count countset)

  let eval_pred_on_run mem run pred = failwith "not implemented"
(*	if not (in_scope mem pred) then (0,0)
	else begin
	  (* this is the problem with the layouts, sad. Think think think *)
	  let all_layouts = memory_on_runs mem run in
		lfoldl
		  (fun (num_true,num_false) ->
			 (fun layout ->
				let count = 
				  IntSet.cardinal (hfind mem.run_layout_to_counts (run,layout)) 
				in
				  if Layout.eval layout pred then 
					(num_true + count, num_false)
				  else
					(num_true, num_false + count)))
		  (0,0) all_layouts 
	end*)
end
