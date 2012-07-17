open Batteries
open Map
open Set
open Utils
open Cil
open Z3
open Cilprinter 
open Difftypes

let canonical_stmt_ht = Hashtbl.create 255 
let inv_canonical_stmt_ht = Hashtbl.create 255
let canonical_sid str sid =
  ht_find canonical_stmt_ht str (fun _ -> sid)

class numToZeroVisitor = object
  inherit nopCilVisitor
  method vstmt s = s.sid <- 0 ; DoChildren
end 

let my_zero = new numToZeroVisitor

class canonicalizeVisitor loc_ht = object
  inherit nopCilVisitor
  method vstmt s = 
    hadd loc_ht s.sid !currentLoc;
    let rhs = (visitCilStmt my_zero (copy s)).skind in
    let stripped_stmt = 
      { labels = [] ; skind = rhs ; sid = 0; succs = [] ; preds = [] ; }
    in
      
    let pretty_printed =
      try 
        Pretty.sprint ~width:80
          (Pretty.dprintf "%a" dn_stmt stripped_stmt)
      with _ -> Printf.sprintf "@%d" s.sid 
    in 
    let cid = canonical_sid pretty_printed s.sid in 
      hadd inv_canonical_stmt_ht s.sid (cid, pretty_printed);
    DoChildren
end

class convertExpsVisitor = object
  inherit nopCilVisitor
  method vstmt s =
	match s.skind with
	  If(e,b1,b2,loc) -> begin
		match e with 
		  Lval(l) -> 
			let exp1 = UnOp(LNot,e,intType) in
			let newexp = UnOp(LNot,exp1,intType) in
			  ChangeDoChildrenPost(s,
								   (fun s ->
									 {s with skind = If(newexp,b1,b2,loc)}))
		| _ -> DoChildren
	  end
	| _ -> DoChildren
end

type path_exploration = 
  | Exploring_Block of Cil.block 
  | Exploring_Statement of Cil.stmt  
  | Exploring_Done 

type path_step =
  | Statement of Cil.stmt * (Cil.exp list)
  | Assume of Cil.exp 

type path = path_step list 

type symexp = Cil.exp
type symmem = (symexp * symexp) list

(**********************************************************************
 * Symbolic Variable State (or Symbolic Register File) 
 *
 * Our state is a simple mapping from variable names to symbolic
 * expressions. We use the existing Cil.exp expression type for
 * symbolic expressions as well.
 *)

type symbolic_variable_state = Cil.exp StringMap.t 
let empty_symbolic_variable_state = StringMap.empty 

type state = 
{
  register_file : symbolic_variable_state ;
  mu : symmem ;
  visited : IntSet.t ;
  assumptions : symexp list ;
  path : path_step list 
}

let empty_state =
{
  register_file = empty_symbolic_variable_state;
  mu = [];
  visited = IntSet.empty ;
  assumptions = [];
  path = []
}


(* returns a new state in which 'value' has been written to memory heap
 * location 'address' *)
let update_memory state address value =
  { state with mu = (address,value) :: state.mu } 

(* returns a new state in which variable 'varinfo' has been assigned
 * the new value 'exp' *)
let assign old_state varinfo exp = 
  let old_sigma = old_state.register_file in 
  { old_state with register_file = StringMap.add varinfo.vname exp old_sigma }

(*
 * Look up a variable in the symbolic state. For example, if we know that
 * [x=10] and [y=z+3] and we lookup "y", we expect to get "z+3" out.
 *)
let lookup state variable =
  let found = match variable with
  | Lval(Var(va),NoOffset) -> 
    begin
      try
        Some(StringMap.find va.vname state.register_file)
      with Not_found -> 
        None
    end 
  | _ -> None (* FIXME: there are many other lval options here *)
  in 
  match found with
  | Some(answer) -> answer
  | None -> variable 

(* returns true if we have already been to this statement *) 
let already_visited state stmt =
  IntSet.mem stmt.sid state.visited 

(* returns a new state in which we have visited 'stmt' *) 
let mark_as_visited old_state stmt = 
  { old_state with visited = IntSet.add stmt.sid old_state.visited } 

let assume state exp = 
  let exp' = exp_str exp in 
  let rec find_previous path = 
    match path with
      Assume(e) :: rest ->
        let e' = exp_str e in
          if e' = exp' then true
          else find_previous rest 
    | _ -> false 
  in
    if find_previous state.path then state else
      { state with path = 
          Assume(exp) :: state.path ; 
        assumptions = exp :: state.assumptions}

(*
 * Rewrite an expression based on the current symbolic state.  For example,
 * if we know that [x=10] and [y=z+3] and we lookup "sin(x+y)", we expect
 * to get "sin(10+z+3)". 
 *)
class substituteVisitor sigma = object
  inherit nopCilVisitor
  method vexpr e = 
    ChangeDoChildrenPost(e,(fun e ->
      lookup sigma e
    ))
end 

let symbolic_variable_state_substitute state exp =
  let sv = new substituteVisitor state in 
    visitCilExpr sv exp 

let symbolic_variable_state_update state varname new_value =
  {state with register_file = StringMap.add varname new_value state.register_file }
let decide state exp =
let ctx = mk_context_x [| |] in
(*    Z3.trace_to_stdout ctx ;  *)

let int_sort = mk_int_sort ctx (* Possible FIXME: reals unhandled *) in
let zero_ast = mk_int ctx 0 int_sort in
let undefined_ast = zero_ast in
let symbol_ht = Hashtbl.create 255 in
(* Every time we encounter the same C variable "foo" we want to map
 * it to the same Z3 node. We use a hash table to track this. *) 
let var_to_ast ctx str = 
  try
    Hashtbl.find symbol_ht str
  with _ -> 
    let sym = mk_string_symbol ctx str in
      (* Possible FIXME: currently we assume all variables are integers. *)
    let ast = mk_const ctx sym int_sort in 
      Hashtbl.replace symbol_ht str ast ;
      ast
in
(* In Z3, boolean-valued and integer-valued expressions are different
 * (i.e., have different _Sort_s). CIL does not have this issue. *) 
let is_binop exp = 
  match exp with 
  | UnOp(LNot,_,_) | BinOp(Lt,_,_,_) | BinOp(Le,_,_,_) 
  | BinOp(Gt,_,_,_) | BinOp(Ge,_,_,_) | BinOp(Eq,_,_,_) 
  | BinOp(Ne,_,_,_) -> true
    | _ -> false
in
(* This is the heart of constraint generation. For every CIL expression
 * (e.g., "x > 10"), convert it to an equivalent Z3 expression. *) 
let rec exp_to_ast ctx exp = 
  match exp with
  | Const(CInt64(i,_,_)) -> (* FIXME: handle large numbers *) 
    Z3.mk_int ctx (Int64.to_int i) int_sort 
  | Const(CChr(c)) -> (* FIXME:  handle characters *) 
    Z3.mk_int ctx (Char.code c) int_sort
  | Lval(Var(va),NoOffset) -> var_to_ast ctx va.vname 
  | UnOp(Neg,e,_) -> mk_unary_minus ctx (exp_to_ast ctx e) 
  | UnOp(LNot,e,_) when is_binop e -> mk_not ctx (exp_to_ast ctx e) 
  | UnOp(LNot,e,_) -> mk_eq ctx (exp_to_ast ctx e) (zero_ast) 

  | BinOp(MinusA,e1,e2,_) -> mk_sub ctx [| exp_to_ast ctx e1; exp_to_ast ctx e2|]
  | BinOp(Mult,e1,e2,_) -> mk_mul ctx [| exp_to_ast ctx e1; exp_to_ast ctx e2|]
  | BinOp(Div,e1,e2,_) -> 
    let ast2 = exp_to_ast ctx e2 in 
    let not_div_by_zero = mk_distinct ctx [| zero_ast ; ast2 |] in 
      Z3.assert_cnstr ctx not_div_by_zero  ; 
      mk_div ctx (exp_to_ast ctx e1) ast2 
  | BinOp(Mod,e1,e2,_) -> mk_mod ctx (exp_to_ast ctx e1) (exp_to_ast ctx e2) 
  | BinOp(Lt,e1,e2,_) -> mk_lt ctx (exp_to_ast ctx e1) (exp_to_ast ctx e2) 
  | BinOp(Le,e1,e2,_) -> mk_le ctx (exp_to_ast ctx e1) (exp_to_ast ctx e2) 
  | BinOp(Gt,e1,e2,_) -> mk_gt ctx (exp_to_ast ctx e1) (exp_to_ast ctx e2) 
  | BinOp(Ge,e1,e2,_) -> mk_ge ctx (exp_to_ast ctx e1) (exp_to_ast ctx e2) 
  | BinOp(Eq,e1,e2,_) ->
    mk_eq ctx (exp_to_ast ctx e1) (exp_to_ast ctx e2) 
  | BinOp(Ne,e1,e2,_) ->
    mk_distinct ctx [| (exp_to_ast ctx e1) ; (exp_to_ast ctx e2) |] 

  | CastE(_,e) -> exp_to_ast ctx e (* Possible FIXME: (int)(3.1415) ? *) 
  | _ -> failwith "undefined_ast"
in
  (* Every assumption along the path has already been added to the context, so
   * all we have to do is convert this new exp to a Z3 expression and then
   * assert it as true *)
  liter 
    (fun exp -> 
      try 
(*        debug "asserting: %s\n" (exp_str exp);*)
        let z3_ast = exp_to_ast ctx exp in 
          Z3.assert_cnstr ctx z3_ast ; 
      with _ -> ())
    (exp :: state.assumptions);
  (* query the theorem prover to see if the model is consistent.  If so, return
   * the new model.  If not, pop it first. *)
(*  debug "CONTEXT:\n %s\n" (Z3.context_to_string ctx);*)

  let made_model = Z3.check ctx in 
    Z3.del_context ctx;
    made_model,state

(* returns true if the given expression represents one of our fresh,
 * unknown symbolic values *) 
(* FIXME: this is not necessarily true given the port from nf.ml *)
let is_unknown_symexp e = match e with
  | Lval(Var(va),NoOffset) when va.vname.[0] = '|' -> true 
  | _ -> false 

(* this convenience function returns the symbolic expression (and 
 * C/CIL expression) associated with 'true' or 'false'. Recall that in 
 * C we have "false == 0" and "true <> 0". *)
let se_of_bool b = 
  if b then Const(CInt64(Int64.one,IInt,None))
  else Const(CInt64(Int64.zero,IInt,None))

(* We will often need to make a fresh symbolic value that we know nothing
 * about (this is like the \forall x. ... in the notes) ... "fresh_value"
 * does that for us *) 
let value_counter = ref 0 
let fresh_value ?va () = 
  let str = 
    match va with
    | None -> "|" 
    | Some(va) -> "|" ^ va.vname 
  in
  let c = !value_counter in
  incr value_counter ;
  let va = makeVarinfo false (Printf.sprintf "%s%d" str c) (TVoid([])) in
  Lval(Var(va),NoOffset)

let rec eval s ce = 
(*  debug "evaluating {%s}\n" (exp_str ce);*)
  match ce with 
  | Lval(Var(va),NoOffset) -> 
    if is_unknown_symexp ce then ce 
    else (try lookup s ce 
      with _ -> fresh_value ())
  (* McCarthy's Select Memory Axiom *) 
  | Lval(Mem(read_addr),NoOffset) -> 
    let read_addr = eval s read_addr in 
    let rec select lst = match lst with
    | [] -> fresh_value () 
    | (written_addr, written_value) :: earlier_writes -> begin 
      let decision,s = decide s (BinOp(Eq,read_addr, written_addr, TInt(IInt,[]))) in
      match decision with
        L_TRUE -> written_value
      | L_FALSE -> select earlier_writes 
      | _ -> fresh_value () 
    end 
    in 
    select s.mu 
  | UnOp(unop,ce,tau) -> UnOp(unop, eval s ce, tau) 
  | BinOp(bop,ce1,ce2,tau) -> begin
    match bop, (eval s ce1), (eval s ce2) with
    (* in a few cases we can compute this "in-line" without pushing it 
     * off to the theorem prover -- this is typically a worthwhile
     * optimization for scalability but might not be worth it to you
     * in this homework *) 
    | PlusA, Const(CInt64(i1,ik1,_)), Const(CInt64(i2,_,_)) ->
      Const(CInt64(Int64.add i1 i2, ik1, None))
    | MinusA, Const(CInt64(i1,ik1,_)), Const(CInt64(i2,_,_)) ->
      Const(CInt64(Int64.sub i1 i2, ik1, None))
    | Mult, Const(CInt64(i1,ik1,_)), Const(CInt64(i2,_,_)) ->
      Const(CInt64(Int64.mul i1 i2, ik1, None))
    | Shiftlt, Const(CInt64(i1,ik1,_)), Const(CInt64(i2,_,_)) ->
      Const(CInt64(Int64.shift_left i1 (Int64.to_int i2), ik1, None))
    | Shiftrt, Const(CInt64(i1,ik1,_)), Const(CInt64(i2,_,_)) ->
      Const(CInt64(Int64.shift_right i1 (Int64.to_int i2), ik1, None))
    | Lt, Const(CInt64(i1,ik1,_)), Const(CInt64(i2,_,_)) ->
      se_of_bool (i1 < i2) 
    | Le, Const(CInt64(i1,ik1,_)), Const(CInt64(i2,_,_)) ->
      se_of_bool (i1 <= i2) 
    | Gt, Const(CInt64(i1,ik1,_)), Const(CInt64(i2,_,_)) ->
      se_of_bool (i1 > i2) 
    | Ge, Const(CInt64(i1,ik1,_)), Const(CInt64(i2,_,_)) ->
      se_of_bool (i1 >= i2) 
    | x, y, z -> BinOp(x,y,z,tau)
    end 
  | CastE(_,ce) -> eval s ce
  | x -> x

(* this function throws away all information we have about the values 
 * of local variables and heap values -- this is typically a conservative
 * "option of last resort" when something happens that we don't know how to
 * model *) 
let throw_away_state old_state =
  let new_sigma = StringMap.mapi (fun old_key old_val -> 
    fresh_value () 
  ) old_state.register_file in
  { old_state with register_file = new_sigma ; mu = [] ; assumptions = [] } 

(*
 * This procedure evaluates an instruction in a given state. It returns
 * either Some(new_state) if symex should continue or None if we should
 * stop. We stop after calling exit(1) or on division by 0 or somesuch.
 *)
let rec handle_instr (i:instr) (s:state) : (state option) = 
(*  debug "handling instr %s\n" (Pretty.sprint ~width:80 (printInstr printer () i));*)
  match i with
  | Set((Var(va),NoOffset),new_val,location) -> 
    let new_val = eval s new_val in
      (Some (assign s va new_val))
  | Set((Mem(ptr_exp),NoOffset),new_val,location) -> 
    let ptr_exp = eval s ptr_exp in 
    let new_val = eval s new_val in 
    let s = update_memory s (ptr_exp) (new_val) in 
    (* magic trick: if you are continuing on a path after *p = 5, then 
     * p must be non-null! *) 
    let s = assume s (BinOp(Ne,ptr_exp,(se_of_bool false),TInt(IInt,[]))) in
    (Some s)

  | Set(_,new_val,location) -> 
(*    debug "Warning: tricky assignment!\n" ;*)
    (* you might want to fix this *) 
    (Some (throw_away_state s ))

  | Call(retval_option, (Lval(Var(va),NoOffset)), args, loc) -> 
    if va.vname = "exit" then None 
    else 
    (match retval_option with
    | None ->  (Some s)
    | Some(lv) -> 
      (* we are intraprocedural, so we assume the function call can 
       * return anything *) 
      let fv = fresh_value ~va () in 
        (handle_instr (Set(lv,fv,loc)) s)
    ) 
  | Call(retval_option, function_ptr_exp, args, location) -> 
(*    debug "Warning: call through function pointer!\n" ;*)
    (* you might want to fix this *) 
    (Some (throw_away_state s))
  | Asm(_) -> (* we don't handle inline asm! *) (Some s)

let path_enumeration (target_fundec : Cil.fundec) =
  let enumerated_paths = ref [] in
  let note_path (s : state) = enumerated_paths := s :: !enumerated_paths in 
  let worklist = Queue.create () in
  let add_to_worklist state where nn nb nc =
    Queue.add (state,where, nn, nb, nc) worklist 
  in 
  let give_up state stmt =
    let state = { state with path = (Statement(stmt, state.assumptions)) :: state.path } in
    let state = mark_as_visited state stmt in
      note_path state
  in 
  let initial_state = ref empty_state in
    List.iter (fun formal -> 
      (* formals start out undefined -- we are not context-sensitive *)
      initial_state := assign !initial_state formal (fresh_value ~va:formal ())
    ) target_fundec.sformals ;

    List.iter (fun local ->
      (* locals start out as zero! *) 
      initial_state := assign !initial_state local (se_of_bool false) 
    ) target_fundec.slocals ; 
(*
  let initialize_variables vars register_file =
    lfoldl (fun register_file varinfo ->
        let new_value = Lval(Var(makeVarinfo false ("_" ^ varinfo.vname) 
                                   (TVoid [])),NoOffset) in
          StringMap.add varinfo.vname new_value register_file 
      ) register_file vars 
  in
  let register_file = 
    initialize_variables (target_fundec.sformals @ target_fundec.slocals) initial_state.register_file 
  in *)
  let initial_state = !initial_state in (*{ initial_state with register_file = register_file } in*)
    add_to_worklist initial_state (Exploring_Block(target_fundec.sbody)) [] [] [] ;
    while not (Queue.is_empty worklist) && (llen !enumerated_paths < 500) do
      (* 
       * state = current symex state
       * here = this dataflow place
       * nn = next normal
       * nb = next if we hit a "break;"
       * nc = next if we hit a "continue;" *)
      let state, here, nn, nb, nc = Queue.pop worklist in 

        match here with
        | Exploring_Done -> 
          (match nn with
          | [] -> note_path state
          | first :: rest ->  add_to_worklist state first rest nb nc)
        | Exploring_Block(b) -> 
          (match b.bstmts with
          | [] -> add_to_worklist state (Exploring_Done) nn nb nc
          | first :: rest -> 
            let followup = (Exploring_Block { b with bstmts = rest }) in 
              add_to_worklist state (Exploring_Statement(first)) (followup :: nn) nb nc)
        | Exploring_Statement(s) -> 
          begin
            if not (already_visited state s) then begin
            let state = mark_as_visited state s in
              match s.skind with
              | Return _ | Goto((*goto_target*) _,_) | Switch _ | TryFinally _ 
              (* possible FIXMEs for a more precise analysis *)
              | TryExcept _ -> give_up state s
              | Instr il -> begin
                let state = mark_as_visited state s in
                let state = { state with path = Statement(s, state.assumptions) :: state.path } in
                let new_state_opt = List.fold_left (fun state_opt instr ->
                  match state_opt with
                  | None -> None
                  | Some(state) -> handle_instr instr state
                ) (Some state) il in
                  match new_state_opt, nn with
                  | None,_ -> give_up state s
                  | Some(new_state), _ -> 
                    add_to_worklist new_state (Exploring_Done) nn nb nc
              end
              | Break _ -> 
                (match nb, nc with 
                | b_hd :: b_tl , c_hd :: c_tl -> 
                  add_to_worklist state (Exploring_Done) b_hd b_tl c_tl
                | _, _ -> give_up state s)
              | Continue _ ->  
                (match nb, nc with 
                | b_hd :: b_tl , c_hd :: c_tl -> 
                  add_to_worklist state (Exploring_Done) c_hd b_tl c_tl
                | _, _ -> give_up state s)
              | If(exp,then_branch,else_branch,_) -> 
(*                debug "processing if %s\n" (exp_str exp);*)
                let process_assumption exp branch =
                  let evaluated = symbolic_variable_state_substitute state exp in
                  let decision,state = decide state evaluated in
                    match decision with
                      L_TRUE | L_UNDEF ->
(*                        debug "unclear\n";*)
                        let state = assume state evaluated in
                        add_to_worklist state (Exploring_Block(branch)) nn nb nc
                    | _ -> (*debug "giving up\n";*) give_up state s
                in
                let then_condition = 
                  match exp with 
                    Lval(l) -> BinOp(Ne, exp,zero,intType)
                  | _ -> exp
                in
                let else_condition = UnOp(LNot,exp,(Cil.typeOf exp)) in
(*                  debug "then: %s\n" (exp_str exp);*)
                  process_assumption then_condition then_branch;
(*                  debug "else: %s\n" (exp_str else_condition);*)
                  process_assumption else_condition else_branch;
              | Loop(loop_block,_,break_opt,continue_opt) -> 
                    add_to_worklist state (Exploring_Block loop_block) nn (nn :: nb) ((here :: nn) :: nc) 
              | Block(b) -> add_to_worklist state (Exploring_Block b) nn nb nc 
            end else begin
              add_to_worklist state (Exploring_Done) nn nb nc
            end
          end
    done ;
    lrev (lmap (fun state -> { state with path = lrev state.path }) !enumerated_paths)

let print_state state = 
  debug "STATE PRINTING:\n";
  liter
    (fun ps ->
      match ps with
        Statement(s,assumptions) ->
          let asstr = stmt_str s in 
            debug "\t{STATEMENT(%s)\n" asstr;
            debug "\tASSUMING[";
            liter (fun exp -> debug "%s, " (exp_str exp)) assumptions;
            debug "]}\n"
      | Assume(exp) ->
        let asstr = exp_str exp in 
          debug "\t{ASSUME(%s)}\n" asstr;
    ) state.path
let path_generation file fht functions = 
  Z3.toggle_warning_messages true ; 
  let location_ht = hcreate 10 in
(*	visitCilFileSameGlobals (new convertExpsVisitor) file;*)
	visitCilFileSameGlobals (new canonicalizeVisitor location_ht) file;
	lfoldl
	  (fun stmtmap funname ->
        debug "function: %s\n" funname;
		let fd = hfind fht funname in
		let feasible_paths = path_enumeration fd in 
          debug "after feasible, %d paths\n" (llen feasible_paths);
(*          liter print_state feasible_paths;
          debug "after printing\n";*)
		let only_stmts = 
          lflat 
            (lmap 
            (fun state -> 
              lfilt 
                (fun path_step ->
                  match path_step with Assume _ -> false | _ -> true)
                state.path) feasible_paths )
		in
		let stmts = 
		  lfoldl
			(fun stmtmap1 path_step ->
			  match path_step with
			  | Statement(s, assumptions) ->
				let assumptions_set = ExpSet.of_enum (List.enum assumptions) in
				let location = hfind location_ht s.sid in
                let cid,str = hfind inv_canonical_stmt_ht s.sid in
                let old_val,_ = if StmtMap.mem (cid,str,s) stmtmap1 then StmtMap.find (cid,str,s) stmtmap1 else ExpSetSet.empty,location in
				  StmtMap.add (cid,str,s) ((ExpSetSet.add assumptions_set old_val),location) stmtmap1
			) StmtMap.empty only_stmts
		in
		  StringMap.add funname stmts stmtmap
	  ) StringMap.empty functions 
