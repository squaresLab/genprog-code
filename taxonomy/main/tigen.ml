open Batteries
open Printf
open Map
open Set
open Globals
open Utils
open Cil
open Z3
open Cilprinter 
open Difftypes

let canonical_sid ht str sid =
  ht_find ht str (fun _ -> sid)

class numToZeroVisitor = object
  inherit nopCilVisitor
  method vstmt s = s.sid <- 0 ; DoChildren
end 

let my_zero = new numToZeroVisitor

class canonicalizeVisitor canon_ht inv_canon_ht loc_ht = object
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
    let cid = canonical_sid canon_ht pretty_printed s.sid in 
      hadd inv_canon_ht s.sid (cid, pretty_printed);
    DoChildren
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
  z3_assumptions : symexp list ;
  cluster_assumptions : symexp list ;
  path : path_step list 
}


let empty_state =
{
  register_file = empty_symbolic_variable_state;
  mu = [];
  visited = IntSet.empty ;
  z3_assumptions = [];
  cluster_assumptions = [];
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

let assume state z3_exp cluster_exp = 
  let exp' = exp_str z3_exp in 
  let rec find_previous path = 
    match path with
      Assume(e) :: rest ->
        let e' = exp_str e in
          if e' = exp' then true
          else find_previous rest 
    | _ -> false 
  in
    if find_previous state.path then  state else begin
(*      let cluster_exp = visitCilExpr my_fix z3_exp in*)
(*      debug "assume, z3_exp: {%s}, cluster_exp: {%s}\n" (exp_str z3_exp) (exp_str cluster_exp);*)
      { state with path = 
          Assume(z3_exp) :: state.path ; 
        z3_assumptions = z3_exp :: state.z3_assumptions;
      cluster_assumptions =  cluster_exp :: state.cluster_assumptions}
    end
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


(* theorem provers are incomplete, so often "Unknown" is the answer *) 
type tp_result = 
  | Must_Be_True
  | Must_Be_False
  | Unknown

(* Uses an external theorem prover to try to decide if "e" is necessarily
 * true. *) 
let valid_regexp = Str.regexp ".*Valid\\." 
let invalid_regexp = Str.regexp ".*Invalid\\." 
let bad_regexp = Str.regexp ".*Bad input" 
let runtime_regexp = Str.regexp "runtime error:"

let vht = Hashtbl.create 255 
let vht_counter = ref 0 
let handle_va va =
  ht_find vht va.vname 
    (fun _ ->
      let res = Printf.sprintf "v%d" !vht_counter in
        incr vht_counter ;
        res)


(* if the expression is already a relational operator, we don't need
 * to coerce on the implicit "!= 0" suffix *) 
let is_relop bop = match bop with
  | Lt                                  (** <  (arithmetic comparison) *)
  | Gt                                  (** >  (arithmetic comparison) *)  
  | Le                                  (** <= (arithmetic comparison) *)
  | Ge                                  (** >  (arithmetic comparison) *)
  | Eq                                  (** == (arithmetic comparison) *)
  | Ne                                  (** != (arithmetic comparison) *)
  | LAnd                                (** logical and. *) 
  | LOr                                 (** logical or. *) 
  -> true
  | _ -> false 

let must_coerce e = match e with
  | BinOp(bop, _, _, _) when is_relop bop -> false
  | UnOp(LNot,_,_) -> false 
  | _ -> true 

let tp_Memo = hcreate 10


type 'a tp_pair = IO.input * 'a IO.output
type feasibility = Definitely_Yes | Definitely_Not | Maybe
let tp_is_up : 'a tp_pair option ref = ref None

let vht_counter = ref 0
(* Canonicalize a Simplify query so that (v12 < 0) and (v5 < 0)
 * both map to (w0 < 0). This helps with TP caching. *)
let v_regexp = Str.regexp "v[0-9]+" 
let canon_ht = Hashtbl.create 255 

let canonicalize_simplify_query str = 
  ht_find canon_ht str
    (fun _ ->
	let id = ref 0 in
	let local_ht = Hashtbl.create 255 in 
	let result = Str.global_substitute v_regexp 
	  (fun this_one ->
	    let this_one = Str.matched_string this_one in 
		  if Hashtbl.mem local_ht this_one then
		    Hashtbl.find local_ht this_one 
		  else begin
		    let name = Printf.sprintf "w%d" !id in
		      incr id ;
		      Hashtbl.replace local_ht this_one name ;
		      name 
		  end 
	  ) str in
(*      Printf.printf "Canon:  In: %s\n" str ;
      Printf.printf "Canon: Out: %s\n" result ;*)
	  Hashtbl.add canon_ht str result ; 
	  result )

let close_server () = 
  match !tp_is_up with
  | None -> ()
  | Some(inchan,outchan) -> begin 
	(try 
	   let _ = Unix.close_process (inchan,outchan) in 
	     ()
	 with _ -> ()) ;
	tp_is_up := None 
  end 
		 
(* This powerful procedure asks the Theorem Prover to decide if "e" 
 * must be true. If anything goes wrong, we return Unknown. *) 
let decide (s:state) e : tp_result = 
  (* symbolic expression representing the address of a variable. Addresses are
     all distinct and are all non-null. *) 
  let addrs_mentioned = Hashtbl.create 5 in 
  let handle_addrof va = 
    let res = "a" ^ (handle_va va) in 
    Hashtbl.replace addrs_mentioned res () ; 
    res
  in 
  match e with
  (* for constants, we don't even need to ask the theorem prover --
   * we can decide it locally *) 
  | Const(CInt64(x,_,_)) -> 
    if x = Int64.zero then Must_Be_False else Must_Be_True
  | Const(CStr(_)) 
  | Const(CWStr(_)) -> Must_Be_True
  | Const(CChr(c)) -> if c = '\000' then Must_Be_False else Must_Be_True
  | _ -> 
    begin 
  (* convert the CIL AST expression into a Simplify Query string.  Note that C
   * has implicit conversions between bools and ints but that Simplify does
   * not. Thus we insert  " != 0" for int-like C expressions. *) 
      let rec to_str e want_bool = 
        let result = 
          match e with
          | Const(CInt64(i64,_,_)) -> 
            let res = if i64 > (Int64.of_int 2147483646) then (Int64.of_int 214748364) else i64 in
            let res = if res < (Int64.of_int (-2147483646)) then (Int64.of_int (-214748364)) else res in
              Int64.to_string res
          | Const(CChr(c)) -> Int.to_string (Char.code c)
          | CastE(_,e) -> to_str e want_bool
          | AddrOf(Var(va),NoOffset) -> handle_addrof va 
          | Lval(Var(va),NoOffset) -> handle_va va
          | UnOp(Neg,e,_) -> sprintf "(- 0 %s)" (to_str e false) 
          | UnOp(LNot,e,_) -> sprintf "(NOT %s)" (to_str e true) 
          | BinOp(bop,e1,e2,_) -> 
            begin
              let bop_str, want_bool = 
                match bop with
                | PlusA | PlusPI -> "+", false 
                | MinusA | MinusPI | MinusPP -> "-", false 
                | Mult -> "*", false 
                | Div -> "/", false 
                | Mod -> "%", false 
                | Lt -> "<", false 
                | Gt -> ">", false 
                | Le -> "<=" , false 
                | Ge -> ">=", false 
                | Ne -> "NEQ", false
                | Eq -> "EQ", false
                | LAnd -> "AND", true
                | LOr -> "OR", true
                | _ -> 
                  failwith "don't know how to ask simplify about this" 
              in
                sprintf "(%s %s %s)" bop_str (to_str e1 want_bool) 
                  (to_str e2 want_bool) 
            end 
          | _ -> failwith "don't know how to ask simplify about this" 
        in 
          if want_bool && must_coerce e then 
            sprintf "(NEQ %s 0)" result 
          else
            result 
      in
        (* function to actually query the theorem prover *)
      let tried_once = ref false in
      let rec query str =
(*        ( debug "Theorem Prover Asking: %s\n" str) ; *)
        (* we memoize queries for efficiency *)
        if hmem tp_Memo str then hfind tp_Memo str
        else 
          let canon = canonicalize_simplify_query str in
            if hmem tp_Memo canon then
              hfind tp_Memo canon
            else 
              match !tp_is_up with
                None ->
                  let inchan,outchan = 
			        Unix.open_process "./Simplify-1.5.4.linux"
                  in
                    tp_is_up := Some(inchan,outchan);
                    query canon
              | Some(inchan,outchan) ->
                output_string outchan (str ^ "\n") ; 
                flush outchan ; 
                let finished = ref None in 
                let rec scan () = 
                  match !finished with
                  | None -> 
                    let reply = input_line inchan in 
(*                      (debug "Theorem Prover Reply: %s\n" reply) ; *)
                      (  if Str.string_match valid_regexp reply 0 then
                          finished := Some(Must_Be_True)
                       else if Str.string_match invalid_regexp reply 0 then
                         finished := Some(Must_Be_False)
                       else if Str.string_match bad_regexp reply 0 then begin 
                         finished := Some(Unknown)
                       end
                      ) ;
                      scan () 
                  | Some(x) -> x
                in 
                  scan ()
      in
        (* actually create the query...*)
        try
          let str = to_str e true in (* the thing we're trying to decide *)
          (* all of the assumptions along the way...*)
          let assumption_string_list = lrev (List.fold_left (fun acc assumption -> 
            try (to_str assumption false) :: acc with _ -> acc
          ) [] s.z3_assumptions) in
          (* we also get to assume that addresses are distinct and non-null *) 
          let distinct_assumption = 
            (Hashtbl.fold (fun name () str -> 
              str ^ name ^ " "
             ) addrs_mentioned "(DISTINCT 0 ") ^ ")"
          in 
          let make_query str = 
            match distinct_assumption :: assumption_string_list with
            | [] -> str
            | big_list -> 
              let b = Buffer.create 255 in
                bprintf b "(IMPLIES (AND" ;
                List.iter (fun elt -> bprintf b " %s" elt) big_list ;
                bprintf b ") %s)" str ;
                Buffer.contents b 
          in 
        (* because Simplify is incomplete, it may say that both PREDICATE
         * and (NOT PREDICATE) can or cannot be proved. We only trust it when
         * it really knows. *)
          let q1 = make_query str in 
          let q2 = make_query (sprintf "(NOT %s)" str) in
            (match query q1, query q2 with
            | Must_Be_True, Must_Be_False -> Must_Be_True
            | Must_Be_False, Must_Be_True -> Must_Be_False
            | _, _ -> Unknown)
        with _ -> Unknown
    end 
  
let decide (s:state) e : tp_result = 
  let result = decide s e in 
  result 

(* returns true if the given expression represents one of our fresh,
 * unknown symbolic values *) 
let is_unknown_symexp e = match e with
  | Lval(Var(va),NoOffset) when va.vname.[0] = '|' -> true 
  | _ -> false 

(* this convenience function returns the symbolic expression (and 
 * C/CIL expression) associated with 'true' or 'false'. Recall that in 
 * C we have "false == 0" and "true <> 0". *)
(* FIXME: do we want this to be an actual boolean? *)
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
    let va1 = makeVarinfo false (Printf.sprintf "%s%d" str c) (TVoid([])) in
      Lval(Var(va1),NoOffset)

let rec eval s ce = 
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
        let decision = decide s (BinOp(Eq,read_addr, written_addr, TInt(IInt,[]))) in
          match decision with
            Must_Be_True -> written_value
          | Must_Be_False -> select earlier_writes 
          | Unknown -> fresh_value () 
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
    | x, y, z ->  BinOp(x,y,z,tau)
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
    { old_state with register_file = new_sigma ; mu = [] ; z3_assumptions = [] ; cluster_assumptions = [] } 

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
    let ptr_exp_z3 = eval s ptr_exp in 
    let new_val = eval s new_val in 
    let s = update_memory s (ptr_exp) (new_val) in 
    (* magic trick: if you are continuing on a path after *p = 5, then 
     * p must be non-null! *) 
      
    let s = assume s (BinOp(Ne,ptr_exp,(se_of_bool false),TInt(IInt,[])))  (BinOp(Ne,ptr_exp,(se_of_bool false),TInt(IInt,[]))) in
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
    (* FIXME: call through a function ptr *)
    (Some (throw_away_state s))
  | Asm(_) -> (* we don't handle inline asm! *) (Some s)

let path_enumeration (target_fundec : Cil.fundec) =
  let enumerated_paths = ref [] in
  let note_path (s : state) = enumerated_paths := s :: !enumerated_paths in 
  let worklist = Stack.create () in
  let add_to_worklist state where nn nb nc =
(*    debug "adding to worklist: ";
    (match where with
      Exploring_Statement(s) -> debug "Exploring_Statement(%s). " (stmt_str s)
    | Exploring_Block(b) -> debug "Exploring_Block. "
    | Exploring_Done -> debug "Exploring_Done. "
    ); 
    debug " nn: %d, nb: %d, nc: %d.\n" (llen nn) (llen nb) (llen nc);*)
    Stack.push (state,where, nn, nb, nc) worklist 
  in
  let give_up_dont_add state stmt = 
    let state = mark_as_visited state stmt in
      note_path state
  in
  let give_up_add state stmt =
    let new_stmt = Statement(stmt, state.cluster_assumptions) in
    let state = { state with path = new_stmt  :: state.path } in
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
  let initial_state = !initial_state in 
    add_to_worklist initial_state (Exploring_Block(target_fundec.sbody)) [] [] [] ;
    while not (Stack.is_empty worklist) && (llen !enumerated_paths < 500) do
      (* 
       * state = current symex state
       * here = this dataflow place
       * nn = next normal
       * nb = next if we hit a "break;"
       * nc = next if we hit a "continue;" *)
      let state, here, nn, nb, nc = Stack.pop worklist in 
        match here with
        | Exploring_Done -> 
          (match nn with
          | [] -> note_path state
          | first :: rest -> add_to_worklist state first rest nb nc)
        | Exploring_Block(b) -> 
          (match b.bstmts with
          | [] -> add_to_worklist state (Exploring_Done) nn nb nc
          | first :: rest -> 
            let followup = (Exploring_Block { b with bstmts = rest }) in 
              add_to_worklist state (Exploring_Statement(first)) (followup :: nn) nb nc)
        | Exploring_Statement(s) when not (already_visited state s) -> 
          begin
            let state = mark_as_visited state s in
              match s.skind with
              (* possible FIXMEs for a more precise analysis *)
              | TryFinally _ | TryExcept _ -> give_up_dont_add state s
              | Return(_) -> give_up_add state s
              | Switch(exp1,block,stmts,_) ->
                let evaluated1 = symbolic_variable_state_substitute state exp1 in
                (* possible FIXME: duff's device, will that be handled properly here? *)
                let rec process_switch stmts =
                  match stmts with
                    stmt :: rest ->
                      (let followup = (Exploring_Block { block with bstmts = rest }) in
                        liter
                          (fun label ->
                            match label with
                              Case(exp2,_) ->
                                let evaluated2 = 
                                  symbolic_variable_state_substitute state (BinOp(Eq, evaluated1,exp2,intType)) 
                                in
                                let decision = decide state evaluated2 in
                                  (match decision with
                                    Must_Be_True | Unknown -> 
                                      let state = assume state evaluated2 (BinOp(Eq,evaluated1,exp2,intType))in
                                        add_to_worklist state (Exploring_Statement(stmt)) (followup :: nn) (nn :: nb) (([]) :: nc)
                                  | Must_Be_False -> ())
                          | Default _ ->
                            add_to_worklist state (Exploring_Statement(stmt)) (followup :: nn) (nn :: nb) (([]) :: nc)
                          | _ -> ()
                          ) stmt.labels; 
                        process_switch rest)
                  | [] -> ()
                in
                  process_switch block.bstmts 
              | Goto(target_stmt_ref, _) -> give_up_dont_add state s
(*                let state = { state with path = Statement(s, state.assumptions) :: state.path } in
                let nn' = lmap (fun s -> Exploring_Statement(s)) !target_stmt_ref.succs in
                  add_to_worklist state (Exploring_Statement(!target_stmt_ref)) nn' [] []*)
              | Instr il -> 
                let state = 
                  { state with path = Statement(s, state.cluster_assumptions) :: state.path } 
                in
                let new_state_opt = 
                  lfoldl (fun state_opt instr ->
                    match state_opt with
                    | None -> None
                    | Some(state) -> handle_instr instr state
                  ) (Some state) il 
                in
                  (match new_state_opt, nn with
                  | None,_ -> give_up_add state s
                  | Some(new_state), _ -> 
                    add_to_worklist new_state (Exploring_Done) nn nb nc)
              | Break _ -> 
                (match nb, nc with 
                | b_hd :: b_tl , c_hd :: c_tl -> 
                  add_to_worklist state (Exploring_Done) b_hd b_tl c_tl
                | _, _ -> give_up_dont_add state s)
              | Continue _ ->  
                let rec get_continue nb nc =
                  match nb, nc with 
                    (* in a switch *)
                  | b_hd :: b_tl , [] :: c_tl -> get_continue b_tl c_tl
                    (* in a loop *)
                  | b_hd :: b_tl ,c_hd :: c_tl ->
                    add_to_worklist state (Exploring_Done) c_hd b_tl c_tl
                  | _, _ -> give_up_dont_add state s
                in
                  get_continue nb nc
              | If(exp,then_branch,else_branch,_) -> 
                let process_assumption exp branch =
                  let evaluated = symbolic_variable_state_substitute state exp in
                  let decision = decide state evaluated in
                    match decision with
                      Must_Be_True | Unknown ->
                        let state = assume state evaluated exp in
                          add_to_worklist state (Exploring_Block(branch)) nn nb nc
                    | Must_Be_False -> give_up_add state s
                in
                let then_condition = exp in
                let else_condition = UnOp(LNot,exp,(Cil.typeOf exp)) in
                  (*                  debug "then: %s\n" (exp_str exp);*)
(*                  debug "else: %s\n" (exp_str else_condition);*)
                  process_assumption then_condition then_branch;
                  process_assumption else_condition else_branch
              | Loop(loop_block,_,break_opt,continue_opt) -> 
                    add_to_worklist state (Exploring_Block loop_block) nn (nn :: nb) ((here :: nn) :: nc) 
              | Block(b) -> add_to_worklist state (Exploring_Block b) nn nb nc 
            end 
        | _ -> add_to_worklist state (Exploring_Done) nn nb nc
    done ;
    lrev (lmap (fun state -> { state with path = lrev state.path }) !enumerated_paths)

let print_state state = 
  debug "STATE PRINTING:\n";
  liter
    (fun ps ->
      match ps with
        Statement(s,cluster_assumptions) ->
          let asstr = stmt_str s in 
            debug "\t{STATEMENT(%s)\n" asstr;
            debug "\tASSUMING[";
            liter (fun exp -> debug "%s, " (exp_str exp)) cluster_assumptions;
            debug "]}\n"
      | Assume(exp) ->
        let asstr = exp_str exp in 
          debug "\t{ASSUME(%s)}\n" asstr;
    ) state.path

let path_generation functions = 
  Z3.toggle_warning_messages true ; 
  let location_ht = hcreate 10 in
  let canonical_ht = hcreate 10 in
  let inv_canonical_stmt_ht = hcreate 10 in
  let my_canon = new canonicalizeVisitor canonical_ht inv_canonical_stmt_ht location_ht in
    lfoldl
	  (fun stmtmap (funname,fd) ->
        debug "function: %s\n" funname;
(*        dumpGlobal defaultCilPrinter Pervasives.stdout (GFun(fd,locUnknown));*)
        Pervasives.flush Pervasives.stdout;
        let fd = visitCilFunction my_canon fd in
		let feasible_paths = path_enumeration fd in 
(*          debug "\nafter feasible, %d paths\n" (llen feasible_paths);*)
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
			  | Statement(s, cluster_assumptions) ->
				let assumptions_set = ExpSet.of_enum (List.enum cluster_assumptions) in
				let location = try hfind location_ht s.sid with Not_found -> Cil.locUnknown in
                let cid,_ = hfind inv_canonical_stmt_ht s.sid in
                let old_val,_ = if StmtMap.mem (cid,s) stmtmap1 then StmtMap.find (cid,s) stmtmap1 else ExpSetSet.empty,location in
				  StmtMap.add (cid,s) ((ExpSetSet.add assumptions_set old_val),location) stmtmap1
			) StmtMap.empty only_stmts
		in
		  StringMap.add funname stmts stmtmap
	  ) StringMap.empty functions 
