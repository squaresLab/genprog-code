open Batteries
open Map
open Set
open Utils
open Cil
open Z3
open Cilprinter 

(**********************************************************************
 * Path Enumeration
 *
 * In this step we take as input a C function and statically enumerate a
 * set of paths through that function. A path is a list of executed
 * statements (e.g., "x=2;" along the path) intermixed with assumptions
 * (i.e., if the path corresponds to the true branch in "if (x < 5) {...}"
 * then you can assume "x < 5" after that point on the path). 
 *
 * Because a function may have many paths, we use a worklist to keep track
 * of which parts we are currently exploring. 
 *)

class noteLocationVisitor loc_ht = object
  inherit nopCilVisitor
  method vstmt s = hadd loc_ht s.sid !currentLoc; DoChildren
end

type path_exploration = 
  | Exploring_Block of Cil.block 
  | Exploring_Statement of Cil.stmt  
  | Exploring_Done 

type path_step =
  | Statement of Cil.stmt 
  | Assume of Cil.exp 

type path = path_step list 

let path_enumeration (target_fundec : Cil.fundec) =
  let enumerated_paths = ref [] in (* gather up our final answer *) 
  let note_path (p : path) = enumerated_paths := p :: !enumerated_paths in 

  (*
   * Each worklist element will contain a five-tuple: 
   * (1) the visited path so far,
   * (2) the current place to explore
   * (3) where to go if the current exploration terminates normally
   * (4) where to go if the current exploration is "break;" 
   * (5) where to go if the current exploration is "continue;" 
   *)
  let worklist = Queue.create () in

  let add_to_worklist path where nn nb nc =
    (* Possible FIXME: To avoid infinite loops in our analysis, if
     * we would enqueue a visit to a statement we've already visited _in
     * this path's history_, we instead give up immediately. *) 
    match where with
    | Exploring_Statement(s) when 
      List.exists (fun already_visited -> match already_visited with
        | Statement(visited_s) when visited_s.sid = s.sid -> true
        | _ -> false
      ) path -> Queue.add (path,Exploring_Done,[],[],[]) worklist
    | _ -> Queue.add (path,where,nn,nb,nc) worklist 
  in 

  (* We start enumerating at the first line of the function body. *) 
  add_to_worklist [] (Exploring_Block(target_fundec.sbody)) [] [] [] ;

  while not (Queue.is_empty worklist) do
    (* nn = next normal
     * nb = next if we hit a "break;"
     * nc = next if we hit a "continue;" *)
    let path, here, nn, nb, nc = Queue.pop worklist in 
    let give_up stmt = 
      (* At various times we will stop exploring along a path but we'll
       * still want to report that path. This function handles such cases. *) 
      add_to_worklist (Statement(stmt) :: path) (Exploring_Done) [] [] []
    in 

    (* The heart of path enumeration is a giant switch statement on 
     * the structure of the code being explored. *) 
    match here with

    | Exploring_Done -> begin 
        match nn with
        | [] -> note_path path (* we're done with this path! *) 
        | first :: rest -> 
          (* We might be done exploring the inside of a "then-branch",
           * for example, but we should then fall through and explore
           * whatever came after that whole if. *) 
          add_to_worklist path first rest nb nc 
      end 

    | Exploring_Block(b) -> begin
        match b.bstmts with
          | [] -> add_to_worklist path (Exploring_Done) nn nb nc 
          | first :: rest -> 
            (* if we hit a block with statements "S1; S2; S3;", 
             * we'll schedule a visit to S1 right away and put
             * "S2; S3;" on the list of things to visit next. *) 
            let followup = (Exploring_Block { b with bstmts = rest }) in 
            add_to_worklist path (Exploring_Statement(first))
              (followup :: nn) nb nc 
      end 

    | Exploring_Statement(s) -> begin
      match s.skind with

      | Instr _ -> (* e.g., handle "x = 2;" *) 
        add_to_worklist (Statement(s) :: path) (Exploring_Done) nn nb nc

      | Return _ -> 
        (* Possible FIXME: This is not (yet) an interprocedural analysis. *)
        give_up s

      | Goto(goto_target,_) -> 
        (* Possible FIXME: Handle totally unstructured programs. *) 
        give_up s 

      | Switch _ -> 
        (* Possible FIXME: Handle switch statements. *) 
        give_up s 

      | TryFinally _ (* Microsoft C Extension *) 
      | TryExcept _ (* Microsoft C Extension *) 
      -> give_up s

      | Break _ -> begin
          match nb, nc with 
          | b_hd :: b_tl , c_hd :: c_tl -> 
            add_to_worklist path (Exploring_Done) b_hd b_tl c_tl 
          | _, _ -> 
            (* break with no enclosing loop structure *)
            give_up s
        end 

      | Continue _ -> begin 
          match nb, nc with 
          | b_hd :: b_tl , c_hd :: c_tl -> 
            add_to_worklist path (Exploring_Done) c_hd b_tl c_tl 
          | _, _ -> 
            (* continue with no enclosing loop structure *) 
            give_up s
        end 

      | If(exp,then_branch,else_branch,_) -> 
        (* As usual in Axiomatic Semantics, when exploring the Then-Branch 
         * you get to assume the conditional is True, and when exploring
         * the Else-Branch you get to assume that it is false. *) 
        let then_condition = exp in
        let else_condition = UnOp(LNot,exp,(Cil.typeOf exp)) in (* == !exp *)
        add_to_worklist  ((Assume then_condition) :: path) 
          (Exploring_Block(then_branch)) nn nb nc ;
        add_to_worklist  ((Assume else_condition) :: path) 
          (Exploring_Block(else_branch)) nn nb nc 

      | Loop(loop_block,_,break_opt,continue_opt) -> 
        (* In CIL, while (b) { c } becomes
         *
         * while (1) {
         *   if (!b) break; 
         *   c;
         * } 
         *
         * Thus all Loops are the equivalent of "while true". *)  
        add_to_worklist path (Exploring_Block loop_block) 
          (here :: nn) 
          (nn :: nb) 
          ((here :: nn) :: nc) 

      | Block(b) -> 
        add_to_worklist path (Exploring_Block b) nn nb nc 

    end 
  done ;

  (* We prepended statements to the front of paths, so we have to
   * reverse them to get the right history order. *) 
  let paths = lmap lrev !enumerated_paths in 

  debug "tigen: %s: %d path(s) enumerated\n" 
    target_fundec.svar.vname 
    (List.length paths) ;
paths

(**********************************************************************
 * Symbolic Variable State (or Symbolic Register File) 
 *
 * Our state is a simple mapping from variable names to symbolic
 * expressions. We use the existing Cil.exp expression type for
 * symbolic expressions as well.
 *)

type symbolic_variable_state = Cil.exp StringMap.t 

let empty_symbolic_variable_state = StringMap.empty 

(* The usual state update: sigma[variable_name := new_value] *) 
let symbolic_variable_state_update 
  (sigma : symbolic_variable_state)  
  (variable_name : string)
  (new_value : Cil.exp) 
  : symbolic_variable_state
  =
  StringMap.add variable_name new_value sigma 

(*
 * Look up a variable in the symbolic state. For example, if we know that
 * [x=10] and [y=z+3] and we lookup "y", we expect to get "z+3" out.
 *)
let symbolic_variable_state_lookup 
      (sigma : symbolic_variable_state) 
      (variable : Cil.exp) 
      : Cil.exp =
  let found = match variable with
  | Lval(Var(va),NoOffset) -> 
    begin
      try
        Some(StringMap.find va.vname sigma)
      with Not_found -> 
        None
    end 
  | Lval(Mem(exp),NoOffset) -> None (* cannot handle memory access *) 
  | Lval(lhost,Field(_)) -> None (* cannot handle field access *) 
  | Lval(lhost,Index(_)) -> None (* cannot handle array index *) 
  | _ -> None (* not a variable *) 
  in 
  match found with
  | Some(answer) -> answer
  | None -> variable 

(*
 * Rewrite an expression based on the current symbolic state.  For example,
 * if we know that [x=10] and [y=z+3] and we lookup "sin(x+y)", we expect
 * to get "sin(10+z+3)". 
 *)
class substituteVisitor (sigma : symbolic_variable_state) = object
  inherit nopCilVisitor
  method vexpr e = 
    ChangeDoChildrenPost(e,(fun e ->
      symbolic_variable_state_lookup sigma e
    ))
end 

let symbolic_variable_state_substitute 
      (sigma : symbolic_variable_state) 
      (exp : Cil.exp) 
      : Cil.exp =
  let sv = new substituteVisitor sigma in 
  visitCilExpr sv exp 

(**********************************************************************
 * Symbolic Execution
 *
 * We build on the "symbolic register file" code above to implement a more
 * generic symbolic execution. Given a "path" (a sequence of statements and
 * assumptions) we update our symbolic register file when we encounter
 * assignment statements and then record every assumption as we make it. 
 *
 * Later, we'll feed those assumptions as constraints to an automated
 * theorem prover to generate test inputs. 
 *)

type symex_state = {
  register_file : symbolic_variable_state ;
  assumptions : Cil.exp list ;
} 

let empty_symex_state = {
  register_file = empty_symbolic_variable_state ;
  assumptions = [] ; 
} 
  
class noteVarVisitor (varset : StringSet.t ref) = object
  inherit nopCilVisitor
  method vvrbl v = 
    varset := StringSet.add v.vname !varset ;
    DoChildren
end 

(* Given a path, produce a final symbolic execution state (a symbolic
 * register file and set of assumptions) associated with the end of that
 * path. *) 
let symbolic_execution (path : path) =
  if true then begin (* enable this for symex debugging *) 
    debug "\ntigen: symex:\n" ;
    List.iter (fun step -> 
      match step with
      | Statement(s) -> 
        debug "%s\n" (Pretty.sprint ~width:80 (dn_stmt () s)) 
      | Assume(e) -> 
        debug "Assume %s\n" (Pretty.sprint ~width:80 (dn_exp () e)) 
    ) path ;
  end ;

  let state = empty_symex_state in 
  (* For each variable mentioned in the path, assign it a default,
   * arbitrary value. We use "_x" to represent the unknown initial
   * value of variable "x". 
   *
   * Possible FIXME: This may not handle memory (i.e., arrays, pointers)
   * correctly. *) 
  let variables = ref StringSet.empty in 
  let nv = new noteVarVisitor variables in 
  List.iter (fun step -> match step with
    | Statement(s) -> ignore (visitCilStmt nv s) 
    | Assume(e) -> ignore (visitCilExpr nv e) 
  ) path ; 
  let new_register_file = StringSet.fold (fun variable_name state ->
    let new_value = Lval(Var(makeVarinfo false ("_" ^ variable_name) 
      (TVoid [])),NoOffset) in
    symbolic_variable_state_update state variable_name new_value 
  ) !variables state.register_file in 
  let state = { state with register_file = new_register_file } in 

  (*
   * Now we walk down every step in the path, handling assignment
   * statements (which update the symbolic register file) and assumptions
   * (which are evaluated and gathered up). 
   *)
	lfoldl (fun (path,state) step ->
      match step with
      | Assume(e) -> (* recall that we get these from "if" statements. *)
		let evaluated_e = symbolic_variable_state_substitute 
          state.register_file e in
		let state = { state with assumptions = evaluated_e :: state.assumptions} in
		  path@[step,state],state
      | Statement(s) -> begin
		match s.skind with
		| Instr(il) -> 
		  let il',state' =
			lfoldl (fun (instrs,state) instr ->
			  match instr with
			  | Set((Var(va),NoOffset),rhs,f) -> 
				let evaluated_rhs = symbolic_variable_state_substitute 
				  state.register_file rhs 
				in 
				let new_register_file = 
				  symbolic_variable_state_update state.register_file va.vname evaluated_rhs 
				in
				let new_instr = (Set((Var(va),NoOffset),evaluated_rhs,f)) in
				let state' =  { state with register_file = new_register_file } in
				  new_instr::instrs,state'
			  | Set((Mem(_),_),_,_) 
			  (* Possible FIXME: cannot handle memory accesses like *p *) 
			  | Set((_,Field(_,_)),_,_)  
			  (* Possible FIXME: cannot handle field accesses like e.fld *) 
			  | Set((_,Index(_,_)),_,_) 
			  (* Possible FIXME: cannot handle array indexing like a[i] *) 
			  | Call _ (* Possible FIXME: cannot handle function calls *) 
			  | Asm _ -> (* cannot handle inline ASM *) instr::instrs, state
			) ([],state) il 
		  in
			path @ [(Statement({s with skind = Instr(il')}), state')], state'
		| _ -> path @ [(step,state)], state 
      end
	) ([],state) path 

(**********************************************************************
 * Constraint Solving
 *
 * Given the final symbolic excution state corresponding to a path,
 * we now want to generate constraints for a theorem prover and solve those
 * constraints. For example, if we know that "x > 10" && "x < 15", we'd
 * like to come up with a concrete assignment like "x == 11". That concrete
 * value is a test input that forces execution down the path in question!
 *)

(* The final constraint solution will be a mapping from variable names to 
 * textual values (i.e., from "x" to "11"). Possible FIXME: This is
 * unlikely to be sufficient for more complicated values (e.g., pointers,
 * arrays).  *) 
type solved_constraints = string StringMap.t 

let solve_constraints
  (target_fundec : Cil.fundec) (* method to generate inputs for *) 
  (state : symex_state) (* final symex state associated with a path *)
  =
  (* We use the Z3 automated theorem prover and SMT solver. We need
   * more than a "yes or no" answer: we need a satisfying assignment (also
   * called a "model"). So we tell Z3 that we want such a model. *) 
  let ctx = mk_context_x [| "MODEL", "true" |] in 
  if false then begin (* enable this for Z3 debugging *) 
    Z3.trace_to_stdout ctx ;  
  end ; 
  (* Much of the work here is converting from CIL Abstract Syntax Trees to
   * Z3 Abstract Syntax Trees. *) 
  let int_sort = mk_int_sort ctx in (* Possible FIXME: reals unhandled *) 
  let zero_ast = mk_int ctx 0 int_sort in 
  let undefined_ast = zero_ast in 

  (* Every time we encounter the same C variable "foo" we want to map
   * it to the same Z3 node. We use a hash table to track this. *) 
  let symbol_ht = Hashtbl.create 255 in
  let var_to_ast str = 
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
    | UnOp(LNot,_,_) 
    | BinOp(Lt,_,_,_) 
    | BinOp(Le,_,_,_) 
    | BinOp(Gt,_,_,_) 
    | BinOp(Ge,_,_,_) 
    | BinOp(Eq,_,_,_) 
    | BinOp(Ne,_,_,_) -> true
    | _ -> false
  in 

  (* This is the heart of constraint generation. For every CIL expression
   * (e.g., "x > 10"), convert it to an equivalent Z3 expression. *) 
  let rec exp_to_ast (exp : Cil.exp) : Z3.ast = match exp with
    | Const(CInt64(i,_,_)) -> 
      (* Possible FIXME: large numbers are not handled *) 
      let i = Int64.to_int i in 
      Z3.mk_int ctx i int_sort 

    | Const(CChr(c)) -> 
      (* Possible FIXME: characters are justed treated as integers *) 
      let i = Char.code c in
      Z3.mk_int ctx i int_sort

    | Const(_) -> 
      (* Possible FIXME: reals, enums, strings, etc., are not handled *) 
      undefined_ast

    | Lval(Var(va),NoOffset) -> var_to_ast va.vname 

    | Lval(_) -> 
      (* Possible FIXME: var.field, *p, a[i], etc., are not handled *) 
      undefined_ast

    | UnOp(Neg,e,_) -> mk_unary_minus ctx (exp_to_ast e) 
    | UnOp(LNot,e,_) when is_binop e -> mk_not ctx (exp_to_ast e) 
    | UnOp(LNot,e,_) -> mk_eq ctx (exp_to_ast e) (zero_ast) 

    | BinOp(PlusA,e1,e2,_) -> mk_add ctx [| exp_to_ast e1; exp_to_ast e2|]
    | BinOp(MinusA,e1,e2,_) -> mk_sub ctx [| exp_to_ast e1; exp_to_ast e2|]
    | BinOp(Mult,e1,e2,_) -> mk_mul ctx [| exp_to_ast e1; exp_to_ast e2|]
    | BinOp(Div,e1,e2,_) -> 
      let ast2 = exp_to_ast e2 in 
      let not_div_by_zero = mk_distinct ctx [| zero_ast ; ast2 |] in 
      Z3.assert_cnstr ctx not_div_by_zero  ; 
      mk_div ctx (exp_to_ast e1) ast2 
    | BinOp(Mod,e1,e2,_) -> mk_mod ctx (exp_to_ast e1) (exp_to_ast e2) 
    | BinOp(Lt,e1,e2,_) -> mk_lt ctx (exp_to_ast e1) (exp_to_ast e2) 
    | BinOp(Le,e1,e2,_) -> mk_le ctx (exp_to_ast e1) (exp_to_ast e2) 
    | BinOp(Gt,e1,e2,_) -> mk_gt ctx (exp_to_ast e1) (exp_to_ast e2) 
    | BinOp(Ge,e1,e2,_) -> mk_ge ctx (exp_to_ast e1) (exp_to_ast e2) 
    | BinOp(Eq,e1,e2,_) -> mk_eq ctx (exp_to_ast e1) (exp_to_ast e2) 
    | BinOp(Ne,e1,e2,_) -> 
      mk_distinct ctx [| (exp_to_ast e1) ; (exp_to_ast e2) |] 
    | CastE(_,e) -> exp_to_ast e (* Possible FIXME: (int)(3.1415) ? *) 
    | _ -> 
      (* addrof, startof, alignof, sizeof, etc., are not handled *) 
      undefined_ast
  in 

  (* For every assumption along the path, convert it to a Z3 expression
   * and tell the theorem prover to assert it as true (i.e., as a
   * constraint). *) 
  List.iter (fun cil_exp -> 
    try 
      let z3_ast = exp_to_ast cil_exp in 
      (*
      debug "tigen: asserting %s\n" 
        (Z3.ast_to_string ctx z3_ast) ; 
      *) 
      Z3.assert_cnstr ctx z3_ast ; 
    with _ -> begin  
    (*
      debug "tigen: cannot convert %s to Z3\n"
        (Pretty.sprint ~width:80 (dn_exp () cil_exp)) ;
        *) 
        ()
    end 
  ) state.assumptions ; 

  (* Now that we've put in all of the constraints, query the theorem
   * prover to see if there is a model that can satisfy them all at the
   * same time. *) 
  let made_model, model = Z3.check_and_get_model ctx in 
	  Z3.del_context ctx; 
	made_model = L_TRUE

(* a symbolic statement is a string representation of that statement
   and a set of strings representing symbolic/substituted
   representations of the expressions corresponding to assumptions
   that hold at that statement *)

let path_generation file fht functions = 
  let canon_ht = hcreate 10 in
  let canonical_stmt str = 
	let num = ht_find canon_ht str (fun _ -> 0) in
	  hrep canon_ht str (num + 1);
	  Printf.sprintf "%s%d" str num
  in
  Z3.toggle_warning_messages true ; 
  let location_ht = hcreate 10 in
	visitCilFileSameGlobals (new noteLocationVisitor location_ht) file;
	lfoldl
	  (fun stmtmap funname ->
		let fd = hfind fht funname in
		let paths = path_enumeration fd in 
		let paths = first_nth paths 500 in (* don't take too long! *) 
	  (* maybe solve paths as we're enumerating them? *)

		let symbolic_states = lmap symbolic_execution paths in 

		let feasible_paths = 
		  lmap fst (lfilt (fun (path,state) -> solve_constraints fd state) symbolic_states)
		in
		  debug "%d feasible paths\n" (llen feasible_paths);
		let all_states = lflat feasible_paths in
		let only_stmts = 
		  lfilt 
			(fun (path_step,_) -> match path_step with Assume _ -> false | _ -> true) 
			all_states
		in
		let stmts = 
		  lfoldl 
			(fun stmtmap1 (path_step,state) ->
			  match path_step with
			  | Statement(s) ->
				let printer = Cilprinter.noLineCilPrinter in
				let stmt_str = Pretty.sprint ~width:80 (printStmt printer () s) in
				let assumptions_lst = 
				  lmap (fun exp -> Pretty.sprint ~width:80 (printExp printer () exp)) state.assumptions
				in
				let assumptions_set = StringSet.of_enum (List.enum assumptions_lst) in
				let location = hfind location_ht s.sid in
				  StringMap.add stmt_str (assumptions_set,location) stmtmap1
			) StringMap.empty only_stmts
		in
		  StringMap.add funname stmts stmtmap
	  ) StringMap.empty functions 
