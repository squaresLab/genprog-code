(* Cheap Bug Isolation *)

open Printf
open Cil

(* constants for printing stuff out *)
let fprintf_va = makeVarinfo true "fprintf" (TVoid [])
let fopen_va = makeVarinfo true "fopen" (TVoid [])
let fflush_va = makeVarinfo true "fflush" (TVoid [])
let stderr_va = makeVarinfo true "_cbi_fout" (TPtr(TVoid [], []))
let fprintf = Lval((Var fprintf_va), NoOffset)
let fopen = Lval((Var fopen_va), NoOffset)
let fflush = Lval((Var fflush_va), NoOffset)
let stderr = Lval((Var stderr_va), NoOffset)

(* the schemes: *)
let do_returns = ref false
let do_branches = ref false
let do_sk = ref false
let do_all = ref false

(* This visitor stuff is taken from coverage and walks over the C program
 * AST and builds the hashtable that maps integers to statements. *) 

let counter = ref 1 
let get_next_count () = 
  let count = !counter in 
  incr counter ;
  count 
(* This makes a deep copy of an arbitrary Ocaml data structure *) 
let copy (x : 'a) = 
  let str = Marshal.to_string x [] in
  (Marshal.from_string str 0 : 'a) 
  (* Cil.copyFunction does not preserve stmt ids! Don't use it! *) 

class numToZeroVisitor = object
  inherit nopCilVisitor
  method vstmt s = s.sid <- 0 ; DoChildren
end 
let my_zero = new numToZeroVisitor

let coverage_ht = Hashtbl.create 4096  

let can_trace sk = match sk with
  | Instr _ 
  | Return _  
  | If _ 
  | Loop _ 
  -> true

  | Goto _ 
  | Break _ 
  | Continue _ 
  | Switch _ 
  | Block _ 
  | TryFinally _ 
  | TryExcept _ 
  -> false 

class numVisitor = object
  inherit nopCilVisitor
  method vblock b = 
    ChangeDoChildrenPost(b,(fun b ->
      List.iter (fun b -> 
        if can_trace b.skind then begin
          let count = get_next_count () in 
          b.sid <- count ;
          let rhs = 
              let bcopy = copy b in
              let bcopy = visitCilStmt my_zero bcopy in 
              bcopy.skind
          in 
          Hashtbl.add coverage_ht count rhs
          (* the copy is because we go through and update the statements
           * to add coverage information later *) 
        end else begin
          b.sid <- 0; 
        end ;
      ) b.bstmts ; 
      b
    ) )
end 

let site = ref 0

let label_count = ref 0

let variables : (Cil.varinfo, Cil.typsig) Hashtbl.t ref = ref (Hashtbl.create 10)
let global_variables = Hashtbl.create 10

(* maps site numbers to location, scheme, and associated expression *)

let site_ht : (int, (Cil.location * string * Cil.exp)) Hashtbl.t = Hashtbl.create 10

(* creates a new site and returns the Const(str) to be
 * passed to fprintf in the instrumented program. Used to also return
 * the site number but I don't think it's useful, really. *)  

let get_next_site scheme exp l = 
  let count = !site in
    incr site ;
    Hashtbl.add site_ht count (l,scheme,exp);
    let str = (sprintf "%d" count)^",%d\n" in
      (Const(CStr(str)))

(* predicates now mean "sites", more or less *)

(* generate printfs etc to be added to the program source code s.t.,
 * when executed, the instrumented program prints the info out to the
 * right place. *)

let make_label () =
  let label = Printf.sprintf "claire_pred%d" !label_count in 
    incr label_count;
    [Label(label,!currentLoc,false)]

let flush_instr () = Call(None,fflush,[stderr],!currentLoc)

let make_printf_instr args = Call(None,fprintf,(stderr::args),!currentLoc)

let make_printf_stmt do_flush args =
  let printf_instr = make_printf_instr args in
  let stkind = 
    if do_flush then begin
	(* TODO: is it OK to make these one statement instead of two?
	   I think it is, but CHECK. Same ? applies below to returns. *)
	Instr([printf_instr;flush_instr()]) 
    end else Instr([printf_instr]) in
    {(mkStmt stkind) with labels = make_label()}
      
let instr_branch e1 l s =
  let str_exp = get_next_site "branches" e1 l in
    make_printf_stmt true [str_exp;e1]

(*let compare_value_zero exp bin_comp loc =
  let cond = BinOp(bin_comp,exp,zero,(TInt(IInt,[])) in
  let str_exp = get_next_site "returns" cond loc in
    make_printf false [str_exp;cond]*)
    
let sig_ret_instrs () = 
  [(make_printf_instr [Const(CStr("function return"))]);
   flush_instr()]

let sig_ret_stmt () = mkStmt (Instr(sig_ret_instrs()))

let instr_rets e l s =
  let conds = 
    List.map (fun cmp -> BinOp(cmp,e,zero,(TInt(IInt,[])))) [Lt;Gt;Eq] in
  let exp_and_conds = 
    List.map (fun cond -> cond, (get_next_site "returns" cond l)) conds in
  let instrs1 =
    List.map (fun (str_exp,cond) ->
		make_printf_instr [str_exp;cond]) exp_and_conds in
  let instrs2 = sig_ret_instrs() in
  let instr_list : Cil.instr list = instrs1 @ instrs2 in
    {(mkStmt (Instr(instr_list))) with labels = make_label()}

(* insert before takes the result of a function to a statement to
 * generate a list of new statements, and then makes a new statement
 * consisting of a block in which the generate statements are inserted
 * before the argument s *)

let insert_before stmts s = mkStmt (Block(mkBlock [stmts;s]))

let instrument s tv fn = 
  if tv then begin
    insert_before (fn s) s
  end else s

class instrumentVisitor = object
  inherit nopCilVisitor

  method vstmt s = 
    ChangeDoChildrenPost
      (s, 
       fun s -> 
	 match s.skind with 
	     If(e1,b1,b2,l) -> instrument s !do_branches (instr_branch e1 l)
	   | Return(Some(e), l) -> instrument s !do_returns (instr_rets e l)
           | Return(None, l) -> insert_before (sig_ret_stmt ()) s  
	   | _ -> s)


		   (*  | Instr(ilist) -> (* Partial.callsEndBasicBlocks *should* (by its 
								own documentation?) put calls in their own blocks.
								If not, more hackery will be required. *)
			   let new_stmts : stmt list = 
				 List.fold_left
				   (fun (accum : stmt list) ->
					  fun (i : instr) ->
						(visit_instr i) @ accum
				   ) [] ilist 
			   in
			   let new_block = (Block(mkBlock (s::new_stmts))) in
				 mkStmt new_block*)
      

  method vfunc fdec =
    
    (* first, get a fresh version of the variables hash table *)
    variables := Hashtbl.copy global_variables;
    (* next, replace all variables in the hashtable with their name and type from here *)
    List.iter 
      (fun v -> Hashtbl.replace !variables v (typeSig v.vtype))
      (fdec.sformals @ fdec.slocals);
    DoChildren
end

let ins_visitor = new instrumentVisitor
let num_visitor = new numVisitor

let main () = begin
  let usageMsg = "Prototype Cheap Bug Isolation Instrumentation\n" in
  let do_pa = ref false in
  let do_cov = ref false in

  let filenames = ref [] in
    (* question: if we're trying to mirror coverage.ml, should we just
       include it so that changes are consistent between files? Hm. *)

  (* FIXME: fix the spaces in the argument descriptions here *)

  let argDescr = [ 
    "--returns", Arg.Set do_returns, " Instrument return values.";
    "--branches", Arg.Set do_branches, " Instrument branches.";
    "--sk", Arg.Set do_sk, " Instrument scalar-pairs.";
    "--default", Arg.Set do_all, " Do all three.";
    "--cov", Arg.Set do_cov, " track information that would be computed by coverage.
                               Does the --calls and --empty options to
                               coverage by default, but not --every-instr." ;
    "--pa", Arg.Set do_pa, " do static pointer analysis to reduce
                             overhead of comparing all variables to
                             one another. For the time being, I'm pretty 
                             sure that this only works if you're only
                             instrumenting *one* file, so don't be stupid 
                             about it." 
  ] in
  let handleArg str = filenames := str :: !filenames in
    Arg.parse (Arg.align argDescr) handleArg usageMsg ;

    if !do_all then begin
      do_returns := true; do_branches := true; do_sk := true
    end;
      
    Cil.initCIL();

(* this map has several purposes:
 * 1) make a list of cil files from the filenames passed in 
 * 2) convert them all to CFG form
 * 3) number them properly for the coverage hashtable info
 * 4) add all defined global variables into a table.
 * 5) build pointer analysis table, if specified 
 * questions: I can only do for one file at a time b/c we don't clear
 * state, I guess? 
 *)
    let files = List.map 
      (fun filename -> 

	 let file = Frontc.parse filename () in
	   (* equivalent of do_cfg option to coverage *)
	   Partial.calls_end_basic_blocks file;
	   Cfg.computeFileCFG file;

	   visitCilFileSameGlobals num_visitor file ; 

	   List.iter 
	     (fun g ->
		match g with 
		  | GVarDecl(vi, l) -> Hashtbl.add global_variables vi (typeSig vi.vtype)
		  | GVar(vi, ii, l) -> Hashtbl.add global_variables vi (typeSig vi.vtype)
		  | _ -> ()) 
	     file.globals;
      
	   Ptranal.analyze_file file;

	   file) !filenames in

      List.iter 
	(fun file -> 
	   begin
	     visitCilFileSameGlobals ins_visitor file;
	     
	     let new_global = GVarDecl(stderr_va,!currentLoc) in 
	       file.globals <- new_global :: file.globals ;
	       
	       let fd = Cil.getGlobInit file in 
	       let lhs = (Var(stderr_va),NoOffset) in 
	       let data_str = file.fileName ^ ".preds" in 
	       let str_exp = Const(CStr(data_str)) in 
	       let str_exp2 = Const(CStr("wb")) in 
	       let instr = Call((Some(lhs)),fopen,[str_exp;str_exp2],!currentLoc) in 
	       let new_stmt = Cil.mkStmt (Instr[instr]) in 
	       let new_stmt = {new_stmt with labels = make_label()} in 
		 fd.sbody.bstmts <- new_stmt :: fd.sbody.bstmts ; 
		 iterGlobals file (fun glob ->
				     dumpGlobal defaultCilPrinter stdout glob ;
				  ) ; 

		 let sites = file.fileName ^ ".sites" in
		 let fout = open_out_bin sites in
		   Marshal.to_channel fout site_ht [] ;
		   Marshal.to_channel fout coverage_ht [] ;
		   (* FIXME: this shouldn't affect reading it back in,
		      yet, at least; but CHECK *)
		   close_out fout ;
	   end
	) files;
end ;;

main () ;;
    
