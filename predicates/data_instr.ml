open Pretty
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

let labels = ref false

(* the schemes: *)
let do_returns = ref false
let do_branches = ref false
let do_sk = ref false
let do_all = ref false

(* Hashtbl for globals *)
let global_vars = Hashtbl.create 100

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

(* maps site numbers to location, scheme, and associated expression *)

let site_ht : (int, (Cil.location * string * Cil.exp)) Hashtbl.t = Hashtbl.create 10

(* creates a new site and returns the Const(str) to be
 * passed to fprintf in the instrumented program. Used to also return
 * the site number but I don't think it's useful, really. *)  

let get_next_site scheme exp l = 
  let count = !site in
    incr site ;
    Hashtbl.add site_ht count (l,scheme,exp);
    let str = (Printf.sprintf "%d" count)^",%d\n" in
      (count, (Const(CStr(str))))

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
	mkStmt stkind
      
let instr_branch e1 l s =
  let _,str_exp = get_next_site "branches" e1 l in
    make_printf_stmt true [str_exp;e1]
    
let instr_rets e l s =
  let conds = 
    List.map (fun cmp -> BinOp(cmp,e,zero,(TInt(IInt,[])))) [Lt;Gt;Eq] in
  let exp_and_conds = 
    List.map (fun cond -> 
				let _, s = get_next_site "returns" cond l in 
				  (s,cond)) conds in
  let instrs1 =
    List.map (fun (str_exp,cond) ->
		make_printf_instr [str_exp;cond]) exp_and_conds in
	mkStmt (Instr(instrs1 @ (flush_instr () :: [])))

class instrumentVisitor = object(self)
  inherit nopCilVisitor

  val local_vars = Hashtbl.create 100

  method print_vars lhs rhs = (* lval and rvals are exps *)
	let count,str = get_next_site "scalar-pairs" lhs !currentLoc in

    (* this code contains a tiny optimization: if this variable is being set to
	 * that variable, no need to compute comparisons between them *) 

    let rec getname exp = 
	  let rec getoffset o =
		match o with
			NoOffset -> ""
		  | Field(fi, o) -> "." ^ fi.fname ^ (getoffset o)
		  | Index(_) -> "[sub]" 
	  in
      match exp with 
		| Lval(Var(vi), o) -> vi.vname ^ (getoffset o)
		| Lval(Mem(e), o) ->
			let memstr = Pretty.sprint 80 (d_exp () e) in
			  memstr ^ (getoffset o)
		| CastE(t, e) -> getname e
		| _ -> ""
	in


	let lname,rname = getname lhs, getname rhs in
	let ltype = typeOf lhs in

	let comparable rhs =
      let rtype = typeOf rhs in
      (* can we compare these types?
       * If so, get the appropriate casts! *)
      let lhs_pointer, rhs_pointer = (isPointerType ltype), (isPointerType rtype) in
      let lhs_array, rhs_array = (isArrayType ltype), (isArrayType rtype) in
      let lhs_arith, rhs_arith = (isArithmeticType ltype), (isArithmeticType rtype) in
      let lhs_integ, rhs_integ = (isIntegralType ltype), (isIntegralType rtype) in
		(lhs_pointer || lhs_array || lhs_arith || lhs_integ) &&
		  (rhs_pointer || rhs_array || rhs_arith || rhs_integ)
    in
	  
	let print_one_var var =
	  let cast_to_ULL va = mkCast va (TInt(IULong,[])) in
	  let format_str lval = 
		let typ = typeOf lval in
		  if (isPointerType typ) || (isArrayType typ) then ("%u", (cast_to_ULL lval))
		  else if (isIntegralType typ) then ("%d",lval) else ("%g",lval)
	  in		
	  let lname = getname var in
	  let lformat,exp = format_str var in
	  let str = (Printf.sprintf "%d,%s," count lname) ^ lformat ^"\n" in
		make_printf_instr [(Const(CStr(str)));exp]
	in
	let first_print = print_one_var lhs in
	let comparables = 
	  List.flatten
		(List.map
		   (fun vars ->
			  Hashtbl.fold 
				(fun _ -> fun vi -> fun accum ->
				   if (not (vi.vname = lname))
					 && (not (vi.vname = rname)) 
					 && (comparable (Lval(var(vi)))) then
					   (Lval(var(vi))) :: accum else accum) vars []) [local_vars;global_vars])
	in
	  first_print :: (List.map print_one_var comparables) @ (flush_instr() :: [])
	
  method vstmt s = 
    let makeBS s = mkStmt (Block (mkBlock s)) in
	  
    (* insert before takes the result of a function to a statement to
     * generate a statement consisting of a list of instructions,
     * though this is not enforced/required by the types or anything,
     * and then makes a new statement 
     * consisting of *two* blocks, one for the generated statement
     * and one for the argument s. Otherwise we'll never move anything
     * in coverage, ever! CHECK is it reasonable to currently not
     * label sk-instrumentation? If not, how to do it, since we
     * instrument sk at the instruction level and labels are at the
     * statement level? We'll want to separate them into another block! *)
    let insert_before stmts s =
	  let instr_list_bs =  { (makeBS [stmts]) with labels = if !labels then make_label() else []} in
		(* CHECK: is it the case that this label is working out properly? 
		   I think it is, but am not totally sure *)
	  let s_bs = makeBS [s] in
		makeBS [instr_list_bs;s_bs]
	in
	let instrument_stmt s tv fn = 
      if tv then begin
		insert_before (fn s) s
      end else s in
      ChangeDoChildrenPost
	(s, 
	 fun s -> 
	   match s.skind with 
	       If(e1,b1,b2,l) -> instrument_stmt s !do_branches (instr_branch e1 l)
	     | Return(Some(e), l) -> 
		 let etyp = typeOf e in
		 let comparable = 
		   ((isPointerType etyp) || (isArrayType etyp) ||
		      (isArithmeticType etyp) || (isIntegralType etyp))  
		   && (not (isConstant e)) in
		     if comparable then
		       instrument_stmt s !do_returns (instr_rets e l) 
		     else s
	     | _ -> s)

  method vfunc fdec = Hashtbl.clear local_vars; DoChildren

  method vinst i = 
	(* do I want to label these in a new block and, if so, how do I do
	   that? *)
    if !do_sk then begin
      let ilist = 
	match i with 
	    Set((h,o), e, l) 
	  | Call(Some((h,o)), e, _, l) ->
		  begin
			(match h with
				Var(vi) -> 
				  if not vi.vglob then 
					Hashtbl.replace local_vars vi.vname vi
			   | _ -> ());
			let instr =
			  (* consider this rule a gigantic heuristic for what we can handle
			   * easily. Memory locations on the left-hand side which include a
			   * field suggest a struct/value we might care about. So even if we
			   * can't resolve it to a varinfo (sadly, pointer analysis appears
			   * kind of unhelpful here), we do instrument it. Clearly this is
			   * imperfect; we'll see how much it screws us up. *)
			  match (h,o) with
				  (Var(_), _) 
				| (_, Field(_)) -> true
				| _ -> false
			in
			  if instr then begin
				let ps = self#print_vars (Lval(h,o)) e in
				  (i :: ps)
			  end else [i]
		  end
	  | _ -> [i] in
	ChangeTo ilist
	end else DoChildren
end

let ins_visitor = new instrumentVisitor
let num_visitor = new numVisitor

let main () = begin
  let usageMsg = "Prototype Cheap Bug Isolation Instrumentation\n" in
  let do_cov = ref false in

  let filenames = ref [] in
    (* question: if we're trying to mirror coverage.ml, should we just
       include it so that changes are consistent between files? *)

  let argDescr = [ 
    "--returns", Arg.Set do_returns, " Instrument return values.";
    "--branches", Arg.Set do_branches, " Instrument branches.";
    "--sp", Arg.Set do_sk, " Instrument scalar-pairs.";
    "--default", Arg.Set do_all, " Do all three.";
    "--cov", Arg.Set do_cov, " track information that would be computed by coverage. \
                               Does the --calls and --empty options to \
                               coverage by default, but not --every-instr." ;
	"--labels", Arg.Set labels, " Label predicate statements." 
  ] in
  let handleArg str = filenames := str :: !filenames in
    Arg.parse (Arg.align argDescr) handleArg usageMsg ;

	(* sometimes the ocaml type system is inexcusably stupid. Those
	   times usually involve objects *)
	let coerce iv = (iv : instrumentVisitor :> Cil.cilVisitor) in

    if !do_all then begin
      do_returns := true; do_branches := true; do_sk := true
    end;
      
    Cil.initCIL();

    List.map 
      (fun filename -> 
		 let file = Frontc.parse filename () in

		   (* equivalent of do_cfg option to coverage *)
		   ignore (Partial.calls_end_basic_blocks file);
		   ignore (Partial.globally_unique_vids file);
		   ignore (Cfg.computeFileCFG file);

		   if !do_cov then visitCilFileSameGlobals num_visitor file ; 
		   
		   visitCilFileSameGlobals (coerce ins_visitor) file;
	     
		   let new_global = GVarDecl(stderr_va,!currentLoc) in 
			 file.globals <- new_global :: file.globals ;
	       
			 let fd = Cil.getGlobInit file in 
			 let lhs = Cil.var(stderr_va) in
			 let data_str = file.fileName ^ ".preds" in 
			 let str_exp = Const(CStr(data_str)) in 
			 let str_exp2 = Const(CStr("wb")) in 
			 let instr = Call((Some(lhs)),fopen,[str_exp;str_exp2],!currentLoc) in 
			 let new_stmt = Cil.mkStmt (Instr[instr]) in 
			 let new_stmt = {new_stmt with labels = if !labels then make_label() else []} in 
			   fd.sbody.bstmts <- new_stmt :: fd.sbody.bstmts ; 

			   (* the following prevents Cil from printing out the
				  damned #line directives in the output. I don't know
				  if the directives are useful in any way, but for the
				  time being I find the output a lot more readable
				  this way *)
			   Cil.lineDirectiveStyle := None;
			   Cprint.printLn := false;
			   (****************************************************)

			   iterGlobals file (fun glob ->
								   dumpGlobal defaultCilPrinter stdout glob ;
								) ; 
			   
			   let sites = file.fileName ^ ".sites" in
			   let fout = open_out_bin sites in
				 Marshal.to_channel fout site_ht [] ;
				 Marshal.to_channel fout !site [] ;
				 Marshal.to_channel fout coverage_ht [] ;
				 (* FIXME: this shouldn't affect reading it back in,
					yet, at least; but CHECK *)
				 close_out fout) !filenames;
				   
end ;;

main () ;;
    
