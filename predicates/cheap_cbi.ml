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

let do_pa = ref false 

(* the schemes: *)
let do_returns = ref false
let do_branches = ref false
let do_sk = ref false
let do_all = ref false

(* Hashtbl for globals *)
let global_vars = Hashtbl.create 100

(* stolen from rmtmps.ml in Cil because I can't figure out how to
   reference it *)
let uninteresting =
  let names = [
    (* Cil.makeTempVar *)
    "__cil_tmp";
    
    (* sm: I don't know where it comes from but these show up all over. *)
    (* this doesn't seem to do what I wanted.. *)
    "iter";

    (* various macros in glibc's <bits/string2.h> *)		   
    "__result";
    "__s"; "__s1"; "__s2";
    "__s1_len"; "__s2_len";
    "__retval"; "__len";

    (* various macros in glibc's <ctype.h> *)
    "__c"; "__res";

    (* We remove the __malloc variables *)
  ] in

  (* optional alpha renaming *)
  let alpha = "\\(___[0-9]+\\)?" in
  
  let pattern = "\\(" ^ (String.concat "\\|" names) ^ "\\)" ^ alpha ^ "$" in
  Str.regexp pattern

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
    let str = (sprintf "%d" count)^",%d\n" in
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
    {(mkStmt stkind) with labels = make_label()}
      
let instr_branch e1 l s =
  let _,str_exp = get_next_site "branches" e1 l in
    make_printf_stmt true [str_exp;e1]
    
let sig_ret_instrs () = 
  [(make_printf_instr [Const(CStr("function return\n"))]);
   flush_instr()]

let instr_rets e l s =
  let conds = 
    List.map (fun cmp -> BinOp(cmp,e,zero,(TInt(IInt,[])))) [Lt;Gt;Eq] in
  let exp_and_conds = 
    List.map (fun cond -> 
				let _, s = get_next_site "returns" cond l in 
				  (cond, s)) conds in
  let instrs1 =
    List.map (fun (str_exp,cond) ->
		make_printf_instr [str_exp;cond]) exp_and_conds in
  let instrs2 = sig_ret_instrs() in
  let instr_list : Cil.instr list = instrs1 @ instrs2 in
    {(mkStmt (Instr(instr_list))) with labels = make_label()}

class instrumentVisitor = object(self)
  inherit nopCilVisitor

  val local_vars = Hashtbl.create 100

  method print_vars first_var =
	(* currently ignoring globals entirely *)
	let count, _ = get_next_site "scalar-pairs" (Lval(Var(first_var), NoOffset)) !currentLoc in 
	let one_var vi =
	  (* CHECK ME: right now we're printing out all variables as
	   * integers. I believe this is reasonable, but I should check! *)
	  let name_str = Const(CStr((sprintf "%d,%s" count vi.vname)^",%d\n")) in 
		make_printf_instr [name_str;Lval(Var(vi),NoOffset)]
	in
	let first_instr = one_var first_var in
	let local_print =  (* FIXME: WE'RE NOT DOING GLOBALS YET *)
	  Hashtbl.fold 
		(fun vname -> 
		   fun vi -> 
			 fun so_far_list -> 
			   if not (vi.vname = first_var.vname) then 
				 (one_var vi) :: so_far_list
			   else so_far_list)
		local_vars [] in
	  first_instr :: local_print @ (flush_instr() :: [])

(* USE Cil.var to make lvals out of varinfos *)
  method vstmt s = 
    (* insert before takes the result of a function to a statement to
     * generate a list of new statements, and then makes a new statement
     * consisting of a block in which the generate statements are inserted
     * before the argument s *)
    let insert_before stmts s = mkStmt (Block(mkBlock [stmts;s])) in
    let instrument_stmt s tv fn = 
      if tv then begin
	insert_before (fn s) s
      end else s in
      ChangeDoChildrenPost
	(s, 
	 fun s -> 
	   match s.skind with 
	       If(e1,b1,b2,l) -> instrument_stmt s !do_branches (instr_branch e1 l)
	     | Return(Some(e), l) -> instrument_stmt s !do_returns (instr_rets e l)
             | Return(None, l) -> insert_before (mkStmt (Instr(sig_ret_instrs ()))) s  
	     | _ -> s)

  method vglob g =
	match g with
	  | GVarDecl(vi, l)
	  | GVar (vi,_,l) -> Hashtbl.add global_vars vi.vname vi; DoChildren
	  | _ -> DoChildren

  method vfunc fdec =
	Hashtbl.clear local_vars;
	List.iter
	  (fun vi -> 
		 Hashtbl.add local_vars vi.vname vi) fdec.sformals;
      ChangeDoChildrenPost
		(fdec, fun fdec -> (Hashtbl.clear local_vars; fdec))

(* am I dealing with aliasing appropriately? *)

  method vinst i = 
    if !do_sk then begin
      let ilist = 
		match i with 
			Set((h,o), e, l) 
		  | Call(Some((h,o)), e, _, l) ->
			  (match h with 
				   Var(vi) ->
					 if not (Str.string_match uninteresting
							   vi.vname 0) then 
					   begin
						 if not (Hashtbl.mem local_vars vi.vname) then
						   Hashtbl.add local_vars vi.vname vi;
						 let ps = self#print_vars vi in
						   (i :: ps) @ [flush_instr()]
					   end else [i]
				 | _ -> [i])
		  | _ -> [i] in
		ChangeTo ilist
    end
    else DoChildren

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
    "--sk", Arg.Set do_sk, " Instrument scalar-pairs.";
    "--default", Arg.Set do_all, " Do all three.";
    "--cov", Arg.Set do_cov, " track information that would be computed by coverage. \
                               Does the --calls and --empty options to \
                               coverage by default, but not --every-instr." ;
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
		   Partial.calls_end_basic_blocks file;
		   Cfg.computeFileCFG file;

		   visitCilFileSameGlobals num_visitor file ; 
		   visitCilFileSameGlobals (coerce ins_visitor) file;
	     
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
				 close_out fout) !filenames;
				   
end ;;

main () ;;
    
