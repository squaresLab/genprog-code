(* 
 * Program Repair Prototype (v2) 
 *
 * Program Representation -- CIL C AST 
 *
 * This is the main implementation of the "Rep" interface. 
 *
 * Notably, this includes code and support for: 
 *  -> compiling C programs
 *  -> running test cases
 *  -> computing "coverage" fault localization information automatically
 *     => or allowing you to specify it from a file
 *  -> deleting/appending/swapping statements in C programs
 *)

open Printf
open Global
open Cil
open Rep
open Pretty

(*************************************************************************
 *************************************************************************
                      CIL Representation - C Programs
 *************************************************************************
 *************************************************************************)
let cilRep_version = "10" 

let use_canonical_source_sids = ref true 
let semantic_check = ref "scope" 
let preprocess = ref false
let preprocess_command = ref "__COMPILER_NAME__ -E __SOURCE_NAME__ __COMPILER_OPTIONS__ > __OUT_NAME__"
let print_line_numbers = ref false 
let multithread_coverage = ref false
let uniq_coverage = ref false
let check_invariant = ref false
let broken_swap = ref false

let _ =
  options := !options @
  [
    "--preprocess", Arg.Set preprocess, " preprocess the C code before parsing. Def: false";
    "--preprocessor", Arg.Set_string preprocess_command, " preprocessor command.  Default: __COMPILER__ -E" ;
    "--no-canonify-sids", Arg.Clear use_canonical_source_sids, " keep identical source smts separate" ;
    "--semantic-check", Arg.Set_string semantic_check, "X limit CIL mutations {none,scope}" ;
    "--print-line-numbers", Arg.Set print_line_numbers, " do print CIL #line numbers" ;
    "--mt-cov", Arg.Set multithread_coverage, "  instrument for coverage with locks.  Avoid if possible.";
    "--uniq", Arg.Set uniq_coverage, "  print each visited stmt only once";
	"--check-invariant", Arg.Set check_invariant, "  check datastructure invariant after mutation/crossover steps.";
	"--broken-swap", Arg.Set broken_swap, "  implement swap in cilrep as it is in the broken cilpatchrep implementation.";
    "--uniq-cov", Arg.Set uniq_coverage, " you should use --uniq instead"
  ] 


(*************************************************************************
 * Initial source code processing
 *************************************************************************)

(* 
 * Here is the list of CIL statementkinds that we consider as
 * possible atomic mutate-able statements. 
 *
 * Different handling is required for expression-level mutation.
 *)
let can_repair_statement sk = 
  match sk with
  | Instr _ | Return _ | If _ | Loop _ -> true

  | Goto _ | Break _ | Continue _ | Switch _ 
  | Block _ | TryFinally _ | TryExcept _ -> false

(* this helper visitor is useful in debugging *)

class printVisitor = object
  inherit nopCilVisitor

  method vfunc fd =
	debug "Entering func: %s\n" fd.svar.vname; DoChildren

  method vstmt s = debug "\tStmt: %d\n" s.sid; DoChildren
end
(* convenience global variable *)
let my_print = new printVisitor

(* This helper visitor resets all stmt ids to zero. *) 
class numToZeroVisitor = object
  inherit nopCilVisitor
  method vstmt s = s.sid <- 0 ; DoChildren
end 
let my_zero = new numToZeroVisitor

(* 
 * This visitor changes empty statement lists (e.g., the else branch in if
 * (foo) { bar(); } ) into dummy statements that we can modify later. 
 *)
class emptyVisitor = object
  inherit nopCilVisitor
  method vblock b = 
    ChangeDoChildrenPost(b,(fun b ->
	  if b.bstmts = [] then 
        mkBlock [ mkEmptyStmt () ] 
      else b 
    ))
end 

(* This visitor makes every instruction into its own statement. *)
class everyVisitor = object
  inherit nopCilVisitor
  method vblock b = 
    ChangeDoChildrenPost(b,(fun b ->
	  let stmts = List.map (fun stmt ->
        match stmt.skind with
        | Instr([]) -> [stmt] 
        | Instr(first :: rest) -> 
            ({stmt with skind = Instr([first])}) ::
            List.map (fun instr -> mkStmtOneInstr instr ) rest 
        | other -> [ stmt ] 
      ) b.bstmts in
      let stmts = List.flatten stmts in
      { b with bstmts = stmts } 
    ))
end 

(* OK.  what do I actually want?  *)
(* For every statement, a map between that statement and the in-scope
   variables.  Preferably indexed by integer index. *)
(* I also want a hashtable that maps vids to varinfos *)

(* This visitor walks over the C program AST and notes all declared global
 * variables. *) 
class globalVarVisitor varset = object
  inherit nopCilVisitor
  method vglob g = 
    List.iter (fun g -> match g with
    | GEnumTag(ei,_)
    | GEnumTagDecl(ei,_) -> 
	(*       varset := IntSet.add ei.ename ei !varset*) () (* FIXME: fix this! *)
    | GVarDecl(v,_) 
    | GVar(v,_,_) -> 
       varset := IntSet.add v.vid !varset 
    | _ -> () 
    ) [g] ; 
    DoChildren
  method vfunc fd = (* function definition *) 
    varset := IntSet.add fd.svar.vid !varset ;
    SkipChildren
end 

(*
 * Extract all of the variable references from a statement. This is used
 * later to check if it is legal to swap/insert this statement into
 * another context. 
 *)

class varrefVisitor setref = object
  inherit nopCilVisitor
  method vvrbl va = 
    setref := IntSet.add va.vid !setref ;
    SkipChildren 
end 

class varinfoVisitor setref = object
  inherit nopCilVisitor
  method vvdec va = 
    setref := IntMap.add va.vid va !setref ;
    DoChildren 
end 

(* 
 * If two statements both print as "x = x + 1", map them to the
 * same statement ID. This is used for picking FIX locations,
 * (to avoid duplicates) but _not_ for FAULT locations.
 * 
 * This functionality is used in particular in numVisitor, below. 
 *)
let canonical_stmt_ht = Hashtbl.create 255 
(* as of Tue Jul 26 11:01:16 EDT 2011, WW verifies that canonical_stmt_ht
 * is _not_ the source of a "memory leak" *) 
let canonical_uniques = ref 0 
let canonical_sid str sid =
  if !use_canonical_source_sids then 
    try
      Hashtbl.find canonical_stmt_ht str
    with _ -> begin
      Hashtbl.add canonical_stmt_ht str sid ;
      incr canonical_uniques ; 
      sid 
    end 
  else 
    sid 

(* This visitor walks over the C program AST and builds the statement map, while
 * tracking in-scope variables, if desired.  
 *
 * CLG: this used to be two visitors, numVisitor and numSemanticVisitor,
 * but I kept accidentally changing one but not the other (curses, cloned
 * code!), so  I've folded them into one.  my_num and my_numsemantic are
 * used almost exactly as before, with a slight change in argument order,
 * so you shouldn't notice overmuch. *)

class numVisitor 
        do_semantic
        globalset  (* all global variables *) 
        (localset : IntSet.t ref)   (* in-scope local variables *) 
        localshave (* maps SID -> in-scope local variables *) 
        localsused (* maps SID -> non-global vars used by SID *) 
        count add_to_stmt_map fname
        = object
  inherit nopCilVisitor
  val current_function = ref "???" 

  method vfunc fd = (* function definition *) 
    ChangeDoChildrenPost(
      begin 
        current_function := fd.svar.vname ; 
		let result = ref IntSet.empty in
		  List.iter
			(fun (v : Cil.varinfo) ->
			  result := IntSet.add v.vid !result)
			(fd.sformals @ fd.slocals);
			localset := !result;
        fd
      end,
		(fun fd -> localset := IntSet.empty ; fd ))
    
  method vblock b = 
    ChangeDoChildrenPost(b,(fun b ->
      List.iter (fun b -> 
        if can_repair_statement b.skind then begin
          b.sid <- !count ; 
          (* the copy is because we go through and update the statements
           * to add coverage information later *) 
          let rhs = (visitCilStmt my_zero (copy b)).skind in
            add_to_stmt_map !count (!current_function,fname) ;
			incr count ; 
              (*
               * Canonicalize this statement. We may have five statements
               * that print as "x++;" but that doesn't mean we want to count 
               * that five separate times. 
               *)
              let stripped_stmt = { 
                labels = [] ; skind = rhs ; sid = 0; succs = [] ; preds = [] ;
              } in 
              let pretty_printed =
                try 
                  Pretty.sprint ~width:80
                    (Pretty.dprintf "%a" dn_stmt stripped_stmt)
                with _ -> Printf.sprintf "@%d" b.sid 
              in 
              let _ = canonical_sid pretty_printed b.sid in 
			(*
			 * Determine the variables used in this statement. This allows us
			 * to restrict modifications to only consider well-scoped
			 * swaps/inserts. 
			 *)
              let used = ref IntSet.empty in 
                ignore(visitCilStmt (new varrefVisitor used) b);
                let in_scope = IntSet.union globalset !localset in 
                let my_locals_used = IntSet.diff !used globalset in 
                  localsused := IntMap.add b.sid my_locals_used !localsused ; 
                  localshave := IntMap.add b.sid !localset !localshave ; 
		end else b.sid <- 0; 
      ) b.bstmts ; 
      b
    ) )
end 

(* my_num numbers an AST without tracking semantic info, my_numsemantic numbers
   an AST while tracking semantic info *) 

let my_num = 
  let dummyMap = ref (IntMap.empty) in
  let dummySet = ref (IntSet.empty) in
    new numVisitor false 
	  !dummySet 
	  dummySet 
	  dummyMap 
	  dummyMap
let my_numsemantic = new numVisitor true


(*************************************************************************
 * Invariant checking on the CIL AST Representation
 *
 * A key Invariant for each variant (genotype) X is:
 *
 * (1) Every atom_id (except 0) in X occurs at most once in X.  
 * (2) Every atom_id in X also occurs in either the original program
 *     or the code bank (oracle fix localization).
 *************************************************************************)

(* 
 * this visitor collects information about the statement ids used in a file
 * to allow checking the datastructure invariant later 
 * *)
class collectIds returnset = object
  inherit nopCilVisitor
  method vstmt s =
    assert(s.sid >= 0); 
    if s.sid > 0 then begin
      assert(not (IntSet.mem s.sid !returnset)); (* no duplicates *) 
      returnset := IntSet.add s.sid !returnset;
    end; DoChildren

end

let my_collect = new collectIds
let invariant_info = ref (IntSet.empty) 
let num_unique_ids = ref 0 

let collect_invariant_info filename file = 
  (* this function collects invariant info -- stmt ids -- for a given Cil file.
     Should be called on the code bank, since that's the "ground truth" against
     which the individual variants are compared.  *)
  let id_set = ref (IntSet.empty) in
	visitCilFileSameGlobals (my_collect id_set) file;
	num_unique_ids := !num_unique_ids + (IntSet.cardinal !id_set);
	invariant_info := IntSet.union !id_set !invariant_info;
	assert((IntSet.cardinal !invariant_info) = !num_unique_ids)

(*************************************************************************
 * Obtaining coverage and Weighted Path Information
 *************************************************************************)

(* convenience/shorthand functions *)
let lval va = Lval((Var va), NoOffset)
let make_call lval fname args = Call(lval, fname, args, !currentLoc)

(* These are CIL variables describing C standard library functions like
 * 'fprintf'. We use them when we are instrumenting the file to
 * print out statement coverage information for fault localization. *)  

let stderr_va = makeVarinfo true "_coverage_fout" (TPtr(TVoid [], []))
let stderr = Lval((Var stderr_va), NoOffset)
let fflush_funname = "fflush"
let fflush = lval (makeVarinfo true fflush_funname (TVoid []))
let memset_funname = "memset" 
let memset = lval (makeVarinfo true memset_funname (TVoid []))
let fprintf_funname = "fprintf" 
let fprintf = lval (makeVarinfo true fprintf_funname (TVoid []))
let vgplain_fmsg_funname = "vgPlain_fmsg"
let fmsg = lval (makeVarinfo true vgplain_fmsg_funname (TVoid []))
let fopen_funname = "fopen"
let fopen = lval (makeVarinfo true fopen_funname (TVoid []))
let fclose_funname = "fclose"
let fclose = lval (makeVarinfo true fclose_funname (TVoid []))
let uniq_array_va = ref
  (makeGlobalVar "___coverage_array" (TArray(charType,None,[])))
let do_not_instrument_these_functions = 
  [ fflush_funname ; memset_funname ; fprintf_funname ; fopen_funname ; fclose_funname ; vgplain_fmsg_funname ] 

(* 
 * Visitor for computing statement coverage (for a "weighted path").
 *
 * This visitor walks over the C program AST and modifies it so that each
 * statment is preceeded by a 'printf' that writes that statement's number
 * to the .path file at run-time. *) 

class covVisitor coverage_outname = 
object
  inherit nopCilVisitor

  method vblock b = 
    ChangeDoChildrenPost(b,(fun b ->
      let result = List.map (fun stmt -> 
        if stmt.sid > 0 then begin
          let str = Printf.sprintf "%d\n" stmt.sid in 
          let print_num = 
            make_call None fprintf [stderr; Const(CStr(str));] 
          in
          let instrs = 
            if !multithread_coverage then begin
              let lval = (Some(Var(stderr_va), NoOffset)) in
              let args = [Const(CStr(coverage_outname)); Const(CStr("a"))] in
              let fopen_fout = make_call lval fopen args in
              let close_fout = make_call None fclose [stderr] in
                [fopen_fout;print_num;close_fout]
            end else 
              let flush = make_call None fflush [stderr] in
                [print_num; flush]
          in
          let skind = 
            if !uniq_coverage then begin
                (* asi = array_sub_i = array[i] *) 
              let iexp = Const(CInt64(Int64.of_int stmt.sid,IInt,None)) in 
              let asi_lval = (Var(!uniq_array_va)), (Index(iexp,NoOffset)) in
              let asi_exp = Lval(asi_lval) in 
              let bexp = BinOp(Eq,asi_exp,zero,ulongType) in
              let set_instr = Set(asi_lval,one,!currentLoc) in 
              let skind = Instr(instrs @[set_instr]) in
              let newstmt = mkStmt skind in 
                If(bexp,mkBlock [newstmt],mkBlock [],!currentLoc)
            end else 
              Instr(instrs)
          in
          let newstmt = mkStmt skind in 
            [ newstmt ; stmt ] 
        end else [stmt] 
      ) b.bstmts in 
        { b with bstmts = List.flatten result } 
    ) )

  method vfunc f = 
    if List.mem f.svar.vname do_not_instrument_these_functions then begin 
      debug "cilRep: WARNING: definition of %s found at %s:%d\n\tcannot instrument for coverage (would be recursive)\n"
        fprintf_funname f.svar.vdecl.file f.svar.vdecl.line ;
      SkipChildren
    end else if not !multithread_coverage then begin
      let outfile = Var(stderr_va), NoOffset in
      let fout_args = [Const(CStr(coverage_outname)); Const(CStr("wb"))] in
      let make_fout = make_call (Some(outfile)) fopen fout_args in
      let additional_instrs =
        if !uniq_coverage then begin
          let uniq_array_exp = Lval(Var(!uniq_array_va),NoOffset) in 
          let sizeof_uniq_array = SizeOfE(uniq_array_exp) in 
            [Call(None,memset,
                  [uniq_array_exp;zero;sizeof_uniq_array],!currentLoc)] 
        end else []
      in
      let new_stmt = Cil.mkStmt (Instr (make_fout :: additional_instrs)) in
      let ifknd = 
        If(BinOp(Eq,Lval(outfile), Cil.zero, Cil.intType),
           { battrs = [] ; bstmts = [new_stmt] }, 
           { battrs = []; bstmts = [] }, !currentLoc)
      in
    let ifstmt = Cil.mkStmt(ifknd) in
    ChangeDoChildrenPost(f,
                 (fun f ->
                   f.sbody.bstmts <- ifstmt :: f.sbody.bstmts;
                   f))
    end else DoChildren

end 


(*************************************************************************
 * Atomic Mutations (e.g., delete on CIL statement) 
 *************************************************************************)

(* For debugging, it is sometimes handy to add an in-source label
 * indicating what we have changed. *) 
let label_counter = ref 0 
let possibly_label s str id =
  if !label_repair then
    let text = sprintf "__repair_%s_%d__%x" str id !label_counter in
    incr label_counter ;
    let new_label = Label(text,!currentLoc,false) in
    new_label :: s.labels 
  else s.labels 

(* Delete a single statement (atom) *)
class delVisitor (to_del : atom_id) = object
  inherit nopCilVisitor
  method vstmt s = ChangeDoChildrenPost(s, fun s ->
      if to_del = s.sid then begin 
        let block = {
          battrs = [] ;
          bstmts = [] ; 
        } in

        { s with skind = Block(block) ;
                 labels = possibly_label s "del" to_del; } 
      end else s
    ) 
end 
let my_del = new delVisitor 

(* Append a single statement (atom) after a given statement (atom) *)
class appVisitor (append_after : atom_id) 
                 (what_to_append : Cil.stmtkind) = object
  inherit nopCilVisitor
  method vstmt s = ChangeDoChildrenPost(s, fun s ->
      if append_after = s.sid then begin 
        let copy = 
          (visitCilStmt my_zero (mkStmt (copy what_to_append))).skind in 
        (* [Wed Jul 27 10:55:36 EDT 2011] WW notes -- if we don't clear
         * out the sid here, then we end up with three statements that
         * all have that SID, which messes up future mutations. *) 
        let s' = { s with sid = 0 } in 
        let block = {
          battrs = [] ;
          bstmts = [s' ; { s' with skind = copy } ] ; 
        } in
        { s with skind = Block(block) ; 
          labels = possibly_label s "app" append_after ;
        } 
      end else s
    ) 
end 
let my_app = new appVisitor 

(* Swap two statements (atoms) *)  
class swapVisitor 
    (sid1 : atom_id) 
    (skind1 : Cil.stmtkind) 
    (sid2 : atom_id) 
    (skind2 : Cil.stmtkind) 
                  = object
  inherit nopCilVisitor
  method vstmt s = ChangeDoChildrenPost(s, fun s ->
      if s.sid = sid1 then begin 
        { s with skind = copy skind2 ;
                 labels = possibly_label s "swap1" sid1 ;
        } 
      end else if s.sid = sid2 then begin 
        { s with skind = copy skind1  ;
                 labels = possibly_label s "swap2" sid2 ;
        }
      end else s 
    ) 
end 
let my_swap = new swapVisitor 

class putVisitor 
    (sid1 : atom_id) 
    (skind1 : Cil.stmtkind) 
                  = object
  inherit nopCilVisitor
  method vstmt s = ChangeDoChildrenPost(s, fun s ->
      if s.sid = sid1 then begin 
        { s with skind = skind1 ;
                 labels = possibly_label s "put" sid1 ;
        } 
      end else s 
    ) 
end
let my_put = new putVisitor

(* this class fixes up the statement ids after a put operation so as to
 * maintain the datastructure invariant.  Make a new one every time you use it
 * or the seen_sids won't be refreshed and everything will be zeroed *)
class fixPutVisitor = object
  inherit nopCilVisitor

  val seen_sids = ref (IntSet.empty)

  method vstmt s =
	if s.sid <> 0 then begin
	  if IntSet.mem s.sid !seen_sids then s.sid <- 0
	  else seen_sids := IntSet.add s.sid !seen_sids;
	  DoChildren
	end else DoChildren
end

let gotten_code = ref (mkEmptyStmt ()).skind
class getVisitor 
    (sid1 : atom_id) 
                  = object
  inherit nopCilVisitor
  method vstmt s = 
    if s.sid = sid1 then 
      (gotten_code := s.skind; SkipChildren)
    else DoChildren
end
let my_get = new getVisitor

class getExpVisitor output first = object
  inherit nopCilVisitor
  method vstmt s = 
    if !first then begin
      first := false ; DoChildren
    end else 
      SkipChildren (* stay within this statement *) 
  method vexpr e = 
    ChangeDoChildrenPost(e, fun e ->
      output := e :: !output ; e
    ) 
end
let my_get_exp = new getExpVisitor 

class putExpVisitor count desired first = object
  inherit nopCilVisitor
  method vstmt s = 
    if !first then begin
      first := false ;
      DoChildren
    end else 
      SkipChildren (* stay within this statement *) 
  method vexpr e = 
    ChangeDoChildrenPost(e, fun e ->
      incr count ;
      match desired with
      | Some(idx,e) when idx = !count -> e
      | _ -> e 
    ) 
end
let my_put_exp = new putExpVisitor 


(*************************************************************************
 * Additional misc Cil file utilities
 *************************************************************************)

exception Found_Stmtkind of Cil.stmtkind

(* This visitor walks over the C program and finds the stmtkind associated
 * with the given statement id (living in the given function). *) 
class findStmtVisitor desired_sid function_name = object
  inherit nopCilVisitor
  method vfunc fd =
    if fd.svar.vname = function_name then
      DoChildren
    else SkipChildren

  method vstmt s = 
    if s.sid = desired_sid then begin
      raise (Found_Stmtkind s.skind)
    end ; DoChildren
end 

(* 
 * Visitor for outputting function information.
 * ZAK: added to get info to perform final selection for hardening
 *
 * This visitor walks over the C program AST and outputs
 * the functions' beginning and ending lines *) 
class funcLineVisitor = object
  inherit nopCilVisitor
  method vfunc fd =
    let firstLine = !currentLoc.line in 
    ChangeDoChildrenPost(fd, (fun fd ->
    let rettype,_,_,_ = splitFunctionType fd.svar.vtype in
    let strtyp = Pretty.sprint 80 (d_typsig () (typeSig rettype)) in 
    let lastLine = !currentLoc.line in 
        (* format: "file,return_type func_name,start,end"  *)
    Printf.printf "[1]%s,[2]%s [3]%s,[4]%d[5],%d\n" !currentLoc.file strtyp fd.svar.vname firstLine lastLine; flush stdout; fd))
end

let found_atom = ref 0 
let found_dist = ref max_int 
class findAtomVisitor (source_file : string) (source_line : int) = object
  inherit nopCilVisitor
  method vstmt s = 
    if s.sid > 0 then begin
      let this_file = !currentLoc.file in 
      let _,fname1,ext1 = split_base_subdirs_ext source_file in 
      let _,fname2,ext2 = split_base_subdirs_ext this_file in 
      if (fname1^"."^ext1) = (fname2^"."^ext2) || 
        Filename.check_suffix this_file source_file || source_file = "" then begin 
          let this_line = !currentLoc.line in 
          let this_dist = abs (this_line - source_line) in 
            if this_dist < !found_dist then begin
              found_atom := s.sid ;
              found_dist := this_dist 
            end 
        end 
    end ;
    DoChildren
end 


let my_findstmt = new findStmtVisitor
let my_find_atom = new findAtomVisitor
let my_flv = new funcLineVisitor


let in_scope_at context_sid moved_sid 
                localshave localsused = 
    if not (IntMap.mem context_sid localshave) then begin
      abort "in_scope_at: %d not found in localshave\n" 
        context_sid ; 
    end ; 
    let locals_here = IntMap.find context_sid localshave in 
    if not (IntMap.mem moved_sid localsused) then begin
      abort "in_scope_at: %d not found in localsused\n" 
        moved_sid ; 
    end ; 
    let required = IntMap.find moved_sid localsused in 
    IntSet.subset required locals_here

let output_cil_file_to_channel (fout : out_channel) (cilfile : Cil.file) = 
  if !print_line_numbers then 
    iterGlobals cilfile (dumpGlobal defaultCilPrinter fout) 
  else 
    iterGlobals cilfile (dumpGlobal Cilprinter.noLineCilPrinter fout) 

let output_cil_file (outfile : string) (cilfile : Cil.file) = 
  let fout = open_out outfile in
	output_cil_file_to_channel fout cilfile ;
    close_out fout

let output_cil_file_to_string ?(xform = Cilprinter.nop_xform) 
                               (cilfile : Cil.file) = 
    (* Use the Cilprinter.ml code to output a Cil.file to a Buffer *) 
  let buf = Buffer.create 10240 in   
    begin if !print_line_numbers then 
        let printer = Cilprinter.toStringCilPrinter xform in 
      iterGlobals cilfile (printer#bGlobal buf) 
    else begin 
      let printer = Cilprinter.noLineToStringCilPrinter xform in 
      iterGlobals cilfile (printer#bGlobal buf) 
    end end ; 
    Buffer.contents buf 


(*************************************************************************
 *************************************************************************
                          CIL Representation 
                  (both single and multi file)
 *************************************************************************
 *************************************************************************)


type cilRep_atom =
  | Stmt of Cil.stmtkind
  | Exp of Cil.exp 
  | Template_Block of Cil.block

class replaceVisitor (replace : atom_id) 
  (replace_with : Cil.stmtkind) = object
	inherit nopCilVisitor

  method vstmt s = ChangeDoChildrenPost(s, fun s ->
      if replace = s.sid then begin 
        let copy = 
          (visitCilStmt my_zero (mkStmt (copy replace_with))).skind in 
        (* [Wed Jul 27 10:55:36 EDT 2011] WW notes -- if we don't clear
         * out the sid here, then we end up with three statements that
         * all have that SID, which messes up future mutations. *) 
        let s' = { s with sid = 0 } in 
        let block = {
          battrs = [] ;
          bstmts = [ { s' with skind = copy } ] ; 
        } in
        { s with skind = Block(block) ; 
          labels = possibly_label s "rep" replace ;
        } 
      end else s
    ) 
  end

let my_rep = new replaceVisitor

class replaceExpVisitor (replace : atom_id) 
  (replace_with : Cil.stmtkind) = object
	inherit nopCilVisitor

	method vstmt s = ChangeDoChildrenPost(s, fun s ->
	  if replace = s.sid then { s with skind = replace_with }
	  else s)
	  
  end

(* These global variables store the original Cil AST info.  Used as the code
 * bank, and in CilPatchRep as the base against which all representations
 * are compared*)

(*let global_cilRep_code_bank = ref StringMap.empty
let global_cilRep_oracle_code = ref StringMap.empty 
let global_cilRep_stmt_map : (string * string) AtomMap.t ref = ref (AtomMap.empty) 
let global_cilRep_var_maps = ref (IntMap.empty, IntMap.empty, IntSet.empty) 
let global_cilRep_varinfo_map = ref IntMap.empty
*)
type ast_info = 
	{ code_bank : Cil.file StringMap.t ;
	  oracle_code : Cil.file StringMap.t ;
	  stmt_map : (string * string) AtomMap.t ;
	  localshave : IntSet.t IntMap.t ;
	  globalsset : IntSet.t ;
	  localsused : IntSet.t IntMap.t ;
	  varinfo : Cil.varinfo IntMap.t ;
	  all_source_sids : IntSet.t }

let empty_info () =
	{ code_bank = StringMap.empty;
	  oracle_code = StringMap.empty ;
	  stmt_map = AtomMap.empty ;
	  localshave = IntMap.empty ;
	  globalsset = IntSet.empty ;
	  localsused = IntMap.empty ;
	  varinfo = IntMap.empty ;
	  all_source_sids = IntSet.empty }

let global_ast_info = ref (empty_info()) 

let hole_regexp = Str.regexp "__hole[0-9]+__"

let registered_c_templates = hcreate 10

exception FoundIt of string

class collectConstraints template_constraints_ht template_code_ht template_name = object
  inherit nopCilVisitor

  method vfunc fundec =
	let hole_ht = hcreate 10 in
	let holes = 
	  lfilt (fun varinfo -> Str.string_match hole_regexp varinfo.vname 0) 
		fundec.slocals in
	  liter
		(fun varinfo ->
		  let htyp =
			let [Attr(_,[AStr(typ)])] = 
			  filterAttributes "holetype" varinfo.vattr in
			  match typ with
				"stmt" -> Stmt_hole
			  | "lval" -> Lval_hole
			  | "exp" -> Exp_hole
			  | _ -> failwith "Unexpected value in htype value"
		  in
		  let constraints = 
			lfoldl
			  (fun constraints ->
				fun attr ->
				  match attr with
					Attr("constraint", [AStr("fault_path")]) -> 
					  ConstraintSet.add Fault_path constraints
				  | Attr("constraint", [AStr("fix_path")]) -> 
					ConstraintSet.add Fix_path constraints
				  | Attr("inscope", [AStr(v)]) -> 
					ConstraintSet.add (InScope(v)) constraints
				  | Attr("reference", [AStr(v)]) -> 
					ConstraintSet.add (Ref(v)) constraints
				  | _ -> constraints
			  ) ConstraintSet.empty varinfo.vattr
		  in
			hrep hole_ht varinfo.vname 
			  { hole_id=varinfo.vname; htyp=htyp; constraints=constraints})
		holes;
	  template_name := fundec.svar.vname;
	  hadd template_constraints_ht !template_name hole_ht;
	  DoChildren
		
  method vblock block =
	match block.battrs with
	  [] -> DoChildren
	| lst ->
	  let hole_ht = hfind template_constraints_ht !template_name in
	  let holes = 
		hfold (fun k -> fun v -> fun lst -> k :: lst) hole_ht [] in
		try
		  liter
			(fun attr ->
			  match attr with
				Attr(name,_) ->
					liter (fun hole -> 
					  if ("__"^name^"__") = hole then 
						raise (FoundIt(hole))
					) holes
			) block.battrs;
		  DoChildren
		with FoundIt(holename) ->
		  begin
			let newattrs = dropAttribute ("__"^holename^"__") block.battrs in
			let code_ht = ht_find template_code_ht !template_name 
			  (fun _ -> hcreate 10) in
			  hadd code_ht holename 
				(Template_Block({ block with battrs=newattrs }));
			  hrep template_code_ht !template_name code_ht;
			  DoChildren
		  end
end

class cilRep = object (self : 'self_type)
  inherit [cilRep_atom] faultlocRepresentation as super

  (***********************************
   * Concrete State Variables
   ***********************************)
  
  val stmt_count = ref 1 

  (* "base" holds the ASTs associated with this representation, as
   * mapping from source file name to Cil AST. 
   *
   * Use self#get_base () to access.
   * "base" is different from "code_bank!"
   *) 

  val base = ref ((StringMap.empty) : Cil.file StringMap.t)

  (***********************************
   * Concrete Methods
   ***********************************)

  (* make a fresh copy of this variant *) 
  method copy () : 'self_type = begin
    let super_copy : 'self_type = super#copy () in 
      super_copy#internal_copy () 
  end

  (* being sure to update our local instance variables *) 
  method internal_copy () : 'self_type = begin
    {< base = ref (Global.copy !base) ; 
       stmt_count = ref !stmt_count >} 
  end

  (* serialize the state *) 
  method save_binary ?out_channel (filename : string) = begin
    let fout = 
      match out_channel with
      | Some(v) -> v
      | None -> open_out_bin filename 
    in 
      Marshal.to_channel fout (cilRep_version) [] ; 
      Marshal.to_channel fout (!global_ast_info.code_bank) [] ;
      Marshal.to_channel fout (!global_ast_info.oracle_code) [] ;
      Marshal.to_channel fout (!global_ast_info.stmt_map) [] ;
      Marshal.to_channel fout (!stmt_count) [] ;
	  let triple = !global_ast_info.localshave,!global_ast_info.localsused, !global_ast_info.all_source_sids in
      Marshal.to_channel fout triple [] ;
      let saved_base = 
        if !output_binrep then
          Some(!base)
        else None 
      in 
      Marshal.to_channel fout (saved_base) [] ;
      super#save_binary ~out_channel:fout filename ;
      debug "cilRep: %s: saved\n" filename ; 
      if out_channel = None then close_out fout 
  end 

  (* load in serialized state *) 
  method load_binary ?in_channel (filename : string) = begin
    assert(StringMap.is_empty (self#get_base())
      || !incoming_pop_file <> "") ;
    let fin = 
      match in_channel with
      | Some(v) -> v
      | None -> open_in_bin filename 
    in 
    let version = Marshal.from_channel fin in
      if version <> cilRep_version then begin
        debug "cilRep: %s has old version\n" filename ;
        failwith "version mismatch" 
      end ;
      global_ast_info := {!global_ast_info with code_bank = Marshal.from_channel fin } ; 
      global_ast_info := {!global_ast_info with oracle_code = Marshal.from_channel fin } ; 
      global_ast_info := {!global_ast_info with stmt_map = Marshal.from_channel fin } ;
      stmt_count := Marshal.from_channel fin ;
      let var_maps = Marshal.from_channel fin in
		global_ast_info := {!global_ast_info with localshave = fst3 var_maps} ;
		global_ast_info := {!global_ast_info with localsused = snd3 var_maps} ;
		global_ast_info := {!global_ast_info with all_source_sids = trd3 var_maps} ;
      (match Marshal.from_channel fin with
      | None -> base := !global_ast_info.code_bank
      | Some(b) -> base := b
      ) ; 
      super#load_binary ~in_channel:fin filename ; 
      debug "cilRep: %s: loaded\n" filename ; 
      if in_channel = None then close_in fin ;
  end 

  method move_to_global () = 
    global_ast_info := {!global_ast_info with code_bank = !base }

  method compute_localization () =
    super#compute_localization () ;
    self#move_to_global () 

  (* print debugging information *)  
  method debug_info () = begin
    debug "cilRep: stmt_count = %d\n" !stmt_count ;
    debug "cilRep: stmts in weighted_path = %d\n" 
      (List.length !fault_localization) ; 
    debug "cilRep: total weight = %g\n"
      (lfoldl (fun total -> fun (i,w) -> total +. w) 0.0 !fault_localization);
    debug "cilRep: stmts in weighted_path with weight >= 1.0 = %d\n" 
      (List.length (List.filter (fun (a,b) -> b >= 1.0) !fault_localization)) ;
    let file_count = ref 0 in 
    let statement_range filename = 
      AtomMap.fold (fun id (stmtkind,filename') (low,high) -> 
        if filename' = filename then
          (min low id),(max high id) 
        else (low,high) 
      ) (self#get_stmt_map()) (max_int,min_int) in  
      StringMap.iter 
        (fun k v -> incr file_count ; 
          let low, high = statement_range k in 
            debug "cilRep: %s (code bank/base file; atoms [%d,%d])\n" k low high 
        ) (self#get_code_bank ()) ; 
      StringMap.iter (fun k v ->
        incr file_count ; 
        let low, high = statement_range k in 
          debug "cilRep: %s (oracle file; atoms [%d,%d])\n" k low high
      ) (self#get_oracle_code ()) ; 
      debug "cilRep: %d file(s) total in representation\n" !file_count ; 
      if !print_func_lines then
        self#output_function_line_nums ;
  end 

  (***********************************
   * the following several functions give access to statements, files, code,
   * etc, in both the base representation and the code bank
   ***********************************)

  (* return the total number of statements, for search strategies that
   * want to iterate over all statements or consider them uniformly 
   * at random *) 
  method max_atom () = !stmt_count 

  method get_stmt_map () = begin
    assert(not (AtomMap.is_empty !global_ast_info.stmt_map)) ;
    !global_ast_info.stmt_map 
  end

  method get_oracle_code () = !global_ast_info.oracle_code

  method get_base () = !base

  method get_code_bank () = begin
    assert(not (StringMap.is_empty !global_ast_info.code_bank));
    !global_ast_info.code_bank
  end

  (* get_stmt gets a statement from the ***code bank*** *)
  method get_stmt stmt_id = begin
    try begin 
      let funname, filename =
        AtomMap.find stmt_id (self#get_stmt_map()) 
      in
      let code_bank = self#get_code_bank () in 
      let oracle_code = self#get_oracle_code () in
      let file_map = 
        try
          List.find (fun map -> StringMap.mem filename map) 
            [ code_bank ; oracle_code ] 
        with Not_found -> 
          let code_bank_size = StringMap.fold (fun key elt acc -> acc + 1)
            code_bank 0 in 
          let oracle_size = StringMap.fold (fun key elt acc -> acc + 1)
            oracle_code 0 in 
          debug "cilrep: code bank size %d, oracle code size %d\n" 
            code_bank_size oracle_size ; 
          abort "cilrep: cannot find stmt id %d in either code bank or oracle\n%s (function)\n%s (file not found)\n"
            stmt_id funname filename 
      in  
      let file_ast = StringMap.find filename file_map in 
        try 
          visitCilFileSameGlobals (my_findstmt stmt_id funname) file_ast ;
          abort "cilrep: cannot find stmt id %d in code bank\n%s (function)\n%s (file)\n" 
            stmt_id funname filename 
        with Found_Stmtkind(skind) -> filename,skind 
    end
    with Not_found -> 
      abort "cilrep: %d not found in stmt_map\n" stmt_id 
  end

  (* gets the file ast from the **base representation** *)
  method get_file stmt_id = begin
    let _,fname = AtomMap.find stmt_id (self#get_stmt_map()) in
      StringMap.find fname (self#get_base())
  end

  (***********************************
   * Functions that manipulate C source code
   ***********************************)
  method from_source_min cilfile_list node_map = begin
    List.iter (fun (filename,diff_script) ->
      assert(StringMap.mem filename !base);
      let base_file = copy (StringMap.find filename !base) in
      let mod_file = Cdiff.repair_usediff base_file node_map diff_script (copy cdiff_data_ht) in
	    base := StringMap.add filename mod_file !base) cilfile_list;
    self#updated()
  end

  (* load in a CIL AST from a C source file *) 
  method from_source (filename : string) = begin 
    debug "cilrep: from_source: stmt_count = %d\n" !stmt_count ; 
    let _,ext = split_ext filename in 
      (match ext with
        "txt" ->
          liter
            (fun fname ->
              base := StringMap.add fname (self#from_source_one_file fname) !base)
            (get_lines filename)
      | "c" | "i" -> 
        base := StringMap.add filename (self#from_source_one_file filename) !base
      | _ -> debug "extension: %s\n" ext; failwith "Unexpected file extension in CilRep#from_source.  Permitted: .c, .txt");
	  debug "stmt_count: %d\n" !stmt_count;
      stmt_count := pred !stmt_count ; 
      self#move_to_global ();
  end 

  method compile source_name exe_name = begin
    let source_name = 
      if !multi_file then begin
        let source_dir,_,_ = split_base_subdirs_ext source_name in 
          StringMap.fold
            (fun fname ->
              fun file ->
                fun source_name -> 
                  let fname' = Filename.concat source_dir fname in 
                    fname'^" "^source_name
            ) (self#get_base ()) ""
      end else source_name
    in
      super#compile source_name exe_name
  end

  (* internal_parse parses one C file! *)
  method internal_parse (filename : string) = 
    let filename = 
      if !preprocess then begin
        debug "cilRep: %s: preprocessing\n" filename ; 
        let outname = 
          if !multi_file then begin
            (try Unix.mkdir "preprocess" 0o755 with _ -> ());
            Filename.concat "preprocess" filename
          end else 
            let base,ext = split_ext filename in 
              base^".i"
        in
        let cmd = 
          Global.replace_in_string  !preprocess_command
            [ 
              "__COMPILER_NAME__", !compiler_name ;
              "__COMPILER_OPTIONS__", !compiler_options ;
              "__SOURCE_NAME__", filename ;
              "__OUT_NAME__", outname
            ] 
        in
          (match Stats2.time "preprocess" Unix.system cmd with
          | Unix.WEXITED(0) -> ()
          | _ -> abort "\t%s preprocessing problem\n" filename ); outname
      end else filename 
    in
      debug "cilRep: %s: parsing\n" filename ; 
      let file = Frontc.parse filename () in 
        debug "cilRep: %s: parsed (%g MB)\n" filename (debug_size_in_mb file); 
        file 

  method get_compiler_command () = 
    "__COMPILER_NAME__ -o __EXE_NAME__ __SOURCE_NAME__ __COMPILER_OPTIONS__ 1>/dev/null 2>/dev/null" 

  method from_source_one_file ?pre:(append_prefix=true) (filename : string) : Cil.file = begin
    let full_filename = 
      if append_prefix && (not !min_flag) then Filename.concat !prefix filename 
      else filename
    in
    let file = self#internal_parse full_filename in 
    let globalset = ref !global_ast_info.globalsset in 
    let localshave = ref !global_ast_info.localshave in
    let localsused = ref !global_ast_info.localsused in
	let varmap = ref !global_ast_info.varinfo in 
    let localset = ref IntSet.empty in
	let stmt_map = ref !global_ast_info.stmt_map in
      visitCilFileSameGlobals (new everyVisitor) file ; 
      visitCilFileSameGlobals (new emptyVisitor) file ; 
      visitCilFileSameGlobals (new varinfoVisitor varmap) file ; 
      let add_to_stmt_map x (skind,fname) = 
		stmt_map := AtomMap.add x (skind,fname) !stmt_map
      in 
		begin
		  match !semantic_check with
		  | "scope" ->
			(* First, gather up all global variables. *) 
			visitCilFileSameGlobals (new globalVarVisitor globalset) file ; 
			(* Second, number all statements and keep track of
			 * in-scope variables information. *) 
			visitCilFileSameGlobals 
              (my_numsemantic
                 !globalset localset localshave localsused 
                 stmt_count add_to_stmt_map filename
              ) file  
		  | _ -> visitCilFileSameGlobals 
			(my_num stmt_count add_to_stmt_map filename) file ; 
		end ;

    (* we increment after setting, so we're one too high: *) 
		let source_ids = ref !global_ast_info.all_source_sids in
		  if !use_canonical_source_sids then begin
			Hashtbl.iter (fun str i ->
			  source_ids := IntSet.add i !source_ids 
			) canonical_stmt_ht 
		  end else 
			for i = 1 to !stmt_count do
			  source_ids := IntSet.add i !source_ids 
			done ;
		  global_ast_info := {!global_ast_info with
			stmt_map = !stmt_map;
			localshave = !localshave;
			localsused = !localsused;
			globalsset = !globalset;
			varinfo = !varmap ;
			all_source_sids = !source_ids };
		  self#internal_post_source filename; file
  end

  method internal_post_source filename = begin
  end 


  method load_oracle (filename : string) = begin
    debug "cilRep: loading oracle: %s\n" filename;
    let base,ext = split_ext filename in 
    let filelist = 
      match ext with 
      | "c" -> [filename]
      | _ -> get_lines filename
    in
      liter (fun fname -> 
		let file = self#from_source_one_file ~pre:false fname in
		  if (StringMap.mem fname !global_ast_info.oracle_code) then begin
            abort "cilRep: %s already present in oracle code\n" fname ;
		  end ; 
		  let oracle = !global_ast_info.oracle_code in 
			global_ast_info := {!global_ast_info with oracle_code = 
				StringMap.add fname file oracle}
      ) filelist;
      stmt_count := pred !stmt_count
  end

  method output_function_line_nums = begin
    debug "cilRep: computing function line numbers\n" ; 
    StringMap.iter
      (fun _ ->
        fun file ->
          visitCilFileSameGlobals my_flv (copy file))
     (self#get_base ());
    debug "cilRep: DONE."
  end

  method internal_compute_source_buffers () = begin
    let output_list = ref [] in 
    let make_name n = if !multi_file then Some(n) else None in
      StringMap.iter (fun (fname:string) (cil_file:Cil.file) ->
        let source_string = output_cil_file_to_string cil_file in
          output_list := (make_name fname,source_string) :: !output_list 
      ) (self#get_base()) ; 
      assert((llen !output_list) > 0);
      !output_list
  end

  method atom_id_of_source_line source_file source_line = begin
    found_atom := (-1);
    found_dist := max_int;
    let oracle_code = self#get_oracle_code () in 
    if StringMap.mem source_file oracle_code then  
      let file = StringMap.find source_file oracle_code in  
      visitCilFileSameGlobals (my_find_atom source_file source_line) file
    else 
      StringMap.iter (fun fname file -> 
            visitCilFileSameGlobals (my_find_atom source_file source_line) file)
      (self#get_base ());
    if !found_atom = (-1) then begin
      debug "cilrep: WARNING: cannot convert %s,%d to atom_id\n" source_file
      source_line ;
      0 
    end else !found_atom
  end

  (***********************************
   * Getting coverage information
   ***********************************)

  (* instruments one Cil file for fault localization *)
  method instrument_one_file file ?g:(globinit=false) coverage_sourcename coverage_outname = begin
	let uniq_globals = 
	  if !uniq_coverage then begin
        let size_exp = (Const(CInt64(Int64.of_int (1 + !stmt_count),IInt,None))) in 
          uniq_array_va := (makeGlobalVar "___coverage_array"
                              (TArray(charType, Some(size_exp), []))) ;
		  [GVarDecl(!uniq_array_va,!currentLoc)]
	  end else []
	in
	let coverage_out = [GVarDecl(stderr_va,!currentLoc)]
	in
	let new_globals = 
	  if not globinit then 
		lmap
		  (fun glob ->
			match glob with
			  GVarDecl(va,loc) -> GVarDecl({va with vstorage = Extern}, loc))
		  (uniq_globals @ coverage_out)
	  else (uniq_globals @ coverage_out)
	in
      file.globals <- new_globals @ file.globals ;
	  let cov_visit = new covVisitor coverage_outname
	  in
      visitCilFileSameGlobals cov_visit file;
      file.globals <- 
        lfilt (fun g ->
          match g with 
            GVarDecl(vinfo,_) ->
              (match vinfo.vstorage with
                Extern when vinfo.vname = "fopen" -> false
              | _ -> true)
          | _ -> true) file.globals;
      ensure_directories_exist coverage_sourcename;
      output_cil_file coverage_sourcename file
  end

  method instrument_fault_localization coverage_sourcename coverage_exename coverage_outname = begin
    debug "cilRep: instrumenting for fault localization\n";
    let source_dir,_,_ = split_base_subdirs_ext coverage_sourcename in 
      ignore(
        StringMap.fold
          (fun fname ->
            fun file ->
              fun globinit ->
                let file = copy file in 
                  if not !multi_file then 
                    self#instrument_one_file file ~g:true coverage_sourcename coverage_outname
                  else 
                    self#instrument_one_file file ~g:globinit (Filename.concat source_dir fname) coverage_outname;
                  false)
          (self#get_base()) true)
  end

  (***********************************
   * Atomic mutations 
   ***********************************)

  (* Atomic Delete of a single statement (atom) *) 
  method delete stmt_id = begin
    let file = self#get_file stmt_id in 
      super#delete stmt_id;
      visitCilFileSameGlobals (my_del stmt_id) file;
	  if !check_invariant then self#check_invariant()
  end

  (* Atomic Append of a single statement (atom) after another statement *) 
  method append append_after what_to_append = begin
    let file = self#get_file append_after in 
    let _,what = 
      try self#get_stmt what_to_append 
      with _ -> abort "cilRep: append: %d not found in code bank\n" what_to_append 
    in 
      super#append append_after what_to_append ; 
      visitCilFileSameGlobals (my_app append_after what) file;
	  if !check_invariant then self#check_invariant()
  end

  (* Return a Set of (atom_ids,fix_weight pairs) that one could append here 
   * without violating many typing rules. *) 
  method append_sources append_after = begin
    let all_sids = !fix_localization in 
    let sids = 
      if !semantic_check = "none" then all_sids
      else  
    lfilt (fun (sid,weight) ->
          in_scope_at append_after sid !global_ast_info.localshave !global_ast_info.localsused 
    ) all_sids
    in
      lfoldl 
        (fun retval ele -> WeightSet.add ele retval) 
        (WeightSet.empty) sids
  end

  (* Atomic Swap of two statements (atoms) *)
  method swap stmt_id1 stmt_id2 = begin
    super#swap stmt_id1 stmt_id2 ; 
	  if !broken_swap then begin
		let stmt_id1,stmt_id2 = 
		  if stmt_id1 <= stmt_id2 then stmt_id1,stmt_id2 else stmt_id2,stmt_id1 in
		let file = self#get_file stmt_id1 in 
		  visitCilFileSameGlobals (my_del stmt_id1) file;
		  let _,what = 
			try self#get_stmt stmt_id2
			with _ -> abort "cilRep: broken_swap: %d not found in code bank\n" stmt_id2
		  in 
			visitCilFileSameGlobals (my_app stmt_id1 what) file;
	  end else begin
		let f1,skind1 = self#get_stmt stmt_id1 in 
		let f2,skind2 = self#get_stmt stmt_id2 in 
		let base = self#get_base () in
		let my_swap = my_swap stmt_id1 skind1 stmt_id2 skind2 in
		  if StringMap.mem f1 base then
			visitCilFileSameGlobals my_swap (StringMap.find f1 base);
		  if f1 <> f2 && (StringMap.mem f2 base) then
			visitCilFileSameGlobals my_swap (StringMap.find f2 base);
		  if !check_invariant then self#check_invariant()
	  end
  end

  (* Return a Set of atom_ids that one could swap here without
   * violating many typing rules. In addition, if X<Y and X and Y
   * are both valid, then we'll allow the swap (X,Y) but not (Y,X).
   *) 
  method swap_sources append_after = begin
    let all_sids = !fault_localization in
    let sids = 
      if !semantic_check = "none" then all_sids
      else 
        lfilt (fun (sid, weight) ->
          in_scope_at sid append_after !global_ast_info.localshave !global_ast_info.localsused 
          && in_scope_at append_after sid !global_ast_info.localshave !global_ast_info.localsused 
        ) all_sids 
    in
    let sids = lfilt (fun (sid, weight) -> sid <> append_after) sids in
      lfoldl
        (fun retval ele -> WeightSet.add ele retval)
        (WeightSet.empty) sids
  end

  (* Atomic replace of two statements (atoms) *)
  method replace stmt_id1 stmt_id2 = begin
    let _,replace_with = self#get_stmt stmt_id2 in 
      super#replace stmt_id1 stmt_id2 ; 
      visitCilFileSameGlobals (my_rep stmt_id1 replace_with) (self#get_file stmt_id1);
	  if !check_invariant then self#check_invariant()
  end

  (* Return a Set of atom_ids that one could replace here without
   * violating many typing rules. In addition, if X<Y and X and Y
   * are both valid, then we'll allow the swap (X,Y) but not (Y,X).
   *) 
  method replace_sources replace = begin
    let all_sids = !fix_localization in
    let sids = 
      if !semantic_check = "none" then all_sids
      else 
        lfilt (fun (sid, weight) ->
          in_scope_at sid replace !global_ast_info.localshave !global_ast_info.localsused 
        ) all_sids 
    in
    let sids = lfilt (fun (sid, weight) -> sid <> replace) sids in
      lfoldl
        (fun retval ele -> WeightSet.add ele retval)
        (WeightSet.empty) sids
  end

  (* get obtains an atom from the current variant, *not* from the code
     bank *) 
  method get stmt_id = begin
    let file = self#get_file stmt_id in
      visitCilFileSameGlobals (my_get stmt_id) file;
      let answer = !gotten_code in
        gotten_code := (mkEmptyStmt()).skind ;
        (Stmt answer) 
  end

  (* put places an atom into the current variant; the code bank is not
     involved *) 

  method put stmt_id stmt = begin
    let file = self#get_file stmt_id in 
    super#put stmt_id stmt ; 
    (match stmt with
    | Stmt(stmt) -> 
      visitCilFileSameGlobals (my_put stmt_id stmt) file;
	  visitCilFileSameGlobals (new fixPutVisitor) file;
    | Exp(e) -> failwith "cilRep#put of Exp subatom" );
	if !check_invariant then self#check_invariant()
  end

  (***********************************
   * Subatoms. Subatoms are Expressions
   ***********************************)
  method subatoms = true 

  method get_subatoms stmt_id = begin
    let file = self#get_file stmt_id in
      visitCilFileSameGlobals (my_get stmt_id) file;
    let answer = !gotten_code in
    let this_stmt = mkStmt answer in
    let output = ref [] in 
    let first = ref true in 
    let _ = visitCilStmt (my_get_exp output first) this_stmt in
      List.map (fun x -> Exp x) !output 
  end
  method get_subatom stmt_id subatom_id = 
	let subatoms = self#get_subatoms stmt_id in
	  List.nth subatoms subatom_id

  method replace_subatom stmt_id subatom_id atom = begin
    let file = self#get_file stmt_id in
    match atom with
    | Stmt(x) -> failwith "cilRep#replace_atom_subatom" 
    | Exp(e) -> 
      visitCilFileSameGlobals (my_get stmt_id) file ;
      let answer = !gotten_code in
      let this_stmt = mkStmt answer in
      let desired = Some(subatom_id, e) in 
      let first = ref true in 
      let count = ref 0 in 
      let new_stmt = visitCilStmt (my_put_exp count desired first) 
        this_stmt in 
        super#note_replaced_subatom stmt_id subatom_id atom ; 
        visitCilFileSameGlobals (my_put stmt_id new_stmt.skind) file;
		visitCilFileSameGlobals (new fixPutVisitor) file
  end

  method replace_subatom_with_constant stmt_id subatom_id =  
    self#replace_subatom stmt_id subatom_id (Exp Cil.zero)

  (* For debugging. *) 

  method atom_to_str atom = begin
    let doc = match atom with
      | Exp(e) -> d_exp () e 
      | Stmt(s) -> dn_stmt () (mkStmt s) 
    in 
      Pretty.sprint ~width:80 doc 
  end

  (***********************************
   * Templates
   ***********************************)

  method get_template tname = hfind registered_c_templates tname

  method load_templates template_file = 
	let _ = super#load_templates template_file in
	let file = Frontc.parse template_file () in
	let template_constraints_ht = hcreate 10 in
	let template_code_ht = hcreate 10 in
	let template_name = ref "" in
	  visitCilFileSameGlobals (new collectConstraints template_constraints_ht template_code_ht template_name) file;
	  hiter
		(fun template_name ->
		  fun hole_constraints ->
			let code = hfind template_code_ht template_name in 
			let as_map : hole_info StringMap.t = 
			  hfold
				(fun (hole_id : string) ->
				  fun (hole_info : hole_info) ->
					fun map ->
					  StringMap.add hole_id hole_info map)
				 hole_constraints (StringMap.empty)
			in
			  hadd registered_c_templates
				template_name
				{ template_name=template_name;
				  hole_constraints=as_map;
				  hole_code_ht = code})
		template_constraints_ht

  method mutate template fillins = 
	let _ = super#mutate template fillins in
	let placeholder_regexp = Str.regexp_string " = ___placeholder___.var" in
	(* FIXME: why won't the blockattributes go away? *)
	let all_holes = 1 -- (StringMap.cardinal template.hole_constraints) in 
	let arg_list = 
	  StringMap.fold
		(fun hole ->
		  fun (typ,id,idopt) ->
			fun arg_list ->
			  let item = 
				match typ with 
				  Stmt_hole -> 
					let _,atom = self#get_stmt id in
					  Fs (mkStmt atom)
				| Exp_hole -> 
				  let exp_id = match idopt with Some(id) -> id in
				  let Exp(atom) = self#get_subatom id exp_id in
					Fe atom
				| Lval_hole -> 
				  let atom = IntMap.find id !global_ast_info.varinfo in
					Fv atom (* FIXME: think about this *)
			  in
				(hole, item) :: arg_list
		) fillins []
	in
	let applied = 
	  hfold
		(fun holename ->
		  fun block ->
			fun lst ->
			  let Template_Block(block) = block in
			  let typ,id,_ = StringMap.find holename fillins in
			  let asstr = Pretty.sprint ~width:80 (printBlock Cilprinter.noLineCilPrinter () block)
			  in
			  let removed_placeholder = 
				Str.global_replace placeholder_regexp "" asstr in
			  let spaces =
				lfoldl
				  (fun current_str ->
					fun holenum ->
					  let holename = Printf.sprintf "__hole%d__" holenum in
					  let addspace_regexp = Str.regexp (")"^holename) in
						if any_match addspace_regexp current_str then
						  Str.global_replace addspace_regexp (") "^holename) current_str
						else current_str
				  ) removed_placeholder all_holes
			  in
			  let copy = 
				lfoldl
				  (fun current_str ->
					fun holenum ->
					  let holename = Printf.sprintf "__hole%d__" holenum in
					  let constraints = StringMap.find  holename template.hole_constraints in
					  let typformat = 
						match constraints.htyp with
						  Stmt_hole -> "%s:"
						| Exp_hole -> "%e:"
						| Lval_hole -> "%v:"
					  in
					  let fullformat = typformat^holename in
					  let current_regexp = Str.regexp (holename^".var;") in
					  let rep = Str.global_replace current_regexp fullformat current_str in
					  let current_regexp = Str.regexp (holename^".var") in
						Str.global_replace current_regexp fullformat rep
				  ) spaces all_holes
			  in
			  let new_code = 
				Formatcil.cStmt copy (fun n t -> failwith "This shouldn't require making new variables") Cil.locUnknown
				  arg_list
			  in
				(id, new_code) :: lst
		  ) template.hole_code_ht []
	in
	  liter
		(fun (to_replace,replace_with) ->
 		  let file = self#get_file to_replace in
 			visitCilFileSameGlobals (my_rep to_replace replace_with.skind) file
		) applied;
 	  if !check_invariant then self#check_invariant()
 	(* FIXME: does the invariant check make sense with mutate? *)
	  
  val template_cache = hcreate 10

  method available_mutations location_id =
	(* We don't precompute these in the interest of efficiency *)
	let iset_of_lst lst = 
	  lfoldl (fun set -> fun item -> IntSet.add item set) IntSet.empty lst
	in
	let pset_of_lst stmt lst = 
	  lfoldl (fun set -> fun item -> PairSet.add (stmt,item) set) PairSet.empty lst
	in
 	let fault_stmts () = iset_of_lst (lmap fst (self#get_fault_localization())) in
 	let fix_stmts () = iset_of_lst (lmap fst (self#get_fix_localization())) in
	let all_stmts () = iset_of_lst (1 -- self#max_atom()) in

	let exp_set start_set =
 	  IntSet.fold
 		(fun stmt -> 
		  fun all_set -> 
 			let subatoms = 1 -- (llen (self#get_subatoms stmt)) in
			  pset_of_lst stmt subatoms)
		start_set PairSet.empty
	in
  	let fault_exps () = exp_set (fault_stmts()) in
 	let fix_exps () = exp_set (fix_stmts ()) in
	let all_exps () = exp_set (all_stmts()) in

	let localshave = !global_ast_info.localshave in
	let lval_set start_set = 
	  IntSet.fold 
		(fun stmt ->
		  fun all_set ->
			IntSet.union all_set (IntMap.find stmt localshave)
		) start_set IntSet.empty
	in
	let fault_lvals () = lval_set (fault_stmts()) in
	let fix_lvals () = lval_set (fix_stmts ()) in
	let all_lvals () = lval_set (all_stmts ()) in

	ht_find template_cache location_id
	  (fun _ ->
		 let other_hole_is_not_dependent_on_this_one = true in
		 let rec fill_one_hole hole_name assignment_so_far unassigned_holes = 
		   let hole_info = StringMap.find hole_name unassigned_holes in
			 match hole_info.htyp with
			   Stmt_hole -> assign_one_stmt_hole hole_name hole_info.constraints IntSet.empty assignment_so_far unassigned_holes
			 | Exp_hole -> assign_one_exp_hole hole_name hole_info.constraints PairSet.empty assignment_so_far unassigned_holes
			 | Lval_hole -> assign_one_lval_hole hole_name hole_info.constraints IntSet.empty assignment_so_far unassigned_holes
		 and assign_one_stmt_hole (hole : string) (constraints : ConstraintSet.t) (candidate_stmts : IntSet.t) (assignment_so_far : filled StringMap.t) (unassigned_holes : hole_info StringMap.t) : (filled StringMap.t * hole_info StringMap.t) list = 
		   let rec all_statements_remaining_that_fill_one_constraint (candidate_stmts : IntSet.t) (assignment_so_far : filled StringMap.t) (unassigned_holes : hole_info StringMap.t) con : (IntSet.t * filled StringMap.t * hole_info StringMap.t) list = 
			 match con with
			 | Fault_path when IntSet.is_empty candidate_stmts -> [fault_stmts(), assignment_so_far, unassigned_holes]
			 | Fault_path -> [IntSet.inter (fault_stmts()) candidate_stmts, assignment_so_far, unassigned_holes]
			 | Fix_path when IntSet.is_empty candidate_stmts -> [fix_stmts(), assignment_so_far, unassigned_holes]
			 | Fix_path -> [IntSet.inter (fix_stmts()) candidate_stmts, assignment_so_far, unassigned_holes]
			 | InScope(other_hole) when StringMap.mem other_hole assignment_so_far ->
			   let candidate_stmts = if IntSet.is_empty candidate_stmts then all_stmts() else candidate_stmts in 
			   let hole_type,in_other_hole,maybe_exp = StringMap.find other_hole assignment_so_far in
				 assert(hole_type <> Lval_hole); (* I think *)
				   [IntSet.filter
					   (fun stmt ->
						 in_scope_at in_other_hole stmt !global_ast_info.localshave !global_ast_info.localsused
					   ) candidate_stmts, assignment_so_far, unassigned_holes]
			 | Ref(other_hole) when StringMap.mem other_hole assignment_so_far -> [] 
			 | InScope(other_hole) 
			 | Ref(other_hole) when other_hole_is_not_dependent_on_this_one -> 
			   let candidate_stmts = if IntSet.is_empty candidate_stmts then all_stmts () else candidate_stmts in
			   let assignments = fill_one_hole other_hole assignment_so_far unassigned_holes in
				 lflatmap
				   (fun (assignment, unassigned_holes) -> 
					 all_statements_remaining_that_fill_one_constraint candidate_stmts assignment unassigned_holes con)
				   assignments
			 | _ (* InScope(other_hole) and Ref(other_hole), for now *) -> []
		   in (* returns a list of candidate assigments and a list of remaining unassigned holes *)
			 if ConstraintSet.is_empty constraints then begin
			   let unassigned_holes = StringMap.remove hole unassigned_holes in
				 IntSet.fold
				   (fun id ->
					 fun assignment_list ->
					   ((StringMap.add hole (Stmt_hole,id,None) assignment_so_far),unassigned_holes) :: assignment_list)
				   candidate_stmts []
			 end else begin
			   let current_constraint = ConstraintSet.min_elt constraints in
			   let remaining_constraints = ConstraintSet.remove current_constraint constraints in
			   let assignment_lst : (IntSet.t * filled StringMap.t * hole_info StringMap.t) list =
				 all_statements_remaining_that_fill_one_constraint candidate_stmts assignment_so_far unassigned_holes  current_constraint
			   in
			   let viable = lfilt (fun (foo,_,_) -> not (IntSet.is_empty foo)) assignment_lst in
				 lflatmap
				   (fun ((candidate_statements,assignment,remaining) : IntSet.t * filled StringMap.t * hole_info StringMap.t) ->
					 assign_one_stmt_hole hole remaining_constraints candidate_statements assignment remaining)
				   viable
			 end
		 and assign_one_exp_hole (hole : string) (constraints : ConstraintSet.t) (candidate_exps : PairSet.t) (assignment_so_far : filled StringMap.t) (unassigned_holes : hole_info StringMap.t) : (filled StringMap.t * hole_info StringMap.t) list = 
 		   let rec all_exps_remaining_that_fill_one_constraint (candidate_exps : PairSet.t) (assignment_so_far : filled StringMap.t) (unassigned_holes : hole_info StringMap.t) con : (PairSet.t * filled StringMap.t * hole_info StringMap.t) list = 
			 match con with
			 | Fault_path when PairSet.is_empty candidate_exps -> [fault_exps(), assignment_so_far, unassigned_holes]
			 | Fault_path -> [PairSet.inter candidate_exps (fault_exps()), assignment_so_far, unassigned_holes]
 			 | Fix_path when PairSet.is_empty candidate_exps -> [fix_exps(), assignment_so_far, unassigned_holes]
 			 | Fix_path -> [PairSet.inter candidate_exps (fix_exps()), assignment_so_far, unassigned_holes]
			 | InScope(other_hole) when StringMap.mem other_hole assignment_so_far ->
			   let candidate_exps = if PairSet.is_empty candidate_exps then all_exps () else candidate_exps in 
			   let hole_type,in_other_hole,_ = StringMap.find other_hole assignment_so_far in
				 assert(hole_type <> Lval_hole);
				 [PairSet.filter
					 (fun (sid,subatom_id) ->
					   in_scope_at in_other_hole sid !global_ast_info.localshave !global_ast_info.localsused)
					 candidate_exps, assignment_so_far, unassigned_holes]
 			 | Ref(other_hole) when StringMap.mem other_hole assignment_so_far -> begin
			   let candidate_exps = if PairSet.is_empty candidate_exps then all_exps() else candidate_exps in 
			   let hole_type, in_other_hole, maybe_exp = StringMap.find other_hole assignment_so_far in
				 match hole_type with
				   Stmt_hole ->
					 let subatoms_there = self#get_subatoms in_other_hole in
					   [PairSet.filter
						 (fun (sid,subatom_id) ->
						   let this_atom = self#get_subatom sid subatom_id in
							 List.mem this_atom subatoms_there
						 ) candidate_exps, assignment_so_far, unassigned_holes]
				 | Exp_hole ->
				   let exp_id = match maybe_exp with Some(id) -> id in
				   let that_exp = self#get_subatom in_other_hole exp_id in
					 [PairSet.filter
					   (fun (sid,subatom_id) ->
						 let this_atom = self#get_subatom sid subatom_id in
						   this_atom == that_exp
					   ) candidate_exps, assignment_so_far, unassigned_holes]
				 | Lval_hole -> failwith "Unimplemented"
			 end
			 | InScope(other_hole)
			 | Ref(other_hole) when other_hole_is_not_dependent_on_this_one -> begin
			   let candidate_exps = if PairSet.is_empty candidate_exps then all_exps() else candidate_exps in 
			   let assignments = fill_one_hole other_hole assignment_so_far unassigned_holes in
				 lflatmap
				   (fun (assignment, unassigned_holes) -> 
					 all_exps_remaining_that_fill_one_constraint candidate_exps assignment unassigned_holes con)
				   assignments
			 end
			 | _ -> [] (* Inscope(other_hole) and Ref(other_hole) *)
 		   in 
			 if ConstraintSet.is_empty constraints then
			   let unassigned_holes = StringMap.remove hole unassigned_holes in
				 PairSet.fold
				   (fun (sid,exp_id) ->
					 fun assignment_list ->
					   ((StringMap.add hole (Exp_hole, sid,Some(exp_id)) assignment_so_far), unassigned_holes) :: assignment_list)
				   candidate_exps []
			 else 
			   let current_constraint = ConstraintSet.min_elt constraints in 
			   let remaining_constraints = ConstraintSet.remove current_constraint constraints in
			   let assignment_lst : (PairSet.t * filled StringMap.t * hole_info StringMap.t) list =
				 all_exps_remaining_that_fill_one_constraint candidate_exps assignment_so_far unassigned_holes current_constraint
			   in
			   let viable = lfilt (fun (foo,_,_) -> not (PairSet.is_empty foo)) assignment_lst in
				 lflatmap
				   (fun ((candidate_exps, assignment,remaining) : PairSet.t * filled StringMap.t * hole_info StringMap.t ) ->
					 assign_one_exp_hole hole remaining_constraints candidate_exps assignment remaining)
				   viable
		 and assign_one_lval_hole (hole : string) (constraints : ConstraintSet.t) (candidate_lvals : IntSet.t) (assignment_so_far : filled StringMap.t) (unassigned_holes : hole_info StringMap.t) : (filled StringMap.t * hole_info StringMap.t) list = 
		   let rec all_lvals_remaining_that_fill_one_constraint (candidate_lvals : IntSet.t) (assignment_so_far : filled StringMap.t) (unassigned_holes : hole_info StringMap.t) con : (IntSet.t * filled StringMap.t * hole_info StringMap.t) list = 
			 match con with
			 | Fault_path when IntSet.is_empty candidate_lvals -> [fault_lvals(), assignment_so_far, unassigned_holes]
			 | Fault_path -> [IntSet.inter candidate_lvals (fault_lvals()) , assignment_so_far, unassigned_holes]
			 | Fix_path when IntSet.is_empty candidate_lvals -> [fix_lvals(), assignment_so_far, unassigned_holes]
			 | Fix_path -> [IntSet.inter candidate_lvals (fix_lvals()), assignment_so_far, unassigned_holes]
			 | InScope(other_hole) when StringMap.mem other_hole assignment_so_far ->
			   let candidate_lvals = if IntSet.is_empty candidate_lvals then all_lvals() else candidate_lvals in
			   let hole_type,in_other_hole,_ = StringMap.find other_hole assignment_so_far in
				 assert(hole_type <> Lval_hole);
				 let locals = IntMap.find in_other_hole !global_ast_info.localshave in
				   [IntSet.filter
					   (fun vid ->
						 IntSet.mem vid locals || IntSet.mem vid !global_ast_info.globalsset
					   ) candidate_lvals, assignment_so_far, unassigned_holes]
			 | Ref(other_hole) when StringMap.mem other_hole assignment_so_far ->
			   begin
				 let candidate_lvals = if IntSet.is_empty candidate_lvals then all_lvals() else candidate_lvals in
				 let hole_type,in_other_hole,maybe_exp = StringMap.find other_hole assignment_so_far in
				   match hole_type with
					 Stmt_hole ->
					   (* Problem with scope: "is the same" <> "can be moved
						  there"; the first references unique vids, the second refers
						  to names.  But!  Does cil rename everything to be unique?
						  Hm, not sure. *)
					   let localsused = IntMap.find in_other_hole !global_ast_info.localsused in
					   let overlap = IntSet.inter localsused candidate_lvals in
						 [overlap, assignment_so_far, unassigned_holes]
				   | Exp_hole 
				   | Lval_hole -> failwith "Unimplemented"
			   end
			 | InScope(other_hole)
			 | Ref(other_hole) when other_hole_is_not_dependent_on_this_one ->
			   let candidate_lvals = if IntSet.is_empty candidate_lvals then all_lvals() else candidate_lvals in (* can probably handle this better but good enough for now *)
			   let assignments = fill_one_hole other_hole assignment_so_far unassigned_holes in
				 lflatmap
				   (fun (assignment, unassigned_holes) -> 
					 all_lvals_remaining_that_fill_one_constraint candidate_lvals assignment unassigned_holes con)
				   assignments
			 | InScope(other_hole) -> (* FIXME: for now *) []
			 | Ref(other_hole) -> []
		   in (* returns a list of candidate assigments and a list of remaining unassigned holes *)
			 if ConstraintSet.is_empty constraints then begin
			   let unassigned_holes = StringMap.remove hole unassigned_holes in
				 IntSet.fold
				   (fun id ->
					 fun assignment_list ->
					   ((StringMap.add hole (Lval_hole,id,None) assignment_so_far),unassigned_holes) :: assignment_list)
				   candidate_lvals []
			 end else begin
			   let current_constraint = ConstraintSet.min_elt constraints in
			   let remaining_constraints = ConstraintSet.remove current_constraint constraints in
			   let assignment_lst : (IntSet.t * filled StringMap.t * hole_info StringMap.t) list =
				 all_lvals_remaining_that_fill_one_constraint candidate_lvals assignment_so_far unassigned_holes current_constraint
			   in
			   let viable = lfilt (fun (foo,_,_) -> not (IntSet.is_empty foo)) assignment_lst in
				 lflatmap
				   (fun ((candidate_lvals,assignment,remaining) : IntSet.t * filled StringMap.t * hole_info StringMap.t) ->
					 assign_one_lval_hole hole remaining_constraints candidate_lvals assignment remaining)
				   viable
			 end
		 in
 		 let all_templs = 
 		   hfold (fun k -> fun v -> fun lst -> v :: lst) registered_c_templates []
 		 in
		 (* partially_fulfilled is a list of templates with a starting
			assignment of the location to one of the holes and a map of holes that
			remain to be filled for that template*)
		 let partially_fulfilled =
		   lflatmap
			 (fun template ->
			   let constraints = template.hole_constraints in
			   let holes = StringMap.fold (fun hole_id -> fun hole_info -> fun lst -> (hole_id, hole_info) :: lst) constraints [] in
			   let filtered = 
				 lfilt 
				   (fun (hole_id, hole_info) -> 
					 (match hole_info.htyp with Stmt_hole -> true | _ -> false) &&
					   ConstraintSet.mem Fault_path hole_info.constraints)
				   holes
			   in
				 lmap (fun (hole_id, hole_info) -> template, StringMap.add hole_id (Stmt_hole, location_id, None) (StringMap.empty), StringMap.remove hole_id constraints) filtered
			 ) all_templs
		 in
		 let rec process_remaining ((template, assignment_so_far, remaining_to_be_assigned) : cilRep_atom template * filled StringMap.t * hole_info StringMap.t) =
		   if StringMap.is_empty remaining_to_be_assigned then [template,1.0,assignment_so_far] else begin
			 let as_lst = StringMap.fold 
			   (fun hole_name ->
				 fun hole_info ->
				   fun lst ->
					 (hole_name, hole_info) :: lst) remaining_to_be_assigned []
			 in
			 let (name,one_hole) = List.hd as_lst in
			 let assignments = fill_one_hole name assignment_so_far remaining_to_be_assigned in
			   lflatmap (fun (assignment, remaining) -> process_remaining (template,assignment,remaining)) assignments 
		   end
		 in
		   lflatmap process_remaining partially_fulfilled)


  (***********************************
   * Structural Differencing
   ***********************************)

  method internal_structural_signature () =
	let final_list, node_map = 
	  StringMap.fold
		(fun key base (final_list,node_map) ->
		  let result = ref StringMap.empty in
		  let node_map = 
			foldGlobals base (fun node_map g1 ->
			  match g1 with
			  | GFun(fd,l) -> 
				let node_id, node_map = Cdiff.fundec_to_ast node_map fd in
				  result := StringMap.add fd.svar.vname node_id !result; node_map
			  | _ -> node_map
			) node_map in
			StringMap.add key !result final_list, node_map
		) (self#get_base ()) (StringMap.empty, Cdiff.init_map())
	in
	  { signature = final_list ; node_map = node_map}


  (***********************************
   * Invariant sanity checking
   ***********************************)

	(* this checks the datastructure invariant, and should be used while testing
	 * (not while in deployment!) and particularly after mutations or crossover
	 * operations. *)
  method check_invariant () = 
	  (* Each non-zero id should only appear once in the representation.  The
	   * collectVisitor checks this on a per-file basis; the assertion below the
	   * visitCilFileSameGlobals checks it between files. Additionally:
	   * --> for each id x in the representation, there exists 1 and only 1 y in
	   * either the code bank or the oracle code base s.t. x = y, and
	   * --> for each id y in the code bank, there exists at most 1 x in the
	   * base representation s.t. x = y and
       * the number of non-zero statement ids remains constant *)
    if IntSet.is_empty !invariant_info then 
      StringMap.iter
        (fun filename ->
          fun file ->
            collect_invariant_info filename file)
        (self#get_code_bank());
	let total_set = ref (IntSet.empty) in
	  StringMap.iter
		(fun filename ->
		  fun file ->
			let id_set = ref (IntSet.empty) in
			visitCilFileSameGlobals (my_collect id_set) file;
			assert(IntSet.is_empty (IntSet.inter !id_set !total_set));
			total_set := IntSet.union !id_set !total_set
		) (self#get_base ());
	  assert ((IntSet.cardinal !total_set) <= !num_unique_ids);
	  IntSet.iter
		(fun id -> assert(IntSet.mem id !invariant_info)) !total_set
	
end
