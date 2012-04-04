(**  Cil C AST.  This is the main implementation of the "Rep" interface for C programs.

 Notably, this includes code and support for: 
  -> compiling C programs
  -> running test cases on C programs
  -> computing "coverage" fault localization information automatically
  -> mutating C programs. 

  -> deleting/appending/swapping statements in C programs or loading/applying
     template-defined mutation operations

	 Supports both the AST and Patch representations for C programs.
*)

open Printf
open Global
open Cil
open Cilprinter
open Template
open Rep
open Pretty
open Minimization

let cilRep_version = "10" 
let semantic_check = ref "scope" 
let multithread_coverage = ref false
let uniq_coverage = ref false

(* CLG, 12/16/11: the "swap bug" was found in internal_calculate_output_xform.
 * internal_calculate_output_xform processes the edit list to see if any one is
 * appropriate at a given statement (while they're being printed out), applies
 * the edit in question if so, and removes that edit from the list of remaining
 * edits.  The problem is that a swap needs to be applied twice, but was also
 * removed after its first application (meaning it ended up being replace, in
 * practice). This bug applies to the ICSE 2012 GMB experiments; this flag
 * produces the buggy swap behavior. *)
let swap_bug = ref false 

let _ =
  options := !options @
  [
    "--semantic-check", Arg.Set_string semantic_check, "X limit CIL mutations {none,scope}" ;
    "--mt-cov", Arg.Set multithread_coverage, "  instrument for coverage with locks.  Avoid if possible.";
    "--uniq", Arg.Set uniq_coverage, "  print each visited stmt only once";
	"--swap-bug", Arg.Set swap_bug, " swap is implemented as in ICSE 2012 GMB experiments." 
  ] 


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

(* the xformRepVisitor applies a transformation function to a C AST.  Used to
   implement the patch representation for C programs, where the original program is
   transformed by a sequence of edit operations only at compile time. *)
class xformRepVisitor (xform : Cil.stmt -> Cil.stmt) = object(self)
  inherit nopCilVisitor

  method vstmt stmt = ChangeDoChildrenPost(stmt, (fun stmt -> xform stmt))
	
end
let my_xform = new xformRepVisitor


(*************************************************************************)
(* Initial source code processing *)
(***********************************************************************)

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
  try
    Hashtbl.find canonical_stmt_ht str
  with _ -> begin
    Hashtbl.add canonical_stmt_ht str sid ;
    incr canonical_uniques ; 
    sid 
  end 

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
 * Obtaining coverage and Weighted Path Information
 *************************************************************************)

(* In March 2012, CLG rewrote the coverage instrumenting code to make use of the
   Cil interpreted constructors.  Basically, Cil can construct an AST by
   interpreting a specially-formatted string that looks a lot like real C.  Back
   when coverage computation was dead-simple, constructing the AST by hand
   (stringing together CIL types) was simpler than the interpreted constructors,
   but with the addition of the uniq and multi-threaded printing options this
   code had gotten *super* busy.  Check out the Formatcil module in CIL to learn
   more about how this works. *)

(* These are CIL variables describing C standard library functions like
 * 'fprintf'. We use them when we are instrumenting the file to
 * print out statement coverage information for fault localization. *)  
let void_t = Formatcil.cType "void *" [] 
let stderr_va = Fv (makeVarinfo true "_coverage_fout" void_t)
let fflush_va = Fv (makeVarinfo true "fflush" void_t)
let memset_va = Fv (makeVarinfo true "memset" void_t)
let fprintf_va = Fv (makeVarinfo true "fprintf" void_t)
let fopen_va = Fv (makeVarinfo true "fopen" void_t)
let fclose_va = Fv (makeVarinfo true "fclose" void_t)
  
let uniq_array_va = ref
  (makeGlobalVar "___coverage_array" (Formatcil.cType "char *" []))
let do_not_instrument_these_functions = 
  [ "fflush" ; "memset" ; "fprintf" ; "fopen" ; "fclose" ; "vgplain_fmsg" ] 

let static_args = 
  [("fout",stderr_va);("fprintf", fprintf_va);
   ("fflush", fflush_va);("fclose", fclose_va);
   ("fopen",fopen_va);("wb_arg", Fg("wb"));
   ("memset", memset_va);("a_arg", Fg("a")); ]

let cstmt stmt_str args = 
  Formatcil.cStmt ("{"^stmt_str^"}") (fun _ -> failwith "no new varinfos!")  !currentLoc 
	(args@static_args)

(* 
 * Visitor for computing statement coverage (for a "weighted path").
 *
 * This visitor walks over the C program AST and modifies it so that each
 * statment is preceeded by a 'printf' that writes that statement's number
 * to the .path file at run-time. *) 

(* FIXME: multithreaded and uniq coverage are not going to play nicely here in
   terms of memset *)
class covVisitor coverage_outname = 
object
  inherit nopCilVisitor

  method vblock b = 
    ChangeDoChildrenPost(b,(fun b ->
      let result = List.map (fun stmt -> 
        if stmt.sid > 0 then begin
		  let str = Printf.sprintf "%d\n" stmt.sid in
		  let print_str = 
				"fprintf(fout, %g:str);\n"^
				"fflush(fout);\n"
		  in
		  let print_str = 
			if !uniq_coverage then 
			  "if ( uniq_array[%d:index] == 0 ) {\n" ^
				print_str^
				"uniq_array[%d:index] = 1; }\n"
			else
			  print_str
		  in
		  let print_str = 
            if !multithread_coverage then 
			  "fout = fopen(%g:fout_g, %g:a_arg);\n"^print_str^"fclose(fout);\n"
			else 
			  print_str 
		  in
		  let newstmt = cstmt print_str 
			[("uniq_array", Fv(!uniq_array_va));("fout_g",Fg coverage_outname);
			 ("index", Fd (stmt.sid)); ("str",Fg(str))]
		  in
            [ newstmt ; stmt ] 
        end else [stmt]
      ) b.bstmts in 
        { b with bstmts = List.flatten result } 
    ) )

  method vfunc f = 
    if List.mem f.svar.vname do_not_instrument_these_functions then begin 
      debug "cilRep: WARNING: definition of fprintf found at %s:%d\n\tcannot instrument for coverage (would be recursive)\n"
        f.svar.vdecl.file f.svar.vdecl.line ;
      SkipChildren
    end else if not !multithread_coverage then begin
	  let uniq_instrs = 
		if !uniq_coverage then
		  "memset(uniq_array, 0, sizeof(uniq_array));\n"
		else "" 
	  in
	  let stmt_str = 
		"if (fout == 0) {\n fout = fopen(%g:fout_g,%g:wb_arg);\n"^uniq_instrs^"}"
	  in
      let ifstmt = cstmt stmt_str 
		[("uniq_array", Fv(!uniq_array_va));("fout_g",Fg coverage_outname);]
	  in
		ChangeDoChildrenPost(f,
							 (fun f ->
							   f.sbody.bstmts <- ifstmt :: f.sbody.bstmts;
							   f))
    end else DoChildren

end 


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



(*************************************************************************
 *************************************************************************
                          CIL Representation 
                  (both single and multi file)
 *************************************************************************
 *************************************************************************)


type cilRep_atom =
  | Stmt of Cil.stmtkind
  | Exp of Cil.exp 

let registered_c_templates = hcreate 10

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

class virtual ['gene] cilRep  = object (self : 'self_type)
  inherit minimizableObject 
  inherit ['gene, cilRep_atom] faultlocRepresentation as super

  val stmt_count = ref 1 

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
    {< history = ref !history; 
	  stmt_count = ref !stmt_count >}
  end

  (* serialize the state *) 
  method serialize ?out_channel ?global_info (filename : string) = begin
    let fout = 
      match out_channel with
      | Some(v) -> v
      | None -> open_out_bin filename 
    in 
      Marshal.to_channel fout (cilRep_version) [] ; 
	  let gval = match global_info with Some(true) -> true | _ -> false in
	  if gval then begin
		Marshal.to_channel fout (!global_ast_info.code_bank) [] ;
		Marshal.to_channel fout (!global_ast_info.oracle_code) [] ;
		Marshal.to_channel fout (!global_ast_info.stmt_map) [] ;
		Marshal.to_channel fout (!stmt_count) [] ;
		let triple = !global_ast_info.localshave,!global_ast_info.localsused, !global_ast_info.all_source_sids in
		  Marshal.to_channel fout triple [] ;
	  end;
      Marshal.to_channel fout (self#get_genome()) [] ;
      super#serialize ~out_channel:fout ?global_info:global_info filename ;
      debug "cilRep: %s: saved\n" filename ; 
      if out_channel = None then close_out fout 
  end 

  (* load in serialized state *) 
  method deserialize ?in_channel ?global_info (filename : string) = begin
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
	  let gval = match global_info with Some(true) -> true | _ -> false in
		if gval then begin
      global_ast_info := {!global_ast_info with code_bank = Marshal.from_channel fin } ; 
      global_ast_info := {!global_ast_info with oracle_code = Marshal.from_channel fin } ; 
      global_ast_info := {!global_ast_info with stmt_map = Marshal.from_channel fin } ;
      stmt_count := Marshal.from_channel fin ;
      let var_maps = Marshal.from_channel fin in
		global_ast_info := {!global_ast_info with localshave = fst3 var_maps} ;
		global_ast_info := {!global_ast_info with localsused = snd3 var_maps} ;
		global_ast_info := {!global_ast_info with all_source_sids = trd3 var_maps} ;
		end;
		self#set_genome (Marshal.from_channel fin);
		super#deserialize ~in_channel:fin ?global_info:global_info filename ; 
      debug "cilRep: %s: loaded\n" filename ; 
      if in_channel = None then close_in fin ;
  end 

  (* print debugging information *)  
  method debug_info () = begin
    debug "cilRep: stmt_count = %d\n" !stmt_count ;
    debug "cilRep: stmts in weighted_path = %d\n" 
      (List.length !fault_localization) ; 
    debug "cilRep: total weight = %g\n"
      (lfoldl (fun total (i,w) -> total +. w) 0.0 !fault_localization);
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
  method get_file (stmt_id : atom_id) : Cil.file = begin
    let _,fname = AtomMap.find stmt_id (self#get_stmt_map()) in
      StringMap.find fname (self#get_base())
  end

  (***********************************
   * Functions that manipulate C source code
   ***********************************)

  (* load in a CIL AST from a C source file *) 
  method from_source (filename : string) = begin 
    debug "cilrep: from_source: stmt_count = %d\n" !stmt_count ; 
    let _,ext = split_ext filename in 
      (match ext with
        "txt" ->
          liter
            (fun fname ->
              global_ast_info := {!global_ast_info with code_bank = StringMap.add fname (self#from_source_one_file fname) !global_ast_info.code_bank })
            (get_lines filename)
      | "c" | "i" -> 
        global_ast_info := {!global_ast_info with code_bank = StringMap.add filename (self#from_source_one_file filename) !global_ast_info.code_bank }
      | _ -> debug "extension: %s\n" ext; failwith "Unexpected file extension in CilRep#from_source.  Permitted: .c, .txt");
	  debug "stmt_count: %d\n" !stmt_count;
      stmt_count := pred !stmt_count 
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
    debug "cilRep: %s: parsing\n" filename ; 
    let file = Frontc.parse filename () in 
      debug "cilRep: %s: parsed (%g MB)\n" filename (debug_size_in_mb file); 
      file 

  method get_compiler_command () = 
    "__COMPILER_NAME__ -o __EXE_NAME__ __SOURCE_NAME__ __COMPILER_OPTIONS__ 1>/dev/null 2>/dev/null" 

  method from_source_one_file (filename : string) : Cil.file = begin
    let full_filename = filename in
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
		let file = self#from_source_one_file fname in
		let oracle = !global_ast_info.oracle_code in 
			global_ast_info := {!global_ast_info with oracle_code = 
				StringMap.add fname file oracle}
      ) filelist;
      stmt_count := pred !stmt_count
  end


  (* 
   * The heart of cilPatchRep -- to print out this variant, we print
   * out the original, *but*, if we encounter a statement that
   * has been changed (say, deleted), we print that statement out
   * differently. 
   *
   * This is implemented via a "transform" function that is applied,
   * by the Printer, to every statement just before it is printed.
   * We either replace that statement with something new (e.g., if
   * we've Deleted it, we replace it with an empty block) or leave
   * it unchanged.
   *) 
  method internal_calculate_output_xform () = begin 

    (* Because the transform is called on every statement, and "no change"
     * is the common case, we want this lookup to be fast. We'll use
     * a hash table on statement ids for now -- we can go to an IntSet
     * later.. *) 
    let relevant_targets = Hashtbl.create 255 in 
    let edit_history = self#get_history () in 
    (* Go through the history and find all statements that are changed. *) 
    List.iter (fun h -> match h with 
      | Delete(x) 
      | Append(x,_) 
	  | Replace(x,_)
      | Replace_Subatom(x,_,_) 
      -> Hashtbl.replace relevant_targets x true 
      | Swap(x,y) -> 
        Hashtbl.replace relevant_targets x true ;
        Hashtbl.replace relevant_targets y true ;
	  | Template(tname,fillins) ->
		let template = self#get_template tname in
		let changed_holes =
		  hfold
			(fun hole_name ->
			  fun _ ->
				fun lst -> hole_name :: lst)
			template.hole_code_ht []
		in
		  liter
			(fun hole ->
			  let _,stmt_id,_ = StringMap.find hole fillins in
				Hashtbl.replace relevant_targets stmt_id true)
			changed_holes
      | Crossover(_,_) -> 
        abort "cilPatchRep: Crossover not supported\n" 
    ) edit_history ; 

    (* As found by Mike Dewey-Vogt, suppose
     * /* S1 == */ if (p) { foo(); } else { /* S5 */ bar(); } 
     * ... and we try "append S1 after S5". We'll keep finding new
     * instances of S5, which might make an infinite loop. So we only
     * want to apply each operation once. We keep track of remaining
     * operations in a list (since the expected number of operations is
     * under 50). 
     *)
    let edits_remaining = 
	  if !swap_bug then ref edit_history else 
		(* double each swap in the edit history, if you want the correct swap behavior
		 * (as compared to the buggy ICSE 2012 behavior); this means each swap
		 * is in the list twice and thus will be applied twice (once at each
		 * relevant location.) *)
		ref (lflat 
			   (lmap 
				  (fun edit -> 
					match edit with Swap _ -> [edit; edit] 
					| _ -> [edit]) edit_history))
	in
    (* Now we build up the actual transform function. *) 
    let the_xform stmt = 
      let this_id = stmt.sid in 
      (* Most statement will not be in the hashtbl. *)  

      if Hashtbl.mem relevant_targets this_id then begin

        (* For Append or Swap we may need to look the source up 
         * in the "code bank". *) 
        let lookup_stmt src_sid =  
          let f,statement_kind = 
            try self#get_stmt src_sid 
            with _ -> (abort "cilPatchRep: %d not found in stmt_map\n" 
                       src_sid) 
          in statement_kind
        in 

        (* If the history is [e1;e2], then e1 was applied first, followed by
         * e2. So if e1 is a delete for stmt S and e2 appends S2 after S1, 
         * we should end up with the empty block with S2 appended. So, in
         * essence, we need to appliy the edits "in order". *) 
        List.fold_left (fun accumulated_stmt this_edit -> 
          let used_this_edit, resulting_statement = match this_edit with
          (* The code for each operation is taken from Cilrep.ml's
           * various visitors. *) 
          | Replace_Subatom(x,subatom_id,atom) when x = this_id -> 
            abort "cilPatchRep: Replace_Subatom not supported\n" 
          | Swap(x,y) when x = this_id  -> 
            let what_to_swap = lookup_stmt y in 
            true, 
              { accumulated_stmt with skind = copy what_to_swap ;
                 labels = possibly_label accumulated_stmt "swap1" y ; } 
          | Swap(y,x) when x = this_id -> 
            let what_to_swap = lookup_stmt y in 
            true, 
            { accumulated_stmt with skind = copy what_to_swap ;
                 labels = possibly_label accumulated_stmt "swap2" y ; } 

          | Delete(x) when x = this_id -> 
            let block = { battrs = [] ; bstmts = [] ; } in
            true, 
            { accumulated_stmt with skind = Block block ; 
                 labels = possibly_label accumulated_stmt "del" x; } 
          | Append(x,y) when x = this_id -> 
            let s' = { accumulated_stmt with sid = 0 } in 
            let what_to_append = lookup_stmt y in 
            let copy = 
              (visitCilStmt my_zero (mkStmt (copy what_to_append))).skind in 
            let block = {
              battrs = [] ;
              bstmts = [s' ; { s' with skind = copy } ] ; 
            } in
            true, 
            { accumulated_stmt with skind = Block(block) ; 
                     labels = possibly_label accumulated_stmt "app" y ; } 

          | Replace(x,y) when x = this_id -> 
            let s' = { accumulated_stmt with sid = 0 } in 
            let what_to_append = lookup_stmt y in 
            let copy = 
              (visitCilStmt my_zero (mkStmt (copy what_to_append))).skind in 
            let block = {
              battrs = [] ;
              bstmts = [{ s' with skind = copy } ] ; 
            } in
            true, 
            { accumulated_stmt with skind = Block(block) ; 
                     labels = possibly_label accumulated_stmt "rep" y ; } 

		  | Template(tname,fillins) -> begin
			  try
				(StringMap.iter
				  (fun hole_name ->
					fun fillin ->
					  let _,id,_ = fillin in 
						if id = this_id then raise (FoundIt(hole_name)))
				  fillins); false, accumulated_stmt
			  with FoundIt(hole_name) -> 
				begin
				  let template = self#get_template tname in
				  let block = hfind template.hole_code_ht hole_name in 
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
								  Fv atom
							in
							  (hole, item) :: arg_list
					  ) fillins []
				  in
				  let asstr = Pretty.sprint ~width:80 (printBlock Cilprinter.noLineCilPrinter () block) in
				  let placeholder_regexp = Str.regexp_string " = ___placeholder___.var" in
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
					true, new_code
				 end
		  end
			  (* Otherwise, this edit does not apply to this statement. *) 
		  | _ -> false, accumulated_stmt
          in 
			if used_this_edit then begin 
              edits_remaining := List.filter (fun x -> x <> this_edit)
				!edits_remaining  
			end ; 
          resulting_statement
        ) stmt !edits_remaining 
      end else stmt 
    in 
    the_xform 
  end 

  method updated () =
	already_signatured := None;
	super#updated()

  val min_script = ref None

  method from_source_min cilfile_list node_map = 
	self#updated ();
	min_script := Some(cilfile_list, node_map)

  method internal_compute_source_buffers () = 
    let make_name n = if !multi_file then Some(n) else None in
	let output_list = 
	  match !min_script with
		Some(difflst, node_map) ->
		  let new_file_map = 
			lfoldl (fun file_map ->
			  fun (filename,diff_script) ->
				let base_file = copy (StringMap.find filename !global_ast_info.code_bank) in
				let mod_file = Cdiff.usediff base_file node_map diff_script (copy cdiff_data_ht) 
				in
				  StringMap.add filename mod_file file_map)
			  (self#get_base ()) difflst 
		  in
			StringMap.fold
			  (fun (fname:string) (cil_file:Cil.file) ->
				fun output_list -> 
				  let source_string = output_cil_file_to_string cil_file in
					(make_name fname,source_string) :: output_list 
			  ) new_file_map [] 
	  | None ->
		let xform = self#internal_calculate_output_xform () in 
		  StringMap.fold
			(fun (fname:string) (cil_file:Cil.file) ->
			  fun output_list ->
				let source_string = output_cil_file_to_string ~xform cil_file in
				  (make_name fname,source_string) :: output_list 
			) (self#get_base ()) [] 
	in
	  assert((llen output_list) > 0);
	  output_list

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
        uniq_array_va := (makeGlobalVar "___coverage_array"
							(Formatcil.cType "char[%d:siz]" [("siz",Fd (1 + !stmt_count))]));
		[GVarDecl(!uniq_array_va,!currentLoc)]
	  end else []
	in
	let Fv(stderr_va) = stderr_va in
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

  val template_cache = hcreate 10

  method set_history new_history = history := new_history 

  method template_available_mutations template_name location_id =
	(* We don't precompute these in the interest of efficiency *)
	let iset_of_lst lst = 
	  lfoldl (fun set item -> IntSet.add item set) IntSet.empty lst
	in
	let pset_of_lst stmt lst = 
	  lfoldl (fun set item -> PairSet.add (stmt,item) set) PairSet.empty lst
	in
 	let fault_stmts () = iset_of_lst (lmap fst (self#get_faulty_atoms())) in
 	let fix_stmts () = iset_of_lst (lmap fst (self#get_faulty_atoms())) in
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

	let lval_set start_set = 
	  IntSet.fold 
		(fun stmt ->
		  fun all_set ->
			IntSet.union all_set (IntMap.find stmt !global_ast_info.localshave)
		) start_set IntSet.empty
	in
	let fault_lvals () = lval_set (fault_stmts()) in
	let fix_lvals () = lval_set (fix_stmts ()) in
	let all_lvals () = lval_set (all_stmts ()) in
	ht_find template_cache location_id
	  (fun _ ->
		 let other_hole_is_not_dependent_on_this_one = true in
		 let rec one_hole (hole : hole_info) (curr_assignment : filled StringMap.t) (holes_left : hole_info StringMap.t) : (filled StringMap.t * hole_info StringMap.t) list = 
		   let hole_return_value fulfills_constraints =
			 lflatmap 
			   (fun (candidates,curr_assignment,holes_left) ->
				 lmap (fun id ->
				   StringMap.add hole.hole_id (hole.htyp,id,None) curr_assignment,
				   StringMap.remove hole.hole_id holes_left)
				   (IntSet.elements candidates)) fulfills_constraints
		   in
		   let stmt () =
			 let rec one_constraint candidates curr_assignment holes_left con = 
			   match con with
			   | Fault_path when IntSet.is_empty candidates -> [fault_stmts(), curr_assignment, holes_left]
			   | Fault_path -> [IntSet.inter (fault_stmts()) candidates, curr_assignment, holes_left]
			   | Fix_path when IntSet.is_empty candidates -> [fix_stmts(), curr_assignment, holes_left]
			   | Fix_path -> [IntSet.inter (fix_stmts()) candidates, curr_assignment, holes_left]
			   | InScope(other_hole) when StringMap.mem other_hole curr_assignment ->
				 let candidates = if IntSet.is_empty candidates then all_stmts() else candidates in 
				 let hole_type,in_other_hole,maybe_exp = StringMap.find other_hole curr_assignment in
				   assert(hole_type <> Lval_hole); (* I think *)
				   [IntSet.filter
					   (fun stmt ->
						 in_scope_at in_other_hole stmt !global_ast_info.localshave !global_ast_info.localsused
					   ) candidates, curr_assignment, holes_left]
			   | Ref(other_hole) when StringMap.mem other_hole curr_assignment -> [] 
			   | InScope(other_hole) 
			   | Ref(other_hole) when other_hole_is_not_dependent_on_this_one -> 
				 let candidates = if IntSet.is_empty candidates then all_stmts () else candidates in
				 let assignments = one_hole (StringMap.find other_hole holes_left) curr_assignment holes_left in
				   lflatmap
					 (fun (assignment, holes_left) -> 
					   one_constraint candidates assignment holes_left con)
					 assignments
			   | _ (* InScope(other_hole) and Ref(other_hole), for now *) -> []
			 in
			   hole_return_value 
				 (lfoldl
					(fun candidates con ->
					  lflatmap
						(fun (candidate_stmts, assignment, remaining) ->
						  lfilt (fun (foo,_,_) -> not (IntSet.is_empty foo))
							(one_constraint candidate_stmts assignment remaining con))
						candidates 
					) [(IntSet.empty, curr_assignment,holes_left)] (ConstraintSet.elements hole.constraints))
		   in
		   let exp () =
 			 let rec one_constraint candidates curr_assignment holes_left con =
			   match con with
			   | Fault_path when PairSet.is_empty candidates -> [fault_exps(), curr_assignment, holes_left]
			   | Fault_path -> [PairSet.inter candidates (fault_exps()), curr_assignment, holes_left]
 			   | Fix_path when PairSet.is_empty candidates -> [fix_exps(), curr_assignment, holes_left]
 			   | Fix_path -> [PairSet.inter candidates (fix_exps()), curr_assignment, holes_left]
			   | InScope(other_hole) when StringMap.mem other_hole curr_assignment ->
				 let candidates = if PairSet.is_empty candidates then all_exps () else candidates in 
				 let hole_type,in_other_hole,_ = StringMap.find other_hole curr_assignment in
				   assert(hole_type <> Lval_hole);
				   [PairSet.filter
					   (fun (sid,subatom_id) ->
						 in_scope_at in_other_hole sid !global_ast_info.localshave !global_ast_info.localsused)
					   candidates, curr_assignment, holes_left]
 			   | Ref(other_hole) when StringMap.mem other_hole curr_assignment -> begin
				 let candidates = if PairSet.is_empty candidates then all_exps() else candidates in 
				 let hole_type, in_other_hole, maybe_exp = StringMap.find other_hole curr_assignment in
				   match hole_type with
					 Stmt_hole ->
					   let subatoms_there = self#get_subatoms in_other_hole in
						 [PairSet.filter
							 (fun (sid,subatom_id) ->
							   let this_atom = self#get_subatom sid subatom_id in
								 List.mem this_atom subatoms_there
							 ) candidates, curr_assignment, holes_left]
				   | Exp_hole ->
					 let exp_id = match maybe_exp with Some(id) -> id in
					 let that_exp = self#get_subatom in_other_hole exp_id in
					   [PairSet.filter
						   (fun (sid,subatom_id) ->
							 let this_atom = self#get_subatom sid subatom_id in
							   this_atom == that_exp
						   ) candidates, curr_assignment, holes_left]
				   | Lval_hole -> failwith "Unimplemented"
			   end
			   | InScope(other_hole)
			   | Ref(other_hole) when other_hole_is_not_dependent_on_this_one ->
				 let candidates = if PairSet.is_empty candidates then all_exps() else candidates in 
				 let assignments = one_hole (StringMap.find other_hole holes_left) curr_assignment holes_left in
				   lflatmap
					 (fun (assignment, holes_left) -> 
					   one_constraint candidates assignment holes_left con)
					 assignments
			   | _ -> [] (* Inscope(other_hole) and Ref(other_hole) *)
 			 in 
			 let fulfills_constraints : (PairSet.t * filled StringMap.t * hole_info StringMap.t) list = 
			   lfoldl
				 (fun candidates con ->
				   lflatmap
					 (fun (candidate_exps, assignment, remaining) ->
					   lfilt (fun (foo,_,_) -> not (PairSet.is_empty foo))
						 (one_constraint candidate_exps assignment remaining con))
					 candidates 
				 ) [(PairSet.empty, curr_assignment,holes_left)] (ConstraintSet.elements hole.constraints)
			 in
			   lflatmap (fun (candidate_exps,assignment_so_far,unassigned_holes) ->
				 PairSet.fold
				   (fun (id1,id2) assignment_list ->
					 ((StringMap.add hole.hole_id (Exp_hole,id1,Some(id2)) assignment_so_far),StringMap.remove hole.hole_id unassigned_holes) :: assignment_list)
				   candidate_exps []) fulfills_constraints
		   in
		   let lval () =
			 let rec one_constraint candidates curr_assignment holes_left con =
			   match con with
			   | Fault_path when IntSet.is_empty candidates -> [fault_lvals(), curr_assignment, holes_left]
			   | Fault_path -> [IntSet.inter candidates (fault_lvals()) , curr_assignment, holes_left]
			   | Fix_path when IntSet.is_empty candidates -> [fix_lvals(), curr_assignment, holes_left]
			   | Fix_path -> [IntSet.inter candidates (fix_lvals()), curr_assignment, holes_left]
			   | InScope(other_hole) when StringMap.mem other_hole curr_assignment ->
				 let candidates = if IntSet.is_empty candidates then all_lvals() else candidates in
				 let hole_type,in_other_hole,_ = StringMap.find other_hole curr_assignment in
				   assert(hole_type <> Lval_hole);
				   let locals = IntMap.find in_other_hole !global_ast_info.localshave in
					 [IntSet.filter
						 (fun vid ->
						   IntSet.mem vid locals || IntSet.mem vid !global_ast_info.globalsset
						 ) candidates, curr_assignment, holes_left]
			   | Ref(other_hole) when StringMap.mem other_hole curr_assignment ->
				 begin
				   let candidates = if IntSet.is_empty candidates then all_lvals() else candidates in
				   let hole_type,in_other_hole,maybe_exp = StringMap.find other_hole curr_assignment in
					 match hole_type with
					   Stmt_hole ->
					   (* Problem with scope: "is the same" <> "can be moved
						  there"; the first references unique vids, the second refers
						  to names.  But!  Does cil rename everything to be unique?
						  Hm, not sure. *)
						 let localsused = IntMap.find in_other_hole !global_ast_info.localsused in
						   [IntSet.inter localsused candidates, curr_assignment, holes_left]
					 | Exp_hole 
					 | Lval_hole -> failwith "Unimplemented"
				 end
			   | InScope(other_hole)
			   | Ref(other_hole) when other_hole_is_not_dependent_on_this_one ->
				 let candidates = if IntSet.is_empty candidates then all_lvals() else candidates in
				 let other_hole = StringMap.find other_hole holes_left in
				 let assignments = one_hole other_hole curr_assignment holes_left in
				   lflatmap
					 (fun (assignment, holes_left) -> 
					   one_constraint candidates assignment holes_left con)
					 assignments
			   | InScope(other_hole) -> (* FIXME: for now *) []
			   | Ref(other_hole) -> []
			 in
			   hole_return_value
				 (lfoldl
					(fun candidates con ->
					  lflatmap
						(fun (candidate_lvals, assignment, remaining) ->
						  lfilt (fun (foo,_,_) -> not (IntSet.is_empty foo))
							(one_constraint candidate_lvals assignment remaining con))
						candidates 
					) [(IntSet.empty, curr_assignment,holes_left)] (ConstraintSet.elements hole.constraints))
		   in
			 match hole.htyp with
			   Stmt_hole -> stmt ()
			 | Exp_hole -> exp ()
			 | Lval_hole -> lval ()
		 in
 		 let all_templs = 
 		   hfold (fun k -> fun v -> fun lst -> v :: lst) registered_c_templates []
 		 in
		 (* partially_fulfilled is a list of templates with a starting
			assignment of the location to one of the holes and a map of holes that
			remain to be filled *)
		 let partially_fulfilled =
		   lflatmap
			 (fun template ->
			   StringMap.fold 
				 (fun hole_id hole_info lst -> 
				   if hole_info.htyp = Stmt_hole && ConstraintSet.mem Fault_path hole_info.constraints then
					 (template.template_name, 
					  StringMap.add hole_id (Stmt_hole,location_id,None) (StringMap.empty), 
					  StringMap.remove hole_id template.hole_constraints) :: lst
				   else lst) template.hole_constraints []
			 ) all_templs
		 in
		 let rec one_template ((template, curr_assignment, remaining_to_be_assigned) : string * filled StringMap.t * hole_info StringMap.t) =
		   if StringMap.is_empty remaining_to_be_assigned then [template,1.0,curr_assignment] else begin
			 let as_lst = StringMap.fold 
			   (fun hole_name ->
				 fun hole_info ->
				   fun lst ->
					 (hole_name, hole_info) :: lst) remaining_to_be_assigned []
			 in
			 let (name,hole_info) = List.hd as_lst in
			 let assignments = one_hole hole_info curr_assignment remaining_to_be_assigned in
			 let assignments = 
			   if StringMap.cardinal curr_assignment > 1 then begin
				 lfilt (fun (assignment,remaining) ->
				   try 
					 StringMap.iter
					   (fun k (t1,id1,eopt1) ->
						 let without = StringMap.remove k assignment in
						   StringMap.iter
							 (fun _ (t2,id2,eopt2) -> 
							   if t1 = t2 && id1 = id2 && eopt1 = eopt2 then raise (FoundIt(k)))
							 without
					   ) assignment; true
				   with FoundIt _ -> false) assignments
			   end else assignments in 
			   lflatmap (fun (assignment, remaining) -> one_template (template,assignment,remaining)) assignments 
		   end
		 in
		   ignore(lflatmap one_template partially_fulfilled); [])



  (***********************************
   * Structural Differencing
   ***********************************)

  method internal_structural_signature () = begin
	let xform = self#internal_calculate_output_xform () in
	let final_list, node_map = 
	  StringMap.fold
		(fun key base (final_list,node_map) ->
		  let base_cpy = (copy base) in
		  visitCilFile (my_xform xform) base_cpy;
		  let result = ref StringMap.empty in
		  let node_map = 
			foldGlobals base_cpy (fun node_map g1 ->
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
  end

  method note_success () =
	(* Diff script minimization *)
	let orig = self#copy () in
	  orig#set_history [];
	  Minimization.do_minimization orig self

  initializer Cil.initCIL ()

end
  
class patchCilRep = object (self : 'self_type)
  inherit [cilRep_atom edit_history] cilRep
  (***********************************
   * Concrete State Variables
   ***********************************)
  method get_base () = !global_ast_info.code_bank
  method genome_length () = llen !history
  method set_genome g = 
	self#set_history g;
	self#updated()

  method get_genome () = !history

  method load_genome_from_string str = 
	let split_repair_history = Str.split (Str.regexp " ") str in
	let repair_history =
      List.fold_left ( fun acc x ->
		let the_action = String.get x 0 in
		  match the_action with
			'd' -> Scanf.sscanf x "%c(%d)" (fun _ id -> (Delete(id)) :: acc)
		  | 'a' -> Scanf.sscanf x "%c(%d,%d)" (fun _ id1 id2 -> (Append(id1,id2)) :: acc)
		  | 's' -> Scanf.sscanf x "%c(%d,%d)" (fun _ id1 id2 -> (Swap(id1,id2)) :: acc)
		  | 'r' -> Scanf.sscanf x "%c(%d,%d)" (fun _ id1 id2 -> (Replace(id1,id2)) :: acc)
	  |  _ -> assert(false)
      ) [] split_repair_history
    in
    self#set_genome (List.rev repair_history);

end


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


class astCilRep = object(self)
  inherit [cilRep_atom] cilRep as super
  inherit [cilRep_atom, cilRep_atom] faultlocRepresentation as faultlocSuper
  method variable_length = false

  (* "base" holds the ASTs associated with this representation, as
   * mapping from source file name to Cil AST. 
   *
   * Use self#get_base () to access.
   * "base" is different from "code_bank!"
   *) 

  val base = ref ((StringMap.empty) : Cil.file StringMap.t)
  method get_base () = !base

  method load_genome_from_string str = 
	let split_repair_history = Str.split (Str.regexp " ") str in
	let repair_history =
      List.fold_left ( fun acc x ->
		let the_action = String.get x 0 in
		  match the_action with
			'd' -> Scanf.sscanf x "%c(%d)" (fun _ id -> (Delete(id)) :: acc)
		  | 'a' -> Scanf.sscanf x "%c(%d,%d)" (fun _ id1 id2 -> (Append(id1,id2)) :: acc)
		  | 's' -> Scanf.sscanf x "%c(%d,%d)" (fun _ id1 id2 -> (Swap(id1,id2)) :: acc)
		  | 'r' -> Scanf.sscanf x "%c(%d,%d)" (fun _ id1 id2 -> (Replace(id1,id2)) :: acc)
	  |  _ -> assert(false)
      ) [] split_repair_history
    in
    self#set_history (List.rev repair_history);
 	
  method get_genome () = lmap self#get (lmap fst !fault_localization)
  method genome_length () = llen !fault_localization

  method set_genome lst =
	self#updated();
	List.iter2 (fun id atom -> self#put id atom) (lmap fst !fault_localization) lst


  (* get obtains an atom from the current variant, *not* from the code
     bank *) 

  (* The "get" method's return value is based on the 'current', 'actual'
   * content of the variant and not the 'code bank'. 
   * 
   * So we get the 'original' answer and then apply all relevant edits that
   * have happened since then. *) 

  (* get obtains an atom from the current variant, *not* from the code
     bank *) 
  method inner_get (stmt_id : atom_id) : cilRep_atom = begin
    let file = self#get_file stmt_id in
      visitCilFileSameGlobals (my_get stmt_id) file;
      let answer = !gotten_code in
        gotten_code := (mkEmptyStmt()).skind ;
        (Stmt answer) 
  end

  (* NOTE: CHECK TO MAKE SURE I PROPERLY REPLACED SUPER/SELF to avoid infinite
	 loops *)

  method get (stmt_id : atom_id) : cilRep_atom = 
    let xform = self#internal_calculate_output_xform () in 
      match (self#inner_get stmt_id) with
      | Stmt(skind) -> 
		let stmt = Cil.mkStmt skind in
		  stmt.sid <- stmt_id ; 
		  let post_edit_stmt = xform stmt in 
			(Stmt(post_edit_stmt.skind))
      | Exp(exp) -> 
		abort "cilPatchRep: get %d returned Exp" stmt_id 
 
  method put stmt_id (stmt : cilRep_atom) = begin
    let file = self#get_file stmt_id in 
    (match stmt with
    | Stmt(stmt) -> 
      visitCilFileSameGlobals (my_put stmt_id stmt) file;
	  visitCilFileSameGlobals (new fixPutVisitor) file;
    | Exp(e) -> failwith "cilRep#put of Exp subatom" );
  end

  method copy () : 'self_type = begin
    let super_copy : 'self_type = super#copy () in 
      super_copy#internal_copy () 
  end

  method internal_copy () : 'self_type = begin
	{< base = ref !base >}
  end

  method compute_localization () =
	super#compute_localization();
	base := copy !global_ast_info.code_bank;
	
end
