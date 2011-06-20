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
 *  -> deleting/appending/swaping statements in C programs
 *)

open Printf
open Global
open Cil
open Utils
open Rep

(*************************************************************************
 *************************************************************************
                      CIL Representation - C Programs
 *************************************************************************
 *************************************************************************)

let use_canonical_source_sids = ref true 
let semantic_check = ref "scope" 
let preprocess = ref false
let preprocess_command = ref !compiler_name
let globinit_file = ref ""
let globinit_func = ref "main"

let _ =
  options := !options @
  [
	"--preprocess", Arg.Set preprocess, " preprocess the C code before parsing. Def: false";
	"--preprocessor", Arg.Set_string preprocess_command, " preprocessor command.  Default: __COMPILER__ -E" ;
    "--no-canonify-sids", Arg.Clear use_canonical_source_sids, " keep identical source smts separate" ;
    "--semantic-check", Arg.Set_string semantic_check, "X limit CIL mutations {none,scope}" ;
	"--gi-file", Arg.Set_string globinit_file, "File to put the call to globinit for coverage instrumentation.  Default: file under repair.";
	"--gi-func", Arg.Set_string globinit_func, "Function to put call to globinit for coverage instrumentation.  Default: main."
	  
  ] 

(* 
 * Here is the list of CIL statementkinds that we consider as
 * possible atomic mutate-able statements. 
 *
 * Different handling is required for expression-level mutation.
 *)
let can_repair_statement sk = match sk with
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

(*************************************************************************
 * Coverage and Weighted Path Information
 *************************************************************************)

(* These are CIL variables describing C standard library functions like
 * 'fprintf'. We use them when we are instrumenting the file to
 * print out statement coverage information for fault localization. *)  
let fprintf_va = makeVarinfo true "fprintf" (TVoid [])
let fopen_va = makeVarinfo true "fopen" (TVoid [])
let fflush_va = makeVarinfo true "fflush" (TVoid [])
let memset_va = makeVarinfo true "memset" (TVoid [])
let stderr_va = makeVarinfo true "_coverage_fout" (TPtr(TVoid [], []))
let fprintf = Lval((Var fprintf_va), NoOffset)
let fopen = Lval((Var fopen_va), NoOffset)
let fflush = Lval((Var fflush_va), NoOffset)
let stderr = Lval((Var stderr_va), NoOffset)
let memset = Lval((Var memset_va), NoOffset)

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

(* This helper visitor resets all stmt ids to zero. *) 
class numToZeroVisitor = object
  inherit nopCilVisitor
  method vstmt s = s.sid <- 0 ; DoChildren
end 
let my_zero = new numToZeroVisitor

(* 
 * If two statements both print as "x = x + 1", map them to the
 * same statement ID. This is used for picking FIX locations,
 * (to avoid duplicates) but _not_ for FAULT locations.
 *)
let canonical_stmt_ht = Hashtbl.create 255 
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
    
(*
 * Extract all of the variable references from a statement. This is used
 * later to check if it is legal to swap/insert this statement into
 * another context. 
 *)
class varinfoVisitor setref = object
  inherit nopCilVisitor
  method vvrbl va = 
    setref := StringSet.add va.vname !setref ;
    SkipChildren 
end 
let my_varinfo = new varinfoVisitor

(* This visitor walks over the C program AST and builds the hashtable that
 * maps integers to statements. *) 
class numVisitor count ht fname = object
  inherit nopCilVisitor
    
  method vblock b = 
    ChangeDoChildrenPost(b,(fun b ->
      List.iter (fun b -> 
        if can_repair_statement b.skind then begin
          b.sid <- !count ;
          let rhs = 
              let bcopy = copy b in
              let bcopy = visitCilStmt my_zero bcopy in 
              bcopy.skind
          in 
          Hashtbl.add ht !count (rhs,fname);
          incr count ; 
          (* the copy is because we go through and update the statements
           * to add coverage information later *) 
        end else begin
          b.sid <- 0; 
        end ;
      ) b.bstmts ; 
      b
    ) )
end 

(* This visitor walks over the C program AST and notes all declared global
 * variables. *) 
class globalVarVisitor varset = object
  inherit nopCilVisitor
  method vglob g = 
    List.iter (fun g -> match g with
    | GEnumTag(ei,_)
    | GEnumTagDecl(ei,_) -> 
       varset := StringSet.add ei.ename !varset 
    | GVarDecl(v,_) 
    | GVar(v,_,_) -> 
       varset := StringSet.add v.vname !varset 
    | _ -> () 
    ) [g] ; 
    DoChildren
  method vfunc fd = (* function definition *) 
    varset := StringSet.add fd.svar.vname !varset ;
    SkipChildren
end 
let my_globalvar = new globalVarVisitor

(* This visitor walks over the C program AST and builds the hashtable that
 * maps integers to statements -- but it ALSO tracks in-scope variables. *) 
class numSemanticVisitor count ht fname
        globalset  (* all global variables *) 
        localset   (* in-scope local variables *) 
        localshave (* maps SID -> in-scope local variables *) 
        localsused (* maps SID -> non-global vars used by SID *) 
        = object
  inherit nopCilVisitor
  method vfunc fd = (* function definition *) 
    ChangeDoChildrenPost(begin 
      localset := StringSet.empty ; 
      List.iter (fun v ->
        localset := StringSet.add v.vname !localset 
      ) (fd.sformals @ fd.slocals)
      ; fd end,
     (fun fd ->
      localset := StringSet.empty ;
      fd
     )) 
    
  method vblock b = 
    ChangeDoChildrenPost(b,(fun b ->
      List.iter (fun b -> 
        if can_repair_statement b.skind then begin
          b.sid <- !count ;
          (* the copy is because we go through and update the statements
           * to add coverage information later *) 
          let rhs = 
              let bcopy = copy b in
              let bcopy = visitCilStmt my_zero bcopy in 
              bcopy.skind
          in 
          Hashtbl.add ht !count (rhs,fname);
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
          let used = ref StringSet.empty in 
          let _ = visitCilStmt (my_varinfo used) (b) in 

          if true then begin (* Sanity Checking *) 
            let in_scope = StringSet.union !localset globalset in 
            StringSet.iter (fun (vname) ->
              if not (StringSet.mem vname in_scope) then begin
                let _ = Pretty.printf 
                  "%s not in local+scope scope at:\n%a\n" 
                  vname 
                  d_stmt b in 
                exit 1 
              end 
            ) !used ; 
          end ; 

          let my_locals_used = StringSet.diff !used globalset in 
          localsused := IntMap.add b.sid
            my_locals_used !localsused ; 
          localshave := IntMap.add b.sid
            !localset !localshave ; 

        end else begin
          b.sid <- 0; 
        end ;
      ) b.bstmts ; 
      b
    ) )
end 

(* 
 * Visitor for computing statement coverage (for a "weighted path").
 *
 * This visitor walks over the C program AST and modifies it so that each
 * statment is preceeded by a 'printf' that writes that statement's number
 * to the .path file at run-time. *) 
class covVisitor = object
  inherit nopCilVisitor
  method vblock b = 
    ChangeDoChildrenPost(b,(fun b ->
      let result = List.map (fun stmt -> 
        if stmt.sid > 0 then begin
          let str = Printf.sprintf "%d\n" stmt.sid in 
	  (* ZAK - comment following line in to print coverage.c file with code file line nums *)
          (*let str = Printf.sprintf "%s,%d\n" !currentLoc.file !currentLoc.line in *)
          let str_exp = Const(CStr(str)) in 
          let instr = Call(None,fprintf,[stderr; str_exp],!currentLoc) in 
          let instr2 = Call(None,fflush,[stderr],!currentLoc) in 
          let skind = Instr([instr;instr2]) in
          let newstmt = mkStmt skind in 
          [ newstmt ; stmt ] 
        end else [stmt] 
      ) b.bstmts in 
      { b with bstmts = List.flatten result } 
			   ) )
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


let my_empty = new emptyVisitor
let my_every = new everyVisitor
let my_num = new numVisitor
let my_numsemantic = new numSemanticVisitor
let my_cv = new covVisitor
let my_flv = new funcLineVisitor

let cilRep_version = "6" 
let label_counter = ref 0 

(*************************************************************************
 * Atomic Mutations (e.g., delete on CIL statement) 
 *************************************************************************)

(* For debugging, it is sometimes handy to add an in-source label
 * indicating what we have changed. *) 
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
      end else begin 
        s 
      end 
    ) 
end 
let my_del = new delVisitor 

(* Append a single statement (atom) after a given statement (atom) *)
class appVisitor (append_after : atom_id) 
                 (what_to_append : Cil.stmtkind) = object
  inherit nopCilVisitor
  method vstmt s = ChangeDoChildrenPost(s, fun s ->
      if append_after = s.sid then begin 
        let copy = copy what_to_append in 
        let block = {
          battrs = [] ;
          bstmts = [s ; { s with skind = copy } ] ; 
        } in
        { s with skind = Block(block) ; 
                 labels = possibly_label s "app" append_after ; 
        } 
      end else begin 
        s 
      end 
    ) 
end 
let my_app = new appVisitor 

(* Swap to statements (atoms) *)  
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

let gotten_code = ref (mkEmptyStmt ()).skind

class getVisitor 
    (sid1 : atom_id) 
                  = object
  inherit nopCilVisitor
  method vstmt s = 
    (if s.sid = sid1 then 
      gotten_code := s.skind
    ) ;
    DoChildren
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




let found_atom = ref 0 
let found_dist = ref max_int 
class findAtomVisitor (source_file : string) (source_line : int) = object
  inherit nopCilVisitor
  method vstmt s = 
    if s.sid > 0 then begin
      let this_file = !currentLoc.file in 
      if source_file = "" || this_file = source_file then begin 
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

let my_find_atom = new findAtomVisitor

let in_scope_at context_sid moved_sid 
                localshave localsused = 
    if not (IntMap.mem context_sid localshave) then begin
      debug "in_scope_at: %d not found in localshave\n" 
        context_sid ; 
      exit 1 ;
    end ; 
    let locals_here = IntMap.find context_sid localshave in 
    if not (IntMap.mem moved_sid localsused) then begin
      debug "in_scope_at: %d not found in localsused\n" 
        moved_sid ; 
      exit 1 ;
    end ; 
    let required = IntMap.find moved_sid localsused in 
    StringSet.subset required locals_here 


(*************************************************************************
 * The CIL Representation
 *************************************************************************)

type cilRep_atom =
  | Stmt of Cil.stmtkind
  | Exp of Cil.exp 

let output_cil_file (outfile : string) (cilfile : Cil.file) = 
  let fout = open_out outfile in
	iterGlobals cilfile (fun glob ->
	  dumpGlobal defaultCilPrinter fout glob) ;
	close_out fout

exception FoundIt ;;

class virtual ['base_type] baseCilRep = object (self : 'self_type)
  inherit [cilRep_atom] faultlocRepresentation as super

  (***********************************
   * Virtual State Variables
   ***********************************)

  val virtual base : 'base_type
  val all_code : (string, Cil.file) Hashtbl.t ref = ref (hcreate 10)
  (***********************************
   * Concrete State Variables
   ***********************************)

  val stmt_count = ref 1 
  val stmt_map = ref (hcreate 255)
  val var_maps = ref (
    IntMap.empty,
    IntMap.empty,
    IntSet.empty) 


  (***********************************
   * Virtual Methods
   ***********************************)

  method virtual output_function_line_nums : unit
  method virtual internal_output_source : string -> unit

  (***********************************
   * Concrete Methods
   ***********************************)

  method get_base () = base

  (* make a fresh copy of this variant *) 
  method copy () : 'self_type = 
    let super_copy : 'self_type = super#copy () in 
      super_copy#internal_copy () 

  (* being sure to update our local instance variables *) 
  method internal_copy () : 'self_type = 
    {< base = ref (Global.copy !base) ; >} 

  (* serialize the state *) 
  method save_binary ?out_channel (filename : string) = begin
    let fout = 
      match out_channel with
      | Some(v) -> v
      | None -> open_out_bin filename 
    in 
      Marshal.to_channel fout (cilRep_version) [] ; 
      Marshal.to_channel fout (!base) [] ;
      Marshal.to_channel fout (!stmt_map) [] ;
      Marshal.to_channel fout (!stmt_count) [] ;
      Marshal.to_channel fout (!var_maps) [] ;
      super#save_binary ~out_channel:fout filename ;
      debug "cilRep: %s: saved\n" filename ; 
      if out_channel = None then close_out fout 
  end 

  (* load in serialized state *) 
  method load_binary ?in_channel (filename : string) = begin
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
      base := Marshal.from_channel fin ; 
      stmt_map := Marshal.from_channel fin ;
      stmt_count := Marshal.from_channel fin ;
      var_maps := Marshal.from_channel fin ; 
      super#load_binary ~in_channel:fin filename ; 
      debug "cilRep: %s: loaded\n" filename ; 
      if in_channel = None then close_in fin 
  end 

  (* print debugging information *)  
  method debug_info () = begin
    debug "cilRep: stmt_count = %d\n" !stmt_count ;
    debug "cilRep: stmts in weighted_path = %d\n" 
      (List.length !weighted_path) ; 
    debug "cilRep: stmts in weighted_path with weight >= 1.0 = %d\n" 
      (List.length (List.filter (fun (a,b) -> b >= 1.0) !weighted_path)) ;
	if !print_func_lines then
      self#output_function_line_nums ;
  end 

  (* internal_parse parses one C file! *)
  method internal_parse (filename : string) = 
	if !preprocess then begin
      debug "cilRep: %s: preprocessing\n" filename ; 

	  (* preprocess the C code *)
	  let cmd = 
		Global.replace_in_string  "__COMPILER_NAME__ -E __SOURCE_NAME__ > __SOURCE_NAME__.i" 
		  [ 
			"__COMPILER_NAME__", !preprocess_command ;
			"__SOURCE_NAME__", filename ;
		  ] 
	  in
		(match Stats2.time "preprocess" Unix.system cmd with
		| Unix.WEXITED(0) -> ()
		| _ -> debug "\t%s preprocessing problem\n" filename; exit 1);
	end;
	let filename = 
	  if !preprocess then filename^".i" else filename
	in
	  debug "cilRep: %s: parsing\n" filename ; 
	  let file = Frontc.parse filename () in 
		debug "cilRep: %s: parsed\n" filename ; 
		file 

  method virtual digest : string -> unit 
  (* Pretty-print this CIL AST to a C file *) 
  method output_source source_name = begin
    Stats2.time "output_source" (fun () -> 
      self#internal_output_source source_name ; 
	  self#digest source_name
    ) () ;
  end 

  (* return the total number of statements, for search strategies that
   * want to iterate over all statements or consider them uniformly 
   * at random *) 
  method max_atom () = !stmt_count 

  method get_compiler_command () = 
    "__COMPILER_NAME__ -o __EXE_NAME__ __SOURCE_NAME__ __COMPILER_OPTIONS__ >& /dev/null" 

  method from_source_one_file (filename : string) : Cil.file = begin
    let file = self#internal_parse filename in 
    let globalset   = ref StringSet.empty in 
	let localset = ref StringSet.empty in
	let localshave = ref (fst3 !var_maps) in
	let localsused = ref (snd3 !var_maps) in 

      visitCilFileSameGlobals my_every file ; 
      visitCilFileSameGlobals my_empty file ; 

      begin match !semantic_check with
      | "scope" -> 
      (* First, gather up all global variables. *) 
		visitCilFileSameGlobals (my_globalvar globalset) file ; 
      (* Second, number all statements and keep track of
       * in-scope variables information. *) 
		visitCilFileSameGlobals 
          (my_numsemantic
			 stmt_count !stmt_map filename
			 !globalset
			 localset
			 localshave
			 localsused 
          ) file ; 
		debug "cilRep: globalset = %d\n" (StringSet.cardinal !globalset) 

      | _ -> visitCilFileSameGlobals (my_num stmt_count !stmt_map filename) file ; 
      end ;
    (* we increment after setting, so we're one too high: *) 
      debug "cilRep: stmt_count = %d\n" !stmt_count ;
      let set_of_all_source_sids = ref (trd3 !var_maps) in 
		if !use_canonical_source_sids then begin
		  Hashtbl.iter (fun str i ->
			set_of_all_source_sids := IntSet.add i !set_of_all_source_sids 
		  ) canonical_stmt_ht 
		end else 
		  for i = 1 to !stmt_count do
			set_of_all_source_sids := IntSet.add i !set_of_all_source_sids 
		  done ;
		debug "cilRep: unique statements = %d\n"
		  (IntSet.cardinal !set_of_all_source_sids);
		var_maps := (
		  !localshave, !localsused,
		  !set_of_all_source_sids); 
		self#internal_post_source filename ;
		hadd !all_code filename file;
		file
  end


  method internal_post_source filename = begin
  end 

  (* instruments one Cil file for fault localization *)
  method insert_globinit file =
	let fd = Cil.getGlobInit ~main_name:!globinit_func file in 
    let lhs = (Var(stderr_va),NoOffset) in 
    let data_str = !coverage_outname in 
    let str_exp = Const(CStr(data_str)) in 
    let str_exp2 = Const(CStr("wb")) in 
    let instr = Call((Some(lhs)),fopen,[str_exp;str_exp2],!currentLoc) in 
    let new_stmt = Cil.mkStmt (Instr[instr]) in 
	  fd.sbody.bstmts <- new_stmt :: fd.sbody.bstmts ;		
	  let new_global = GVarDecl(stderr_va,!currentLoc) in 
		file.globals <- new_global :: file.globals 

  method inner_output_function_line_nums file = begin
	debug "cilRep: computing function line numbers\n" ; 
    visitCilFileSameGlobals my_flv file ;
    debug "DONE."
  end

  method instrument_one_file file ?g:(globinit=true) coverage_sourcename =
    debug "cilRep: instrumenting %s for fault localization information\n" coverage_sourcename ; 
    visitCilFileSameGlobals my_cv file ; 
	if globinit then 
	  self#insert_globinit file
	else begin
	  let ext = GVarDecl({stderr_va with vstorage=Extern }, !currentLoc) in
		file.globals <- ext :: file.globals
	end;
	output_cil_file coverage_sourcename file

  method get_coverage  coverage_sourcename coverage_exename coverage_outname = 
	  (* run the instrumented program *) 
	let res, _ = self#internal_test_case coverage_exename	coverage_sourcename (Positive 1) in
	  if not res then begin 
		debug "ERROR: coverage FAILS test Positive 1 (coverage_exename=%s)\n" coverage_exename ;
		if not !allow_coverage_fail then exit 1 
	  end ;
	  if !fix_scheme = "path" then 
		Unix.rename coverage_outname !fix_file
	  else
		Unix.rename coverage_outname (coverage_outname ^".pos");
	  let res, _ = (self#internal_test_case coverage_exename 
					  coverage_sourcename (Negative 1)) in 
		if res then begin 
		  debug "ERROR: coverage PASSES test Negative 1\n" ;
		  if not !allow_coverage_fail then exit 1 
		end ;
		Unix.rename coverage_outname (coverage_outname ^ ".neg") ;
	(* now we have a positive path and a negative path *) 

  method atom_id_of_source_line source_file source_line = begin
    found_atom := (-1);
    found_dist := max_int;
	if hmem !all_code source_file then 
	  let file = hfind !all_code source_file in 
		visitCilFileSameGlobals (my_find_atom source_file source_line) file
	else begin
	  debug "WARNING: cannot find file %s in atom_id_of_source_line; cannot make promises about my behavior.\n" source_file;
	  (try
		 hiter
		   (fun filename ->
			 fun file ->
			   visitCilFileSameGlobals (my_find_atom source_file source_line) file;
			   if !found_atom <> (-1) then raise FoundIt)
		   !all_code
	   with FoundIt -> ());
	end;
    if !found_atom = (-1) then begin
	  debug "WARNING: cannot convert %s,%d to atom_id" source_file source_line ;
	  0 
    end else !found_atom 
  end

  method inner_delete stmt_id file = 
	super#delete stmt_id;
    visitCilFileSameGlobals (my_del stmt_id) file

  method inner_append append_after what_to_append what file = 
    super#append append_after what_to_append ; 
    visitCilFileSameGlobals (my_app append_after what) file

  (* Return a Set of (atom_ids,fix_weight pairs) that one could append here without
   * violating many typing rules. *) 
  method append_sources append_after = 
    let localshave, localsused, _ = !var_maps in 
	let all_sids = !fix_weights in 
    let sids = 
	  if !semantic_check = "none" then all_sids
      else  
		lfilt (fun (sid,weight) ->
          in_scope_at append_after sid localshave localsused 
		) all_sids
    in
	let retval = ref (WeightSet.empty) in
	  liter
		(fun ele -> retval := WeightSet.add ele !retval)
		sids; !retval

  (* Return a Set of atom_ids that one could swap here without
   * violating many typing rules. In addition, if X<Y and X and Y
   * are both valid, then we'll allow the swap (X,Y) but not (Y,X).
   *) 
  method swap_sources append_after = 
    let localshave, localsused, _ = !var_maps in 
	let all_sids = !fix_weights in
    let sids = if !semantic_check = "none" then
		all_sids
      else 
		lfilt (fun (sid, weight) ->
          in_scope_at sid append_after localshave localsused 
          && 
			in_scope_at append_after sid localshave localsused 
		) all_sids 
    in
	let retval = ref (WeightSet.empty) in
	  liter
		(fun ele -> retval := WeightSet.add ele !retval) sids; !retval

  method inner_put file stmt_id stmt = 
    super#put stmt_id stmt ; 
    match stmt with
    | Stmt(stmt) -> 
      visitCilFileSameGlobals (my_put stmt_id stmt) file
    | Exp(e) -> failwith "cilRep#put of Exp subatom" 

  method inner_get file stmt_id =
    visitCilFileSameGlobals (my_get stmt_id) file ;
    let answer = !gotten_code in
      gotten_code := (mkEmptyStmt()).skind ;
      (Stmt answer) 


  (***********************************
									  * Subatoms are Expressions
  ***********************************)
  method subatoms = true 

  method atom_to_str atom = 
    let doc = match atom with
      | Exp(e) -> d_exp () e 
      | Stmt(s) -> dn_stmt () (mkStmt s) 
    in 
      Pretty.sprint ~width:80 doc 

  method inner_get_subatoms stmt_id file = 
    visitCilFileSameGlobals (my_get stmt_id) file ;
    let answer = !gotten_code in
    let this_stmt = mkStmt answer in
    let output = ref [] in 
    let first = ref true in 
    let _ = visitCilStmt (my_get_exp output first) this_stmt in
      List.map (fun x -> Exp x) !output 

  method inner_replace_subatom stmt_id subatom_id atom file = 
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
        visitCilFileSameGlobals (my_put stmt_id new_stmt.skind) file

  method inner_replace_subatom_with_constant stmt_id subatom_id =  
    self#replace_subatom stmt_id subatom_id (Exp Cil.zero)

end

(* CIL representation for a single-file repair *)

class cilRep = object (self : 'self_type) 
  inherit [Cil.file ref] baseCilRep as super

  val base = ref Cil.dummyFile

  (***********************************
   * Methods
   ***********************************)

  (* load in a CIL AST from a C source file *) 
  method from_source (filename : string) = begin 
	stmt_count := 1 ;
	debug "CilRep from_source: %s\n" filename; 
	base := super#from_source_one_file filename;
    stmt_count := pred !stmt_count ; 
  end 

  method load_oracle (oracle_file : string) = begin
	debug "cilRep oracle_file: %s\n" oracle_file;
	let old_count = !stmt_count in 
	  ignore(self#from_source_one_file oracle_file);
	  stmt_count := pred !stmt_count;
	  let atmst = 
		lfoldl
		  (fun set ->
			fun ele ->
			  AtomSet.add ele set) (!codeBank) (old_count -- !stmt_count) in
		codeBank := atmst;
		debug "done loading oracle_file %s\n" oracle_file;
  end

  method digest source_name = 
    let digest = Digest.file source_name in  
      already_sourced := Some([source_name],[digest]) ; 

  method internal_output_source source_name = 
	try
	(* if you've done truly evil mutation -- for example, changing
	 * ( *fptr )(args) into (1)(args) -- CIL will die here with
	 * internal sanity checking. Basically, this means you have
	 * a C file that can't compile, so this exception handling
       * causes us to print out an empty file. This problem was
	 * noticed by Briana around Fri Apr 16 10:25:11 EDT 2010.
	 *)
	  output_cil_file source_name !base
	with _ -> () ;
      
  method output_function_line_nums = 
    assert(!base <> Cil.dummyFile) ; 
	self#inner_output_function_line_nums (copy !base)

  method instrument_fault_localization coverage_sourcename coverage_exename coverage_outname = 
    assert(!base <> Cil.dummyFile) ; 
    debug "cilRep: computing fault localization information\n" ; 
    let file = copy !base in 
	  self#instrument_one_file file coverage_sourcename;
	  if !globinit_file <> "" then begin
		(* FIXME: this doesn't make much sense as it stands b/c compile for single
		   cil rep won't move it to the appropriate place; think about that! *)
		let cov_prefix,ext = split_ext coverage_sourcename in
		let gi_file = self#internal_parse !globinit_file in
		  self#insert_globinit gi_file;
		  let basename,ext=split_ext !globinit_file in
			output_cil_file (cov_prefix^"."^basename^"."^ext) gi_file
	  end;
		(* compile the instrumented program *) 
	  if not (self#compile ~keep_source:true coverage_sourcename coverage_exename) then 
		begin
		  debug "ERROR: cannot compile %s\n" coverage_sourcename ;
		  exit 1 
		end ;
	  self#get_coverage coverage_sourcename coverage_exename coverage_outname

  (* Atomic Delete of a single statement (atom) *) 
  method delete stmt_id = super#inner_delete stmt_id !base

  (* Atomic Append of a single statement (atom) after another statement *) 
  method append append_after what_to_append = 
    let what,_ = 
      try Hashtbl.find !stmt_map what_to_append 
      with _ -> debug "append: %d not found in stmt_map\n" what_to_append ; exit 1
    in 
	  super#inner_append append_after what_to_append what !base


  (* Atomic Swap of two statements (atoms) *)
  method swap stmt_id1 stmt_id2 = 
    super#swap stmt_id1 stmt_id2 ; 
    let skind1,_ = 
	  ht_find !stmt_map stmt_id1 
		(fun _ -> debug "swap: %d not found in stmt_map\n" stmt_id1 ; exit 1)
    in 
    let skind2,_ = 
	  ht_find !stmt_map stmt_id2
		(fun _ -> debug "swap: %d not found in stmt_map\n" stmt_id2 ; exit 1)
    in 
	  visitCilFileSameGlobals (my_swap stmt_id1 skind1 stmt_id2 skind2) !base
								 


  method put stmt_id stmt = super#inner_put !base stmt_id stmt ; 

  method get stmt_id = super#inner_get !base stmt_id

  (***********************************
   * Structural Differencing
   ***********************************)
  method structural_signature = 
    let result = ref StringMap.empty in 
    iterGlobals !base (fun g1 ->
      match g1 with
      | GFun(fd,l) -> 
          let node_id = Cdiff.fundec_to_ast fd in 
          result := StringMap.add fd.svar.vname node_id !result  
      | _ -> () 
    ) ; 
    !result 

  (***********************************
   * Subatoms are Expressions
   ***********************************)

  method get_subatoms stmt_id = super#inner_get_subatoms stmt_id !base

  method replace_subatom stmt_id subatom_id atom = 
	super#inner_replace_subatom stmt_id subatom_id atom !base

  method replace_subatom_with_constant stmt_id subatom_id = 
	super#inner_replace_subatom_with_constant stmt_id subatom_id
end 

class multiCilRep = object (self : 'self_type)
  inherit [(string, (Cil.file * string)) Hashtbl.t ref] baseCilRep as super

  val base = ref (hcreate 10)

  method get_test_command () = 
    "__TEST_SCRIPT__ __EXE_NAME__ __TEST_NAME__ __PORT__ __FITNESS_FILE__ __SOURCE_NAME__ >& /dev/null" 

  method compile ?(keep_source=false) source_name exe_name = 
	let prefix,ext = split_ext source_name in 
	let new_name = 
	  hfold
		(fun orig_path ->
		  fun (file,basename) ->
			fun source_name ->
			  prefix^"."^basename^"."^ext^" "^source_name
		) !base ""
	in
	  super#compile ~keep_source:keep_source new_name exe_name

  method from_source filelist = 
	liter
	  (fun multiline ->
		let orig_path = !prefix^multiline in 
		let parsed = self#from_source_one_file orig_path in
		let basename,ext = split_ext multiline in
		  hadd !base orig_path (parsed,basename))
	  (get_lines filelist);
    stmt_count := pred !stmt_count ; 
	let range = 1 -- (!stmt_count) in
	let atmst = 
	  lfoldl
		(fun set ->
		  fun ele ->
			AtomSet.add ele set) (AtomSet.empty) range in
	  changeLocs := atmst;
	  codeBank := atmst

  method load_oracle (filelist : string) = begin
	debug "multicilrep: load_oracle: %s\n" filelist;
	let old_count = !stmt_count in 
	liter
	  (fun fname ->
		(* potential FIXME: prefix?? *)
		ignore(self#from_source_one_file fname))
	  (get_lines filelist);
	stmt_count := pred !stmt_count;
	let atmst = 
	  lfoldl
		(fun set ->
		  fun ele ->
			AtomSet.add ele set) (!codeBank) (old_count -- !stmt_count) in
	  codeBank := atmst
  end

  method digest source_name =  ()
(*	debug "WARNING: digest not implemented for multiCilRep yet!\n" *)

  method internal_output_source source_name =
(*	Printf.printf "Multicilrep internal_output_source: %s\n" source_name; flush stdout;*)
	let b,ext = split_ext source_name in 
	hiter
	  (fun fname ->
		fun (file,basename) ->
		  let output_name = b^"."^basename^"."^ext in 
(*			debug "MultiCilRep: output %s to %s\n" basename output_name; *)
			try
			  output_cil_file output_name file
			with _ -> () ;
	  ) !base

  method get stmt_id = 
	let _,file = hfind !stmt_map stmt_id in 
	  self#inner_get (fst (hfind !base file)) stmt_id

  method put stmt_id stmt = 
	let _,file = hfind !stmt_map stmt_id in
	  self#inner_put (fst (hfind !base file)) stmt_id stmt

  method output_function_line_nums = 
	assert((hlen !base) <> 0);
	hiter
	  (fun path ->
		fun (file,basename) ->
		  self#inner_output_function_line_nums (copy file))
	  !base

  method instrument_fault_localization 
	coverage_sourcename
	coverage_exename
	coverage_outname = 
	assert((hlen !base) <> 0);
    debug "multiCilRep: computing fault localization information\n" ; 
	if !globinit_file = "" then begin
	  debug "WARNING: globinit_file unspecified in multiCilRep; assuming main.c";
	  globinit_file := "main.c"
	end;
	let cov_prefix,ext = split_ext coverage_sourcename in 
	let globinited = ref false in
	hiter
	  (fun path ->
		fun (file,basename) ->
		  let file = copy file in 
		  let globinit = (path^"."^ext) = !globinit_file
		  in
			if globinit then globinited := true;
			self#instrument_one_file file ~g:globinit (cov_prefix^"."^basename^"."^ext)
	  ) !base;
	if not !globinited then begin
	  let gi_file = self#internal_parse !globinit_file in
		self#insert_globinit gi_file;
		let basename,ext=split_ext !globinit_file in
		output_cil_file (cov_prefix^"."^basename^"."^ext) gi_file
	end;
	if not (self#compile ~keep_source:true coverage_sourcename coverage_exename) then 
	  begin
		debug "ERROR: cannot compile %s\n" coverage_sourcename ;
		exit 1 
	  end ;
	self#get_coverage coverage_sourcename coverage_exename coverage_outname

  method structural_signature = failwith "Please don't call me on multiCilRep!"
	
  method delete stmt_id = 
	let s,f = hfind !stmt_map stmt_id in 
	  super#inner_delete stmt_id (fst (hfind !base f))

  (* Atomic Append of a single statement (atom) after another statement *) 
  method append append_after what_to_append = 
    let after,after_file = 
      try Hashtbl.find !stmt_map append_after
      with _ -> debug "append: %d not found in stmt_map\n" append_after ; exit 1
    in 
    let what,f = 
      try Hashtbl.find !stmt_map what_to_append 
      with _ -> debug "append: %d not found in stmt_map\n" what_to_append ; exit 1
    in 
	  super#inner_append append_after what_to_append what (fst (hfind !base after_file))


  (* Atomic Swap of two statements (atoms) *)
  method swap stmt_id1 stmt_id2 = 
    super#swap stmt_id1 stmt_id2 ; 
    let skind1,f1 = 
	  ht_find !stmt_map stmt_id1 
		(fun _ -> debug "swap: %d not found in stmt_map\n" stmt_id1 ; exit 1)
    in 
    let skind2,f2 = 
	  ht_find !stmt_map stmt_id2
		(fun _ -> debug "swap: %d not found in stmt_map\n" stmt_id2 ; exit 1)
    in 
	  if hmem !base f1 then 
		visitCilFileSameGlobals (my_swap stmt_id1 skind1 
								   stmt_id2 skind2) (fst (hfind !base f1));
	  if f1 <> f2 && (hmem !base f2) then
	  visitCilFileSameGlobals (my_swap stmt_id1 skind1 
								 stmt_id2 skind2) (fst (hfind !base f2));



end
