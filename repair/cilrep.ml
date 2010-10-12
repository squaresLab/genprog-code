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
open Utils
open Global
open Cil
open Rep

(*************************************************************************
 *************************************************************************
                      CIL Representation - C Programs
 *************************************************************************
 *************************************************************************)

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

(* This visitor walks over the C program AST and builds the hashtable that
 * maps integers to statements. *) 
class numVisitor count ht = object
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
          Hashtbl.add ht !count rhs;
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

let my_empty = new emptyVisitor
let my_every = new everyVisitor
let my_num = new numVisitor
let my_cv = new covVisitor

let cilRep_version = "4" 
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

(*************************************************************************
 * The CIL Representation
 *************************************************************************)
let allow_coverage_fail = ref false 
let predict_input = ref ""

let _ =
  options := !options @
  [
    "--allow-coverage-fail", Arg.Set allow_coverage_fail, " allow coverage to fail its test cases" ;
  ] 

class cilRep = object (self : 'self_type) 
  inherit [Cil.stmtkind] faultlocRepresentation as super

  (***********************************
									  * State Variables
  ***********************************)
  val base = ref Cil.dummyFile
  val stmt_map = ref (Hashtbl.create 255)
  val stmt_count = ref 1 

  (***********************************
									  * Methods
  ***********************************)

  method get_base () = base

  method get_compiler_command () = 
    "__COMPILER_NAME__ -o __EXE_NAME__ __SOURCE_NAME__ __COMPILER_OPTIONS__ >& /dev/null" 

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
      super#load_binary ~in_channel:fin filename ; 
      debug "cilRep: %s: loaded\n" filename ; 
      if in_channel = None then close_in fin 
  end 

  method load_predict ?in_channel (filename : string) = begin
	try
	  let fin = 
		match in_channel with
		| Some(v) -> v
		| None -> open_in_bin filename
	  in
		base := Marshal.from_channel fin;
		stmt_map := Marshal.from_channel fin ;
		stmt_count := Marshal.from_channel fin ;
		debug "cilRep: %s loaded\n" filename ;
		if in_channel = None then close_in fin; true
	with _ ->pprintf "something failed\n"; flush stdout; false
  end

  (* print debugging information *)  
  method debug_info () = begin
    debug "cilRep: stmt_count = %d\n" !stmt_count ;
    debug "cilRep: stmts in weighted_path = %d\n" 
      (List.length !weighted_path) ; 
    debug "cilRep: stmts in weighted_path with weight >= 1.0 = %d\n" 
      (List.length (List.filter (fun (a,b) -> b >= 1.0) !weighted_path)) ; 
  end 

  (* load in a CIL AST from a C source file *) 
  method from_source (filename : string) = begin 
    debug "cilRep: %s: parsing\n" filename ; 
    let file = Frontc.parse filename () in 
      debug "cilRep: %s: parsed\n" filename ; 
      visitCilFileSameGlobals my_every file ; 
      visitCilFileSameGlobals my_empty file ; 
      visitCilFileSameGlobals (my_num stmt_count !stmt_map) file ; 
      (* we increment after setting, so we're one too high: *) 
      stmt_count := pred !stmt_count ; 
      base := file ; 
  end 

  (* Pretty-print this CIL AST to a C file *) 
  method output_source source_name = begin
    Stats2.time "output_source" (fun () -> 
								   let fout = open_out source_name in
									 begin try 
									   (* if you've done truly evil mutation -- for example, changing
										* ( *fptr )(args) into (1)(args) -- CIL will die here with
										* internal sanity checking. Basically, this means you have
										* a C file that can't compile, so this exception handling
										* causes us to print out an empty file. This problem was
										* noticed by Briana around Fri Apr 16 10:25:11 EDT 2010.
										*)
									   iterGlobals !base (fun glob ->
															dumpGlobal defaultCilPrinter fout glob ;
														 ) ; 
									 with _ -> () end ;
									 close_out fout ;
									 let digest = Digest.file source_name in  
									   already_sourced := Some([digest]) ; 
								) () 
  end 


  method instrument_fault_localization 
    coverage_sourcename 
    coverage_exename 
    coverage_outname 
    = begin
      assert(!base <> Cil.dummyFile) ; 
      debug "cilRep: computing fault localization information\n" ; 
      let file = copy !base in 
		visitCilFileSameGlobals my_cv file ; 
		let new_global = GVarDecl(stderr_va,!currentLoc) in 
		  file.globals <- new_global :: file.globals ; 
		  let fd = Cil.getGlobInit file in 
		  let lhs = (Var(stderr_va),NoOffset) in 
		  let data_str = coverage_outname in 
		  let str_exp = Const(CStr(data_str)) in 
		  let str_exp2 = Const(CStr("wb")) in 
		  let instr = Call((Some(lhs)),fopen,[str_exp;str_exp2],!currentLoc) in 
		  let new_stmt = Cil.mkStmt (Instr[instr]) in 
			fd.sbody.bstmts <- new_stmt :: fd.sbody.bstmts ; 
			(* print out the instrumented source code *) 
			let fout = open_out coverage_sourcename in
			  iterGlobals file (fun glob ->
								  dumpGlobal defaultCilPrinter fout glob ;
							   ) ; 
			  close_out fout ;
			  (* compile the instrumented program *) 
			  if not (self#compile ~keep_source:true 
						coverage_sourcename coverage_exename) then begin
				debug "ERROR: cannot compile %s\n" coverage_sourcename ;
				exit 1 
			  end ;
			  (* run the instrumented program *) 
			  let res, _ = self#internal_test_case coverage_exename
				coverage_sourcename (Positive 1) in
				if not res then begin 
				  debug "ERROR: coverage FAILS test Positive 1 (coverage_exename=%s)\n" coverage_exename ;
				  if not !allow_coverage_fail then exit 1 
				end ;
				Unix.rename coverage_outname (coverage_outname ^ ".pos") ;
				let res, _ = (self#internal_test_case coverage_exename 
								coverage_sourcename (Negative 1)) in 
				  if res then begin 
					debug "ERROR: coverage PASSES test Negative 1\n" ;
					if not !allow_coverage_fail then exit 1 
				  end ;
				  Unix.rename coverage_outname (coverage_outname ^ ".neg") ;
				  (* now we have a positive path and a negative path *) 

	end 

  (* return the total number of statements, for search strategies that
   * want to iterate over all statements or consider them uniformly 
   * at random *) 
  method max_atom () = !stmt_count 

  method atom_id_of_source_line source_file source_line = begin
    found_atom := (-1);
    found_dist := max_int;
    visitCilFileSameGlobals (my_find_atom source_file source_line) !base ;
    if !found_atom = (-1) then begin
      debug "WARNING: cannot convert %s,%d to atom_id" source_file
      source_line ;
      0 
    end else !found_atom 
  end 

  (* Atomic Delete of a single statement (atom) *) 
  method delete stmt_id = 
    super#delete stmt_id ; 
    visitCilFileSameGlobals (my_del stmt_id) !base  

  (* Atomic Append of a single statement (atom) after another statement *) 
  method append append_after what_to_append = 
    super#append append_after what_to_append ; 
    let what = 
      try Hashtbl.find !stmt_map what_to_append 
      with _ -> debug "append: %d not found\n" what_to_append ; exit 1
    in 
      visitCilFileSameGlobals (my_app append_after what) !base;

  (* Atomic Swap of two statements (atoms) *)
  method swap stmt_id1 stmt_id2 = 
    super#swap stmt_id1 stmt_id2 ; 
    let skind1 = 
      try Hashtbl.find !stmt_map stmt_id1 
      with _ -> debug "swap: %d not found\n" stmt_id1 ; exit 1
    in 
    let skind2 = 
      try Hashtbl.find !stmt_map stmt_id2 
      with _ -> debug "swap: %d not found\n" stmt_id2 ; exit 1
    in 
      visitCilFileSameGlobals (my_swap stmt_id1 skind1 
                                 stmt_id2 skind2) !base  

  method put stmt_id stmt = 
    super#put stmt_id stmt ; 
    visitCilFileSameGlobals (my_put stmt_id stmt) !base

  method get stmt_id =
    visitCilFileSameGlobals (my_get stmt_id) !base ;
    let answer = !gotten_code in
      gotten_code := (mkEmptyStmt()).skind ;
      answer
		
end 
