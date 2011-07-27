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
open Rep
open Pretty

(*************************************************************************
 *************************************************************************
                      CIL Representation - C Programs
 *************************************************************************
 *************************************************************************)

let use_canonical_source_sids = ref true 
let semantic_check = ref "scope" 
let preprocess = ref false
let preprocess_command = ref "__COMPILER_NAME__ -E __SOURCE_NAME__ __COMPILER_OPTIONS__ > __OUT_NAME__"
let print_line_numbers = ref false 
let multithread_coverage = ref false
let uniq_coverage = ref false

let _ =
  options := !options @
  [
    "--preprocess", Arg.Set preprocess, " preprocess the C code before parsing. Def: false";
    "--preprocessor", Arg.Set_string preprocess_command, " preprocessor command.  Default: __COMPILER__ -E" ;
    "--no-canonify-sids", Arg.Clear use_canonical_source_sids, " keep identical source smts separate" ;
    "--semantic-check", Arg.Set_string semantic_check, "X limit CIL mutations {none,scope}" ;
    "--print-line-numbers", Arg.Set print_line_numbers, " do print CIL #line numbers" ;
    "--mt-cov", Arg.Set multithread_coverage, "  instrument for coverage with locks.  Avoid if possible.";
    "--uniq-cov", Arg.Set uniq_coverage, "  print each visited stmt only once"
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

(* These are CIL variables describing C standard library functions like
 * 'fprintf'. We use them when we are instrumenting the file to
 * print out statement coverage information for fault localization. *)  

let stderr_va = makeVarinfo true "_coverage_fout" (TPtr(TVoid [], []))
let stderr = Lval((Var stderr_va), NoOffset)
let mutex = ref (Lval((Var stderr_va), NoOffset))
let fflush_va = makeVarinfo true "fflush" (TVoid [])
let fflush = Lval((Var fflush_va), NoOffset)
let memset_va = makeVarinfo true "memset" (TVoid [])
let memset = Lval((Var memset_va), NoOffset)
let fprintf_va = makeVarinfo true "fprintf" (TVoid [])
let fprintf = Lval((Var fprintf_va), NoOffset)
let fopen_va = makeVarinfo true "fopen" (TVoid [])
let fopen = Lval((Var fopen_va), NoOffset)
let fclose_va = makeVarinfo true "fclose" (TVoid [])
let fclose = Lval((Var fclose_va), NoOffset)

let uniq_array_va = ref
  (makeGlobalVar "___coverage_array" (TArray(charType,None,[])))

(* 
 * Visitor for computing statement coverage (for a "weighted path").
 *
 * This visitor walks over the C program AST and modifies it so that each
 * statment is preceeded by a 'printf' that writes that statement's number
 * to the .path file at run-time. *) 

let make_call lval fname args = Call(lval, fname, args, !currentLoc)
let print_counter = ref 0


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
			  let fopen_fout = 
				make_call (Some(Var(stderr_va), NoOffset)) fopen [Const(CStr(coverage_outname)); Const(CStr("a"))] 
			  in 
			  let close_fout = 
				make_call None fclose [stderr]
			  in
				[fopen_fout;print_num;close_fout]
			end else 
			  begin
				let flush = make_call None fflush [stderr] in
				  [print_num; flush]
			  end
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
	if not !multithread_coverage then begin
	  let outfile = Var(stderr_va), NoOffset in
      let make_fout = 
		make_call (Some(outfile)) fopen [Const(CStr(coverage_outname)); Const(CStr("wb"))] 
      in
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



let output_cil_file_to_channel (fout : out_channel) (cilfile : Cil.file) = 
  if !print_line_numbers then 
    iterGlobals cilfile (dumpGlobal defaultCilPrinter fout) 
  else 
    iterGlobals cilfile (dumpGlobal Cilprinter.noLineCilPrinter fout) 

let output_cil_file (outfile : string) (cilfile : Cil.file) = 
  let fout = open_out outfile in
  output_cil_file_to_channel fout cilfile ;
	close_out fout

let output_cil_file_to_string (cilfile : Cil.file) = 
  if true then  

    (* Use the Cilprinter.ml code to output a Cil.file to a Buffer *) 
    let buf = Buffer.create 10240 in   
    begin if !print_line_numbers then 
      iterGlobals cilfile (Cilprinter.toStringCilPrinter#bGlobal buf) 
    else 
      iterGlobals cilfile (Cilprinter.noLineToStringCilPrinter#bGlobal buf) 
    end ; 
    Buffer.contents buf 

  else begin
    (* CLG: this is a hack implemented on 7/14/11 to get around the
       blocked-pipe problem and should be removed when aforemention
       problem is addressed *)
    output_cil_file "tempfile.c" cilfile;
    let buffer = Buffer.create 10240 in
    liter (fun line ->
      Buffer.add_string buffer line) (get_lines "tempfile.c");
    (try Unix.unlink "tempfile.c" with _ -> ());
    Buffer.contents buffer
  end 

(*************************************************************************
 *************************************************************************
                          BASE CIL Representation 
                  (shared by single file and multi file) 
 *************************************************************************
 *************************************************************************)

class cilRep = object (self : 'self_type)
  inherit [cilRep_atom] faultlocRepresentation as super

  (***********************************
   * Virtual State Variables
   ***********************************)

  val base = ref (StringMap.empty)
  val oracle_code : (string, Cil.file) Hashtbl.t ref = ref (Hashtbl.create 10)

  (***********************************
   * Concrete State Variables
   ***********************************)

  val stmt_count = ref 1 
  val stmt_map = ref (Hashtbl.create 255)
  val var_maps = ref (
    IntMap.empty,
    IntMap.empty,
    IntSet.empty) 


  (***********************************
   * Virtual Methods
   ***********************************)

  method virtual output_function_line_nums : unit
  (* method virtual internal_output_source : string -> unit *)

  (***********************************
   * Concrete Methods
   ***********************************)

  method get_base () = base

  method get_file stmt_id =
	let fname = snd (hfind !stmt_map stmt_id) in
	  StringMap.find fname !base

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

  (* load in a CIL AST from a C source file *) 
  method from_source (filename : string) = begin 
	let _,ext = split_ext filename in 
	  (match ext with
		"txt" ->
		  liter
			(fun fname ->
			  base := StringMap.add fname (self#from_source_one_file fname) !base)
			(get_lines filename)
	  | "c" | "i" -> 
(* CLG : do we need this?		stmt_count := 1 ; *)
		base := StringMap.add filename (self#from_source_one_file filename) !base
	  | _ -> debug "extension: %s\n" ext; failwith "Unexpected file extension in CilRep#from_source.  Permitted: .c, .txt");
		stmt_count := pred !stmt_count ; 
  end 

  method compile source_name exe_name = 
	let source_name = 
	  if !multi_file then begin
		let source_dir,_,_ = split_base_subdirs_ext source_name in 
		  StringMap.fold
			(fun fname ->
			  fun file ->
				fun source_name -> 
				  let fname' = Filename.concat source_dir fname in 
					fname'^" "^source_name
			) !base ""
	  end else source_name
	in
	  super#compile source_name exe_name

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


  (* return the total number of statements, for search strategies that
   * want to iterate over all statements or consider them uniformly 
   * at random *) 
  method max_atom () = !stmt_count 


  (* Atomic Delete of a single statement (atom) *) 
  method delete stmt_id = 
	super#delete stmt_id;
    visitCilFileSameGlobals (my_del stmt_id) (self#get_file stmt_id)

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
	let file = StringMap.find after_file !base in
      super#append append_after what_to_append ; 
      visitCilFileSameGlobals (my_app append_after what) file

  (* Return a Set of (atom_ids,fix_weight pairs) that one could append here without
   * violating many typing rules. *) 
  method append_sources append_after = 
    let localshave, localsused, _ = !var_maps in 
	let all_sids = !codeBank in 
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

  (* Atomic Swap of two statements (atoms) *)
  method swap stmt_id1 stmt_id2 = 
	(* from singlecilrep *)
    super#swap stmt_id1 stmt_id2 ; 
    let skind1,f1 = 
	  ht_find !stmt_map stmt_id1 
		(fun _ -> debug "swap: %d not found in stmt_map\n" stmt_id1 ; exit 1)
    in 
    let skind2,f2 = 
	  ht_find !stmt_map stmt_id2
		(fun _ -> debug "swap: %d not found in stmt_map\n" stmt_id2 ; exit 1)
    in 
	  if StringMap.mem f1 !base then 
		visitCilFileSameGlobals (my_swap stmt_id1 skind1 
								   stmt_id2 skind2) (StringMap.find f1 !base);
	  if f1 <> f2 && (StringMap.mem f2 !base) then
		visitCilFileSameGlobals (my_swap stmt_id1 skind1 
								   stmt_id2 skind2) (StringMap.find f2 !base);


  (* Return a Set of atom_ids that one could swap here without
   * violating many typing rules. In addition, if X<Y and X and Y
   * are both valid, then we'll allow the swap (X,Y) but not (Y,X).
   *) 
  method swap_sources append_after = 
    let localshave, localsused, _ = !var_maps in 
	let all_sids = !codeBank in
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

  (* get obtains an atom from the current variant, *not* from the code
     bank *) 
  method get stmt_id =
	let file = self#get_file stmt_id in
    visitCilFileSameGlobals (my_get stmt_id) file ;
    let answer = !gotten_code in
      gotten_code := (mkEmptyStmt()).skind ;
      (Stmt answer) 

  (* put places an atom into the current variant; the code bank is not
     involved *) 

  method put stmt_id stmt = 
	let file = self#get_file stmt_id in 
    super#put stmt_id stmt ; 
    match stmt with
    | Stmt(stmt) -> 
      visitCilFileSameGlobals (my_put stmt_id stmt) file
    | Exp(e) -> failwith "cilRep#put of Exp subatom" 


  (***********************************
   * Subatoms are Expressions
   ***********************************)
  method subatoms = true 

  method get_subatoms stmt_id = 
	let file = self#get_file stmt_id in
    visitCilFileSameGlobals (my_get stmt_id) file ;
    let answer = !gotten_code in
    let this_stmt = mkStmt answer in
    let output = ref [] in 
    let first = ref true in 
    let _ = visitCilStmt (my_get_exp output first) this_stmt in
      List.map (fun x -> Exp x) !output 

  method replace_subatom stmt_id subatom_id atom = 
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
        visitCilFileSameGlobals (my_put stmt_id new_stmt.skind) file

  method replace_subatom_with_constant stmt_id subatom_id =  
    self#replace_subatom stmt_id subatom_id (Exp Cil.zero)

  (* For debugging. *) 

  method atom_to_str atom = 
    let doc = match atom with
      | Exp(e) -> d_exp () e 
      | Stmt(s) -> dn_stmt () (mkStmt s) 
    in 
      Pretty.sprint ~width:80 doc 

  (***********************************
   * Structural Differencing
   ***********************************)
  method structural_signature = 
	assert(not !multi_file);
    let result = ref StringMap.empty in 
	  StringMap.iter
		(fun _ ->
		  fun base ->
			iterGlobals base (fun g1 ->
			  match g1 with
			  | GFun(fd,l) -> 
				let node_id = Cdiff.fundec_to_ast fd in 
				  result := StringMap.add fd.svar.vname node_id !result  
			  | _ -> () 
			) ) !base;
      !result 

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
		| _ -> debug "\t%s preprocessing problem\n" filename; exit 1); outname
	end else filename 
	in
	  debug "cilRep: %s: parsing\n" filename ; 
	  let file = Frontc.parse filename () in 
		debug "cilRep: %s: parsed\n" filename ; 
		file 

  method get_compiler_command () = 
    "__COMPILER_NAME__ -o __EXE_NAME__ __SOURCE_NAME__ __COMPILER_OPTIONS__ 1>/dev/null 2>/dev/null" 

  method from_source_one_file ?pre:(append_prefix=true) (filename : string) : Cil.file = begin
	let full_filename = 
	  if append_prefix then Filename.concat !prefix filename 
	  else filename
	in
    let file = self#internal_parse full_filename in 
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
		file
  end


  method internal_post_source filename = begin
  end 

  method load_oracle (filename : string) = begin
    debug "loading oracle: %s\n" filename;
	let base,ext = split_ext filename in 
	let filelist = 
	  match ext with 
		"c" -> [filename]
	  | _ -> get_lines filename
	in
	  liter
		(fun fname -> 
		  let file = self#from_source_one_file ~pre:false fname in
			hadd !oracle_code fname file)
		filelist;
	stmt_count := pred !stmt_count
  end

  (* instruments one Cil file for fault localization *)
  method instrument_one_file file ?g:(globinit=false) coverage_sourcename coverage_outname =
    let new_globals = 
		if !uniq_coverage then begin
          let size = 1 + !stmt_count in 
          let size_exp = (Const(CInt64(Int64.of_int size,IInt,None))) in 
			uniq_array_va := (makeGlobalVar "___coverage_array"
								(TArray(charType, Some(size_exp), []))) ;
			if globinit then 
			  [GVarDecl(!uniq_array_va,!currentLoc); GVarDecl(stderr_va,!currentLoc)]
			else 
			  [GVarDecl({!uniq_array_va with vstorage=Extern},!currentLoc);GVarDecl({stderr_va with vstorage=Extern }, !currentLoc) ]
		end 
	  else if globinit then begin
		[GVarDecl(stderr_va, !currentLoc) ]
	  end
	  else
		[GVarDecl({stderr_va with vstorage=Extern }, !currentLoc) ]
	in
      file.globals <- new_globals @ file.globals ;
	  visitCilFileSameGlobals (my_cv coverage_outname) file;
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

(* end of methods from virtual superclass representation *)

(* start of methods from FaultLocRep superclass *)
  method output_function_line_nums = begin
    debug "cilRep: computing function line numbers\n" ; 
	StringMap.iter
	  (fun _ ->
		fun file ->
		  visitCilFileSameGlobals my_flv (copy file))
	!base;
    debug "cilRep: DONE."
  end

  method internal_compute_source_buffers () = 
    let output_list = ref [] in 
    let make_name n = if !multi_file then Some(n) else None in
    StringMap.iter (fun (fname:string) (cil_file:Cil.file) ->
      let source_string = output_cil_file_to_string cil_file in
      output_list := (make_name fname,source_string) :: !output_list 
    ) !base ; 
	  assert((llen !output_list) > 0);
	  !output_list


  method atom_id_of_source_line source_file source_line = begin
    found_atom := (-1);
    found_dist := max_int;
	if hmem !oracle_code source_file then 
	  let file = hfind !oracle_code source_file in 
		visitCilFileSameGlobals (my_find_atom "" source_line) file
	else 
	  StringMap.iter
		(fun fname ->
		  fun file ->
			visitCilFileSameGlobals (my_find_atom source_file source_line) file)
		!base;
    if !found_atom = (-1) then begin
      debug "WARNING: cannot convert %s,%d to atom_id\n" source_file
      source_line ;
      0 
    end else !found_atom
  end
(* end of methods from FaultLocRep superclass *)

(* CilRep specific methods *)

  method inner_output_function_line_nums = begin
    debug "cilRep: computing function line numbers\n" ; 
	StringMap.iter
	  (fun _ ->
		fun file ->
		  visitCilFileSameGlobals my_flv (copy file))
	!base;
    debug "cilRep: DONE."
  end

  method instrument_fault_localization coverage_sourcename coverage_exename coverage_outname = 
    debug "cilRep: instrumenting for fault localization";
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
		 !base true)


end
