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
let cilRep_version = "9" 

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

(* This helper visitor resets all stmt ids to zero. *) 
class numToZeroVisitor = object
  inherit nopCilVisitor
  method vstmt s = s.sid <- 0 ; DoChildren
end 

(* convenience global variable *)
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
 * CLG: this used to be two visitors, numVisitor and numSemanticVisitor, but
 * I kept accidentally changing one but not the other (curses, cloned code!), so  
 * I've folded them into one.  my_num and my_numsemantic are used almost exactly
 * as before, with a slight change in argument order, so you shouldn't notice
 * overmuch. *)

class numVisitor 
        do_semantic
        globalset  (* all global variables *) 
        localset   (* in-scope local variables *) 
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
        if do_semantic then begin
          localset := StringSet.empty ; 
          List.iter (fun v ->
            localset := StringSet.add v.vname !localset 
          ) (fd.sformals @ fd.slocals)
        end; fd
      end,
     (fun fd ->
       if do_semantic then localset := StringSet.empty ;
       fd
     )) 
    
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
            if do_semantic then begin
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
                ignore(visitCilStmt (new varinfoVisitor used) (b));

                if true then begin (* Sanity Checking *) 
                  let in_scope = StringSet.union !localset globalset in 
                    StringSet.iter (fun (vname) ->
                      if not (StringSet.mem vname in_scope) then begin
                        let _ = Pretty.printf "%s not in local+scope scope at:\n%a\n" 
                          vname d_stmt b in
                          exit 1
                      end;
                    ) !used ; 
                end ; 

                let my_locals_used = StringSet.diff !used globalset in 
                  localsused := IntMap.add b.sid my_locals_used !localsused ; 
                  localshave := IntMap.add b.sid !localset !localshave ; 
            end
        end else b.sid <- 0; 
      ) b.bstmts ; 
      b
    ) )
end 

(* my_num numbers an AST without tracking semantic info, my_numsemantic numbers
   an AST while tracking semantic info *) 

let my_num = 
  let dummyMap = ref (IntMap.empty) in
  let dummySet = ref (StringSet.empty) in
    new numVisitor false !dummySet dummySet dummyMap dummyMap
let my_numsemantic = new numVisitor true


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
let fflush = lval (makeVarinfo true "fflush" (TVoid []))
let memset = lval (makeVarinfo true "memset" (TVoid []))
let fprintf = lval (makeVarinfo true "fprintf" (TVoid []))
let fopen = lval (makeVarinfo true "fopen" (TVoid []))
let fclose = lval (makeVarinfo true "fclose" (TVoid []))
let uniq_array_va = ref
  (makeGlobalVar "___coverage_array" (TArray(charType,None,[])))

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
    if not !multithread_coverage then begin
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
    StringSet.subset required locals_here 

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

(* These global variables store the original Cil AST info.  Used as the code
 * bank, and in CilPatchRep as the base against which all representations
 * are compared*)

let global_cilRep_code_bank = ref StringMap.empty
let global_cilRep_oracle_code = ref StringMap.empty 
let global_cilRep_stmt_map = ref (AtomMap.empty) 
let global_cilRep_var_maps = ref (IntMap.empty, IntMap.empty, IntSet.empty) 

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
    {< base = ref (Global.copy !base) ; >} 
  end

  (* serialize the state *) 
  method save_binary ?out_channel (filename : string) = begin
    let fout = 
      match out_channel with
      | Some(v) -> v
      | None -> open_out_bin filename 
    in 
      Marshal.to_channel fout (cilRep_version) [] ; 
      Marshal.to_channel fout (!global_cilRep_code_bank) [] ;
      Marshal.to_channel fout (!global_cilRep_oracle_code) [] ;
      Marshal.to_channel fout (!global_cilRep_stmt_map) [] ;
      Marshal.to_channel fout (!stmt_count) [] ;
      Marshal.to_channel fout (!global_cilRep_var_maps) [] ;
      super#save_binary ~out_channel:fout filename ;
      debug "cilRep: %s: saved\n" filename ; 
      if out_channel = None then close_out fout 
  end 

  (* load in serialized state *) 
  method load_binary ?in_channel (filename : string) = begin
    assert(StringMap.is_empty !base) ;
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
      global_cilRep_code_bank := Marshal.from_channel fin ; 
      global_cilRep_oracle_code := Marshal.from_channel fin ; 
      global_cilRep_stmt_map := Marshal.from_channel fin ;
      stmt_count := Marshal.from_channel fin ;
      global_cilRep_var_maps := Marshal.from_channel fin ; 
      super#load_binary ~in_channel:fin filename ; 
      debug "cilRep: %s: loaded\n" filename ; 
      if in_channel = None then close_in fin ;
      base := !global_cilRep_code_bank
  end 


  (* print debugging information *)  
  method debug_info () = begin
    debug "cilRep: stmt_count = %d\n" !stmt_count ;
    debug "cilRep: stmts in weighted_path = %d\n" 
      (List.length !fault_localization) ; 
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
    assert(not (AtomMap.is_empty !global_cilRep_stmt_map)) ;
    !global_cilRep_stmt_map 
  end

  method get_oracle_code () = !global_cilRep_oracle_code

  method get_base () = !base

  method get_code_bank () = begin
    assert(not (StringMap.is_empty !global_cilRep_code_bank));
    !global_cilRep_code_bank
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

  (* load in a CIL AST from a C source file *) 
  method from_source (filename : string) = begin 
    let _,ext = split_ext filename in 
      (match ext with
        "txt" ->
          liter
            (fun fname ->
              global_cilRep_code_bank := StringMap.add fname (self#from_source_one_file fname) !global_cilRep_code_bank)
            (get_lines filename)
      | "c" | "i" -> 
        global_cilRep_code_bank := StringMap.add filename (self#from_source_one_file filename) !global_cilRep_code_bank
      | _ -> debug "extension: %s\n" ext; failwith "Unexpected file extension in CilRep#from_source.  Permitted: .c, .txt");
      stmt_count := pred !stmt_count ; 
      base := !global_cilRep_code_bank;
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
      if append_prefix then Filename.concat !prefix filename 
      else filename
    in
    let file = self#internal_parse full_filename in 
    let globalset   = ref StringSet.empty in 
    let localset = ref StringSet.empty in
    let localshave = ref (fst3 !global_cilRep_var_maps) in
    let localsused = ref (snd3 !global_cilRep_var_maps) in 
      
      visitCilFileSameGlobals (new everyVisitor) file ; 
      visitCilFileSameGlobals (new emptyVisitor) file ; 
      let add_to_stmt_map x (skind,fname) = 
        global_cilRep_stmt_map := AtomMap.add x (skind,fname) !global_cilRep_stmt_map
      in 

      begin match !semantic_check with
      | "scope" -> 
        (* First, gather up all global variables. *) 
        visitCilFileSameGlobals (new globalVarVisitor globalset) file ; 
        (* Second, number all statements and keep track of
         * in-scope variables information. *) 
        visitCilFileSameGlobals 
              (my_numsemantic
                 !globalset
                 localset
                 localshave
                 localsused 
                 stmt_count add_to_stmt_map filename
              ) file  
              (*
        debug "cilRep: globalset = %d (%g MB)\n" (StringSet.cardinal !globalset) (debug_size_in_mb !globalset) 
        *)

      | _ -> 
        visitCilFileSameGlobals 
          (my_num stmt_count add_to_stmt_map filename) file ; 
    end ;
    (* we increment after setting, so we're one too high: *) 
      (* debug "cilRep: stmt_count = %d\n" !stmt_count  ; *)
    let set_of_all_source_sids = ref (trd3 !global_cilRep_var_maps) in 
      if !use_canonical_source_sids then begin
        Hashtbl.iter (fun str i ->
          set_of_all_source_sids := IntSet.add i !set_of_all_source_sids 
        ) canonical_stmt_ht 
      end else 
        for i = 1 to !stmt_count do
          set_of_all_source_sids := IntSet.add i !set_of_all_source_sids 
        done ;
      (*
        debug "cilRep: unique statements = %d (%g MB)\n"
          (IntSet.cardinal !set_of_all_source_sids)
      (debug_size_in_mb !set_of_all_source_sids)
      ;
    debug "cilRep: |stmt_map| = %g MB\n" (debug_size_in_mb !stmt_map) ; 
    *) 
    (* debug "cilRep: |var_maps| = %g MB\n" (debug_size_in_mb !var_maps) ; *)
      global_cilRep_var_maps := (
        !localshave, !localsused,
        !set_of_all_source_sids); 
      self#internal_post_source filename ;
      file
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
      if (StringMap.mem fname !global_cilRep_oracle_code) then begin
            abort "cilRep: %s already present in oracle code\n" fname ;
      end ; 
      global_cilRep_oracle_code := 
        StringMap.add fname file !global_cilRep_oracle_code
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
      ) !base ; 
      assert((llen !output_list) > 0);
      !output_list
  end

  method atom_id_of_source_line source_file source_line = begin
    found_atom := (-1);
    found_dist := max_int;
    let oracle_code = self#get_oracle_code () in 
    if StringMap.mem source_file oracle_code then  
      let file = StringMap.find source_file oracle_code in  
      visitCilFileSameGlobals (my_find_atom "" source_line) file
    else 
      StringMap.iter (fun fname file -> 
            visitCilFileSameGlobals (my_find_atom source_file source_line) file)
      (self#get_base ());
    if !found_atom = (-1) then begin
      debug "WARNING: cannot convert %s,%d to atom_id\n" source_file
      source_line ;
      0 
    end else !found_atom
  end

  (***********************************
   * Getting coverage information
   ***********************************)

  (* instruments one Cil file for fault localization *)
  method instrument_one_file file ?g:(globinit=false) coverage_sourcename coverage_outname = begin
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
      visitCilFileSameGlobals (new covVisitor coverage_outname) file;
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

  (***********************************
   * Atomic mutations 
   ***********************************)

  (* Atomic Delete of a single statement (atom) *) 
  method delete stmt_id = begin
    let file = self#get_file stmt_id in 
      super#delete stmt_id;
      visitCilFileSameGlobals (my_del stmt_id) file;
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
  end

  (* Return a Set of (atom_ids,fix_weight pairs) that one could append here 
   * without violating many typing rules. *) 
  method append_sources append_after = begin
    let localshave, localsused, _ = !global_cilRep_var_maps in 
    let all_sids = !fix_localization in 
    let sids = 
      if !semantic_check = "none" then all_sids
      else  
    lfilt (fun (sid,weight) ->
          in_scope_at append_after sid localshave localsused 
    ) all_sids
    in
      lfoldl 
        (fun retval ele -> WeightSet.add ele retval) 
        (WeightSet.empty) sids
  end

  (* Atomic Swap of two statements (atoms) *)
  method swap stmt_id1 stmt_id2 = begin
    super#swap stmt_id1 stmt_id2 ; 
    let f1,skind1 = self#get_stmt stmt_id1 in 
    let f2,skind2 = self#get_stmt stmt_id2 in 
    let base = self#get_base () in
    let my_swap = my_swap stmt_id1 skind1 stmt_id2 skind2 in
      if StringMap.mem f1 base then
        visitCilFileSameGlobals my_swap (StringMap.find f1 base);
      if f1 <> f2 && (StringMap.mem f2 base) then
        visitCilFileSameGlobals my_swap (StringMap.find f2 base);
  end

  (* Return a Set of atom_ids that one could swap here without
   * violating many typing rules. In addition, if X<Y and X and Y
   * are both valid, then we'll allow the swap (X,Y) but not (Y,X).
   *) 
  method swap_sources append_after = begin
    let localshave, localsused, _ = !global_cilRep_var_maps in 
    let all_sids = !fix_localization in
    let sids = 
      if !semantic_check = "none" then all_sids
      else 
        lfilt (fun (sid, weight) ->
          in_scope_at sid append_after localshave localsused 
          && in_scope_at append_after sid localshave localsused 
        ) all_sids 
    in
    let sids = lfilt (fun (sid, weight) -> sid <> append_after) sids in
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
    match stmt with
    | Stmt(stmt) -> 
      visitCilFileSameGlobals (my_put stmt_id stmt) file
    | Exp(e) -> failwith "cilRep#put of Exp subatom" 
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
        visitCilFileSameGlobals (my_put stmt_id new_stmt.skind) file
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
   * Structural Differencing
   ***********************************)
  method structural_signature = begin
    let result = ref (StringMap.empty) in
    assert(not !multi_file);
    StringMap.iter
      (fun key base ->
        iterGlobals base (fun g1 ->
          match g1 with
          | GFun(fd,l) -> 
            let node_id = Cdiff.fundec_to_ast fd in 
              result := StringMap.add fd.svar.vname node_id !result
          | _ -> ()
        )) (self#get_base ()); !result
  end
end
