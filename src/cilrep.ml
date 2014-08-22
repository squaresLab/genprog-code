(*
 *
 * Copyright (c) 2012-2013, 
 *  Wes Weimer          <weimer@cs.virginia.edu>
 *  Stephanie Forrest   <forrest@cs.unm.edu>
 *  Claire Le Goues     <legoues@cs.virginia.edu>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. The names of the contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)
(**  Cil C AST.  This is the main implementation of the "Rep" interface for C programs.

     Notably, this includes code and support for: 
     {ul
     {- compiling C programs}
     {- running test cases on C programs}
     {- computing "coverage" fault localization information automatically}
     {- mutating C programs.}
     {- deleting/appending/swapping statements in C programs or loading/applying
     template-defined mutation operations}}

     Supports both the Patch and AST representations for C programs.  Patch is
     now the default. 
*)

open Printf
open Global
open Cil
open Cilprinter
open Template
open Rep
open Pretty
open Minimization

(**/**)
let jungloid_weight = ref 0.0
let jungloid_file = ref ""
let semantic_check = ref "scope" 
let multithread_coverage = ref false
let uniq_coverage = ref false
let swap_bug = ref false 
let template_cache_file = ref ""
let ignore_standard_headers = ref false 
let ignore_dead_code = ref false 
let ignore_equiv_appends = ref false 
let ignore_string_equiv_fixes = ref false 
let ignore_untyped_returns = ref false 

let _ =
  options := !options @
    [
      "--template-cache", Arg.Set_string template_cache_file,
       "save the template computations to avoid wasting time." ;

      "--jungloid-weight", Arg.Set_float jungloid_weight,
      "X cumulative weight to assign to jungloid sub-atoms" ;

      "--jungloid-file", Arg.Set_string jungloid_file,
      "name file listing names of jungloids to use" ;

      "--semantic-check", Arg.Set_string semantic_check, 
      "X limit CIL mutations {none,scope}" ;

      "--mt-cov", Arg.Set multithread_coverage, 
      "  instrument for coverage with locks.  Avoid if possible.";

      "--uniq", Arg.Set uniq_coverage, 
      "  print each visited stmt only once";

      "--swap-bug", Arg.Set swap_bug, 
      " swap is implemented as in ICSE 2012 GMB experiments." ;

      "--ignore-dead-code", Arg.Set ignore_dead_code,
      " do not make known-dead mutations." ; 

      "--ignore-standard-headers", Arg.Set ignore_standard_headers, 
      " do not mutate C library #include headers." ;

      "--ignore-equiv-appends", Arg.Set ignore_equiv_appends, 
      " do not make equivalent append mutations." ; 

      "--ignore-string-equiv-fixes", Arg.Set ignore_string_equiv_fixes, 
      " do not consider string-equivalent fixes twice." ; 

      "--ignore-untyped-returns", Arg.Set ignore_untyped_returns, 
      " do not insert 'return' if the types mismatch." ; 
    ] 
(**/**)

(** {8 High-level CIL representation types/utilities } *)

let cilRep_version = "14" 

(** use CIL to parse a C file. This is called out as a utility function
    because CIL parser has global state hidden in the Errormsg module.
    If this state is not reset, every parse after one that has a parse
    error will mistakenly die. *)
let cil_parse file_name = 
  Errormsg.hadErrors := false ; (* critical! *) 
  Frontc.parse file_name () 

type cilRep_atom =
  | Stmt of Cil.stmtkind
  | Exp of Cil.exp 

type liveness_information = 
  ((atom_id, StringSet.t) Hashtbl.t) option 

(** The AST info for the original input Cil file is stored in a global variable
    of type [ast_info].  *)
type ast_info = 
    { code_bank : Cil.file StringMap.t ;
        (** filename to file map of represented files *)
      jungloids : (float * Cil.varinfo * Cil.typ list) StringMap.t ;
        (** name to (weight, varinfo, \[param_types\]) map of declared functions *)
      subatom_sources : (cilRep_atom * float) list option ;
        (** list of (subatom, weight) pairs available for substitution *)
      oracle_code : Cil.file StringMap.t ;
        (** filename to file map of oracle source code *)
      stmt_map : (string * string) AtomMap.t ;
        (** stmt_id to (function_name, filename) of available statements *)
      localshave : IntSet.t IntMap.t ;
        (** stmt_id to vid map of variables in scope at each statement *)
      globalshave: IntSet.t IntMap.t ; 
        (** stmt_id to vid map of global variables declared before each stmt *)
      globalsset : IntSet.t ;
        (** set of all variable IDs of globals in program *)
      localsused : IntSet.t IntMap.t ;
        (** stmt_id to vid map of variables (local or global) used in stmt *)
      varinfo : Cil.varinfo IntMap.t ;
        (** vid to varinfo map of all variables in program *)
      all_source_sids : IntSet.t ;
        (** set of all stmt_id in program *)

      (* Liveness information is used for --ignore-dead-code *) 
      liveness_before : liveness_information ; 
      liveness_after  : liveness_information ; 
      liveness_failures : StringSet.t ; 

      (* all_appends is computed by --ignore-equiv-appends. If it is
       * AtomMap.empty, then the entire fix localization is valid at
       * each fault location. Otherwise, given a fault location, this
       * maps it to a list of fixes that could be appended there (picking
       * only one representative from each equivalence class, etc.). *) 
      all_appends : ((atom_id * float) list) AtomMap.t ;
    }

(**/**)
let empty_info () =
  { code_bank = StringMap.empty;
    jungloids = StringMap.empty;
    subatom_sources = None;
    oracle_code = StringMap.empty ;
    stmt_map = AtomMap.empty ;
    localshave = IntMap.empty ;
    globalshave = IntMap.empty ; 
    globalsset = IntSet.empty ;
    localsused = IntMap.empty ;
    varinfo = IntMap.empty ;
    all_source_sids = IntSet.empty ;
    liveness_before = None ; 
    liveness_after = None ; 
    liveness_failures = StringSet.empty ; 
    all_appends = AtomMap.empty ; 
    }
(**/**)

let global_ast_info = ref (empty_info()) 

(** bookkeeping for 'super mutant' support *)
let super_mutant_global_varinfo =
  Cil.makeGlobalVar "__genprog_mutant" (TInt(IInt,[]))
let super_mutant_getenv_varinfo = 
  Cil.makeGlobalVar "getenv" (TFun(TPtr(TInt(IChar,[]),[]),None,false,[]))
let super_mutant_atoi_varinfo = 
  Cil.makeGlobalVar "atoi" (TFun(TInt(IInt,[]),None,false,[]))

(** stores loaded templates *)
let registered_c_templates = hcreate 10

class collectTypelabels result = object
  inherit nopCilVisitor

  method vstmt stmt = 
    let str,stmt = Cdiff.stmt_to_typelabel stmt in
      result := (IntSet.add str !result);
      SkipChildren
end

let standard_headers = ref None 

(*
$ `gcc -print-prog-name=cc1` -v < /dev/null
ignoring nonexistent directory
"/usr/lib/gcc/x86_64-linux-gnu/4.6/../../../../x86_64-linux-gnu/include"
#include "..." search starts here:
#include <...> search starts here:
/usr/lib/gcc/x86_64-linux-gnu/4.6/include
/usr/local/include
/usr/lib/gcc/x86_64-linux-gnu/4.6/include-fixed
/usr/include
End of search list.
Analyzing compilation unit
*) 

(* --ignore-standard-headers requires us to be able to determine what
 * the standard include paths are. Currently we only support this for GCC.
 * If /usr/include is a standard include path, then /usr/include/FOO is
 * a standard header for all postfixes FOO. 
 *)
let get_standard_headers () = 
  match !standard_headers with 
  | Some(x) -> x
  | None when not !ignore_standard_headers -> []  
  | None -> begin
    let tfile = Filename.temp_file "genprog" "headers" in 
    let command = Printf.sprintf 
      (* this -print-prog-name arcana is GCC specific *) 
      "`%s -print-prog-name=cc1` -v < /dev/null 2>%s" 
      !compiler_name tfile in 
    let result = match system command with
      | Unix.WEXITED(0) -> 
        let path_file_str = try file_to_string tfile with e -> 
          debug "cilrep: %s read failed: %s\n" command 
          (Printexc.to_string e) ; "" 
        in 
        let regexp = Str.regexp "[:= \n\t]" in
        let parts = Str.split regexp path_file_str in
        let parts = List.filter (fun x ->
          x <> "" && x.[0] = '/'
        ) parts in
        List.iter (fun p ->
          debug "cilRep: coverage ignores: %s\n" p
        ) parts ; 
        standard_headers := Some(parts) ;
        parts 

      | _ -> 
        debug "cilRep: %s failed\n" command ;
        standard_headers := Some([]) ;
        [] 
    in 
    result 
  end 

(* If --ignore-standard-headers is true, locations inside standard header
 * files like /usr/include/stdio.h are not valid for fault localization.
 * (If you think about it, there's no program-specific patch for a
 * system-wide header file.) 
 *
 * Otherwise, all locations are valid. *) 
let can_repair_location loc = 
  if !ignore_standard_headers then begin
    let sh_list = get_standard_headers () in 
    let prefix_match directory file = 
      let minsize = min (String.length directory) (String.length file) in
      let suba = String.sub directory 0 minsize in 
      let subb = String.sub file 0 minsize in
      let result = suba = subb in
      (*
      if result then begin
        debug "cilRep: %s locations ignored\n" file
      end ;
      *) 
      result 
    in 
    not (List.exists (fun sh_directory -> prefix_match sh_directory loc.file 
    ) sh_list) 
  end else true 

(** @param context_sid location being moved to @param moved_sid statement being
    moved @param localshave mapping between statement IDs and sets of variable
    IDs in scope at that statement ID @param localsused mapping between
    statement IDs and sets of variable IDs used at that ID @return boolean
    signifying if all the locals used by [moved_sid] are in scope at location
    [context_sid] *)
let in_scope_at context_sid moved_sid 
    localshave globalshave localsused = 
  if not (IntMap.mem context_sid localshave) then begin
    abort "in_scope_at: %d not found in localshave\n" context_sid ; 
  end ; 
  if not (IntMap.mem context_sid globalshave) then begin
    abort "in_scope_at: %d not found in globalshave\n" context_sid ; 
  end ; 
  if not (IntMap.mem moved_sid localsused) then begin
    abort "in_scope_at: %d not found in localsused\n" moved_sid ; 
  end ; 
  let locals_here = IntMap.find context_sid localshave in 
  let globals_here = IntMap.find context_sid globalshave in 
  let required = IntMap.find moved_sid localsused in 
  IntSet.subset required (IntSet.union locals_here globals_here) 

(** the xformRepVisitor applies a transformation function to a C AST.  Used to
   implement the patch representation for C programs, where the original program
   is transformed by a sequence of edit operations only at compile time. 

    @param xform function that potentially changes a Cil.stmt to return a new
    Cil.stmt
*)
class xformRepVisitor
      (xform : Cil.fundec -> Cil.stmt -> Cil.stmt)
      (bxform : Cil.fundec -> Cil.fundec)
    = object(self)
  inherit nopCilVisitor

  val mutable current = None

  method vfunc func =
    let previous = current in
      current <- Some(func);
      ChangeDoChildrenPost(bxform func, (fun func -> current <- previous; func))

  method vstmt stmt =
    match current with
    | Some(func) -> ChangeDoChildrenPost(stmt, (fun stmt -> xform func stmt))
    | None -> failwith "xformRepVisitor: statement outside of function"
end


(** {8 Initial source code processing} *)

(**/**)
let canonical_stmt_ht = Hashtbl.create 255 
(* as of Tue Jul 26 11:01:16 EDT 2011, WW verifies that canonical_stmt_ht
 * is _not_ the source of a "memory leak" *) 
let canonical_uniques = ref 0 
(**/**)

(** If two statements both print as "x = x + 1", map them to the same statement
    ID. This is used for picking FIX locations, (to avoid duplicates) but _not_
    for FAULT locations.

    @param str string representation of a statement
    @param sid statement id of the statement
    @return int a canonical ID for that statement. 
*)
let canonical_sid str sid =
  try
    Hashtbl.find canonical_stmt_ht str
  with _ -> begin
    Hashtbl.add canonical_stmt_ht str sid ;
    incr canonical_uniques ; 
    sid 
  end 

(** Checks whether a statement is considered mutate-able.  Different handling is
    required for expression-level mutation. 

    @param Cil.stmtkind type of a statement
    @return bool corresponding to whether it's a mutatable statement. 
*)
let can_repair_statement sk = 
    match sk with
    | Instr _ | Return _ | If _ | Loop _ -> true

    | Goto _ | Break _ | Continue _ | Switch _ 
    | Block _ | TryFinally _ | TryExcept _ -> false


(** This helper visitor resets all stmt ids to zero. *) 
class numToZeroVisitor = object
  inherit nopCilVisitor
  method vstmt s = s.sid <- 0 ; DoChildren
end 

(** This visitor changes empty statement lists (e.g., the else branch in if
    (foo) \{ bar(); \} ) into dummy statements that we can modify later. *)
class emptyVisitor = object
  inherit nopCilVisitor
  method vblock b = 
    ChangeDoChildrenPost(b,(fun b ->
      if b.bstmts = [] then 
        mkBlock [ mkEmptyStmt () ] 
      else b 
    ))
end 

(** This visitor makes every instruction into its own statement. *)
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

(**/**)
let my_zero = new numToZeroVisitor
let my_xform = new xformRepVisitor
(**/**)

(** This visitor walks over the C program AST and notes all declared global
    variables.
    
    @param varset reference to an IntSet.t where the info is stored.
*)
class globalVarVisitor 
  varset = object
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

(** This visitor extracts all variable references from a piece of AST. This is used
    later to check if it is legal to swap/insert this statement into another
    context.

    @param setref reference to an IntSet.t where the info is stored.
*)
class varrefVisitor setref = object
  inherit nopCilVisitor
  method vvrbl va = 
    setref := IntSet.add va.vid !setref ;
    SkipChildren 
end 

class labelVisitor labelsetref = object
  inherit nopCilVisitor
  method vstmt s = 
    List.iter (fun lab -> match lab with
      | Label(lab,_,_) -> labelsetref := StringSet.add lab !labelsetref
      | _ -> () 
    ) s.labels ;
    DoChildren
end 
let my_label = new labelVisitor

class cannotRepairVisitor atomsetref = object
  inherit nopCilVisitor 
  method vstmt s = 
    (if s.sid <> 0 then atomsetref := IntSet.add s.sid !atomsetref) ;
    DoChildren
  method vfunc fd = (* function definition *) 
    if can_repair_location fd.svar.vdecl then 
      SkipChildren
    else
      DoChildren
end 
let my_cannotrepair = new cannotRepairVisitor

(** This visitor extracts all variable declarations from a piece of AST.

    @param setref reference to an IntSet.t where the info is stored.  *)
class varinfoVisitor setref = object
  inherit nopCilVisitor
  method vvdec va = 
    setref := IntMap.add va.vid va !setref ;
    DoChildren 
end 

(** This visitor walks over the C program AST, numbers the nodes, and builds the
    statement map, while tracking in-scope variables, if desired.

    @param do_semantic boolean; whether to store info for a semantic check 
    @param globalseen IntSet.t ref storing all global variables seen thus far
    @param localset IntSet.t ref to store in-scope local variables
    @param localshave IntSet.t ref mapping a stmt_id to in scope local variables 
    @param globalshave IntSet.t ref mapping a stmt_id to previously-declared global variables 
    @param localsused IntSet.t ref mapping stmt_id to vars used by stmt_id
    @param count int ref maximum statement id
    @param add_to_stmt_map function of type int -> (string * string) -> (),
    takes a statement id and a function and filename and should add the info to
    some sort of statement map.
    @param fname string, filename
*)
class numVisitor 
  do_semantic
  (globalseen : IntSet.t ref) (* all global variables seen thus far *) 
  (localset : IntSet.t ref)   (* in-scope local variables *) 
  localshave (* maps SID -> in-scope local variables *) 
  globalshave (* maps SID -> previously-declared global variables *) 
  localsused (* maps SID -> non-global vars used by SID *) 
  count add_to_stmt_map fname
  = object
    inherit nopCilVisitor
    val current_function = ref "???" 

    method vglob g = 
      List.iter (fun g -> match g with
      | GVarDecl(v,_) 
      | GVar(v,_,_) -> 
        globalseen := IntSet.add v.vid !globalseen 
      | _ -> () 
      ) [g] ; 
      DoChildren

    method vfunc fd = (* function definition *) 
      globalseen := IntSet.add fd.svar.vid !globalseen ;
      (* if can_repair_location fd.svar.vdecl then  
         Tue May  7 16:06:14 EDT 2013 WRW -- this approach changes the
         numbering of statements, which means that saved coverage
         information becomes invalid, which means that you can't run
         the many-bugs experiments out of the box. Thus, this is disabled
         here and is handled in reduce_fix_space instead. 
      *)
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
      (* else SkipChildren *)
        
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
                let my_locals_used = !used (* IntSet.diff !used !globalsseen *) in 
                localsused := IntMap.add b.sid my_locals_used !localsused ; 
                globalshave := IntMap.add b.sid !globalseen !globalshave ;
                localshave := IntMap.add b.sid !localset !localshave ; 
                if not (IntSet.subset (my_locals_used)
                  (IntSet.union !globalseen !localset)) then begin
                  debug "cilRep: WARNING: numVisitor: scope mismatch\n" ;
                  debug "\tused:" ;
                  IntSet.iter (fun x -> debug " %d" x) my_locals_used ;
                  debug "\n\tlocalset:" ;
                  IntSet.iter (fun x -> debug " %d" x) !localset ;
                  debug "\n\tglobalseen:" ;
                  IntSet.iter (fun x -> debug " %d" x) !globalseen ;
                  debug "\n" ; 
                end 
          end else b.sid <- 0; 
        ) b.bstmts ; 
        b
      ) )
  end 

(**/**)
(* my_num numbers an AST without tracking semantic info, my_numsemantic numbers
   an AST while tracking semantic info *) 
let my_num = 
  let dummyMap = ref (IntMap.empty) in
  let dummySet = ref (IntSet.empty) in
    new numVisitor false 
      dummySet 
      dummySet 
      dummyMap 
      dummyMap
      dummyMap
let my_numsemantic = new numVisitor true
(**/**)

(*** Obtaining coverage and Weighted Path Information ***)

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
(**/**)
let void_t = Formatcil.cType "void *" [] 

(* For most functions, we would like to use the prototypes as defined in the
   standard library used for this compiler. We do this by preprocessing a simple
   C file and extracting the prototypes from there. Since this should only
   happen when actually computing fault localization, the prototypes are lazily
   cached in the va_table. fill_va_table is called by the coverage visitor to
   fill the cache (if necessary) and retrieve the cstmt function along with the
   list of function declarations. *) 

let va_table = Hashtbl.create 10

let fill_va_table = ref 
  ((fun () -> failwith "fill_va_table uninitialized") : unit -> 
  (string -> (Global.StringMap.key * Cil.formatArg) list -> Cil.stmt) *
           Cil.global list Global.StringMap.t)



let uniq_array_va = ref
  (makeGlobalVar "___coverage_array" (Formatcil.cType "char *" []))
let uniq_int_va = ref
  (makeGlobalVar "___coverage_array_already_memset" (Cil.intType))

let do_not_instrument_these_functions = 
  [ "fflush" ; "memset" ; "fprintf" ; "fopen" ; "fclose" ; "vgPlain_fmsg" ; "vgPlain_memset" ] 

(**/**)

(** Visitor for computing statement coverage (for a "weighted path"); walks over
    the C program AST and modifies it so that each statement is preceeded by a
    'printf' that writes that statement's number to the .path file at run-time.
    Determines prototypes for instrumentation functions (e.g. fprintf) that are
    not present in the original code.
    
    @param variant rep the cilrep object being visited; used to preprocess headers
    @param prototypes mapref StringMap to fill with missing prototypes
    @param coverage_outname path filename
    @param found_fmsg valgrind-specific boolean reference
*) 
(* FIXME: multithreaded and uniq coverage are not going to play nicely here in
   terms of memset *)

class covVisitor variant prototypes coverage_outname found_fmsg = 
object
  inherit nopCilVisitor

  val mutable declared = false

  val cstmt = fst (!fill_va_table ())

  method vglob g =
    let missing_proto n vtyp =
      match vtyp with
      | TFun(_,_,_,tattrs) ->
        List.exists (fun tattr ->
          match tattr with
          | Attr("missingproto",_) -> true
          | _ -> false
        ) tattrs
      | _ -> false
    in
      if not declared then begin
        declared <- true;
        prototypes := snd (!fill_va_table ());
      end;
      (* Replace CIL-provided prototypes (which are probably wrong) with the
         ones we extracted from the system headers; but keep user-provided
         prototypes, since the user may have had a reason for specifying them *)
      match g with
      | GVarDecl(vi,_) when (StringMap.mem vi.vname !prototypes) ->
        if missing_proto vi.vname vi.vtype then begin
          ChangeDoChildrenPost([], fun gs -> gs)
        end else begin
          prototypes := StringMap.remove vi.vname !prototypes;
          ChangeDoChildrenPost([g], fun gs -> gs)
        end
      | _ -> ChangeDoChildrenPost([g], fun gs -> gs)

  method vblock b = 
    ChangeDoChildrenPost(b,(fun b ->
      let result = List.map (fun stmt -> 
        if stmt.sid > 0 then begin
          let str = 
            if !is_valgrind then 
              Printf.sprintf "FMSG: (%d)\n" stmt.sid 
            else
              Printf.sprintf "%d\n" stmt.sid
          in
          let print_str = 
            if !is_valgrind then 
              "vgPlain_fmsg(%g:str);"
            else 
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
      let block = 
        { b with bstmts = List.flatten result } 
      in
        block
    ) )

  method vvdec vinfo = 
    if vinfo.vname = "vgPlain_fmsg" then
      found_fmsg := true;
    DoChildren

  method vfunc f = 
    if !found_fmsg then begin 
      if List.mem f.svar.vname do_not_instrument_these_functions then begin 
        debug "cilRep: WARNING: definition of fprintf found at %s:%d\n" 
          f.svar.vdecl.file f.svar.vdecl.line ;
        debug "\tcannot instrument for coverage (would be recursive)\n";
        SkipChildren
      end else if not !multithread_coverage then begin
        let uniq_instrs = 
          if !uniq_coverage then
            let memset_str = 
              if !is_valgrind then "vgPlain_memset(uniq_array, 0, sizeof(uniq_array))"
                 else "memset(uniq_array, 0, sizeof(uniq_array))"
            in
            Printf.sprintf "if(uniq_int == 0) {\n uniq_int = 1;\n %s;\n}\n" memset_str
          else "" 
        in
        let stmt_str = 
          if !is_valgrind then
            uniq_instrs
          else 
            "if (fout == 0) {\n fout = fopen(%g:fout_g,%g:wb_arg);\n"^uniq_instrs^"}"
        in
        let ifstmt = cstmt stmt_str 
          [("uniq_array", Fv(!uniq_array_va));("fout_g",Fg coverage_outname); ("uniq_int", Fv(!uniq_int_va))]
        in
          ChangeDoChildrenPost(f,
                               (fun f ->
                                 f.sbody.bstmts <- ifstmt :: f.sbody.bstmts;
                                 f))
      end else DoChildren
    end else SkipChildren
end 

(** {8 Utilities to inspect, modify, or otherwise manipulate CIL ASTs} With the
    exception of the get visitors for expressions and the replacement
    visitors, most of these are used exclusively by the [astCilRep].
    [patchCilRep] modifies the underlying AST by applying a transformation to
    it at serialization time. *)

(**/**)
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
exception Found_Stmtkind of Cil.stmtkind
let found_atom = ref []
let found_dist = ref max_int 
(**/**)

(** This visitor walks over the C program and finds the [stmtkind] associated
    with the given statement id (living in the given function). 

    @param desired_sid int, id of the statement we're looking for
    @param function_name string, function name to look in
    @raise Found_stmtkind with the statement kind if it is located.
*) 
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

class hasConditionalEdit edits outvar = object
  inherit nopCilVisitor
  method vstmt s = 
    List.iter (fun e -> match e with
      | Conditional(cond,Delete(x)) -> if x = s.sid then outvar := true
      | Conditional(cond,Append(x,y)) -> if x = s.sid then outvar := true
      | Conditional(cond,_) -> failwith "hasConditionalEdit: unhandled conditional edit"
      | _ -> () 
    ) edits ;
    if !outvar then SkipChildren else DoChildren
end 
let my_has_conditional_edit = new hasConditionalEdit 

(** This visitor walks over the C program and finds the [fundec] 
    enclosing the given statement id. 

    @param desired_sid int, id of the statement we're looking for
    @param found_fundec fundec ref, output
    @raise Found_Fundec if it is located.
*) 
exception Found_Fundec 
class findEnclosingFundecVisitor desired_sid found_fundec = object
  inherit nopCilVisitor
  method vfunc fd =
    found_fundec := fd ; 
    DoChildren

  method vstmt s = 
    if s.sid = desired_sid then begin
      raise (Found_Fundec) 
    end ; DoChildren
end 

exception Found_Statement 
class findEnclosingLoopVisitor desired_sid loop_count = object
  inherit nopCilVisitor
  method vfunc fd =
    loop_count := 0 ; 
    DoChildren

  method vstmt s = 
    if s.sid = desired_sid then begin
      raise (Found_Statement) 
    end ; 
    match s.skind with
    | Loop _ -> 
      incr loop_count ; 
      ChangeDoChildrenPost(s, (fun s -> decr loop_count ; s)) 
    | _ -> DoChildren
end 

exception Found_BreakContinue
class findBreakContinueVisitor = object
  inherit nopCilVisitor

  method vstmt s = 
    match s.skind with
    | Loop _ -> SkipChildren (* any breaks inside a loop are fine *)  
    | Break _ | Continue _ -> raise Found_BreakContinue
    | _ -> DoChildren
end 

(** This visitor walks over the C program and to find the atom at the given line
    of the given file. 

    @param source_file string, file to look in
    @param source_line, int line number to look at
*) 
class findAtomVisitor (source_file : string) (source_line : int) = object
  inherit nopCilVisitor
  method vstmt s = 
    if s.sid > 0 then begin
      let this_file = !currentLoc.file in 
      let _,fname1,ext1 = split_base_subdirs_ext source_file in 
      let _,fname2,ext2 = split_base_subdirs_ext this_file in 
        if (fname1^"."^ext1) = (fname2^"."^ext2) || 
          Filename.check_suffix this_file source_file || source_file = "" then 
          begin 
            let this_line = !currentLoc.line in 
            let this_dist = abs (this_line - source_line) in 
              if this_dist < !found_dist then begin
                found_atom := [s.sid] ;
                found_dist := this_dist 
              end else if this_dist = !found_dist then
                found_atom := s.sid :: !found_atom
          end 
    end ;
    DoChildren
end 

(** This visitor finds the statement associated with the given statement ID.
    Sets the reference [gotten_code] to the ID.
      
    @param sid1 atom_id to get
*)
class getVisitor 
  (sid1 : atom_id) 
  = object
    inherit nopCilVisitor
    method vstmt s = 
      if s.sid = sid1 then 
        (gotten_code := s.skind; SkipChildren)
      else DoChildren
  end

(** This visitor finds the expressions associated with the given statement ID.
    Sets the reference output to the expressions.
    
    @param output list reference for the output 
*)
class getExpVisitor output = object
  inherit nopCilVisitor
  method vexpr e = 
    ChangeDoChildrenPost(e, fun e ->
      output := e :: !output ; e
    ) 
end

(** This visitor puts an expression into the given Cil statement. 
      
    @param desired optional expression to put at that location
*)
class putExpVisitor prefix desired = object (self)
  inherit nopCilVisitor

  val count = ref (-1)

  method vexpr e = 
    ChangeDoChildrenPost(e, fun e ->
      incr count ;
      match desired with
      | Some(idx,e') when idx = !count ->
        self#queueInstr prefix;
        e'
      | _ -> e 
    ) 
end

(** Delete a single statement (atom) 

    @param to_del atom_id to be deleted
*)
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

(** Append a single statement (atom) after a given statement (atom) 

    @param append_after id where the append is happening
    @param what_to_append Cil.stmtkind to be appended
*)
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

(** Swap two statements (atoms) 

    @param sid1 atom_id of first statement
    @param skind1 Cil.stmtkind of first statement
    @param sid2 atom_id of second statement
    @param skind2 Cil.stmtkind of second statement
*)  
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

(** Replace one statement with another (atoms) 

    @param replace atom_id of statement to be replaced
    @param replace_with Cil.stmtkind with which it should be replaced.
*)  
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

(** This visitor puts a statement directly into place; used the CIL AST Rep.
    Does an exact replace; does not copy or zero.

    @param sid1 atom_id of statement to be replaced
    @param skind to be put at location [sid1]
 *)
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

(** This visitor fixes up the statement ids after a put operation so as to
    maintain the datastructure invariant.  Make a new one every time you use it or
    the [seen_sids] won't be refreshed and everything will be zeroed *)
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

class sidToLabelVisitor = object
  inherit nopCilVisitor
  method vstmt s = 
    let new_label = Label(Printf.sprintf " %d"s.sid,locUnknown,false) in 
    s.labels <- new_label :: s.labels ; 
    ChangeTo(s) 
end 

(** This visitor computes per-statement livenes information. It computes
the variables that are live BEFORE the statement (lb), those that are live
AFTER the statement (la), and those functions for which there is a failure
to compute liveness (lf). We need BEFORE and AFTER separately to handle
insert vs. append correctly. 

Under the hood, this just uses Cil's liveness-computing library. *) 
class livenessVisitor lb la lf = object
  inherit nopCilVisitor

  method vfunc f = 
    Cfg.clearCFGinfo f;
    ignore(Cfg.cfgFun f);
    (try 
      Errormsg.hadErrors := false ; 
      Liveness.computeLiveness f ; 
      DoChildren
     with e -> 
        debug "cilRep: liveness failure for %s: %s\n"
          f.svar.vname (Printexc.to_string e) ; 
        lf := StringSet.add f.svar.vname !lf;
        SkipChildren
    ) 

  method vstmt s = 
    match s.labels with
    | (Label(first,_,_)) :: rest ->
      if first <> "" && first.[0] = ' ' then begin
        let genprog_sid = my_int_of_string first in 
        let sla = Liveness.getPostLiveness s in (* returns a varinfo set *) 
        let after = try Hashtbl.find la (genprog_sid) with _ ->
          StringSet.empty in 
        let after = Usedef.VS.fold (fun varinfo sofar ->
          StringSet.add varinfo.vname sofar
        ) sla after in 
        Hashtbl.replace la (genprog_sid) after ;

        let slb = Liveness.getLiveness s in (* returns a varinfo set *) 
        let before = try Hashtbl.find lb (genprog_sid) with _ ->
          StringSet.empty in 
        let before = Usedef.VS.fold (fun varinfo sofar ->
          StringSet.add varinfo.vname sofar
        ) slb before in 
        Hashtbl.replace lb (genprog_sid) before ;
      end ;
      DoChildren
    | _ -> DoChildren 
end 

let isIntegralType t = 
  match unrollType t with
    (TInt _ | TEnum _) -> true
  | _ -> false

let isArithmeticType t = 
  match unrollType t with
    (TInt _ | TEnum _ | TFloat _) -> true
  | _ -> false
    

let isPointerType t = 
  match unrollType t with
    TPtr _ -> true
  | _ -> false

let isScalarType t =
  isArithmeticType t || isPointerType t

let isFunctionType t = 
  match unrollType t with
    TFun _ -> true
  | _ -> false

(**
 * Determines whether a value of type [t1] can be used in place of a value of
 * type [t2].
 *)
let rec isCompatible t1 t2 =
  match (unrollType t1), (unrollType t2) with
  | TVoid(_),            TVoid(_)            -> true
  | TInt(_),             TInt(_)             -> true
  | TFloat(_),           TFloat(_)           -> true
  | TPtr(t1',_),         TPtr(t2',_)         -> isCompatible t1' t2'
  | TArray(t1',_,_),     TArray(t2',_,_)     -> isCompatible t1' t2'
  | TComp(c1,_),         TComp(c2,_)         -> c1.cname = c2.cname
  | TEnum(e1,_),         TEnum(e2,_)         -> e1.ename = e2.ename
  | TBuiltin_va_list(_), TBuiltin_va_list(_) -> true

  | TNamed(t1',_), _ -> isCompatible t1'.ttype t2
  | _, TNamed(t2',_) -> isCompatible t1 t2'.ttype

  | TInt(_),   TEnum(_)  -> false
  | TEnum(_),  TInt(_)   -> true
  | TInt(_),   TFloat(_) -> true
  | TFloat(_), TInt(_)   -> false

  | TFun(r1,args1,va1,_), TFun(r2,args2,va2,_) ->
    (* if the return value of the new function can replace the return value of
       the old function ... *)
    if isCompatible r1 r2 then begin
      let rec match_args args1 args2 =
        match args1, args2 with
        | (_,t1',_) :: args1', (_,t2',_) :: args2' ->
          (* if the argument to the old function can be used as an argument to
             the new function ... *)
          (isCompatible t2' t1') && (match_args args1' args2')
        | [], [] -> true
        | [],  _ -> va1
        | _,  [] -> va2
      in
        match_args (argsToList args1) (argsToList args2)
    end else
      false

  | _ -> false

(**/**)
let my_put_exp ?(insert = (fun _ -> []),[]) stmt rand e =
  let exprs = ref [] in
  let stmt = visitCilStmt (new getExpVisitor exprs) stmt in
  let exprs = Array.of_list (List.rev !exprs) in
  let flip f x y = f y x in
  let get_replacement validator =
    (* get the index of a valid expression from the list *)
    let candidates, _ =
      Array.fold_left (fun (lst,i) e' ->
        if validator (typeOf e') then
          i :: lst, i + 1
        else
          lst, i + 1
      ) ([], 0) exprs
    in
    if (llen candidates) > 0 then
      let pct = rand#nextFloat () in
      let index = int_of_float (pct *. (float_of_int (llen candidates))) in
      List.nth candidates index
    else
      (-1)
  in
  (* find an expression that can be replaced by the new one *)
  let target = get_replacement (isCompatible (typeOf e)) in
  if target == -1 then
    stmt
  else
    (* find expressions that can replace the function arguments *)
    let args =
      lmap (fun t -> get_replacement ((flip isCompatible) t)) (snd insert)
    in
    if (List.for_all (fun n -> n >= 0) (target :: args)) then
      (* find arguments that can be replaced by the target *)
      let holes, _, _ =
        lfoldl (fun (holes, i, x) t ->
          if isCompatible t x then
            i :: holes, i + 1, x
          else
            holes, i + 1, x
        ) ([], 0, typeOf exprs.(target)) (snd insert)
      in
      let instrs =
        if (llen args) > 0 && (llen holes) > 0 then
          let hole = List.hd (random_order holes) in
          let args, _ =
            lfoldl (fun (args, i) arg ->
              if i == hole then
                target :: args, i + 1
              else
                arg :: args, i + 1
            ) ([], 0) args
          in
          let args = lmap (fun n -> exprs.(n)) (List.rev args) in
          (fst insert) args
        else
          []
      in
        visitCilStmt (new putExpVisitor instrs (Some(target,e))) stmt
    else
      stmt

let my_get = new getVisitor
let my_get_exp = new getExpVisitor 
let my_findstmt = new findStmtVisitor
let my_findenclosingfundec = new findEnclosingFundecVisitor
let my_findenclosingloop = new findEnclosingLoopVisitor
let my_findbreakcontinue = new findBreakContinueVisitor
let my_find_atom = new findAtomVisitor
let my_del = new delVisitor 
let my_app = new appVisitor 
let my_swap = new swapVisitor 
let my_rep = new replaceVisitor
let my_put = new putVisitor
let my_liveness = new livenessVisitor
let my_sid_to_label = new sidToLabelVisitor
(**/**)

exception MissingDefinition of string

(**
 * Determine the namespace and name for a global. The boolean indicates whether
 * the global represents a full definition (true) or just a declaration (false).
 *
 * There are three namespaces for globals defined in C: one for typedefs
 * (`DType), one for tagged types (structs, unions and enums: `DTag), and one
 * for variables and functions (`DVar). For example, you can have a struct and
 * a typedef with the same name since they are in different namespaces;
 * however, attempting to have a struct and an enum with the same name will
 * cause a compile error.
 *)
let get_dependency_tag g =
  match g with
  | GType(ti,_)        -> Some(`DType, false, ti.tname)
  | GCompTag(ci,_)     -> Some(`DTag,  true,  ci.cname)
  | GCompTagDecl(ci,_) -> Some(`DTag,  false, ci.cname)
  | GEnumTag(ei,_)     -> Some(`DTag,  true,  ei.ename)
  | GEnumTagDecl(ei,_) -> Some(`DTag,  false, ei.ename)
  | GVarDecl(vi,_)     -> Some(`DVar,  false, vi.vname)
  | GVar(vi,_,_)       -> Some(`DVar,  true,  vi.vname)
  | GFun(fd,_)         -> Some(`DVar,  true,  fd.svar.vname)
  | _                  -> None

(**
 * Generates a human-readable string form of the tag for debugging and error
 * messages.
 *)
let string_of_tag tag =
  match tag with
  | (`DType, true,  n) -> "typedef " ^ n ^ " (def)"
  | (`DType, false, n) -> "typedef " ^ n ^ " (decl)"
  | (`DTag,  true,  n) -> "tag " ^ n ^ " (def)"
  | (`DTag,  false, n) -> "tag " ^ n ^ " (decl)"
  | (`DVar,  true,  n) -> "var " ^ n ^ " (def)"
  | (`DVar,  false, n) -> "var " ^ n ^ " (decl)"

(**
 * [toposort instantiation_table visited globals tag] inserts the global
 * represented by [tag] into the list of globals, followed by the definitions
 * it requires, and returns the updated list of globals. Note that this list is
 * in the reverse order that the compiler expects. The [instantiation_table] is
 * a Hashtbl mapping dependency tags to the globals they represent. The
 * [visited] argument is a Hashtable allowing quick determination of whether a
 * global is in the list, in order to avoid multiple definitions and infinite
 * loops.
 *)
let rec toposort_one instantiations visited gs ((d,b,n) as tag) =
  let root = Hashtbl.find instantiations tag in
  if Hashtbl.mem visited tag then
    gs
  else begin
    let use_array_type = ref false in
    let children = ref [] in
    let enable_recursion = ref false in
    let visitor = object (self)
      inherit nopCilVisitor

      (**
       * Implements the dependency between the current [tag] (passed to
       * [toposort_one]) and the [tag'] passed to this method. Recursively
       * calls [toposort_one] to ensure that all the depdendencies for [tag']
       * are recorded in [gs] before the global represented by [tag'].
       *
       * Returns [SkipChildren] as a convenience to the calling visitation
       * method.
       *)
      method private add_deps ((d',b',n') as tag') : ('a visitAction) =
        (* Find the tag for a global satisfying this dependency. This could be
           the actual tag requested, if it exists, or a tag for a definition
           when we depend on a declaration. *)
        let tag'' =
          if Hashtbl.mem instantiations tag' then
            tag'
          else if Hashtbl.mem instantiations (d', true, n') then
            (d', true, n')
          else
            raise (MissingDefinition (string_of_tag tag'))
        in
        (* Guard that we do not depend on ourselves. *)
        if !enable_recursion && (d <> d' || n <> n') then
          children := toposort_one instantiations visited !children tag'';
        SkipChildren

      method vtype t =
        (* We always need the definition of a type in an array, but only need
           the declaration of a type that is only pointed to. *)
        let depended_type, b' =
          match t with
          | TArray(t',_,_) -> use_array_type := true; t', true
          | TPtr(t',_)     -> t', false
          | t'             -> t', b
        in

        (* Make sure the type we depend on has its dependencies met. *)
        match depended_type with
        | TNamed(ti,_) -> self#add_deps (`DType, b', ti.tname)
        | TComp(ci,_)  -> self#add_deps (`DTag, b', ci.cname)
        | TEnum(ei,_)  -> self#add_deps (`DTag, b', ei.ename)
        | _ -> DoChildren

      method vexpr e =
        match e with
        (* We are dependent on the definitions of types that the program takes
           the size or alignment of. Since we can only take the size or
           alignment when processing a definition, the current tag boolean is
           correct when accessed in [vtype]. *)

        | SizeOf(t)   -> ignore (self#vtype t); SkipChildren
        | AlignOf(t)  -> ignore (self#vtype t); SkipChildren

        (* We also need the definitions of p ointed-to types when doing pointer
           match since incrementing a pointer adjusts it by a multiple of the
           size of the data type. *)

        | BinOp((PlusPI|IndexPI|MinusPI),e1,_,_) ->
          ignore (self#vtype (typeOf (Lval(Mem(e1),NoOffset))));
          DoChildren
        | BinOp(MinusPP,e1,e2,_) ->
          ignore (self#vtype (typeOf (Lval(Mem(e1),NoOffset))));
          ignore (self#vtype (typeOf (Lval(Mem(e2),NoOffset))));
          DoChildren

        (* For everything else, we depend on the type of the expression itself
           since the compiler needs to know how to represent the result. *)

        | _ ->
          ignore (self#vtype (typeOf e));
          DoChildren

      (* We depend on the types of the hosts of all lvals since the compiler
         needs to know the layout of structures before it can access their
         fields. *)

      method vlval (host,off) =
        let _ = self#vtype (typeOfLval (host, NoOffset)) in
        DoChildren

      (* We also depend on declarations for any global variables used. *)

      method vvrbl vi =
        if vi.vglob then
          ignore (self#add_deps (`DVar, false, vi.vname));
        SkipChildren
    end in

    (* Although there is only one way to make a typedef, other code may depend
       on the typedef as a "declaration" or as a "definition". Usually, the
       typedef "declaration" depends on the declaration of the renamed type
       while the typedef "definition" depends on the definition of the renamed
       type. But even a "declaration" of a typedef of an array type requires
       the full definition of the renamed type. (Actually a typedef
       "declaration" may not even require a declaration of the named type at
       all, but this approach is usually good enough.) *)
 
    let b =
      match d with
      | `DType ->
        let _ = visitCilGlobal visitor root in
        b || !use_array_type
      | _ -> b
    in

    (* Visit the global, which will ensure our dependencies are in the list. *)

    children := gs;
    enable_recursion := true;
    let _ = visitCilGlobal visitor root in

    (* Check that visiting the dependencies didn't insert anything that would
       satisfy a dependency on us before inserting. *)

    if not (Hashtbl.mem visited (d,b,n))
        && not (d = `DType && (Hashtbl.mem visited (d, not b, n))) then
      children := root :: !children;

    (* Mark this as visited. If it is a definition, mark the declaration as
       well. *)

    Hashtbl.replace visited (d,b,n) true;
    if b then
      Hashtbl.replace visited (d, false, n) true;
    !children
  end

(**
 * Sorts the given globals so that all declarations and definitions appear
 * before their first use. If the optional ~roots argument it given, only
 * returns the dependencies (drawn from the other list) needed to compile those
 * globals. Otherwise, treats all globals as the roots.
 *)
let toposort_globals ?roots:((roots: global list) = []) (globals: global list) =
  let filter_map f xs =
    List.rev (List.fold_left (fun ys x ->
      match f x with
      | Some(y) -> y :: ys
      | None    -> ys
    ) [] xs)
  in
  let roots =
    match roots with
    | [] -> filter_map get_dependency_tag globals
    | _  -> filter_map get_dependency_tag roots
  in

  (* Build the instantiations and visited table for [toposort_one] *)
  let instantiations = Hashtbl.create (List.length globals) in
  List.iter (fun g ->
    match get_dependency_tag g with
    | Some(`DType,_,n) ->
      Hashtbl.replace instantiations (`DType,false,n) g;
      Hashtbl.replace instantiations (`DType,true,n) g;
    | Some(d,true,n) ->
      if not (Hashtbl.mem instantiations (d,false,n)) then begin
        match g with
        | GCompTag(ci,loc) ->
          Hashtbl.replace instantiations (d,false,n) (GCompTagDecl(ci,loc))
        | GEnumTag(ei,loc) ->
          Hashtbl.replace instantiations (d,false,n) (GEnumTagDecl(ei,loc))
        | GVar(vi,_,loc) ->
          Hashtbl.replace instantiations (d,false,n) (GVarDecl(vi,loc))
        | GFun(fd,loc) ->
          Hashtbl.replace instantiations (d,false,n) (GVarDecl(fd.svar,loc))
        | _ -> ()
      end;
      Hashtbl.replace instantiations (d,true,n) g
    | Some(tag) -> Hashtbl.add instantiations tag g
    | None      -> ()
  ) globals;

  let visited = Hashtbl.create (List.length globals) in

  (* Toposort each of the roots, using the same instantiations and visited
     tables. The result is reversed, so reverse it before returning. *)

  let deps =
    List.fold_left (toposort_one instantiations visited) [] roots
  in
  List.rev deps

(** {8 CIL Representation implementations } The virtual superclass implements
    much of the source code processing.  The only conceptual difference between
    the [patchCilRep] and [astCilRep] is the type of the underlying gene.
    Notably, both make use of [global_ast_info] and thus have global state that
    must be considered especially when serializing or deserializing.  Note that
    not all concrete methods appear in the ocamldoc; obvious things like
    [compile], for example, are typically ommitted.  I only include them if they
    can fail or if they have particularly interesting features or pre/post
    conditions as compared to the other representation implementations. *)


class virtual ['gene] cilRep  = object (self : 'self_type)
  (** Cil-based individuals are minimizable *)
  inherit minimizableObject 
        (** the underlying code is [cilRep_atom] for all C-based individuals *)
  inherit ['gene, cilRep_atom] faultlocRepresentation as super

  (**/**)

  val stmt_count = ref 1 

  method copy () : 'self_type =
    let super_copy : 'self_type = super#copy () in 
      super_copy#internal_copy () 

  method internal_copy () : 'self_type =
    {< history = ref (copy !history);
       stmt_count = ref !stmt_count >}

  (* serialize the state *) 
  method serialize ?out_channel ?global_info (filename : string) =
    let fout = 
      match out_channel with
      | Some(v) -> v
      | None -> open_out_bin filename 
    in 
      Marshal.to_channel fout (cilRep_version) [] ; 
      let gval = match global_info with Some(true) -> true | _ -> false in
        if gval then begin
          Marshal.to_channel fout (!stmt_count) [] ;
          Marshal.to_channel fout (!global_ast_info.code_bank) [] ;
          Marshal.to_channel fout (!global_ast_info.jungloids) [] ;
          Marshal.to_channel fout (!global_ast_info.subatom_sources) [];
          Marshal.to_channel fout (!global_ast_info.oracle_code) [] ;
          Marshal.to_channel fout (!global_ast_info.stmt_map) [] ;
          Marshal.to_channel fout (!global_ast_info.localshave) [] ;
          Marshal.to_channel fout (!global_ast_info.globalshave) [] ;
          Marshal.to_channel fout (!global_ast_info.globalsset) [];
          Marshal.to_channel fout (!global_ast_info.localsused) [] ;
          Marshal.to_channel fout (!global_ast_info.varinfo) [];
          Marshal.to_channel fout (!global_ast_info.all_source_sids) [] ;
          Marshal.to_channel fout (!global_ast_info.liveness_before) [] ;
          Marshal.to_channel fout (!global_ast_info.liveness_after) [] ;
          Marshal.to_channel fout (!global_ast_info.liveness_failures) [] ;
          Marshal.to_channel fout (!global_ast_info.all_appends) [] ;
        end;
        Marshal.to_channel fout (self#get_genome()) [] ;
        debug "cilRep: %s: saved\n" filename ; 
        super#serialize ~out_channel:fout ?global_info:global_info filename ;
        debug "cilrep done serialize\n";
        if out_channel = None then close_out fout 
(**/**)

  (** @raise Fail("version mismatch") if the version specified in the binary file is
      different from the current [cilRep_version] *)
  method deserialize ?in_channel ?global_info (filename : string) = begin
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
      let gval = match global_info with Some(n) -> n | _ -> false in
        if gval then begin
          let _ = stmt_count := Marshal.from_channel fin in
          let code_bank = Marshal.from_channel fin in
          let jungloids = Marshal.from_channel fin in
          let subatom_sources = Marshal.from_channel fin in
          let oracle_code = Marshal.from_channel fin in
          let stmt_map = Marshal.from_channel fin in
          let localshave = Marshal.from_channel fin in 
          let globalshave = Marshal.from_channel fin in 
          let globalsset = Marshal.from_channel fin in 
          let localsused = Marshal.from_channel fin in 
          let varinfo = Marshal.from_channel fin in 
          let all_source_sids = Marshal.from_channel fin in 
          let liveness_before = Marshal.from_channel fin in 
          let liveness_after = Marshal.from_channel fin in 
          let liveness_failures = Marshal.from_channel fin in 
          let all_appends = Marshal.from_channel fin in 
            global_ast_info :=
              { code_bank = code_bank;
                jungloids = jungloids;
                subatom_sources = subatom_sources;
                oracle_code = oracle_code;
                stmt_map = stmt_map ;
                localshave = localshave ;
                globalsset = globalsset ;
                globalshave = globalshave ; 
                localsused = localsused ;
                varinfo = varinfo;
                all_source_sids = all_source_sids ;
                liveness_before = liveness_before ; 
                liveness_after = liveness_after ; 
                liveness_failures = liveness_failures ; 
                all_appends = all_appends ; 
                }
        end;
        self#set_genome (Marshal.from_channel fin);
        super#deserialize ~in_channel:fin ?global_info:global_info filename ; 
        debug "cilRep: %s: loaded\n" filename ; 
        if in_channel = None then close_in fin ;
  end 

  (**/**)


  (* Use an approximation to the program equivalence relation to 
   * remove duplicate edits (i.e., those that would yield semantically
   * equivalent programs) from consideration. Command line arguments
   * control the exact approximation(s) used. *) 
  method reduce_fix_space () = 
    super#reduce_fix_space () ;

    if !ignore_standard_headers then begin
      (* if a location is in the standard headers, remove it from the
         fault space (we can't write a patch to it) and from the
         fix space (it wasn't written by the same devs, so we have no/less
         reason to believe it will be a fix) 
       *) 
      let cannot_repair = ref IntSet.empty in 
      StringMap.iter (fun str file ->
        visitCilFileSameGlobals (my_cannotrepair cannot_repair) file ;
      ) !global_ast_info.code_bank ; 
      let cannot_repair = !cannot_repair in 
      debug "cilRep: atoms in standard headers: %d\n" 
        (IntSet.cardinal cannot_repair) ; 

      fault_localization := 
        List.filter (fun (atom,w) ->
          not (IntSet.mem atom cannot_repair)
        ) !fault_localization ; 
      fix_localization := 
        List.filter (fun (atom,w) ->
          not (IntSet.mem atom cannot_repair)
        ) !fix_localization ; 
    end ; 

    let original_fixes = List.length !fix_localization in 
    let fixes_seen = Hashtbl.create 255 in 

    (* As a hideous hack, some of the CIL libraries that we use
     * renumber all of the statement IDs. Since we're using the statement
     * IDs to track our atom_ids, that would be very bad. When using such
     * ill-behaved CIL libraries we make a copy of the AST and store our
     * atom_id as a special __label:, allowing us to extract the
     * generated information later and map it back to our atom_ids. *) 
    let atom_id_to_str atom_id = 
      let where,skind = self#get_stmt atom_id in 
      let stripped_stmt = { 
        labels = [] ; skind = skind ; sid = 0; succs = [] ; preds = [] ;
      } in 
      let pretty_printed =
        try 
          Pretty.sprint ~width:80
            (Pretty.dprintf "%a" dn_stmt stripped_stmt)
        with _ -> Printf.sprintf "@%d" atom_id
      in 
      pretty_printed
    in 

    (* A very simple approximation. "x=0" might occur in two places in the
     * program (fix space), at which point we'd consider appending "x=0"
     * twice to every statement. Since syntactic equality implies semantic
     * equality, if multiple strings are exactly equal, just use one. *) 
    if !ignore_string_equiv_fixes then begin 

      liter (fun (atom_id, weight) ->
        let pretty_printed = atom_id_to_str atom_id in 
        let sofar = try Hashtbl.find fixes_seen pretty_printed with _ -> 0. in 
        let new_weight = max sofar weight in 
        Hashtbl.replace fixes_seen pretty_printed new_weight ; 
        (*
        debug "string equality: atom %d\n%S\n" atom_id pretty_printed 
        *) 
      ) !fix_localization ;
      fix_localization := List.filter (fun (atom_id, weight) -> 
        let pretty_printed = atom_id_to_str atom_id in 
        try 
          let w' = Hashtbl.find fixes_seen pretty_printed in
          Hashtbl.remove fixes_seen pretty_printed ;
          w' = weight 
        with Not_found -> false
      ) !fix_localization ; 

      debug "cilRep: fix-space quotient by string: %d -> %d\n" 
        original_fixes 
        (List.length !fix_localization) ; 

    end ; 

    (* Use insights from instruction scheduling to partition blocks
     * into equivalence classes with respect to edits. *) 
    if !ignore_equiv_appends then begin
      try 
      let files = !global_ast_info.code_bank in 
      let all_appends = ref AtomMap.empty in 
      liter (fun (src_atom_id,fix_w) ->
        let src_where, src_skind = 
          try self#get_stmt src_atom_id 
          with e -> 
            debug "cilRep: ERROR: --ignore-equiv-appends: src_atom_id %d not found\n" src_atom_id ; raise e 
          in 
        let src_effects = Progeq.effects_of_stmtkind files src_skind in 
        let parts = 
          try 
            Stats2.time "progeq partition" (fun () -> 
            Progeq.partition !global_ast_info.code_bank src_effects 
            ) () 
          with e -> 
            debug "cilRep: WARNING: cannot compute partition: %s\n" 
              (Printexc.to_string e) ; raise e 
        in 
        (* 
         * for each partition, filter out
         *  -- destinations with no atom_id
         *  -- destinations not in fault_loc
         * then choose pick one exemplar
         *)
        let add_append dst_atom_id = 
          let sources_at_dest = 
            try AtomMap.find dst_atom_id !all_appends with _ -> []
          in 
          let sources_at_dest = 
            (src_atom_id, fix_w) :: sources_at_dest in
          all_appends := AtomMap.add dst_atom_id sources_at_dest !all_appends
        in 

        liter (fun partition -> 
          let partition = List.filter (fun dst_stmt -> 
            dst_stmt.sid != 0 
            &&
            (List.exists (fun (fault_atom_id,fault_w) -> 
              fault_atom_id = dst_stmt.sid) !fault_localization)
          ) partition in 
          match partition with
          | [] -> (* do nothing *) () 
          | fault_stmt :: rest -> 
            (* FIXME: currently we always choose the first. This is not
             * a huge problem, since they're all semantically equivalence,
             * but one of them may compile faster or somesuch. *) 
            add_append fault_stmt.sid ; 

            (* debugging *) 
            (* 
            if rest <> [] then begin 
              debug "Yes Appending:\n\t%s\nAfter:\n\t%s\n" 
                (Pretty.sprint ~width:80 
                (dn_stmt () ({ labels = [] ; 
                  skind = src_skind ; sid = 0; succs = [] ; preds = [] ; })))
                (Pretty.sprint ~width:80 (dn_stmt () fault_stmt)) ; 
              let rec debug_rest lst = match lst with 
                | [] -> () 
                | hd :: tl -> 
                  debug "Not After: %s\n" 
                    (Pretty.sprint ~width:80 (dn_stmt () hd)) ;
                  debug_rest tl 
              in
              debug_rest rest 
            end ; 
            *) 
          ) parts ;
      ) !fix_localization ; 

      global_ast_info := {!global_ast_info with
        all_appends = !all_appends ;
      } 
      with e -> 
        debug "cilRep: ERROR: --ignore-equiv-appends: %s\n" 
          (Printexc.to_string e) 

    end ; 
    () 

  method atom_to_str atom = 
    let doc = match atom with
      | Exp(e) -> d_exp () e 
      | Stmt(s) -> dn_stmt () (mkStmt s) 
    in 
      Pretty.sprint ~width:80 doc 

  method debug_info () =
    debug "cilRep: stmt_count = %d\n" !stmt_count ;
    debug "cilRep: stmts in fault path = %d (total weight = %g)\n" 
      (List.length !fault_localization)
      (lfoldl (fun total (i,w) -> total +. w) 0.0 !fault_localization);
    debug "cilRep: stmts in fault path with weight >= 1.0 = %d\n" 
      (List.length (List.filter (fun (a,b) -> b >= 1.0) !fault_localization)) ;
    debug "cilRep: stmts in fix path = %d (total weight = %g)\n"
      (List.length !fix_localization)
      (lfoldl (fun total (i,w) -> total +. w) 0.0 !fix_localization);
    debug "cilRep: stmts in fix path with weight >= 1.0 = %d\n"
      (List.length (List.filter (fun (a,b) -> b >= 1.0) !fix_localization)) ;
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
            debug "cilRep: %s (code bank/base file; atoms [%d,%d])\n" 
              k low high 
        ) (self#get_code_bank ()) ; 
      StringMap.iter (fun k v ->
        incr file_count ; 
        let low, high = statement_range k in 
          debug "cilRep: %s (oracle file; atoms [%d,%d])\n" k low high
      ) (self#get_oracle_code ()) ; 
      debug "cilRep: %d file(s) total in representation\n" !file_count ; 

  method max_atom () = !stmt_count 

  (**/**)

  (** 

      {8 Methods that access to statements, files, code, etc, in both the base
      representation and the code bank} *)

  method get_stmt_map () = 
    assert(not (AtomMap.is_empty !global_ast_info.stmt_map)) ;
    !global_ast_info.stmt_map 

  method get_oracle_code () = !global_ast_info.oracle_code

  method get_code_bank () = 
    assert(not (StringMap.is_empty !global_ast_info.code_bank));
    !global_ast_info.code_bank

  (** gets a statement, indexed by its unique integer ID, from the {b code
      bank}, *not the current variant*.  Will also look in the oracle code if
      available/necessary.  As distinct from method {b get} (horrific naming
      scheme to be rectified...later?), which gets a statement from the *current
      variant* (which may have been changed/affected by random mutations made to
      the original). 
      
      @param stmt_id id of statement we're looking for
      @return (filename,stmtkind) file that statement is in and the statement itself
      @raise Fail("stmt_id not found in stmt map")
  *)
  method get_stmt stmt_id =
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
          let code_bank_size = 
            StringMap.fold (fun key elt acc -> acc + 1) code_bank 0 
          in 
          let oracle_size = 
            StringMap.fold (fun key elt acc -> acc + 1) oracle_code 0 
          in 
          let _ =
            debug "cilrep: code bank size %d, oracle code size %d\n" 
              code_bank_size oracle_size 
          in
            abort "cilrep: cannot find stmt id %d in code bank or oracle\n%s (function)\n%s (file not found)\n"
              stmt_id funname filename 
      in  
      let file_ast = StringMap.find filename file_map in 
        try 
          visitCilFileSameGlobals (my_findstmt stmt_id funname) file_ast ;
          abort "cilrep: cannot find stmt %d in code bank\n%s (function)\n%s (file)\n" 
            stmt_id funname filename 
        with Found_Stmtkind(skind) -> filename,skind 
    end
    with Not_found -> 
      abort "cilrep: %d not found in stmt_map\n" stmt_id 

  (** gets the file ast from the {b base representation} This function can fail
     if the statement is not in the statement map or the id is otherwise not
     valid. 

      @param stmt_id statement we're looking for
      @return Cil.file file the statement is in
      @raise Not_found(filename) if either the id or the file name associated
      with it are not vaild. 
  *)
  method get_file (stmt_id : atom_id) : Cil.file =
    let _,fname = AtomMap.find stmt_id (self#get_stmt_map()) in
      StringMap.find fname (self#get_base())

  (** gets the id of the atom at a given location in a file.  Will print a
      warning and return -1 if the there is no atom found at that location 
      
      @param source_file string filename
      @param source_line int line number
      @return atom_id of the atom at the line/file combination *)
  method atom_id_of_source_line source_file source_line =
    found_atom := [];
    found_dist := max_int;
    let oracle_code = self#get_oracle_code () in 
      if StringMap.mem source_file oracle_code then  
        let file = StringMap.find source_file oracle_code in  
          visitCilFileSameGlobals (my_find_atom source_file source_line) file
      else 
        StringMap.iter (fun fname file -> 
          visitCilFileSameGlobals (my_find_atom source_file source_line) file)
          (self#get_base ());
      if !found_atom = [] then begin
        debug "cilrep: WARNING: cannot convert %s,%d to atom_id\n" source_file
          source_line ;
        [0]
      end else !found_atom


  (** {8 Methods for loading and instrumenting source code} *)

  (** loads a CIL AST from a C source file or collection of C source files.
      
      @param filename string with extension .txt, .c, .i, .cu, .cu
      @raise Fail("unexpected file extension") if given anything else
  *)
  method from_source (filename : string) = begin 
    debug "cilrep: from_source: pre: stmt_count = %d\n" !stmt_count ; 
    let _,ext = split_ext filename in 
    let code_bank = 
      match ext with
        "txt" ->
          lfoldl
            (fun map fname ->
              let parsed = self#from_source_one_file fname in
                StringMap.add fname parsed map)
            !global_ast_info.code_bank (get_lines filename)
      | "c" | "i" | "cu" | "cg" -> 
        let parsed = self#from_source_one_file filename in 
          StringMap.add filename parsed !global_ast_info.code_bank
      | _ -> 
        abort "Unexpected file extension %s in CilRep#from_source.  Permitted: .c, .i, .cu, .cu, .txt" ext
    in
      global_ast_info := {!global_ast_info with code_bank = code_bank};
      stmt_count := pred !stmt_count ;
      debug "cilrep: from_source: post: stmt_count: %d\n" !stmt_count;
  end 

  (** parses one C file 
      
      @param filename a .c file to parse
      @return Cil.file the parsed file
      @raise Fail("parse failure") can fail in Cil/Frontc parsing *)
  method private internal_parse (filename : string) = 
    debug "cilRep: %s: parsing\n" filename ; 
    let file = cil_parse filename in 
      debug "cilRep: %s: parsed (%g MB)\n" filename (debug_size_in_mb file); 
      file 

  (** parses and then processes one C file.  Collects all necessary data for
      semantic checking and updates the global ast information in
      [global_ast_info].

      @param filename source file
      @return Cil.file parsed/numbered/processed Cil file *)
  method private from_source_one_file (filename : string) : Cil.file =
    let full_filename = 
      if Sys.file_exists filename then filename else (Filename.concat !prefix filename)
    in
    let file = self#internal_parse full_filename in 
    let globalset = ref !global_ast_info.globalsset in 
    let globalseen = ref IntSet.empty in 
    let localshave = ref !global_ast_info.localshave in
    let globalshave = ref !global_ast_info.globalshave in 
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
                 globalseen
                 localset 
                 localshave 
                 globalshave
                 localsused 
                 stmt_count add_to_stmt_map filename
              ) file  
          | _ -> visitCilFileSameGlobals 
            (my_num stmt_count add_to_stmt_map filename) file ; 
        end ;

      let la, lb, lf = if !ignore_dead_code then begin
        debug "cilRep: computing liveness\n" ; 
        let la = Hashtbl.create 255 in
        let lb = Hashtbl.create 255 in
        let lf = ref StringSet.empty in 
        let copy_file_ast = copy file in 
        visitCilFileSameGlobals (my_sid_to_label) copy_file_ast ; 
        visitCilFileSameGlobals (my_liveness la lb lf) copy_file_ast ;
        debug "cilRep: computed liveness\n" ; 
        (Some la), (Some lb), !lf
      end else None, None, StringSet.empty 
      in 

      let jungloids =
        let get_weight =
          if !jungloid_file <> "" then begin
            let regexp = Str.regexp "[ ,\t]" in
            let valid =
              lfoldl (fun s line ->
                try
                  match Str.split regexp line with
                  | [name] -> StringMap.add name 1.0 s
                  | [name; weight] ->
                    let weight = float_of_string weight in
                    StringMap.add name weight s
                with _ ->
                  abort "ERROR: from_source_one_file: %s: malformed line\n%s\n"
                    !jungloid_file line
              ) StringMap.empty (get_lines !jungloid_file)
            in
            fun n -> try StringMap.find n valid with Not_found -> 0.0
          end else
            fun n -> if (String.sub n 0 1) <> "_" then 1.0 else 0.0
        in
        foldGlobals file (fun jungloids g ->
          match g with
          | GVarDecl({vtype = TFun(rt, Some(args), _, _); _} as vi, _)
              when (llen args) > 0 && (get_weight vi.vname) > 0.0 ->
            let weight = get_weight vi.vname in
            let arg_types = lmap (fun (_, at, _) -> at) args in
            (* only allow functions where the return type can be used for any
               argument *)
            let compatible =
              lfoldl (fun b at -> b && isCompatible rt at) true arg_types
            in
            if compatible then
              StringMap.add vi.vname (weight, vi, arg_types) jungloids
            else
              jungloids
          | _ -> jungloids
        ) !global_ast_info.jungloids
      in

        (* we increment after setting, so we're one too high: *) 
        let source_ids = ref !global_ast_info.all_source_sids in
            Hashtbl.iter (fun str i ->
              source_ids := IntSet.add i !source_ids 
            ) canonical_stmt_ht ;
          global_ast_info := {!global_ast_info with
            stmt_map = !stmt_map;
            jungloids = jungloids;
            localshave = !localshave;
            localsused = !localsused;
            globalshave = !globalshave;
            globalsset = !globalset;
            varinfo = !varmap ;
            all_source_sids = !source_ids ;
            liveness_before = la ;
            liveness_after = lb ; 
            liveness_failures = lf ; 
            };
          self#internal_post_source filename; file

  (** oracle localization is permitted on C files.
      @param filename oracle file/set of files
  *)
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
          global_ast_info := 
            {!global_ast_info with oracle_code = 
                StringMap.add fname file oracle}
      ) filelist;
      stmt_count := pred !stmt_count
  end

  (**/**)
  method internal_post_source filename = ()

  method compile source_name exe_name =
    let source_name = 
      if !use_subdirs then begin
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

  method updated () =
(*    already_signatured := None;*)
    super#updated()

  (**/**)

  (*** Getting coverage information ***)

  (** instruments one Cil file for fault localization.  Outputs it to disk.
      
      @param globinit optional: whether to force the call to globinit to appear
      in this file
      @param coverage_sourcename name of the output source code
      @param coverage_outname name of the path file
 *)
  method private instrument_one_file 
    file ?g:(globinit=false) coverage_sourcename coverage_outname = 
    let uniq_globals = 
      if !uniq_coverage then begin
        let array_typ = 
          Formatcil.cType "char[%d:siz]" [("siz",Fd (1 + !stmt_count))] 
        in
        let init_zero_info = {init = Some(makeZeroInit Cil.intType)} in
          uniq_array_va := makeGlobalVar "___coverage_array" array_typ;
          uniq_int_va := makeGlobalVar "___coverage_array_already_memset" Cil.intType;
          [GVarDecl(!uniq_array_va,!currentLoc); 
           GVarDecl(!uniq_int_va,!currentLoc); 
           GVar(!uniq_int_va,init_zero_info,!currentLoc)]
      end else []
    in
    let new_globals = 
      if not globinit then 
        lrev 
          (lfoldl
             (fun acc glob ->
               match glob with
                 GVarDecl(va,loc) -> GVarDecl({va with vstorage = Extern}, loc) :: acc
               | _ -> acc
             ) [] uniq_globals)
      else uniq_globals
    in
      file.globals <- new_globals @ file.globals ;

    let prototypes = ref StringMap.empty in
    let cov_visit = if !is_valgrind then 
          new covVisitor self prototypes coverage_outname (ref false)
        else new covVisitor self prototypes coverage_outname (ref true)
    in
      (* prepend missing prototypes for instrumentation code *)
      visitCilFile cov_visit file;
      if not !is_valgrind then begin
        file.globals <- 
          StringMap.fold (fun _ protos accum ->
            protos @ accum
          ) !prototypes file.globals;
        begin
          try
            file.globals <- toposort_globals file.globals;
          with MissingDefinition(s) ->
            debug "cilRep: toposorting failure (%s)!\n" s;
        end
      end;
      ensure_directories_exist coverage_sourcename;
      output_cil_file coverage_sourcename file

  (**/**)
  method instrument_fault_localization 
    coverage_sourcename coverage_exename coverage_outname = 
    debug "cilRep: instrumenting for fault localization\n";
    let source_dir,_,_ = split_base_subdirs_ext coverage_sourcename in 
      ignore(
        StringMap.fold
          (fun fname file globinit ->
            let file = copy file in 
              if not !use_subdirs then 
                self#instrument_one_file file ~g:true 
                  coverage_sourcename coverage_outname
              else 
                self#instrument_one_file file ~g:globinit 
                  (Filename.concat source_dir fname) coverage_outname;
              false)
          (self#get_base()) true) ;
    debug "cilRep: done instrumenting for fault localization\n"

  (*** Atomic mutations ***)

  (** Determines if a given atom can be inserted after another given
      atom and still yield a valid genome (i.e., a C program that can be
      compiled). Currently, the following checks are performed: 

      (1) Do not insert a 'break' or 'continue' into a scope with no
      enclosing while loops, because that yields an invalid C program that
      gcc will reject (fitness 0). 

      (2) Do not insert 'label_X:' into a scope that already has 'label_X:'
      because duplicate labels yield an invalid C program that gcc will
      reject.  

      (3) If --ignore-untyped-returns is set, do not insert "return value;"
      into a function with static return type void.

      (4) If --ignore-dead-code is set, do not insert "X=1" if "X" is dead
      at the given location. 

      This function is typically called internally by methods
      append_source, swap_sources and replace_sources. 
      
      @param before optional: check if it can be inserted before
      (prepended); the default is 'after' (appended)
      @param insert_after_sid the destination statement id 
      @param src_sid the source statement_id
 *)
  method can_insert ?(before=false) insert_after_sid src_sid =  
    let src_file,src_gotten_code = self#get_stmt src_sid in

    (* don't insert break/continue if no enclosing loop *) 
    (
        let has_break_continue = 
          try 
            ignore (visitCilStmt my_findbreakcontinue 
              (mkStmt src_gotten_code)) ;
            false
          with Found_BreakContinue -> true
        in 
        if has_break_continue then begin
          (* is destination inside an enclosing loop *) 
          let dst_file = self#get_file insert_after_sid in
          let loop_count = ref 0 in 
          try 
            visitCilFileSameGlobals (my_findenclosingloop insert_after_sid
              loop_count) dst_file ; 
            (* could not find! *) 
            debug "cilRep: ERROR: could not find statement %d\n" 
              insert_after_sid ; 
            true
          with _ -> !loop_count > 0 
        end else true 
    )
    && 

    (* don't insert "label_1:" into a function that already contains
       "label_1:" *) 
    (
      let src_labels = ref StringSet.empty in 
      ignore (visitCilStmt (my_label src_labels) (mkStmt src_gotten_code)) ;
      if StringSet.is_empty !src_labels then begin
        (* no labels means we won't create duplicate labels *) 
        true
      end else begin
        let fdref = ref Cil.dummyFunDec in 
        let dst_file = self#get_file insert_after_sid in
          try 
            visitCilFileSameGlobals (my_findenclosingfundec insert_after_sid
              fdref) dst_file ; 
            (* could not find! *) 
            debug "cilRep: ERROR: could not find fundec containing %d\n" 
              insert_after_sid ; 
            true
          with Found_Fundec -> begin
            let dst_fun_labels = ref StringSet.empty in
            ignore (visitCilFunction (my_label dst_fun_labels) !fdref) ;
            (* we can do this edit if they define no labels in common *) 
            StringSet.is_empty
              (StringSet.inter !src_labels !dst_fun_labels )
          end 
      end 

    ) &&

    (* --ignore-untyped-retruns: Don't append "return 3.2;" in a function
     * with type void *) 
    (if !ignore_untyped_returns then begin 
        match src_gotten_code with 
        | Return(eo,_) -> begin 
          (* find function containing 'insert_after_sid' *) 
          let fdref = ref Cil.dummyFunDec in 
          let dst_file = self#get_file insert_after_sid in
          try 
            visitCilFileSameGlobals (my_findenclosingfundec insert_after_sid
              fdref) dst_file ; 
            (* could not find! *) 
            debug "cilRep: ERROR: could not find fundec containing %d\n" 
              insert_after_sid ; 
            true
          with Found_Fundec -> begin
            match eo, (!fdref.svar.vtype) with
            | None, TFun(tau,_,_,_) when isVoidType tau -> 
              true
            | None, TFun(tau,_,_,_) -> false
            | Some(e), TFun(t2,_,_,_) -> 
              let t1 = typeOf e in
              (*
              debug "return: %s - %s\n" 
                (Pretty.sprint ~width:80 (dn_type () t1))
                (Pretty.sprint ~width:80 (dn_type () t2)) ;
                *) 
              (isScalarType t1 = isScalarType t2) &&
              (isPointerType t1 = isPointerType t2) 
            | _, _ -> false 
          end 
        end 
        | _ -> true 
    end else true) 

    && 

    (* --ignore-dead-code: don't append X=1 at L: if X is dead after L: *) 
    (match 
      if before then !global_ast_info.liveness_before 
      else !global_ast_info.liveness_after with
    | None -> true
    | Some(alive_ht) -> begin
        match src_gotten_code with 
        | Instr [ Set((Var(va),_),rhs,loc) ] -> 
        (* first, does insert_after_sid live in a fundec we failed
         * to compute liveness about? *) 
          let no_liveness_at_dest = begin
            let fdref = ref Cil.dummyFunDec in 
            let dst_file = self#get_file insert_after_sid in
            try 
              visitCilFileSameGlobals (my_findenclosingfundec insert_after_sid
                fdref) dst_file ; 
              false 
            with Found_Fundec -> 
              StringSet.mem !fdref.svar.vname !global_ast_info.liveness_failures
          end in
          if no_liveness_at_dest then 
            true 
          else begin 
            let res = if Hashtbl.mem alive_ht (insert_after_sid) then begin
              let liveset = Hashtbl.find alive_ht insert_after_sid in
              StringSet.mem va.vname liveset 
            end else false in 
            res 
          end 
        | _ -> true 
        end)


  (* Return a Set of (atom_ids,fix_weight pairs) that one could append here 
   * without violating many typing rules. *) 
  method append_sources append_after =
    let all_sids = 
      if AtomMap.is_empty !global_ast_info.all_appends then 
        !fix_localization 
      else begin 
        try AtomMap.find append_after !global_ast_info.all_appends 
        with Not_found -> [] 
      end 
    in 
    let sids = 
      if !semantic_check = "none" then all_sids
      else  
        lfilt (fun (sid,weight) ->
          in_scope_at append_after sid 
            !global_ast_info.localshave 
            !global_ast_info.globalshave 
            !global_ast_info.localsused 
        ) all_sids
    in
    let sids = 
      lfilt (fun (sid,weight) ->
        self#can_insert append_after sid) sids
    in 
      lfoldl 
        (fun retval ele -> WeightSet.add ele retval) 
        (WeightSet.empty) sids
        
  (* Return a Set of atom_ids that one could swap here without violating many
   * typing rules. In addition, if X<Y and X and Y are both valid, then we'll
   * allow the swap (X,Y) but not (Y,X).  *)
  method swap_sources append_after = 
    let all_sids = !fault_localization in
    let sids = 
      if !semantic_check = "none" then all_sids
      else 
        lfilt (fun (sid, weight) ->
          in_scope_at sid append_after 
            !global_ast_info.localshave 
            !global_ast_info.globalshave
            !global_ast_info.localsused 
          && in_scope_at append_after sid 
            !global_ast_info.localshave
            !global_ast_info.globalshave
            !global_ast_info.localsused 
        ) all_sids 
    in
    let sids = lfilt (fun (sid, weight) -> sid <> append_after) sids in
    let sids = lfilt (fun (sid, weight) -> 
      self#can_insert ~before:true sid append_after &&
      self#can_insert ~before:true append_after sid 
    ) sids in 
      lfoldl (fun retval ele -> WeightSet.add ele retval)
        (WeightSet.empty) sids

  (* Return a Set of atom_ids that one could replace here without violating many
   * typing rules. In addition, if X<Y and X and Y are both valid, then we'll
   * allow the swap (X,Y) but not (Y,X).  *)
  method replace_sources replace =
    let all_sids = !fix_localization in
    let sids = 
      if !semantic_check = "none" then all_sids
      else 
        lfilt (fun (sid, weight) ->
          in_scope_at sid replace 
            !global_ast_info.localshave 
            !global_ast_info.globalshave 
            !global_ast_info.localsused 
        ) all_sids 
    in
    let sids = lfilt (fun (sid, weight) -> sid <> replace) sids in
    let sids = lfilt (fun (sid, weight) -> self#can_insert ~before:true replace sid) sids in 

      lfoldl (fun retval ele -> WeightSet.add ele retval)
        (WeightSet.empty) sids

  (**/**)

  (** {8 Subatoms} 

      Subatoms are permitted on C files; subatoms are Expressions
  *)

  method subatoms = true 

  method get_subatoms stmt_id =
    let file = self#get_file stmt_id in
      visitCilFileSameGlobals (my_get stmt_id) file;
      let answer = !gotten_code in
      let this_stmt = mkStmt answer in
      let output = ref [] in 
      let _ = visitCilStmt (my_get_exp output) this_stmt in
        List.map (fun x -> Exp x) (List.rev !output)

  method get_subatom_sources stmt_id =
    match !global_ast_info.subatom_sources with
    | Some(sources) ->
      let get_varnames vids =
        IntSet.fold (fun vid vars ->
          let vi = IntMap.find vid !global_ast_info.varinfo in
          StringSet.add vi.vname vars
        ) vids StringSet.empty
      in
      let locals = IntMap.find stmt_id !global_ast_info.localshave in
      let globals = IntMap.find stmt_id !global_ast_info.globalshave in
      let localvars = get_varnames (IntSet.union locals globals) in
      List.fold_left (fun subatoms subatom ->
        match subatom with
        | Stmt(_), _ -> subatom :: subatoms
        | Exp(e), _ ->
          let varsused = ref IntSet.empty in
          let _ = visitCilExpr (new varrefVisitor varsused) e in
          let varsused = get_varnames !varsused in
          if StringSet.subset varsused localvars then begin
            subatom :: subatoms
          end else
            subatoms
      ) [] sources
    | None ->
      let primitive_subatoms, total =
        lfoldl (fun (subatoms, total) (atom, weight) ->
          lfoldl (fun (subatoms, total) subatom ->
            match subatom with
            | Exp(subatom) ->
              (Exp(subatom), weight) :: subatoms, weight +. total
            | _ -> failwith "cilRep#get_subatom returns atom"
          ) (subatoms, total) (self#get_subatoms atom)
        ) ([], 0.0) !fix_localization
      in
      let primitive_subatoms, total =
        let avg = total /. (float_of_int (llen primitive_subatoms)) in
        IntMap.fold (fun _ vi (subatoms, total) ->
          match vi.vtype with
          | TFun(_) | TPtr(TFun(_),_) -> subatoms, total
          | _ ->
            let noinsert =
              List.fold_left (fun noinsert (Attr(name, params)) ->
                noinsert || List.exists (fun p ->
                  match p with
                  | AStr("noinsert") | ACons("noinsert", _) -> true
                  | _ -> false
                ) params
              ) false (filterAttributes "genprog" vi.vattr)
            in
            if noinsert then
              subatoms, total
            else
              (Exp(Lval(var vi)), avg) :: subatoms, total +. avg
        ) !global_ast_info.varinfo (primitive_subatoms, total)
      in
      if !jungloid_weight > 0.0 then begin
        let jungloids =
          StringMap.fold (fun _ j js -> j :: js) !global_ast_info.jungloids []
        in
        let unops = [ Neg; BNot; LNot; ] in
        let binops =
          [ PlusA; MinusA; Mult; Div; Mod; Shiftlt; Shiftrt; BAnd; BXor; BOr ]
        in
        let ops_weight =
          (lfoldl (+.) 0.0 (lmap fst3 jungloids))
            +. (float_of_int (llen binops))
            +. (float_of_int (llen unops))
        in
        let weight =
          total *. !jungloid_weight /. ops_weight
        in
        let dummyLval = var (makeVarinfo true "@" voidType) in
        let dummyExpr = Lval(dummyLval) in
        let subatoms =
          lfoldl (fun subatoms (w,vi,ts) ->
            let args = lmap (fun _ -> dummyExpr) ts in
            let subatom =
              Stmt(Instr([Call(None, Lval(var vi), args, locUnknown)]))
            in
            (subatom, weight *. w) :: subatoms
          ) primitive_subatoms jungloids
        in
        let subatoms =
          lfoldl (fun subatoms op ->
            let ins =
              Set(dummyLval, UnOp(op, dummyExpr, intType), locUnknown)
            in
            (Stmt(Instr([ins])), weight) :: subatoms
          ) subatoms unops
        in
        let subatoms =
          lfoldl (fun subatoms op ->
            let ins =
              Set(dummyLval, BinOp(op, dummyExpr, dummyExpr, intType), locUnknown)
            in
            (Stmt(Instr([ins])), weight) :: subatoms
          ) subatoms binops
        in
        global_ast_info := {!global_ast_info with
          subatom_sources = Some(subatoms);
        }
      end else begin
        global_ast_info := {!global_ast_info with
          subatom_sources = Some(primitive_subatoms);
        }
      end;
      self#get_subatom_sources stmt_id

  method get_subatom stmt_id subatom_id = 
    let subatoms = self#get_subatoms stmt_id in
      List.nth subatoms subatom_id

  method replace_subatom_with_constant stmt_id entropy =  
    self#replace_subatom stmt_id entropy (Exp Cil.zero)

  (** {8 Templates} Templates are implemented for C files and may be loaded,
      typically from [Search]. *)

  method get_template tname = hfind registered_c_templates tname

  method load_templates template_file = 
    let _ = super#load_templates template_file in
      (try
      if !template_cache_file <> "" then begin
        let fin = open_in_bin !template_cache_file in
        let ht = Marshal.from_channel fin in
          hiter (fun k v -> hadd template_cache k v) ht;
          close_in fin
      end
      with _ -> ());
    let old_casts = !Cil.insertImplicitCasts in
      Cil.insertImplicitCasts := false;
    let file = cil_parse template_file in
      Cil.insertImplicitCasts := old_casts;
    let template_constraints_ht = hcreate 10 in
    let template_code_ht = hcreate 10 in
    let template_name = ref "" in
    let my_constraints = new collectConstraints 
      template_constraints_ht template_code_ht template_name
    in
      visitCilFileSameGlobals my_constraints file;
      hiter
        (fun template_name hole_constraints ->
          let code = hfind template_code_ht template_name in 
          let as_map : hole_info StringMap.t = 
            hfold
              (fun hole_id hole_info map ->
                StringMap.add hole_id hole_info map)
              hole_constraints (StringMap.empty)
          in
            hadd registered_c_templates
              template_name
              { template_name=template_name;
                hole_constraints=as_map;
                hole_code_ht = code})
        template_constraints_ht;
          (* FIXME: this probability is almost certainly wrong *)
      hiter (fun str _ ->  mutations := (Template_mut(str), 1.0) :: !mutations) registered_c_templates

  (* check to make sure we only overwrite hole 1 *)
  method template_available_mutations template_name location_id =
    (* Utilities for template available mutations *)
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
        (fun stmt all_set -> 
          let subatoms = 0 -- ((llen (self#get_subatoms stmt)) - 1) in
            PairSet.union (pset_of_lst stmt subatoms) all_set)
        start_set PairSet.empty
    in
    let fault_exps () = exp_set (iset_of_lst (first_nth (random_order (IntSet.elements (fault_stmts()))) 10)) in
    let fix_exps () = exp_set (iset_of_lst (first_nth (random_order (IntSet.elements (fix_stmts ()))) 10)) in
    let all_exps () = exp_set (iset_of_lst (first_nth (random_order (IntSet.elements (all_stmts()))) 10)) in
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
    let template = hfind registered_c_templates template_name in
    (* one_hole finds all satisfying assignments for a given template hole *)
    (* I've carefully constructed templates to fulfill dependencies in order *)
    let strip_code_str code =
      let as_str = Pretty.sprint ~width:80 (dn_stmt () code) in
      let split = Str.split whitespace_regexp as_str in
        lfoldl (fun x acc -> x ^ " " ^ acc) "" split
    in
    let rec internal_stmt_constraints current constraints  = 
        if IntSet.is_empty current then current
        else begin
          match constraints with 
          | Fix_path :: rest -> 
            internal_stmt_constraints (IntSet.inter (fix_stmts()) current) rest 
          | Fault_path :: rest -> 
            internal_stmt_constraints (IntSet.inter (fault_stmts()) current) rest 
          | HasVar(str) :: rest -> 
            let filtered = 
              IntSet.filter 
                (fun location ->
                  let localshave = IntMap.find location !global_ast_info.localshave in
                  let varinfo = !global_ast_info.varinfo in
                    IntSet.exists
                      (fun vid -> 
                        let va = IntMap.find vid  varinfo in
                          va.vname = str) (IntSet.union localshave !global_ast_info.globalsset))
                current 
            in 
              internal_stmt_constraints filtered rest
          | ExactMatches(str) :: rest ->
            let match_code = hfind template.hole_code_ht str in 
            let match_str = strip_code_str (mkStmt (Block(match_code))) in
            let filtered = 
              IntSet.filter
                (fun location ->
                  let loc_str = strip_code_str (mkStmt (snd (self#get_stmt location))) in
                    match_str = loc_str)
                current
            in
              internal_stmt_constraints filtered rest 
          | FuzzyMatches(str) :: rest ->
            let match_code = hfind template.hole_code_ht str in 
            let _,(_,match_tl,_) = Cdiff.stmt_to_typelabel (mkStmt (Block(match_code))) in
            let filtered = 
              IntSet.filter
                (fun location ->
                let _,(_,self_tl,_) = Cdiff.stmt_to_typelabel (mkStmt (snd (self#get_stmt location))) in
                let match_str = strip_code_str (mkStmt match_tl) in
                let loc_str = strip_code_str (mkStmt self_tl) in 
                  match_str = loc_str)
                current 
            in
              internal_stmt_constraints filtered rest
          | r1 :: rest -> internal_stmt_constraints current rest 
          | [] -> current                     
        end
    in
    let stmt_hole (hole : hole_info) (assignment : filled StringMap.t) = 
      let constraints = hole.constraints in 
      let start = 
        if ConstraintSet.mem (Fault_path) constraints then (fault_stmts())
        else if ConstraintSet.mem (Fix_path)  constraints then (fix_stmts ())
        else all_stmts ()
      in
        internal_stmt_constraints start (ConstraintSet.elements constraints)
    in
    let exp_hole (hole : hole_info) (assignment : filled StringMap.t) =
      let constraints = hole.constraints in 
      let start = 
        if ConstraintSet.mem (Fault_path) constraints then (fault_exps())
        else if ConstraintSet.mem (Fix_path)  constraints then (fix_exps ())
        else all_exps ()
      in
      let rec internal_constraints current constraints  = 
        if PairSet.is_empty current then current
        else begin
          match constraints with 
          | Fault_path :: rest -> 
            internal_constraints (PairSet.inter (fault_exps()) current) rest 
          | Fix_path :: rest -> 
            internal_constraints (PairSet.inter (fix_exps()) current) rest 
          | InScope(other_hole) :: rest ->
            let typ,in_other_hole,_ = StringMap.find other_hole assignment in 
            let filtered = 
              PairSet.filter
                (fun (sid,subatom_id) ->
                 in_scope_at in_other_hole sid 
                  !global_ast_info.localshave 
                  !global_ast_info.globalshave 
                  !global_ast_info.localsused)
                current
            in
              internal_constraints filtered rest
          | r1 :: rest -> internal_constraints current rest 
          | [] -> current                     
        end
      in
        internal_constraints start (ConstraintSet.elements constraints)
    in
    let lval_hole  (hole : hole_info) (assignment : filled StringMap.t) =
      let constraints = hole.constraints in 
      let start = 
        if ConstraintSet.exists (fun con -> match con with InScope _ -> true | _ -> false) constraints then
          let filt = ConstraintSet.filter (fun con -> match con with InScope _ -> true | _ -> false) constraints in
          let InScope(other_hole) = List.hd (ConstraintSet.elements filt) in
          let _,sid,_ = StringMap.find other_hole assignment in 
            IntSet.union !global_ast_info.globalsset
              (IntMap.find sid !global_ast_info.localshave)
        else if ConstraintSet.mem (Fault_path) constraints then (fault_lvals())
        else if ConstraintSet.mem (Fix_path)  constraints then (fix_lvals ())
        else all_lvals ()
      in
      let rec internal_constraints current constraints  = 
        if IntSet.is_empty current then current
        else begin
          match constraints with 
          | Fix_path :: rest -> 
            internal_constraints (IntSet.inter (fix_lvals()) current) rest 
          | Fault_path :: rest -> 
            internal_constraints (IntSet.inter (fault_lvals()) current) rest 
          | HasType(str) :: rest -> 
            assert(str = "int");
            let varinfo = !global_ast_info.varinfo in 
            let filtered = 
              IntSet.filter
                (fun lval -> 
                  let va = IntMap.find lval varinfo in
                    va.vtype = intType ||
                      va.vtype = uintType) current 
            in
              internal_constraints filtered rest
          | Ref(other_hole) :: rest ->
            let _,sid,_ = StringMap.find other_hole assignment in
            let localsused = IntMap.find sid !global_ast_info.localsused in 
            let filtered = 
              IntSet.filter
                (fun lval ->IntSet.mem lval localsused) current in
              internal_constraints filtered rest                    
          | r1 :: rest -> internal_constraints current rest 
          | [] -> current                     
        end
      in
        internal_constraints start (ConstraintSet.elements constraints)
    in      
    let one_hole (hole : hole_info) (assignment : filled StringMap.t) =
        match hole.htyp with
          Stmt_hole -> 
            let solutions = IntSet.elements (stmt_hole hole assignment) in
              lmap (fun id -> StringMap.add hole.hole_id (Stmt_hole,id,None)  assignment) solutions
        | Lval_hole ->
            let solutions = IntSet.elements (lval_hole hole assignment) in
              lmap (fun id -> StringMap.add hole.hole_id (Lval_hole,id,None)  assignment) solutions
        | Exp_hole ->
          let solutions = PairSet.elements (exp_hole hole assignment) in 
            lmap (fun (id,subid) -> 
              StringMap.add hole.hole_id (Exp_hole,id,(Some(subid)))  assignment)
              solutions 
    in
    let rec one_template (assignment, unassigned) =
      if (llen unassigned) == 0 then [assignment,1.0] else begin
        let name,hole_info = List.hd unassigned in
        let remaining = List.tl unassigned in 
        let assignments = one_hole hole_info assignment in
          lflatmap 
            (fun assignment -> 
              one_template (assignment,remaining)) 
            assignments 
      end
    in
      ht_find template_cache (location_id, template_name)
        (fun _ -> 
          let first_hole = StringMap.find  "__hole1__" template.hole_constraints in 
          let hole_id = first_hole.hole_id in
          let fulfills_constraints = internal_stmt_constraints (IntSet.singleton location_id) (ConstraintSet.elements first_hole.constraints) in
            if (IntSet.cardinal fulfills_constraints) > 0 then begin
              let template_constraints = 
                StringMap.remove hole_id template.hole_constraints
              in
              let template_constraints = StringMap.fold (fun k v acc -> (k,v) :: acc) template_constraints [] in
              let template_constraints = List.sort (fun (k1,_) (k2,_) -> compare k1 k2) template_constraints in
              let start = 
                StringMap.add hole_id (Stmt_hole,location_id,None) (StringMap.empty), 
                template_constraints
              in
                  one_template start
            end else [])
        
  (** {8 Structural Differencing} [min_script], [construct_rep], and
      [internal_structural_signature] are used for minimization and partially
      implement the [minimizableObject] interface.  The latter function is
      implemented concretely in the subclasses. *)

  val min_script = ref None

  method construct_rep patch script =
    let _ = 
      match patch with
        Some(patch) -> self#load_genome_from_string patch
      | None ->
        begin
          match script with
            Some(cilfile_list,node_map) ->
              min_script := Some(cilfile_list, node_map)
          | None -> 
            abort "cilrep#construct_rep called with nothing from which to construct the rep"
        end
    in
      self#updated ()

  method is_max_fitness () = Fitness.test_to_first_failure (self :> ('a, 'b) Rep.representation)


  (** implemented by subclasses.  In our case, generates a fresh version of the
      base representation and calls minimize *)
  method virtual note_success : unit -> unit
end
  
class templateReplace replacements_lval replacements_exp replacements_stmt = object
  inherit nopCilVisitor

  method vstmt stmt = 
    match stmt.skind with
      Instr([Set(lval,e,_)]) ->
        (match e with 
          Lval(Var(vinfo),_) when vinfo.vname = "___placeholder___" ->
            (match lval with
              Var(vinfo),_ -> 
                  ChangeTo(hfind replacements_stmt vinfo.vname)
            | _ -> DoChildren)
        | _ -> DoChildren)
    | _ -> DoChildren

  method vexpr exp =
    match exp with
      Lval(Var(vinfo),_) ->
        if hmem replacements_exp vinfo.vname then
          ChangeTo(hfind replacements_exp vinfo.vname)
        else DoChildren
    | _ -> DoChildren

  method vlval lval = 
    match lval with
      (Var(vinfo),o) ->
        if hmem replacements_lval vinfo.vname then begin
        ChangeTo(hfind replacements_lval vinfo.vname)
        end else DoChildren
    | _ -> DoChildren
end
(** [patchCilRep] is the default C representation.  The individual is a list of
    edits *)
class patchCilRep = object (self : 'self_type)
  inherit [cilRep_atom edit_history] cilRep
  (*** State Variables **)

  (** [get_base] for [patchCilRep] just returns the code bank from the
      [global_ast_info].  Note the difference between this behavior and the
      [astCilRep] behavior. *)
  method get_base () = !global_ast_info.code_bank

  (** {8 Genome } the [patchCilRep] genome is just the history *)

  method genome_length () = llen !history

  method set_genome g = 
    history := g;
    self#updated()

  method get_genome () = !history

  (** @param str history string, such as is printed out by fitness
      @raise Fail("unexpected history element") if the string contains something
      unexpected *)
  method load_genome_from_string str = 
    let split_repair_history = Str.split (Str.regexp " ") str in
    let repair_history =
      List.fold_left ( fun acc x ->
        let the_action = String.get x 0 in
          match the_action with
            'd' -> 
              Scanf.sscanf x "%c(%d)" (fun _ id -> (Delete(id)) :: acc)
          | 'a' -> 
            Scanf.sscanf x "%c(%d,%d)" 
              (fun _ id1 id2 -> (Append(id1,id2)) :: acc)
          | 's' -> 
            Scanf.sscanf x "%c(%d,%d)" 
              (fun _ id1 id2 -> (Swap(id1,id2)) :: acc)
          | 'r' -> 
            Scanf.sscanf x "%c(%d,%d)" 
              (fun _ id1 id2 -> (Replace(id1,id2)) :: acc)
          |  _ -> assert(false)
      ) [] split_repair_history
    in
      self#set_genome (List.rev repair_history)


  (** {8 internal_calculate_output_xform } 

      This is the heart of cilPatchRep -- to print out this variant, we print
      out the original, *but*, if we encounter a statement that has been changed
      (say, deleted), we print that statement out differently.

      This is implemented via a "transform" function that is applied,
      by the Printer, to every statement just before it is printed.
      We either replace that statement with something new (e.g., if
      we've Deleted it, we replace it with an empty block) or leave
      it unchanged. *)

  (** @return the_xform the transform to apply when printing this variant to
      disk for compilation.
  *) 
  method private internal_calculate_output_xform () = 
    (* Because the transform is called on every statement, and "no change"
     * is the common case, we want this lookup to be fast. We'll use
     * a hash table on statement ids for now -- we can go to an IntSet
     * later.. *) 
    let relevant_targets = Hashtbl.create 255 in 
    let edit_history = self#get_history () in 
    (* Go through the history and find all statements that are changed. *) 
    let rec process h = 
        match h with 
        | Conditional(_,h') -> process h' 
        | Delete(x) | Append(x,_) 
        | Replace(x,_) | Replace_Subatom(x,_,_) 
          -> Hashtbl.replace relevant_targets x true 
        | Swap(x,y) -> 
          Hashtbl.replace relevant_targets x true ;
          Hashtbl.replace relevant_targets y true ;
        | Template(tname,fillins) ->
          let template = self#get_template tname in
          let changed_holes =
            hfold (fun hole_name _ lst -> hole_name :: lst)           
              template.hole_code_ht []
          in
            liter
              (fun hole ->
                let _,stmt_id,_ = StringMap.find hole fillins in
                  Hashtbl.replace relevant_targets stmt_id true)
              changed_holes
        | Crossover(_,_) -> 
          abort "cilPatchRep: Crossover not supported\n" 
    in 
    List.iter process  edit_history ;
    let edits_remaining = 
      if !swap_bug then ref edit_history else 
        (* double each swap in the edit history, if you want the correct swap
         * behavior (as compared to the buggy ICSE 2012 behavior); this means
         * each swap is in the list twice and thus will be applied twice (once at
         * each relevant location.) *)
        ref (lflatmap
               (fun edit -> 
                 match edit with Swap(x,y) -> [edit; Swap(y,x)] 
                 | _ -> [edit]) edit_history)
    in
    (* Now we build up the actual transform function. *) 
    let the_xform fd stmt = 
      let this_id = stmt.sid in 
      (* For Append or Swap we may need to look the source up 
       * in the "code bank". *) 
      let lookup_stmt src_sid =  
        let f,statement_kind = 
          try self#get_stmt src_sid 
          with _ -> 
            (abort "cilPatchRep: %d not found in stmt_map\n" src_sid) 
        in statement_kind
      in 
      (* helper functions to simplify the code in the transform-construction fold
         below.  Taken mostly from the visitor functions that did this
         previously *)
      let swap accumulated_stmt x y =
        let what_to_swap = lookup_stmt y in 
          true, { accumulated_stmt with skind = copy what_to_swap ;
            labels = possibly_label accumulated_stmt "swap1" y ; } 
      in
      let cond_delete cond accumulated_stmt x = 
        let e1 = Lval(Var(super_mutant_global_varinfo), NoOffset) in 
        let e2 = integer cond in 
        let tau = TInt(IInt,[]) in 
        let exp = BinOp(Ne,e1,e2,tau) in 
        let b1 = { battrs = [] ; bstmts = [accumulated_stmt] } in
        let b2 = { battrs = [] ; bstmts = [] ; } in 
        let my_if = mkStmt (If(exp,b1,b2,locUnknown)) in
        true, 
        { my_if with labels = possibly_label my_if "cdel" x } 
      in 
      let cond_append cond accumulated_stmt x y = 
        let s' = { accumulated_stmt with sid = 0 } in 
        let what_to_append = lookup_stmt y in 
        let copy = 
          (visitCilStmt my_zero (mkStmt (copy what_to_append))).skind 
        in 
        let e1 = Lval(Var(super_mutant_global_varinfo), NoOffset) in 
        let e2 = integer cond in 
        let tau = TInt(IInt,[]) in 
        let exp = BinOp(Eq,e1,e2,tau) in 
        let b1 = { battrs = [] ; bstmts = [mkStmt copy] } in
        let b2 = { battrs = [] ; bstmts = [] ; } in 
        let my_if = mkStmt (If(exp,b1,b2,locUnknown)) in
        let block = {
          battrs = [] ;
          bstmts = [s' ; my_if] ; 
        } in
          true, 
          { accumulated_stmt with skind = Block(block) ; 
            labels = possibly_label accumulated_stmt "capp" y ; } 
      in 

      let delete accumulated_stmt x = 
        let block = { battrs = [] ; bstmts = [] ; } in
          true, 
          { accumulated_stmt with skind = Block block ; 
            labels = possibly_label accumulated_stmt "del" x; } 
      in
      let append accumulated_stmt x y =
        let s' = { accumulated_stmt with sid = 0 } in 
        let what_to_append = lookup_stmt y in 
        let copy = 
          (visitCilStmt my_zero (mkStmt (copy what_to_append))).skind 
        in 
        let block = {
          battrs = [] ;
          bstmts = [s' ; { s' with skind = copy } ] ; 
        } in
          true, 
          { accumulated_stmt with skind = Block(block) ; 
            labels = possibly_label accumulated_stmt "app" y ; } 
      in
      let replace accumulated_stmt x y =
        let s' = { accumulated_stmt with sid = 0 } in 
        let what_to_append = lookup_stmt y in 
        let copy = 
          (visitCilStmt my_zero (mkStmt (copy what_to_append))).skind 
        in 
        let block = {
          battrs = [] ;
          bstmts = [{ s' with skind = copy } ] ; 
        } in
          true, 
          { accumulated_stmt with skind = Block(block) ; 
            labels = possibly_label accumulated_stmt "rep" y ; } 
      in
      let replace_subatom accumulated_stmt x entropy atom =
        let rand = (new Prng.well512)#setSeed [entropy] in
        match atom with 
        | Stmt(Instr([Call(_, f, _, loc)])) ->
          let accumulated_stmt =
            match accumulated_stmt.skind with
            | Block(b) -> copy accumulated_stmt
            | _ ->
              let blk =
                mkBlock [{accumulated_stmt with sid = 0; labels = []}]
              in
              { accumulated_stmt with skind = Block blk }
          in
          let t, args, _, _ = splitFunctionType (typeOf f) in
          let arg_ts = lmap (fun (_,a,_) -> a) (argsToList args) in
          let vi = makeTempVar fd t in
          let mk_call es =
            let args = lmap2 (fun e t -> mkCast e t) es arg_ts in
            [Call(Some(var vi), f, args, loc)]
          in
          let accumulated_stmt =
            my_put_exp accumulated_stmt ~insert:(mk_call,arg_ts) rand (Lval(var vi))
          in
            true,
            { accumulated_stmt with
              labels = possibly_label accumulated_stmt "rep_subatom" x ;
            }
        | Stmt(Instr([Set(_, e, _)])) ->
          let exprs = ref [] in
          let stmt = visitCilStmt (new getExpVisitor exprs) accumulated_stmt in
          let compatible types =
            (* find expressions that can replace one of the given types *)
            lfilt (fun e' ->
              List.exists (isCompatible (typeOf e')) types
            ) !exprs
          in
          begin match e with
          | BinOp(op,_,_,_) ->
            let types =
              match op with
              | PlusA | MinusA | Mult | Div -> [ intType; TFloat(FFloat,[]) ]
              | Mod | Shiftlt | Shiftrt | BAnd | BXor | BOr -> [ intType ]
              | _ -> []
            in
            (* find an expression that can be a valid argument to the op, then
               find a second expression that is compatible with the first one
               we picked *)
            let candidates = random_order (compatible types) in
            if (llen candidates) > 0 then
              let e1 = List.hd candidates in
              let e2 = List.hd (random_order (compatible [typeOf e1])) in
              let e = BinOp(op, e1, e2, (typeOf e2)) in
              let new_stmt = my_put_exp (copy accumulated_stmt) rand e in
                true,
                { accumulated_stmt with skind = new_stmt.skind ;
                  labels = possibly_label accumulated_stmt "rep_subatom" x ;
                }
            else
              false, accumulated_stmt
          | UnOp(op,_,_) ->
            let types =
              match op with
              | Neg -> [ intType; TFloat(FFloat,[]) ]
              | _   -> [ intType ]
            in
            let candidates = random_order (compatible types) in
            if (llen candidates) > 0 then
              let e' = List.hd candidates in
              let e = UnOp(op, e', typeOf e') in
              let new_stmt = my_put_exp (copy accumulated_stmt) rand e in
              true,
                { accumulated_stmt with skind = new_stmt.skind ;
                  labels = possibly_label accumulated_stmt "rep_subatom" x ;
                }
            else
              false, accumulated_stmt
          | _ -> false, accumulated_stmt
          end
        | Stmt(x) -> failwith "cilRep#replace_atom_subatom"
        | Exp(e) ->
          let new_stmt = my_put_exp (copy accumulated_stmt) rand e in
            true, 
            { accumulated_stmt with skind = new_stmt.skind ;
              labels = possibly_label accumulated_stmt "rep_subatom" x ;
            }
      in
      let template accumulated_stmt tname fillins this_id = 
        try
          StringMap.iter
            (fun hole_name (_,id,_) ->
              if id = this_id then raise (FoundIt(hole_name)))
            fillins; 
          false, accumulated_stmt
        with FoundIt(hole_name) -> begin
          let template = self#get_template tname in
          let block = { (hfind template.hole_code_ht hole_name) with battrs = [] }in 
            let lval_replace = hcreate 10 in
            let exp_replace = hcreate 10 in
            let stmt_replace = hcreate 10 in
            let _ = 
              StringMap.iter
                (fun hole (typ,id,idopt) ->
                  match typ with 
                    Stmt_hole -> 
                      let _,atom = self#get_stmt id in
                        hadd stmt_replace hole (mkStmt atom)
                  | Exp_hole -> 
                    let exp_id = match idopt with Some(id) -> id in
                    let Exp(atom) = self#get_subatom id exp_id in
                      hadd exp_replace hole atom
                  | Lval_hole ->
                    let atom = IntMap.find id !global_ast_info.varinfo in
                      hadd lval_replace hole (Var(atom),NoOffset)
                ) fillins
            in
            let block = visitCilBlock (new templateReplace lval_replace exp_replace stmt_replace) block in
            let new_code = mkStmt (Block(block)) in
              true, { accumulated_stmt with skind = new_code.skind ; labels = possibly_label accumulated_stmt tname this_id }
        end
      in
        (* Most statements will not be in the hashtbl. *)  
        if Hashtbl.mem relevant_targets this_id then begin
          (* If the history is [e1;e2], then e1 was applied first, followed by
           * e2. So if e1 is a delete for stmt S and e2 appends S2 after S1, 
           * we should end up with the empty block with S2 appended. So, in
           * essence, we need to apply the edits "in order". *) 
          let edits, stmt =
            List.fold_left 
              (fun (edits,accumulated_stmt) this_edit -> 
                let used_this_edit, resulting_statement = 
                  match this_edit with
                  | Conditional(cond,Delete(x)) -> 
                    if x = this_id then cond_delete cond accumulated_stmt x 
                    else false, accumulated_stmt 
                  | Conditional(cond,Append(x,y)) -> 
                    if x = this_id then cond_append cond accumulated_stmt x y 
                    else false, accumulated_stmt 
                  | Conditional(c,e) -> 
                    debug "Conditional: %d %s\n" c
                      (self#history_element_to_str e) ; 
                    failwith "internal_calculate_output_xform: unhandled conditional edit"
                  | Replace_Subatom(x,entropy,atom) when x = this_id -> 
                    replace_subatom accumulated_stmt x entropy atom
                  | Swap(x,y) when x = this_id  -> swap accumulated_stmt x y
                  | Delete(x) when x = this_id -> delete accumulated_stmt x
                  | Append(x,y) when x = this_id -> append accumulated_stmt x y
                  | Replace(x,y) when x = this_id -> replace accumulated_stmt x y
                  | Template(tname,fillins) -> 
                    template accumulated_stmt tname fillins this_id
                  (* Otherwise, this edit does not apply to this statement. *) 
                  | _ -> false, accumulated_stmt
                in 
                  if used_this_edit then
                    edits, resulting_statement
                  else
                    (this_edit :: edits), resulting_statement
              ) ([],stmt) !edits_remaining 
          in
            edits_remaining := lrev edits;
            stmt
        end else stmt 
    in 
      the_xform 

  (**/**)
  (* computes the source buffers for this variant.  @return (string * string)
      list pair of filename and string source buffer corresponding to that
      file *)
  method private internal_compute_source_buffers () = 
    let make_name n = if !use_subdirs then Some(n) else None in
    let output_list = 
      match !min_script with
        Some(difflst, node_map) ->
          let new_file_map = 
            lfoldl (fun file_map (filename,diff_script) ->
              let base_file = 
                copy (StringMap.find filename !global_ast_info.code_bank) 
              in
              let mod_file = 
                Cdiff.usediff base_file node_map diff_script (copy cdiff_data_ht)
              in
                StringMap.add filename mod_file file_map)
              (self#get_base ()) difflst 
          in
            StringMap.fold
              (fun (fname:string) (cil_file:Cil.file) output_list ->
                let source_string = output_cil_file_to_string cil_file in
                  (make_name fname,source_string) :: output_list 
              ) new_file_map [] 
      | None ->
        let xform = self#internal_calculate_output_xform () in 
        let edits = self#get_history () in 
        let bxform f = 
          if !super_mutant then begin 
            let has_conditionals = ref false in
            ignore (visitCilBlock 
              (my_has_conditional_edit edits has_conditionals) f.sbody) ;
            if !has_conditionals then begin
              (* prepend ... 

              if (global_var == 0) {
                tmp = getenv("genprog") ;
                if (tmp != NULL) 
                  global_var = atoi(tmp) 
              } 

              ... to each method that cares about conditional mutations
              *) 
              let f = copy f in 
              let tvarinfo = makeTempVar f ~insert:true 
                (TPtr(TInt(IChar,[]),[])) in 

              let e1 = Lval(Var(tvarinfo), NoOffset) in 
              let e2 = zero in 
              let tau = TInt(IInt,[]) in 
              let inner_if_exp = BinOp(Ne,e1,e2,tau) in 

              let outer_call_instr = 
                Call( (Some((Var(tvarinfo),NoOffset))) , 
                      (Lval(Var(super_mutant_getenv_varinfo),NoOffset)), 
                      [ Const(CStr(!super_mutant_env_var)) ],
                      locUnknown) 
              in 

              let e1 = Lval(Var(super_mutant_global_varinfo), NoOffset) in 
              let e2 = zero in 
              let outer_if_exp = BinOp(Eq,e1,e2,tau) in 

              let inner_call_instr = 
                Call( (Some((Var(super_mutant_global_varinfo),NoOffset))) , 
                      (Lval(Var(super_mutant_atoi_varinfo),NoOffset)), 
                      [ Lval(Var(tvarinfo),NoOffset) ],
                      locUnknown) 
              in 

              let inner_if_stmt = mkStmt 
                (If(inner_if_exp, 
                    mkBlock [ mkStmt (Instr[inner_call_instr]) ],
                    mkBlock [ ], locUnknown)) in

              let inner_block = mkBlock [ 
                mkStmt (Instr[outer_call_instr]) ;
                inner_if_stmt 
              ] in
              let outer_if_stmt = mkStmt
                (If(outer_if_exp, inner_block, mkBlock [ ], locUnknown))
              in 
              let body = f.sbody in
              let body = { body with bstmts = outer_if_stmt :: body.bstmts } in 
              { f with sbody = body } 
            end else f
          end else f 
        in 

        let first_file = ref true in 
        StringMap.fold
          (fun (fname:string) (cil_file:Cil.file) output_list ->
            let cil_file = 
              if !first_file && !super_mutant then begin
                { cil_file with globals = 
                (GVar(super_mutant_global_varinfo,
                      {init=Some(SingleInit(integer 0)) },
                      locUnknown)) 
                :: 
                GVarDecl({super_mutant_getenv_varinfo with
                  vstorage = Extern},locUnknown)
                :: 
                GVarDecl({super_mutant_atoi_varinfo with
                  vstorage = Extern},locUnknown)
                :: 
                cil_file.globals } 
              end else if !super_mutant then begin
                { cil_file with globals = 
                GVarDecl({ super_mutant_global_varinfo
                  with vstorage = Extern},locUnknown) 
                :: 
                GVarDecl({super_mutant_getenv_varinfo with
                  vstorage = Extern},locUnknown)
                :: 
                GVarDecl({super_mutant_atoi_varinfo with
                  vstorage = Extern},locUnknown)
                :: cil_file.globals } 
              end else copy cil_file 
            in 
            first_file := false;
            visitCilFileSameGlobals (my_xform xform bxform) cil_file;
            let source_string = output_cil_file_to_string cil_file in
            (make_name fname,source_string) :: output_list 
          ) (self#get_base ()) [] 
    in
      assert((llen output_list) > 0);
      output_list

  method private internal_structural_signature () =
    let xform = self#internal_calculate_output_xform () in
    let bxform = fun x -> x in
    let final_list, node_map = 
      StringMap.fold
        (fun key base (final_list,node_map) ->
          let base_cpy = (copy base) in
            visitCilFile (my_xform xform bxform) base_cpy;
            let result = ref StringMap.empty in
            let node_map = 
              foldGlobals base_cpy (fun node_map g1 ->
                match g1 with
                | GFun(fd,l) -> 
                  let node_id, node_map = Cdiff.fundec_to_ast node_map fd in
                    result := StringMap.add fd.svar.vname node_id !result; 
                    node_map
                | _ -> node_map
              ) node_map in
              StringMap.add key !result final_list, node_map
        ) (self#get_base ()) (StringMap.empty, Cdiff.init_map())
    in
      { signature = final_list ; node_map = node_map}
        
  method note_success () =
    (* Diff script minimization *)
    let orig = self#copy () in
      orig#set_genome [];
      Minimization.do_minimization 
        (orig :> minimizableObjectType) 
        (self :> minimizableObjectType) 
        (self#name())

  (**/**)

end


(** astCilRep implements the original conception of Cilrep, in which an
    individual was an entire AST with a weighted path through it.  The list of
    atoms along the weighted path and their weights serves as the genome.  We
    use [patchCilRep] more often; this is kept mostly for legacy purposes.  The
    mutation functions look slightly different as compared to [patchCilRep] in
    terms of implementation (as [astCilRep] keeps a copy of the entire tree per
    variant) but the API is the same, so they don't appear in the
    documentation.  *)
class astCilRep = object(self)
  inherit [(cilRep_atom * float)] cilRep as super

  (** [astCilRep] genomes are of fixed length *)
  method variable_length = false

  (** 

      [base] for [astCilRep] is the list of actual ASTs that represents this
      variant.  Note the distinction between this and the [patchCilRep]
      behavior.  Use [self#get_base()] to access. *)

  val base = ref ((StringMap.empty) : Cil.file StringMap.t)
  method get_base () = 
    assert(not (StringMap.is_empty !base));
    !base

  (**/**)

  method from_source (filename : string) = begin
    super#from_source filename;
    base := copy !global_ast_info.code_bank;
  end   

  method deserialize ?in_channel ?global_info (filename : string) = 
    super#deserialize ?in_channel:in_channel ?global_info:global_info filename;
    base := copy !global_ast_info.code_bank

  method internal_copy () : 'self_type = {< base =  ref (copy !base) >}

  (**/**)

  (** {8 Genome } the [astCilRep] genome is a list of atom, weight pairs. *)
        

  method get_genome () = 
    lmap (fun (atom_id,w) -> self#get atom_id, w) !fault_localization

  method genome_length () = llen !fault_localization

  method set_genome lst =
    self#updated();
    (* CLG: not sure how I feel about this; the "is_empty" is a proxy for seeing
       if this is a deserialization of a cached representation, because if so,
       the fault localization hasn't been loaded yet, and once it is, it *should*
       match the base code bank that was loaded in.  This feels a little hacky,
       though, so I need to ruminate on it a bit... *)
    if not (StringMap.is_empty !base) then
      List.iter2
        (fun (atom,_) (atom_id,_) ->
          self#put atom_id atom
        ) lst !fault_localization


  (** The observed behavior here is identical to that for [patchCilRep], even
      though the implementation is different 

      @param str history string, such as is printed out by fitness
      @raise Fail("unexpected history element") if the string contains something
      unexpected
  *)
  method load_genome_from_string str = 
    let split_repair_history = Str.split (Str.regexp " ") str in
      liter ( fun x ->
        let the_action = String.get x 0 in
          match the_action with
            'd' ->
              let to_delete = 
                Scanf.sscanf x "%c(%d)" (fun _ id -> id)
              in
                self#delete to_delete
          | 'a' -> 
            let append_after,what_to_append =
              Scanf.sscanf x "%c(%d,%d)" (fun _ id1 id2 -> (id1,id2))
                
            in
              self#append append_after what_to_append
          | 's' -> 
            let swap,swap_with = 
              Scanf.sscanf x "%c(%d,%d)" (fun _ id1 id2 -> (id1,id2))
            in 
              self#swap swap swap_with
          | 'r' -> 
            let replace,replace_with =
              Scanf.sscanf x "%c(%d,%d)" (fun _ id1 id2 -> id1,id2)
            in
              self#replace replace replace_with
          |  _ -> abort "unrecognized element %s in history string\n" x
      ) split_repair_history


  (** [get] returns the statement associated with the statement id as found in
      the 'current', 'actual' variant, {b not the code bank.}  Thus it can
      return a different answer as compared to [get_stmt], which finds the
      statement associated with the ID in the code bank itself.  

      @param stmt_id id of the statement we're looking for
      @return cilRep_atom the atom associated with that id.  Always a
      statement. 
      @raise NotFound if the call to get_file failed.  This is most likely to
      happen if you should have called get_stmt (for example, because the
      statement in question is from the oracle code, not the current variant). 
  *)
  method get stmt_id = 
    let file = self#get_file stmt_id in
      visitCilFileSameGlobals (my_get stmt_id) file;
      let answer = !gotten_code in
        gotten_code := (mkEmptyStmt()).skind ;
        (Stmt answer) 


  (** [put] replaces the statement in the current variant at stmt_id with a new
      statement.  Modifies the current variant.  You almost never want to call
      this directly. 

      @param stmt_id id of the statement to replace
      @param cilRep_atom statement to replace it with 
      @raise Fail("cilRep#put of Exp subatom") if the second parameter is not a
      statement atom.  
  *)
  method put stmt_id (stmt : cilRep_atom) =
    let file = self#get_file stmt_id in 
      (match stmt with
      | Stmt(stmt) -> 
        visitCilFileSameGlobals (my_put stmt_id stmt) file;
        visitCilFileSameGlobals (new fixPutVisitor) file;
      | Exp(e) -> failwith "cilRep#put of Exp subatom" );

  (**/**)
  method private internal_compute_source_buffers () = begin
    let output_list = ref [] in 
    let make_name n = if !use_subdirs then Some(n) else None in
      StringMap.iter (fun (fname:string) (cil_file:Cil.file) ->
        let source_string = output_cil_file_to_string cil_file in
          output_list := (make_name fname,source_string) :: !output_list 
      ) (self#get_base()) ; 
      assert((llen !output_list) > 0);
      !output_list
  end

  (***********************************
   * Atomic mutations 
   ***********************************)


  method apply_template template_name fillins =
    super#apply_template template_name fillins;
    failwith "Claire hasn't yet fixed template application for astCilRep."

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

  (* Atomic Swap of two statements (atoms) *)
  method swap stmt_id1 stmt_id2 = begin
    super#swap stmt_id1 stmt_id2 ; 
	  if !swap_bug then begin
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
			visitCilFileSameGlobals my_swap (StringMap.find f2 base)
	  end
  end

  (* Atomic replace of two statements (atoms) *)
  method replace stmt_id1 stmt_id2 = begin
    let _,replace_with = self#get_stmt stmt_id2 in 
      super#replace stmt_id1 stmt_id2 ; 
      visitCilFileSameGlobals (my_rep stmt_id1 replace_with) (self#get_file stmt_id1)
  end

  method replace_subatom stmt_id entropy atom = begin
    let file = self#get_file stmt_id in
    match atom with
    | Stmt(x) -> failwith "cilRep#replace_atom_subatom" 
    | Exp(e) -> 
      visitCilFileSameGlobals (my_get stmt_id) file ;
      let answer = !gotten_code in
      let this_stmt = mkStmt answer in
      let new_stmt =
        my_put_exp this_stmt ((new Prng.well512)#setSeed [entropy]) e
      in 
        super#replace_subatom stmt_id entropy atom;
        visitCilFileSameGlobals (my_put stmt_id new_stmt.skind) file;
		visitCilFileSameGlobals (new fixPutVisitor) file
  end

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

  method note_success () =
    (* Diff script minimization *)
    let orig = self#copy () in
    let orig_genome = 
      lmap
        (fun (atom_id,w) ->
          (Stmt(snd (self#get_stmt atom_id))),w) 
        !fault_localization in
      orig#set_genome orig_genome;
      Minimization.do_minimization 
        (orig :> minimizableObjectType) 
        (self :> minimizableObjectType) 
        (self#name())

end

(* Global initialization. Moved this down here while debugging Dorn's 
 * code so that you wouldn't have to pass in a variant to get access to C
 * parsing. Since this comes after Cilrep, it can make one. *) 
let _ = 
  fill_va_table := (fun () -> 
  let vnames =
    [ "fclose"; "fflush"; "fopen"; "fprintf"; "memset"; "vgPlain_fmsg"; "_coverage_fout" ; "vgPlain_memset" ]
  in
  if Hashtbl.length va_table = 0 then begin
    let source_file, chan = Filename.open_temp_file "tmp" ".c" in
    Printf.fprintf chan "#include <stdio.h>\n";
    Printf.fprintf chan "#include <string.h>\n";
    Printf.fprintf chan "FILE * _coverage_fout;\n";
    Printf.fprintf chan "int main() { return 0; }\n"; 
    close_out chan;
    let temp_variant = (new patchCilRep) in 

    let preprocessed = Filename.temp_file "tmp" ".c" in
    debug "cilRep: preprocessing IO function signatures: %s %s\n" 
      source_file preprocessed;
    let cleanup () =
      if Sys.file_exists source_file then
        Sys.remove source_file;
      if Sys.file_exists preprocessed then
        Sys.remove preprocessed
    in
    if temp_variant#preprocess source_file preprocessed then begin
      try
        let cilfile = 
          try cil_parse preprocessed 
          with e -> 
            debug "cilrep: fill_va_table: Frontc.parse: %s\n" 
              (Printexc.to_string e) ; raise e 
        in
        if !Errormsg.hadErrors then
          Errormsg.parse_error
            "cilRep: fill_va_table: failure while preprocessing stdio header file declarations\n";
        iterGlobals cilfile (fun g ->
          match g with
          | GVarDecl(vi,_) | GVar(vi,_,_) when lmem vi.vname vnames ->
            let decls = ref [] in
            let visitor = object (self)
              inherit nopCilVisitor

              method private depend t =
                match t with
                | TComp(ci,_) -> decls := GCompTagDecl(ci,locUnknown) :: !decls
                | TEnum(ei,_) -> decls := GEnumTagDecl(ei,locUnknown) :: !decls
                | _ -> ()

              method vtype t =
                match t with
                | TNamed(_) ->
                  ChangeDoChildrenPost(unrollType t, fun t -> self#depend t; t)
                | _ ->
                  self#depend t; DoChildren
            end in
            let _ = visitCilGlobal visitor g in
            let decls = g :: !decls in
            Hashtbl.add va_table vi.vname (vi,decls,true)
          | _ -> ()
        )
      with Frontc.ParseError(msg) ->
      begin
        debug "cilRep: fill_va_table: %s\n" msg;
        Errormsg.hadErrors := false
      end
    end;
    debug "cilRep: done preprocessing IO function signatures\n";
    cleanup()
  end;
  let static_args = lfoldl (fun lst x ->
      let is_fout = x = "_coverage_fout" in
      if not (Hashtbl.mem va_table x) then begin
        let vi = makeVarinfo true x void_t in
        Hashtbl.add va_table x (vi, [GVarDecl(vi,locUnknown)], is_fout)
      end;
      let name = if is_fout then "fout" else x in
      let vi, _, _ = Hashtbl.find va_table x in
      (name, Fv vi) :: lst
    ) [("wb_arg", Fg("wb")); ("a_arg", Fg("a"));] vnames
  in
  let cstmt stmt_str args = 
    Formatcil.cStmt ("{"^stmt_str^"}") (fun _ -> failwith "no new varinfos!")  !currentLoc 
    (args@static_args)
  in
  let global_decls = lfoldl (fun decls x ->
    match Hashtbl.find va_table x with
    | (_, gs, true) -> StringMap.add x gs decls
    | _             -> decls
    ) StringMap.empty vnames
  in
  cstmt, global_decls
  ) 
