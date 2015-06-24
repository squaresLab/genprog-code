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
open Minimization

(**/**)
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

let cilRep_version = "17" 

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

(** maps atom IDs to a set of names of variables *)
type liveness_information = 
  ((atom_id, StringSet.t) Hashtbl.t) option 

(** The AST info for the original input Cil file is stored in a global variable
    of type [ast_info].  *)
type ast_info = 
    { code_bank : Cil.file StringMap.t ;
      (** program code: maps filenames to ASTs *)
      oracle_code : Cil.file StringMap.t ;
      (** additional/external code: maps filenames to ASTs *)
      stmt_map : (string * string) AtomMap.t ;
      (** maps atom IDs to the (function name, filename) in which it is found *)
      varinfo : Cil.varinfo IntMap.t ;
      (** maps variable IDs to the corresponding varinfo *)

      (* Liveness information is used for --ignore-dead-code *) 
      liveness_before : liveness_information ; 
      (** maps atom IDs to the set of names of variables live before the statement *)
      liveness_after  : liveness_information ; 
      (** maps atom IDs to the set of names of variables live after the statement *)
      liveness_failures : StringSet.t ; 
      (** set of functions for which liveness could not be computed *)

      (* all_appends is computed by --ignore-equiv-appends. If it is
       * AtomMap.empty, then the entire fix localization is valid at
       * each fault location. Otherwise, given a fault location, this
       * maps it to a list of fixes that could be appended there (picking
       * only one representative from each equivalence class, etc.). *) 
      all_appends : ((atom_id * float) list) AtomMap.t ;
      (** maps atom IDs to the fix-localization valid for append at that statement *)
    }

(** Information associated with each statement for various analyses. These are
    all grouped in a single structure to facilitate retrieval and updating when
    statements are inserted into the program. It is recommended that this
    structure be explicitly initialized (i.e., don't use [{old with field=...}])
    so that, if new fields are added, the compiler can identify locations where
    the field value should be initialized.
  *)
type stmt_info =
  {
    in_func : fundec ; (** function containing this statement *)

    (* variable scoping *)
    local_ids   : IntSet.t ; (** set of in-scope local variable IDs *)
    global_ids  : IntSet.t ; (** set of in-scope global variable IDs *)
    usedvars    : IntSet.t ; (** set of variable IDs used in statement *)

    decl_labels : StringSet.t ; (** set of declared (non-switch) labels *)
  }

(** Establishes "null" or "bottom" values for stmt_info fields. *)
let empty_stmt_info =
  {
    in_func     = dummyFunDec ;
    local_ids   = IntSet.empty ;
    global_ids  = IntSet.empty ;
    usedvars    = IntSet.empty ;
    decl_labels = StringSet.empty ;
  }

(**/**)
let empty_info () =
  { code_bank = StringMap.empty;
    oracle_code = StringMap.empty ;
    stmt_map = AtomMap.empty ;
    varinfo = IntMap.empty ;
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

let in_scope_at context_info moved_info =
  match !semantic_check with
  | "none" -> true
  | "scope" ->
    let in_scope =
      IntSet.union context_info.local_ids context_info.global_ids
    in
      IntSet.subset moved_info.usedvars in_scope 
  | _ -> failwith ("unknown semantic check '"^(!semantic_check)^"'")

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
          stmt.skind <- Instr([first]);
          stmt :: List.map (fun instr -> mkStmtOneInstr instr ) rest 
        | other -> [ stmt ] 
      ) b.bstmts in
      let stmts = List.flatten stmts in
        { b with bstmts = stmts } 
    ))
end 

(**/**)
let my_zero = new numToZeroVisitor
(**/**)

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

(** This visitor walks over the C program AST, checking the statement numbers
    it encounters. Statements are numbered in depth-first pre-order by default.
    
    Whenever it encounters a statement that does not have the expected
    statement ID (either no ID has been assigned to the statement yet or the
    statement was inserted later), it calls [unexpected] with the offending
    statement. The callback can assign a new statement ID (not necessarily
    the expected ID) which will cause the count to continue from the new ID.

    @param max_id      int ref maximum ID seen so far
    @param unexpected  stmt -> int -> string -> unit callback to handle
    unexpected statement IDs
*)
class numVisitor max_id unexpected =
  let next_id = ref (!max_id + 1) in
object
  inherit nopCilVisitor

  val mutable current_function = "???" 

  method vfunc fd =
    let prev_function = current_function in
    current_function <- fd.svar.vname ;
    ChangeDoChildrenPost(fd, fun fd -> current_function <- prev_function; fd)

  method vstmt s =
    if (abs s.sid) <> !next_id then begin
      unexpected s !next_id current_function ;
      next_id := abs s.sid
    end ;
    max_id := max !next_id !max_id;
    incr next_id;
    DoChildren
end

(** This visitor walks over the C program AST, collecting in scope variables.
    This visitor must start from the top of the AST. Otherwise, it may miss
    the declarations for in-scope variables and fail to include them in the
    results.

    @param read_info  function to get any existing info for a statement
    @param write_info function to set info for a statement *)
class scopeVisitor
  (read_info : int -> stmt_info)
  (write_info : int -> stmt_info -> unit) =
object
  inherit nopCilVisitor

  val mutable globals_seen = IntSet.empty
  val mutable locals_seen  = IntSet.empty
  val mutable current_fun  = dummyFunDec

  method vglob g =
    begin match g with
    | GVarDecl(vi,_) | GVar(vi,_,_) ->
      globals_seen <- IntSet.add vi.vid globals_seen
    | GFun(fd,_) ->
      globals_seen <- IntSet.add fd.svar.vid globals_seen
    | _ -> ()
    end ;
    DoChildren

  method vfunc fd =
    let oldfundec = current_fun in
    let oldlocals = locals_seen in
      current_fun <- fd ;
      locals_seen <- List.fold_left (fun s vi -> IntSet.add vi.vid s)
          IntSet.empty (fd.sformals @ fd.slocals) ;
      ChangeDoChildrenPost(fd, fun fd ->
        current_fun <- oldfundec;
        locals_seen <- oldlocals;
        fd
      )

  method vstmt s =
    let used = ref IntSet.empty in
      ignore (visitCilStmt (new varrefVisitor used) s) ;

      (* sanity check *)
      if not (IntSet.subset !used (IntSet.union globals_seen locals_seen)) then
      begin
        let debug_vids prefix vids =
          debug "\t%s:" prefix ;
          IntSet.iter (debug " %d") vids ;
          debug "\n"
        in
        debug "cilRep: WARNING: scopeVisitor: scope mismatch\n" ;
        debug_vids "used" !used ;
        debug_vids "globals seen" globals_seen ;
        debug_vids "locals seen" locals_seen ;
      end ;

      let old = read_info s.sid in
        write_info s.sid {
          in_func     = current_fun ;
          local_ids   = locals_seen ;
          global_ids  = globals_seen ;
          usedvars    = !used ;
          decl_labels = old.decl_labels ;
        } ;
        DoChildren
end

class labelVisitor
  (read_info : int -> stmt_info)
  (write_info : int -> stmt_info -> unit) =
object
  inherit nopCilVisitor

  val mutable lnames = StringSet.empty

  method vstmt s =
    let external_names = lnames in
    ChangeDoChildrenPost(s, fun s ->
      lnames <-
        List.fold_left (fun names -> function
          | Label(name,_,_) -> StringSet.add name names
          | _ -> names
        ) lnames s.labels ;
      let old = read_info s.sid in
        write_info s.sid {
          in_func     = old.in_func ;
          local_ids   = old.local_ids ;
          global_ids  = old.global_ids ;
          usedvars    = old.usedvars ;
          decl_labels = StringSet.diff lnames external_names ;
        } ;
        s
    )
end

(**/**)
(** my_num walks over the C program AST, numbers the nodes, and builds the
    statement map.

    @param max_id int ref maximum ID seen so far
    @param add_to_stmt_map function of type int -> (string * string) -> (),
    takes a statement id and a function and filename and should add the info to
    some sort of statement map.
    @param fname string, filename
*)
let my_num max_id add_to_stmt_map fname =
  new numVisitor max_id
    (fun s expected funname ->
      if can_repair_statement s.skind then begin
        s.sid <- expected ; 
        add_to_stmt_map expected (funname,fname) ;
        (*
        * Canonicalize this statement. We may have five statements
        * that print as "x++;" but that doesn't mean we want to count 
        * that five separate times. The copy is because we go through and
        * update the statements to add coverage information later *) 
        let rhs = (visitCilStmt my_zero (copy s)).skind in
        let stripped_stmt = { 
          labels = [] ; skind = rhs ; sid = 0; succs = [] ; preds = [] ;
        } in 
        let pretty_printed =
          try 
            Pretty.sprint ~width:80
              (Pretty.dprintf "%a" dn_stmt stripped_stmt)
          with _ -> Printf.sprintf "@%d" s.sid 
        in 
          ignore (canonical_sid pretty_printed s.sid)
      end else
        s.sid <- -expected;
    )
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
exception Found_Stmt of Cil.stmt
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
      raise (Found_Stmt s)
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

(** This visitor walks over the C program and to find the atoms at the given line
    of the given file. One line may result in multiple atoms because of the way
    that Cil preprocessing pulls apart certain statements (cf foo = arr[bar()]).
    Will return 0 when a line is a perfect match for an atom we wouldn't normally repair; 

    @param source_file string, file to look in
    @param [source_line], int line numbers to look at
*) 
class findAtomVisitor (source_file : string) (source_line : int) (found_atoms) (found_dist) = object
  inherit nopCilVisitor
  method vstmt s = 
      let this_file = !currentLoc.file in 
      let _,fname1,ext1 = split_base_subdirs_ext source_file in 
      let _,fname2,ext2 = split_base_subdirs_ext this_file in 
        if (fname1^"."^ext1) = (fname2^"."^ext2) || 
          Filename.check_suffix this_file source_file || source_file = "" then 
          begin 
            let this_line = !currentLoc.line in 
            let this_dist = abs (this_line - source_line) in 
              if this_dist = !found_dist then begin
                if s.sid > 0 then
                  found_atoms := s.sid :: !found_atoms
              end else if this_dist < !found_dist then begin
                if s.sid > 0 then begin
                  found_atoms := [s.sid] ;
                  found_dist := this_dist 
                end
              end 
          end;
    DoChildren
end 
(** This visitor walks over the C program and to find the atom at the given line
    of the given file. 

    @param source_file string, file to look in
    @param source_line, int line number to look at
*) 
exception FoundAtom of string * int

class findLineVisitor (source_atom : int) = object
  inherit nopCilVisitor
  method vstmt s = 
    if s.sid == source_atom then 
      raise (FoundAtom(!currentLoc.file,!currentLoc.line))
    else DoChildren
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
    @param first boolean ref, whether we've started counting yet (starts at
    false)
*)
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

(** This visitor puts an expression into the given Cil statement. 
      
    @param count integer, which expression in the expressions associated with
    the statement should be replaced.
    @param desired optional expression to put at that location
    @param first boolean ref, whether we've started counting yet (starts at
    false)
*)
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
    (* FIXME: shouldn't this be DoChildren ? *)
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
    let update_liveness sid liveness ht =
      let old = ht_find ht sid (fun () -> StringSet.empty) in
      let live_names =
        Usedef.VS.fold (fun vi names -> StringSet.add vi.vname names)
          liveness old
      in
        hrep ht sid live_names
    in
    match s.labels with
    | (Label(first,_,_)) :: rest ->
      if first <> "" && first.[0] = ' ' then begin
        let genprog_sid = my_int_of_string first in 
        update_liveness genprog_sid (Liveness.getPostLiveness s) la ;
        update_liveness genprog_sid (Liveness.getLiveness s) lb ;
      end ;
      DoChildren
    | _ -> DoChildren 
end 

class debugVisitor = object
  inherit nopCilVisitor
  method vstmt s = 
    if s.sid <> 0 then
      debug "stmt: %d\n" s.sid;
    DoChildren
end

(**/**)
let my_put_exp = new putExpVisitor 
let my_get = new getVisitor
let my_get_exp = new getExpVisitor 
let my_findstmt = new findStmtVisitor
let my_findenclosingloop = new findEnclosingLoopVisitor
let my_findbreakcontinue = new findBreakContinueVisitor
let my_find_atom = new findAtomVisitor
let my_find_line = new findLineVisitor
let my_del = new delVisitor 
let my_app = new appVisitor 
let my_swap = new swapVisitor 
let my_rep = new replaceVisitor
let my_put = new putVisitor
let my_liveness = new livenessVisitor
let my_sid_to_label = new sidToLabelVisitor
let my_debug = new debugVisitor
(**/**)

let debug_code_bank () =
  let files = !global_ast_info.code_bank in
    StringMap.iter (fun fname file ->
      debug "filename: %s\n" fname;
      visitCilFileSameGlobals my_debug file)
      files

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
  method output name = self#output_source name

  (**/**)

  (* JD: This value must be mutable to allow self#copy() to work. I don't know
     that it also needs to be a ref cell, so I just left it as-is. *)
  val mutable stmt_count = ref 0 

  (** Caches data computed about each statement for various analyses. This can
      safely be shared between all copies. However, it must be mutable (or a ref
      cell) to simplify deserialize. *)
  val mutable stmt_data : (int, stmt_info) Hashtbl.t = Hashtbl.create 257

  method copy () : 'self_type =
    let super_copy : 'self_type = super#copy () in 
      (* Don't create a copy of stmt_count. It should be shared between all
         instances.
      stmt_count <- ref !stmt_count;
      *)
      super_copy

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
          Marshal.to_channel fout (stmt_data) [] ;
          Marshal.to_channel fout (!global_ast_info.code_bank) [] ;
          Marshal.to_channel fout (!global_ast_info.oracle_code) [] ;
          Marshal.to_channel fout (!global_ast_info.stmt_map) [] ;
          Marshal.to_channel fout (!global_ast_info.varinfo) [];
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
          stmt_data <- Marshal.from_channel fin ;
          let code_bank = Marshal.from_channel fin in
          let oracle_code = Marshal.from_channel fin in
          let stmt_map = Marshal.from_channel fin in
          let varinfo = Marshal.from_channel fin in 
          let liveness_before = Marshal.from_channel fin in 
          let liveness_after = Marshal.from_channel fin in 
          let liveness_failures = Marshal.from_channel fin in 
          let all_appends = Marshal.from_channel fin in 
            global_ast_info :=
              { code_bank = code_bank;
                oracle_code = oracle_code;
                stmt_map = stmt_map ;
                varinfo = varinfo;
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

  method private get_fix_space_info sid =
    ht_find stmt_data sid (fun () -> empty_stmt_info)

  method private get_fault_space_info sid =
    hfind stmt_data sid

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
      let where, s = self#get_stmt atom_id in 
      let stripped_stmt = { 
        labels = [] ; skind = s.skind ; sid = 0; succs = [] ; preds = [] ;
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
        let src_where, src_stmt = 
          try self#get_stmt src_atom_id 
          with e -> 
            debug "cilRep: ERROR: --ignore-equiv-appends: src_atom_id %d not found\n" src_atom_id ; raise e 
          in 
        let src_effects = Progeq.effects_of_stmtkind files src_stmt.skind in 
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
    debug "cilRep: stmt_count = %d\n" (AtomSet.cardinal (self#get_atoms()));
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
            debug "cilRep: %s (code bank/base file; atoms [%d,%d])\n" 
              k low high 
        ) (self#get_code_bank ()) ; 
      StringMap.iter (fun k v ->
        incr file_count ; 
        let low, high = statement_range k in 
          debug "cilRep: %s (oracle file; atoms [%d,%d])\n" k low high
      ) (self#get_oracle_code ()) ; 
      debug "cilRep: %d file(s) total in representation\n" !file_count ; 

  method get_atoms () =
    AtomMap.fold
      (fun i _ atoms -> AtomSet.add i atoms)
      !global_ast_info.stmt_map AtomSet.empty

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
        with Found_Stmt(s) -> filename,s 
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
    let found_atoms = ref [] in
    let found_dist = ref max_int in
    let oracle_code = self#get_oracle_code () in 
    let _ = 
      if StringMap.mem source_file oracle_code then  
        let file = StringMap.find source_file oracle_code in  
          visitCilFileSameGlobals (my_find_atom source_file source_line found_atoms found_dist) file
      else 
        StringMap.iter (fun fname file -> 
          visitCilFileSameGlobals (my_find_atom source_file source_line found_atoms found_dist) file)
          (self#get_base ())
    in
      !found_atoms 

  method private source_line_of_atom_id id = 
    try
      List.iter 
        (fun map ->
          StringMap.iter (fun fname file ->
            visitCilFileSameGlobals (my_find_line id) file)
            map)
        [(self#get_base()); (self#get_oracle_code())];
        debug "cilrep: WARNING: cannot convert atom id %d to source file/line!\n" id; "",0
    with FoundAtom(fname,line) -> fname,line

  method get_named_globals name =
    let search_file fname cilfile accum =
      foldGlobals cilfile (fun accum g ->
        match g with
        | GType(ti,_)        when ti.tname = name -> g :: accum
        | GCompTag(ci,_)     when ci.cname = name -> g :: accum
        | GCompTagDecl(ci,_) when ci.cname = name -> g :: accum
        | GEnumTag(ei,_)     when ei.ename = name -> g :: accum
        | GEnumTagDecl(ei,_) when ei.ename = name -> g :: accum
        | GVarDecl(vi,_)     when vi.vname = name -> g :: accum
        | GVar(vi,_,_)       when vi.vname = name -> g :: accum
        | GFun(fd,_)         when fd.svar.vname = name -> g :: accum
        | _ -> accum
      ) accum
    in
    let globals = StringMap.fold search_file (self#get_oracle_code ()) [] in
    let globals = StringMap.fold search_file (self#get_base ()) globals in
    List.rev globals

  (** {8 Methods for loading and instrumenting source code} *)

  (** loads a CIL AST from a C source file or collection of C source files.
      
      @param filename string with extension .txt, .c, .i, .cu, .cu
      @raise Fail("unexpected file extension") if given anything else
  *)
  method from_source (filename : string) = begin 
    debug "cilrep: from_source: pre: stmt_count = %d\n" (AtomSet.cardinal (self#get_atoms())); 
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
      debug "cilrep: from_source: post: stmt_count: %d\n" (AtomSet.cardinal (self#get_atoms()));
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
    let varmap = ref !global_ast_info.varinfo in 
    let stmt_map = ref !global_ast_info.stmt_map in
      visitCilFileSameGlobals (new everyVisitor) file ; 
      visitCilFileSameGlobals (new emptyVisitor) file ; 
      visitCilFileSameGlobals (new varinfoVisitor varmap) file ; 
      let add_to_stmt_map x (skind,fname) = 
        stmt_map := AtomMap.add x (skind,fname) !stmt_map
      in 
        (* Second, number all statements and keep track of
         * in-scope variables information. *) 
        visitCilFileSameGlobals my_zero file;
        visitCilFileSameGlobals
          (my_num stmt_count add_to_stmt_map filename) file;
        visitCilFileSameGlobals
          (new scopeVisitor self#get_fix_space_info (hrep stmt_data)) file;
        visitCilFileSameGlobals
          (new labelVisitor self#get_fix_space_info (hrep stmt_data)) file;

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

          global_ast_info := {!global_ast_info with
            stmt_map = !stmt_map;
            varinfo = !varmap ;
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
      ) filelist
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
      super#compile source_name exe_name;

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
          Formatcil.cType "char[%d:siz]" [("siz",Fd (!stmt_count+1))] 
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
            ignore (visitCilStmt my_findbreakcontinue (mkStmt src_gotten_code.skind)) ;
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
      (* FIXME: labels in a function may change due to delete and replace *)
      let src_labels = (self#get_fix_space_info src_sid).decl_labels in
      if StringSet.is_empty src_labels then begin
        (* no labels means we won't create duplicate labels *) 
        true
      end else begin
        let fd = (self#get_fault_space_info insert_after_sid).in_func in
        let dst_fun_labels =
          List.fold_left (fun names s ->
            StringSet.union (self#get_fault_space_info s.sid).decl_labels names
          ) StringSet.empty fd.sbody.bstmts
        in
          (* we can do this edit if they define no labels in common *) 
          StringSet.is_empty
            (StringSet.inter src_labels dst_fun_labels )
      end 

    ) &&

    (* --ignore-untyped-retruns: Don't append "return 3.2;" in a function
     * with type void *) 
    (if !ignore_untyped_returns then begin 
        match src_gotten_code.skind with 
        | Return(eo,_) -> begin 
          (* find function containing 'insert_after_sid' *) 
          let fd = (self#get_fault_space_info insert_after_sid).in_func in
            match eo, (fd.svar.vtype) with
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
        | _ -> true 
    end else true) 

    && 

    (* --ignore-dead-code: don't append X=1 at L: if X is dead after L: *) 
    (match 
      if before then !global_ast_info.liveness_before 
      else !global_ast_info.liveness_after with
    | None -> true
    | Some(alive_ht) -> begin
        match src_gotten_code.skind with 
        | Instr [ Set((Var(va),_),rhs,loc) ] -> 
        (* first, does insert_after_sid live in a fundec we failed
         * to compute liveness about? *) 
          let no_liveness_at_dest =
            let fd = (self#get_fault_space_info insert_after_sid).in_func in
            StringSet.mem fd.svar.vname !global_ast_info.liveness_failures
          in
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
      let dst = self#get_fault_space_info append_after in
        lfilt (fun (sid, _) ->
          in_scope_at dst (self#get_fix_space_info sid)
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
    (* FIXME: should we prevent swapping an if-statement with one of its
       branches? *)
    let all_sids = !fault_localization in
    let sids = 
      let here = self#get_fault_space_info append_after in
        lfilt (fun (sid, _) ->
          let there = self#get_fault_space_info sid in
            in_scope_at here there && in_scope_at there here
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
      let dst = self#get_fault_space_info replace in
        lfilt (fun (sid, _) ->
          in_scope_at dst (self#get_fix_space_info sid)
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
      let first = ref true in 
      let _ = visitCilStmt (my_get_exp output first) this_stmt in
        List.map (fun x -> Exp x) !output 

  method get_subatom stmt_id subatom_id = 
    let subatoms = self#get_subatoms stmt_id in
      List.nth subatoms subatom_id

  method replace_subatom_with_constant stmt_id subatom_id =  
    self#replace_subatom stmt_id subatom_id (Exp Cil.zero)

  (** {8 Templates} Templates are implemented for C files and may be loaded,
      typically from [Search]. *)

  method get_template tname = hfind registered_c_templates tname

  method load_templates template_file = 
    if template_file <> "" then begin
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
    end;
    Lasetemplates.configure_templates ()

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
    let all_stmts () = self#get_atoms () in
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
    let lval_set get_info start_set = 
      IntSet.fold 
        (fun stmt ->
          fun all_set ->
            IntSet.union all_set (get_info stmt).local_ids
        ) start_set IntSet.empty
    in
    let fault_lvals () = lval_set (self#get_fault_space_info) (fault_stmts()) in
    let fix_lvals () = lval_set (self#get_fix_space_info) (fix_stmts ()) in
    let template = hfind registered_c_templates template_name in

    (* one_hole finds all satisfying assignments for a given template hole *)
    (* I've carefully constructed templates to fulfill dependencies in order *)
    let strip_code_str code =
      let as_str = Pretty.sprint ~width:80 (dn_stmt () code) in
      let split = Str.split whitespace_regexp as_str in
        lfoldl (fun x acc -> x ^ " " ^ acc) "" split
    in

    let rec internal_stmt_constraints current constraints  = 
      if (IntSet.is_empty current) || ((List.length constraints) == 0) then current
      else begin
        let filtered = 
        match (List.hd constraints) with 
        | Fix_path -> IntSet.inter (fix_stmts()) current
        | Fault_path -> IntSet.inter (fault_stmts()) current
        | HasVar(str) -> 
          IntSet.filter 
            (fun location ->
              let info = self#get_fault_space_info location in
              let available = IntSet.union info.local_ids info.global_ids in
              let varinfo = !global_ast_info.varinfo in
                IntSet.exists
                  (fun vid -> 
                    let va = IntMap.find vid  varinfo in
                      va.vname = str) available)
            current 
        | ExactMatches(str) ->
          let match_code = hfind template.hole_code_ht str in 
          let match_str = strip_code_str (mkStmt (Block(match_code))) in
            IntSet.filter
              (fun location ->
                let loc_str = strip_code_str (mkStmt (snd (self#get_stmt location)).skind) in
                  match_str = loc_str)
              current
        | FuzzyMatches(str) ->
          let match_code = hfind template.hole_code_ht str in 
          let _,(_,match_tl,_) = Cdiff.stmt_to_typelabel (mkStmt (Block(match_code))) in
            IntSet.filter
              (fun location ->
                let _,(_,self_tl,_) = Cdiff.stmt_to_typelabel (mkStmt (snd (self#get_stmt location)).skind) in
                let match_str = strip_code_str (mkStmt match_tl) in
                let loc_str = strip_code_str (mkStmt self_tl) in 
                  match_str = loc_str)
              current 
        | InScope  ->
          let dst = self#get_fault_space_info location_id in
          IntSet.filter
            (fun sid ->
              let src = self#get_fix_space_info sid in
                in_scope_at dst src) current
        (* | Ref of string I think Ref for statements just means "matches" or "fuzzy
           matches", if it means anything at all
        *)
        | _  -> current 
        in
          internal_stmt_constraints filtered (List.tl constraints)
      end
    in
    let stmt_hole (hole : hole_info) (assignment : filled StringMap.t) = 
      let constraints = hole.constraints in 
      let start,constraints = 
        if ConstraintSet.mem (Fault_path) constraints then (fault_stmts()),ConstraintSet.remove (Fault_path) constraints
        else if ConstraintSet.mem (Fix_path)  constraints then (fix_stmts ()),ConstraintSet.remove (Fix_path) constraints
        else (all_stmts ()), constraints
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
          | InScope :: rest ->
            let dst = self#get_fault_space_info location_id in
            let filtered = 
              PairSet.filter
                (fun (sid,_) ->
                  in_scope_at dst (self#get_fix_space_info sid))
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
      (* lval holes have an implicit "in scope" constraint with the instantiation location (hole 1, or the location passed to this function *)
      let constraints = hole.constraints in 
      let info = self#get_fault_space_info location_id in
      let start = IntSet.union info.local_ids info.global_ids in
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
            let localsused = (self#get_fix_space_info sid).usedvars in
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
          HStmt -> 
            let solutions = IntSet.elements (stmt_hole hole assignment) in
              lmap (fun id -> StringMap.add hole.hole_id (HStmt,id,None) assignment) solutions
        | HLval ->
          let solutions = IntSet.elements (lval_hole hole assignment) in
            lmap (fun id -> StringMap.add hole.hole_id (HLval,id,None)  assignment) solutions
        | HExp ->
          let solutions = PairSet.elements (exp_hole hole assignment) in 
            lmap (fun (id,subid) -> 
              StringMap.add hole.hole_id (HExp,id,(Some(subid)))  assignment)
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
                StringMap.add hole_id (HStmt,location_id,None) (StringMap.empty), 
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
  inherit [cilRep_atom edit_history] cilRep as super
  (*** State Variables **)

  (** [get_base] for [patchCilRep] just returns the code bank from the
      [global_ast_info].  Note the difference between this behavior and the
      [astCilRep] behavior. *)
  method get_base () = !global_ast_info.code_bank

  (** {8 Genome } the [patchCilRep] genome is just the history *)

  method genome_length () = llen !history

  method set_genome g = 
    history := (uniq g);
    self#updated()

  method get_genome () = !history

  (** @param str history string, such as is printed out by fitness
      @raise Fail("unexpected history element") if the string contains something
      unexpected *)
  method load_genome_from_string str = 
    let rec scan_history_element b =
      Scanf.bscanf b "%s@(" (fun action -> match action with
        | "l" -> Scanf.bscanf b "%s@)"   (fun name -> LaseTemplate(name))
        | "d" -> Scanf.bscanf b "%d)"    (fun id -> Delete(id))
        | "a" -> Scanf.bscanf b "%d,%d)" (fun dst src -> Append(dst,src))
        | "s" -> Scanf.bscanf b "%d,%d)" (fun id1 id2 -> Swap(id1,id2))
        | "r" -> Scanf.bscanf b "%d,%d)" (fun dst src -> Replace(dst,src))
        | "e" -> failwith "cannot parse subatoms"
        | "?" -> Scanf.bscanf b "%d,%r)" scan_history_element
            (fun cond hist -> Conditional(cond, hist))
        | _ ->
          (* someone decided that "regular" templates use their name instead of
             an action code, so everything else might just be a template... *)
          failwith "cannot parse templates (not enough info in string)"
      )
    in
    let split_repair_history = Str.split (Str.regexp " ") str in
    let repair_history =
      List.map
        (fun x -> Scanf.sscanf x "%r" scan_history_element (fun h -> h))
        split_repair_history
    in
      self#set_genome repair_history

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
    let applicable_templates = ref [] in
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
        | LaseTemplate(name) ->
          if not (List.mem name !applicable_templates) then
            applicable_templates := name :: !applicable_templates;
    in 
    List.iter process  edit_history ;
    (* Process the templates in reverse order so that the first template can
       just overwrite the behavior of later ones *)
    let template_changes =
      let get_fun_by_name name =
        let varinfos =
          filter_map (fun g ->
            match g with
            | GFun(fd,_) -> Some(fd.svar)
            | GVarDecl(vi,_) when isFunctionType vi.vtype -> Some(vi)
            | _ -> None
          ) (self#get_named_globals name)
        in
          match varinfos with
          | vi :: _ -> vi
          | _ -> fst3 (Hashtbl.find va_table name)
      in
        List.fold_left (fun changemap name ->
          let template_fun = StringMap.find name Lasetemplates.templates in
          let process_file _ cilfile imap =
            foldGlobals cilfile (fun imap g ->
              match g with
              | GFun(fd, _) ->
                IntMap.fold (fun i v m ->
                  Hashtbl.replace relevant_targets i true;
                  IntMap.add i v m
                ) (template_fun get_fun_by_name fd) imap 
              | _ -> imap
            ) imap
          in
          let changes =
            StringMap.fold process_file (self#get_oracle_code ()) IntMap.empty
          in
          let changes =
            StringMap.fold process_file (self#get_base ()) changes
          in
            StringMap.add name changes changemap
        ) StringMap.empty !applicable_templates
    in
    let out_of_partition =
      if !partition < 0 then []
      else
        Hashtbl.fold (fun i _ is ->
          try
            if self#is_in_partition i !partition then is else i :: is
          with Not_found -> i :: is
        ) relevant_targets []
    in
    let _ = List.iter (Hashtbl.remove relevant_targets) out_of_partition in
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

    (* For Append or Swap we may need to look the source up 
     * in the "code bank". *) 
    let lookup_stmt src_sid =  
      let f,stmt = 
        try self#get_stmt src_sid 
        with _ -> 
          (abort "cilPatchRep: %d not found in stmt_map\n" src_sid) 
      in stmt.skind
    in 
    let collect_sids s =
      let sids = ref AtomSet.empty in
      let _ = visitCilStmt (object
          inherit nopCilVisitor
          method vstmt s =
            sids := AtomSet.add (abs s.sid) !sids;
            DoChildren
        end) s
      in
        !sids
    in
    let renumber olds s =
      let _ = visitCilStmt (my_zero) s in
      let _ = visitCilStmt (my_num stmt_count (fun _ _ -> ()) "") s in
        s
    in
    let renumber' old_sid s =
      renumber {dummyStmt with skind = lookup_stmt old_sid; sid = old_sid} s
    in

    (* helper functions to simplify the code in the transform-construction fold
       below.  Taken mostly from the visitor functions that did this
       previously *)
    let swap accumulated_stmt x y =
      let what_to_swap = lookup_stmt y in 
        renumber' x { accumulated_stmt with skind = copy what_to_swap ;
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
      renumber' x { my_if with labels = possibly_label my_if "cdel" x } 
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
        renumber' x { accumulated_stmt with skind = Block(block) ; 
          labels = possibly_label accumulated_stmt "capp" y ; } 
    in 

    let delete accumulated_stmt x = 
      let block = { battrs = [] ; bstmts = [] ; } in
        renumber' x { accumulated_stmt with skind = Block block ; 
          labels = possibly_label accumulated_stmt "del" x; } 
    in
    let append accumulated_stmt x y =
      let copy = mkStmt (copy (lookup_stmt y)) in
      let block = { battrs = [] ; bstmts = [ copy ] ; } in
      let s' = renumber' x (mkStmt (Block(block))) in
        block.bstmts <- accumulated_stmt :: block.bstmts ;
        s'.labels <- possibly_label s' "app" y ;
        s'
    in
    let replace accumulated_stmt x y =
      let s' =
        renumber' x { accumulated_stmt with skind = copy (lookup_stmt y) }
      in
        s'.labels <- possibly_label s' "rep" y ;
        s'
    in
    let replace_subatom accumulated_stmt x subatom_id atom =
      match atom with 
        Stmt(x) -> failwith "cilRep#replace_atom_subatom"
      | Exp(e) ->
        let desired = Some(subatom_id, e) in
        let first = ref true in 
        let count = ref 0 in
        let new_stmt = visitCilStmt (my_put_exp count desired first)
          (copy accumulated_stmt) in
          renumber' x { accumulated_stmt with skind = new_stmt.skind ;
            labels = possibly_label accumulated_stmt "rep_subatom" x ;
          }
    in
    let template accumulated_stmt tname fillins this_id = 
      try
        StringMap.iter
          (fun hole_name (_,id,_) ->
            if id = this_id then raise (FoundIt(hole_name)))
          fillins; 
        accumulated_stmt
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
                  HStmt -> 
                    let _,atom = self#get_stmt id in
                      hadd stmt_replace hole (mkStmt atom.skind)
                | HExp -> 
                  let exp_id = match idopt with Some(id) -> id in
                  let Exp(atom) = self#get_subatom id exp_id in
                    hadd exp_replace hole atom
                | HLval ->
                  let atom = IntMap.find id !global_ast_info.varinfo in
                    hadd lval_replace hole (Var(atom),NoOffset)
              ) fillins
          in
          let block = visitCilBlock (new templateReplace lval_replace exp_replace stmt_replace) block in
          let new_code = mkStmt (Block(block)) in
            renumber' this_id { accumulated_stmt with skind = new_code.skind ; labels = possibly_label accumulated_stmt tname this_id }
      end
    in

    (* Now we build up the actual transform function. *) 
    List.map (fun this_edit fd stmt ->
      let this_id = stmt.sid in 
        (* Most statements will not be in the hashtbl. *)  
        if Hashtbl.mem relevant_targets this_id then begin
          match this_edit with
          | LaseTemplate(name) ->
            let changes = StringMap.find name template_changes in
              (try
                renumber stmt (IntMap.find this_id changes)
              with Not_found -> stmt)
          | Conditional(cond,Delete(x)) -> 
            if x = this_id then cond_delete cond stmt x 
            else stmt 
          | Conditional(cond,Append(x,y)) -> 
            if x = this_id then cond_append cond stmt x y 
            else stmt 
          | Conditional(c,e) -> 
            debug "Conditional: %d %s\n" c
              (self#history_element_to_str e) ; 
            failwith "internal_calculate_output_xform: unhandled conditional edit"
          | Replace_Subatom(x,subatom_id,atom) when x = this_id -> 
            replace_subatom stmt x subatom_id atom
          | Swap(x,y) when x = this_id  -> swap stmt x y
          | Delete(x) when x = this_id -> delete stmt x
          | Append(x,y) when x = this_id -> append stmt x y
          | Replace(x,y) when x = this_id -> replace stmt x y
          | Template(tname,fillins) -> template stmt tname fillins this_id
          (* Otherwise, this edit does not apply to this statement. *) 
          | _ -> stmt
        end else stmt
    ) !edits_remaining

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
        let xforms = self#internal_calculate_output_xform () in 
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
              end else cil_file 
            in 
            first_file := false;
            let source_string = output_cil_file_to_string
              ~xforms ~bxform cil_file in
            (make_name fname,source_string) :: output_list 
          ) (self#get_base ()) [] 
    in
      assert((llen output_list) > 0);
      output_list

  method private internal_structural_signature () =
    let xforms = self#internal_calculate_output_xform () in
    let final_list, node_map = 
      StringMap.fold
        (fun key base (final_list,node_map) ->
          let base_cpy = (copy base) in
            List.iter (fun xform ->
              visitCilFile (my_xform xform nop_bxform) base_cpy
            ) xforms;
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
      visitCilFileSameGlobals (my_app append_after what.skind) file;
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
			visitCilFileSameGlobals (my_app stmt_id1 what.skind) file;
	  end else begin
		let f1,s1 = self#get_stmt stmt_id1 in 
		let f2,s2 = self#get_stmt stmt_id2 in 
		let base = self#get_base () in
		let my_swap = my_swap stmt_id1 s1.skind stmt_id2 s2.skind in
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
      visitCilFileSameGlobals (my_rep stmt_id1 replace_with.skind) (self#get_file stmt_id1)
  end

  (* application of a named LASE template *)
  method lase_template name =
    let template_fun = StringMap.find name Lasetemplates.templates in
    let get_fun_by_name name =
      let varinfos =
        filter_map (fun g ->
          match g with
          | GFun(fd,_) -> Some(fd.svar)
          | GVarDecl(vi,_) when isFunctionType vi.vtype -> Some(vi)
          | _ -> None
        ) (self#get_named_globals name)
      in
        match varinfos with
        | vi :: _ -> vi
        | _ -> fst3 (Hashtbl.find va_table name)
    in
    let changed_stmts = Hashtbl.create 255 in
    let _ =
      StringMap.iter (fun _ cilfile ->
        iterGlobals cilfile (fun g ->
          match g with
          | GFun(fd,_) ->
            IntMap.iter (Hashtbl.add changed_stmts)
              (template_fun get_fun_by_name fd)
          | _ -> ()
        )
      ) (self#get_base ())
    in
    let xform _ stmt =
      try
        Hashtbl.find changed_stmts stmt.sid
      with Not_found ->
        stmt
    in
    StringMap.iter (fun _ cilfile ->
      visitCilFileSameGlobals (my_xform xform nop_bxform) cilfile
    ) (self#get_base ())

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
        super#replace_subatom stmt_id subatom_id atom;
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
          (Stmt((snd (self#get_stmt atom_id)).skind)),w) 
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
