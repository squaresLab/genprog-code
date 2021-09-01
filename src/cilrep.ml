(*
 *
 * Copyright (c) 2012-2018,
 *  Wes Weimer          <weimerw@umich.edu>
 *  Stephanie Forrest   <steph@asu.edu>
 *  Claire Le Goues     <clegoues@cs.cmu.edu>
 *  Eric Schulte        <eschulte@cs.unm.edu>
 *  Jeremy Lacomis      <jlacomis@cmu.edu>
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
let semantic_check = ref "exact"
let multithread_coverage = ref false
let uniq_coverage = ref false
let swap_bug = ref false
let ignore_standard_headers = ref false
let ignore_dead_code = ref false
let ignore_equiv_appends = ref false
let ignore_string_equiv_fixes = ref false
let ignore_untyped_returns = ref false

let _ =
  options := !options @
             [
               "--semantic-check", Arg.Set_string semantic_check,
               " limit CIL mutations by requiring matching variables" ;

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

(** The AST info for the original input Cil file is stored in a global variable
    of type [ast_info].  *)
type ast_info =
  { code_bank : Cil.file StringMap.t ;
    (** program code: maps filenames to ASTs *)
    oracle_code : Cil.file StringMap.t ;
    (** additional/external code: maps filenames to ASTs *)
    fault_localization : (atom_id * float) list
  }

type syntax_scope_flags = SS_break | SS_continue
module SyntaxScopeSet = Set.Make(struct
    type t = syntax_scope_flags
    let compare = compare
  end)

let string_of_syntax_scope = function
  | SS_break -> "break"
  | SS_continue -> "continue"

(** Information associated with each statement for various analyses. These are
    all grouped in a single structure to facilitate retrieval and updating when
    statements are inserted into the program.
*)
(* [Wed Jun 10 16:03:02 EDT 2015] JD confirms that the space used for all the
   Nones when liveness and equivalent-appends are not used is negligible.
   They add less than 2% on python-bug-69609-69616, one of the larger
   scenarios, relative to storing them in their own separate data structures.
*)
(* FIXME: some of these can be split into a [func_info] structure since they
   are the same for all statements in a function. *)
type stmt_info =
  {
    in_file : string ;  (** file containing this statement *)
    at_loc : location ; (** location of this statement *)
    in_func : int ;     (** VID of function containing this statement *)

    (* variable scoping *)
    local_ids   : IntSet.t ; (** set of in-scope local variable IDs *)
    global_ids  : IntSet.t ; (** set of in-scope global variable IDs *)
    usedvars    : IntSet.t ; (** set of variable IDs used in statement *)

    (* enclosing structure *)
    syntax_needed  : SyntaxScopeSet.t ;
    syntax_allowed : SyntaxScopeSet.t ;

    (* liveness analysis: None indicates analysis no performed on this stmt *)
    (* Liveness information is used for --ignore-dead-code *)
    live_before : IntSet.t option ; (** set of variables live before stmt *)
    live_after  : IntSet.t option ; (** set of variables live after stmt *)

    (* unique_appends is used for --ignore-equiv-appends *)
    unique_appends : AtomSet.t option ; (** unique appends to this stmt *)

    decl_labels : StringSet.t ; (** set of declared (non-switch) labels *)
  }

(** Establishes "null" or "bottom" values for stmt_info fields. *)
let empty_stmt_info =
  {
    in_file        = "<unknown>" ;
    at_loc         = locUnknown ;
    in_func        = -1 ;
    local_ids      = IntSet.empty ;
    global_ids     = IntSet.empty ;
    usedvars       = IntSet.empty ;
    syntax_needed  = SyntaxScopeSet.empty ;
    syntax_allowed = SyntaxScopeSet.empty ;
    live_before    = None ;
    live_after     = None ;
    unique_appends = None ;
    decl_labels    = StringSet.empty ;
  }

(**/**)
let empty_info () =
  { code_bank = StringMap.empty;
    oracle_code = StringMap.empty ;
    fault_localization = [] ;
  }
(**/**)

let global_ast_info = ref (empty_info())

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

let semantic_equiv_vars varmap available =
  match !semantic_check with
  | "none"  -> fun n -> [n]
  | "exact" -> fun n -> if IntSet.mem n available then [n] else []
  | "name"  ->
    let available' =
      IntSet.fold (fun n available' ->
          let vi = IntMap.find n varmap in
          let old =
            try StringMap.find vi.vname available' with Not_found -> []
          in
          StringMap.add vi.vname (vi.vid::old) available'
        ) available StringMap.empty
    in
    (fun n ->
       let name = (IntMap.find n varmap).vname in
       try StringMap.find name available' with Not_found -> [])
  | _ -> failwith ("unknown semantic check '"^(!semantic_check)^"'")

let check_available_vars varmap required available =
  let get_equivalents = semantic_equiv_vars varmap available in
  IntSet.for_all (fun n -> (llen (get_equivalents n)) > 0) required

let in_scope_at varmap context_info moved_info =
  let in_scope = IntSet.union context_info.local_ids context_info.global_ids in
  check_available_vars varmap moved_info.usedvars in_scope

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
  | _ -> false

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
class varinfoVisitor varsetref funsetref = object
  inherit nopCilVisitor
  method vfunc fd =
    funsetref := IntMap.add fd.svar.vid fd !funsetref ;
    DoChildren
  method vvdec va =
    varsetref := IntMap.add va.vid va !varsetref ;
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
    @param next_id     int ref ID to start numbering from
    @param unexpected  stmt -> int -> string -> unit callback to handle
    unexpected statement IDs
*)
class numVisitor max_id next_id unexpected =
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

(** This visitor walks over the C program AST and initializes the stmt_info
    data structure for each statement. This should be used when the file is
    first read in, after the statements have been numbered. *)
class infoVisitor (filename : string) (write_info : int -> stmt_info -> unit) =
  object
    inherit nopCilVisitor

    method vstmt s =
      write_info s.sid { empty_stmt_info with
                         in_file = filename;
                         at_loc  = get_stmtLoc s.skind;
                       } ;
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
        (* No sanity check when semantic_check is "none" because we expect to
           insert code with out-of-scope variables. *)
        if !semantic_check <> "none" then begin
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
      write_info s.sid { old with
                         in_func     = current_fun.svar.vid ;
                         local_ids   = locals_seen ;
                         global_ids  = globals_seen ;
                         usedvars    = !used ;
                       } ;
      DoChildren
  end

class syntaxScopeVisitor
    (read_info : int -> stmt_info)
    (write_info : int -> stmt_info -> unit) =
  object
    inherit nopCilVisitor

    val mutable allowed = SyntaxScopeSet.empty
    val mutable needed  = SyntaxScopeSet.empty

    method vfunc fd =
      allowed <- SyntaxScopeSet.empty ;
      needed  <- SyntaxScopeSet.empty ;
      DoChildren

    method vstmt s =
      let old_allowed = allowed in
      let old_needed  = needed in

      needed <- SyntaxScopeSet.empty ;
      begin match s.skind with
        | Switch _   ->
          allowed <- SyntaxScopeSet.add SS_break allowed
        | Loop _     ->
          allowed <- SyntaxScopeSet.add SS_break allowed ;
          allowed <- SyntaxScopeSet.add SS_continue allowed
        | _ -> ()
      end ;

      ChangeDoChildrenPost(s, fun s ->
          begin match s.skind with
            | Break _    -> needed <- SyntaxScopeSet.add SS_break needed
            | Continue _ -> needed <- SyntaxScopeSet.add SS_continue needed
            | Switch _ ->
              needed <- SyntaxScopeSet.remove SS_break needed
            | Loop _ ->
              needed <- SyntaxScopeSet.remove SS_break needed ;
              needed <- SyntaxScopeSet.remove SS_continue needed
            | _ -> ()
          end ;
          allowed <- old_allowed ;
          let old = read_info s.sid in
          write_info s.sid { old with
                             syntax_allowed = allowed ;
                             syntax_needed  = needed ;
                           } ;
          needed <- SyntaxScopeSet.union old_needed needed ;
          s
        )
  end

class labelVisitor
    (read_info : int -> stmt_info)
    (write_info : int -> stmt_info -> unit) =
  object
    inherit nopCilVisitor

    val mutable lnames = StringSet.empty

    method vfunc fd =
      (* reset the label names we've seen, since we can repeat the same label in
         different functions *)
      lnames <- StringSet.empty ;
      DoChildren

    method vstmt s =
      let external_names = lnames in
      ChangeDoChildrenPost(s, fun s ->
          lnames <-
            List.fold_left (fun names -> function
                | Label(name,_,_) -> StringSet.add name names
                | _ -> names
              ) lnames s.labels ;
          let old = read_info s.sid in
          write_info s.sid { old with
                             decl_labels = StringSet.diff lnames external_names ;
                           } ;
          s
        )
  end

(**/**)
let unexpected_num s expected funname =
  if can_repair_statement s.skind then begin
    s.sid <- expected ;
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
    s.sid <- -expected

(** my_num walks over the C program AST, numbering the nodes.

    @param max_id int ref maximum ID seen so far
*)
let my_num max_id = new numVisitor max_id (ref (!max_id + 1)) unexpected_num
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

exception Found_Stmt of Cil.stmt
(**/**)

(** This visitor walks over the C program and finds the [stmt] associated
    with the given statement id (living in the given function).

    @param function_name string, function name to look in
    @param desired_sid int, id of the statement we're looking for
    @raise Found_Stmt with the statement if it is located.
*)
class findStmtVisitor ?function_name desired_sid = object
  inherit nopCilVisitor
  method vfunc fd =
    match function_name with
    | Some(name) when fd.svar.vname <> name -> SkipChildren
    | _ -> DoChildren

  method vstmt s =
    if s.sid = desired_sid then begin
      raise (Found_Stmt s)
    end ; DoChildren
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

(** This visitor finds the expressions associated with the given statement ID.
    Sets the reference output to the expressions.

    @param output list reference for the output
*)
class getExpVisitor output = object
  inherit nopCilVisitor
  val mutable first = true
  method vstmt s =
    if first then begin
      first <- false ;
      ChangeDoChildrenPost(s, fun s -> output := lrev !output; s)
    end else
      SkipChildren (* stay within this statement *)
  method vexpr e =
    output := e :: !output ;
    DoChildren
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

let get_subtree_labels s =
  let loc = get_stmtLoc s.skind in

  let infos = ref [] in
  let reader _ = empty_stmt_info in
  let writer sid info = infos := (sid, info) :: !infos in
  let _ = visitCilStmt (new labelVisitor reader writer) s in
  let info = List.assoc s.sid !infos in

  let labels =
    StringSet.fold (fun txt labels ->
        Label(txt, loc, true) :: labels
      ) info.decl_labels []
  in
  List.rev labels

class labelRenameVisitor (used_labels : StringSet.t) =
  let rename_map = ref StringMap.empty in
  let rec make_unique name guess num =
    if StringMap.mem name !rename_map then
      StringMap.find name !rename_map
    else if not (StringSet.mem guess used_labels) then begin
      rename_map := StringMap.add name guess !rename_map ;
      guess
    end else
      make_unique name (Printf.sprintf "%s__%d" name num) (num+1)
  in
  let uniquify_labels labels =
    lmap (function
        | (Label(name, loc, b) as lbl) ->
          let name' = make_unique name name 0 in
          if name = name' then lbl else Label(name', loc, b)
        | lbl -> lbl
      ) labels
  in
  object
    inherit nopCilVisitor

    val mutable rename_map = StringMap.empty

    method vstmt s =
      s.labels <- uniquify_labels s.labels ;
      begin match s.skind with
        | Goto(target,_) -> (!target).labels <- uniquify_labels (!target).labels
        | _ -> ()
      end;
      DoChildren
  end

class insertionVisitor
    (update : (bool -> stmt -> stmt -> stmt) option) (do_append : bool) =
  object
    inherit nopCilVisitor

    val labels = ref StringSet.empty
    val reader = fun _ -> empty_stmt_info
    val write_to =
      fun lbls _ info -> lbls := StringSet.union !lbls info.decl_labels

    method vfunc fd =
      labels := StringSet.empty ;
      let _ = visitCilFunction (new labelVisitor reader (write_to labels)) fd in
      DoChildren

    method private prepare_to_insert_subtree old_s new_s =
      let get_stmt_labels s =
        let lbls = ref StringSet.empty in
        let _ = visitCilStmt (new labelVisitor reader (write_to lbls)) s in
        !lbls
      in
      let old_labels = get_stmt_labels old_s in
      let used_labels =
        if do_append then begin
          !labels
        end else
          StringSet.diff !labels old_labels
      in
      let new_s = visitCilStmt (new labelRenameVisitor used_labels) (copy new_s) in
      let new_s =
        match update with
        | Some(f) -> f do_append old_s new_s
        | _       -> new_s
      in
      if not do_append then begin
        let kept_labels =
          StringSet.fold (fun s lbls ->
              Label(s, get_stmtLoc old_s.skind, true) :: lbls
            ) (StringSet.diff old_labels (get_stmt_labels new_s)) []
        in
        new_s.labels <- (lrev kept_labels) @ new_s.labels
      end ;
      new_s
  end

(** Append a single statement (atom) after a given statement (atom)

    @param append_after id where the append is happening
    @param what_to_append Cil.stmtkind to be appended
*)
class appVisitor
    ?(update=None)
    (append_after : atom_id)
    (what_to_append : Cil.stmt) =
  object
    inherit insertionVisitor update true as super
    method vstmt s =
      if append_after <> s.sid then
        DoChildren
      else
        ChangeDoChildrenPost(s, fun s ->
            let s' = super#prepare_to_insert_subtree s what_to_append in
            let both = mkStmt (Block(mkBlock [s; s'])) in
            both.sid <- 0 ;
            both.labels <- possibly_label both "app" append_after ;
            both
          )
  end

(** Replace one statement with another (atoms)

    @param replace atom_id of statement to be replaced
    @param replace_with Cil.stmtkind with which it should be replaced.
*)
class replaceVisitor
    ?(update=None)
    (possible_label : string)
    (replace : atom_id)
    (replace_with : Cil.stmt) =
  object (self)
    inherit insertionVisitor update false as super

    method vstmt s =
      if replace <> s.sid then
        DoChildren
      else begin
        let s' = super#prepare_to_insert_subtree s replace_with in
        s'.labels <- possibly_label s' possible_label replace ;
        ChangeTo(s')
      end
  end

class templateReplace
    replacements_lval
    replacements_exp
    replacements_stmt =
  object inherit nopCilVisitor

    method vstmt stmt =
      match stmt.skind with
        Instr([Set(lval,e,_)]) ->
        (match e with
           Lval(Var(vinfo),_) ->
           if vinfo.vname = "___placeholder_hole___" ||
              vinfo.vname = "___placeholder_position___" then begin
             (match lval with
                Var(vinfo),_ ->
                ChangeTo(hfind replacements_stmt vinfo.vname)
              | _ -> DoChildren)
           end else DoChildren
         | _ -> DoChildren)
      | Instr(_) ->  DoChildren
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

class replaceSubatomVisitor atom_id subatom_id atom =
  object
    inherit nopCilVisitor

    val mutable this_sid = 0

    val this_subatom = ref (-1)

    method vstmt s =
      let parent_sid = this_sid in
      this_sid <- s.sid ;
      ChangeDoChildrenPost(s, fun s -> this_sid <- parent_sid ; s)

    method vexpr e =
      if this_sid = atom_id then begin
        incr this_subatom;
        if !this_subatom = subatom_id then
          ChangeTo(atom)
        else
          DoChildren
      end else
        DoChildren
  end

class laseTemplateVisitor
    ?(update=None)
    template_name
    allowed
  =
  let named_globals = ref StringMap.empty in
  let template_fun = StringMap.find template_name Lasetemplates.templates in
  let get_fun_by_name name =
    if StringMap.mem name !named_globals then
      StringMap.find name !named_globals
    else
      fst3 (Hashtbl.find va_table name)
  in
  object
    inherit nopCilVisitor

    val mutable stmts_to_change = IntMap.empty

    method vglob g =
      begin match g with
        | GVarDecl(vi, _) | GVar(vi, _, _) ->
          named_globals := StringMap.add vi.vname vi !named_globals
        | GFun(fd, _) ->
          named_globals := StringMap.add fd.svar.vname fd.svar !named_globals
        | _ -> ()
      end;
      DoChildren

    method vfunc fd =
      let old_stmts = stmts_to_change in
      stmts_to_change <- template_fun get_fun_by_name fd ;
      ChangeDoChildrenPost(fd, fun fd -> stmts_to_change <- old_stmts; fd)

    method vstmt s =
      if (allowed s.sid) && (IntMap.mem s.sid stmts_to_change) then
        let s' = copy (IntMap.find s.sid stmts_to_change) in
        let _ =
          match update with
          | Some(f) -> f false s s'
          | None    -> s'
        in
        s'.labels <- possibly_label s' template_name s.sid ;
        ChangeDoChildrenPost(s', id)
      else
        DoChildren
  end

class sidToLabelVisitor = object
  inherit nopCilVisitor
  method vstmt s =
    let new_label = Label(Printf.sprintf " %d" s.sid,locUnknown,false) in
    s.labels <- new_label :: s.labels ;
    DoChildren
end

(** This visitor computes per-statement livenes information. It computes
    the variables that are live BEFORE the statement (lb), those that are live
    AFTER the statement (la), and those functions for which there is a failure
    to compute liveness (lf). We need BEFORE and AFTER separately to handle
    insert vs. append correctly.

    Under the hood, this just uses Cil's liveness-computing library. *)
class livenessVisitor read_info write_info = object
  inherit nopCilVisitor

  val mutable failed = false

  method vfunc f =
    Cfg.clearCFGinfo f;
    ignore(Cfg.cfgFun f);
    (try
       Errormsg.hadErrors := false ;
       Liveness.computeLiveness f ;
       failed <- false ;
       DoChildren
     with e ->
       debug "cilRep: liveness failure for %s: %s\n"
         f.svar.vname (Printexc.to_string e) ;
       failed <- true ;
       DoChildren
    )

  method vstmt s =
    match s.labels with
    | (Label(first,_,_)) :: rest when first <> "" && first.[0] = ' ' ->
      let genprog_sid = my_int_of_string first in
      let before, after =
        if failed then None, None
        else
          let get_liveness sid liveness =
            Usedef.VS.fold (fun vi ids -> IntSet.add vi.vid ids)
              liveness IntSet.empty
          in
          let after = get_liveness genprog_sid (Liveness.getPostLiveness s) in
          let before = get_liveness genprog_sid (Liveness.getLiveness s) in
          Some(before), Some(after)
      in
      let old = read_info genprog_sid in
      write_info genprog_sid { old with
                               live_before    = before ;
                               live_after     = after ;
                             } ;
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
let my_get_exp = new getExpVisitor
let my_findstmt = new findStmtVisitor
let my_find_line = new findLineVisitor
let my_del to_del = new replaceVisitor "del" to_del (mkStmt (Block(mkBlock [])))
let my_app = new appVisitor
let my_rep = new replaceVisitor "rep"
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

  (** Maps variable IDs to the corresponding varinfo object *)
  val mutable varmap : varinfo IntMap.t ref = ref IntMap.empty

  (** Maps variable IDs to the corresponding fundec object *)
  val mutable fix_funmap : fundec IntMap.t ref = ref IntMap.empty

  method copy () : 'self_type =
    let super_copy : 'self_type = super#copy () in
    (* Don't create a copy of stmt_count, stmt_data, or varmap. They should
       be shared between all instances.
       stmt_count <- ref !stmt_count;
       stmt_data  <- Hashtbl.copy stmt_data;
       varmap <- ref !varmap;
       fix_funmap <- ref !fix_funmap;
    *)
    super_copy

  method private internal_debug_shared_private_size () =
    let get_size = debug_size_in_mb in

    (* Need to serialize all of the shared rep data at once to avoid
       double-counting objects that are referenced in multiple data structures.
       The private data can be computed by subtraction, since the
       double-counted objects belong to the shared state anyway. *)

    let shared_data = stmt_count, stmt_data, varmap, fix_funmap in
    let self_size = get_size self in
    let shared_size = get_size shared_data in
    let common_rep_size = get_size (global_ast_info, shared_data) in

    debug "cilRep: shared size: %g MB\n" common_rep_size;
    debug "cilRep: private size: %g MB\n"
      (self_size -. shared_size);

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
      Marshal.to_channel fout (!varmap) [] ;
      Marshal.to_channel fout (!fix_funmap) [] ;
      Marshal.to_channel fout (!global_ast_info.code_bank) [] ;
      Marshal.to_channel fout (!global_ast_info.oracle_code) [] ;
      Marshal.to_channel fout (!global_ast_info.fault_localization) [] ;
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
      let _ = varmap := Marshal.from_channel fin in
      let _ = fix_funmap := Marshal.from_channel fin in
      let code_bank = Marshal.from_channel fin in
      let oracle_code = Marshal.from_channel fin in
      let localization = Marshal.from_channel fin in
      global_ast_info :=
        { code_bank = code_bank;
          oracle_code = oracle_code;
          fault_localization = localization;
        }
    end;
    self#set_genome (Marshal.from_channel fin);
    super#deserialize ~in_channel:fin ?global_info:global_info filename ;
    debug "cilRep: %s: loaded\n" filename ;
    if in_channel = None then close_in fin ;
  end

  (**/**)

  method private get_source_files () =
    let info = !global_ast_info in
    StringMap.fold StringMap.add info.code_bank info.oracle_code

  method virtual private get_current_files : unit -> Cil.file StringMap.t

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
      StringMap.iter (fun _ file ->
          visitCilFileSameGlobals (my_cannotrepair cannot_repair) file ;
        ) (self#get_source_files ()) ;
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
            if w' = weight then
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
      if !do_nested then begin
        debug "WARNING: --ignore-equiv-appends is not currently compatible " ;
        debug "with --do-nested. Nested mutations will not be able to " ;
        debug "ignore equivalent appends.\n"
      end ;
      let all_fix_ids =
        List.fold_left (fun ids (id,_) -> AtomSet.add id ids)
          AtomSet.empty !fix_localization
      in
      let all_appends = Hashtbl.create 257 in
      try
        let files = self#get_current_files () in
        AtomSet.iter (fun src_atom_id ->
            let _, src_stmt = self#get_stmt src_atom_id in
            let src_effects = Progeq.effects_of_stmt files src_stmt in
            let parts =
              try
                Stats2.time "progeq partition" (fun () ->
                    Progeq.partition files src_effects
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
              let old_appends =
                ht_find all_appends dst_atom_id (fun () -> AtomSet.empty)
              in
              hrep all_appends dst_atom_id (AtomSet.add src_atom_id old_appends)
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
          ) all_fix_ids ;

        Hashtbl.iter (fun dst_atom_id valid ->
            let info = self#get_fault_space_info dst_atom_id in
            hrep stmt_data dst_atom_id { info with unique_appends = Some(valid) }
          ) all_appends ;

      with e ->
        debug "cilRep: ERROR: --ignore-equiv-appends: %s\n"
          (Printexc.to_string e)

    end ;
    self#internal_debug_shared_private_size ()

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
      AtomSet.fold (fun id (low,high) ->
          let info = self#get_fault_space_info id in
          if info.in_file = filename then
            (min low id),(max high id)
          else (low,high)
        ) (self#get_atoms ()) (max_int,min_int) in
    StringMap.iter
      (fun k v -> incr file_count ;
        let low, high = statement_range k in
        debug "cilRep: %s (code bank/base file; atoms [%d,%d])\n"
          k low high
      ) (!global_ast_info.code_bank) ;
    StringMap.iter (fun k v ->
        incr file_count ;
        let low, high = statement_range k in
        debug "cilRep: %s (oracle file; atoms [%d,%d])\n" k low high
      ) (!global_ast_info.oracle_code) ;
    debug "cilRep: %d file(s) total in representation\n" !file_count ;

  method get_atoms () =
    Hashtbl.fold (fun i _ atoms ->
        (* JD would like to get rid of the notion of "repairable" and
           "unrepairable" statements, but is instead just implementing this
           hack for now. *)
        if i > 0 then AtomSet.add i atoms else atoms
      ) stmt_data AtomSet.empty

  (**/**)

  (**

      {8 Methods that access to statements, files, code, etc, in both the base
      representation and the code bank} *)

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
    try
      let info = hfind stmt_data stmt_id in
      let fd = IntMap.find info.in_func !fix_funmap in
      let funname, filename = fd.svar.vname, info.in_file in
      begin try
          let _ = visitCilFunction (my_findstmt stmt_id) fd in
          abort "cilrep: cannot find stmt id %d in code bank\n%s %s\n%s %s\n"
            stmt_id funname "(function)" filename "(file)"
        with Found_Stmt(s) ->
          filename, s
      end
    with Not_found ->
      abort "cilrep: cannot find stmt id %d in code bank\n" stmt_id

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
    try
      let info = self#get_fault_space_info stmt_id in
      let file = StringMap.find info.in_file (self#get_current_files()) in

      let _ = visitCilFileSameGlobals (my_findstmt stmt_id) file in

      (* It is safe to use the fix_funmap to get the function name here, since
         GenProg does not modify the function names. *)
      let fd = IntMap.find info.in_func !fix_funmap in
      abort "cilrep: cannot find stmt id %d in program\n\t%s %s\n\t%s %s\n"
        stmt_id fd.svar.vname "(function)" info.in_file "(file)"
    with Found_Stmt answer ->
      answer
       | Not_found ->
         abort "cilrep: cannot find stmt id %d in program\n" stmt_id

  (** gets the file ast from the {b base representation} This function can fail
      if the statement is not in the statement map or the id is otherwise not
      valid.

      @param stmt_id statement we're looking for
      @return Cil.file file the statement is in
      @raise Not_found(filename) if either the id or the file name associated
      with it are not vaild.
  *)
  method get_file (stmt_id : atom_id) : Cil.file =
    let fname = (self#get_fix_space_info stmt_id).in_file in
    StringMap.find fname (self#get_current_files())

  (** gets the id of the atom at a given location in a file.  Will print a
      warning and return -1 if the there is no atom found at that location

      @param source_file string filename
      @param source_line int line number
      @return atom_id of the atom at the line/file combination *)
  method atom_id_of_source_line source_file source_line =
    let match_file =
      if source_file = "" then
        fun _ -> true
      else
        let basename = Filename.basename source_file in
        fun fname ->
          (basename = (Filename.basename fname)) ||
          (Filename.check_suffix fname source_file)
    in
    let _, sids =
      Hashtbl.fold (fun sid info (dist,sids) ->
          let this_dist = abs (info.at_loc.line - source_line) in
          if (sid > 0) && (match_file info.at_loc.file) then begin
            if this_dist = dist then
              (this_dist, sid :: sids)
            else if this_dist < dist then
              (this_dist, [sid])
            else
              (dist, sids)
          end else
            (dist, sids)
        ) stmt_data (max_int, [])
    in
    sids

  method private source_line_of_atom_id id =
    let info = self#get_fix_space_info id in
    info.at_loc.file, info.at_loc.line

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

    self#internal_debug_shared_private_size ()
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

  (** visits an AST and collects information about the statements for use in
      various analyses *)
  method private internal_collect_stmt_info
      (file : Cil.file)
      (reader : int -> stmt_info)
      (writer : int -> stmt_info -> unit) : unit =
    visitCilFileSameGlobals (new scopeVisitor reader writer) file;
    visitCilFileSameGlobals (new syntaxScopeVisitor reader writer) file;
    visitCilFileSameGlobals (new labelVisitor reader writer) file;

    if !ignore_dead_code then begin
      debug "cilRep: computing liveness\n" ;
      let copy_file_ast = copy file in
      visitCilFileSameGlobals (my_sid_to_label) copy_file_ast ;
      visitCilFileSameGlobals (my_liveness reader writer) copy_file_ast ;
      debug "cilRep: computed liveness\n" ;
    end

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
    visitCilFileSameGlobals (new everyVisitor) file ;
    visitCilFileSameGlobals (new emptyVisitor) file ;
    visitCilFileSameGlobals (new varinfoVisitor varmap fix_funmap) file ;
    (* Second, number all statements and keep track of
     * in-scope variables information. *)
    visitCilFileSameGlobals my_zero file;
    visitCilFileSameGlobals (my_num stmt_count) file;

    let reader = self#get_fix_space_info in
    let writer = hrep stmt_data in
    visitCilFileSameGlobals (new infoVisitor filename writer) file;

    self#internal_collect_stmt_info file reader writer;

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

  method compute_localization () =
    super#compute_localization ();
    global_ast_info := {!global_ast_info with
                        fault_localization = !fault_localization}

  (**/**)
  method internal_post_source filename = ()

  method compile source_name exe_name =
    let source_name =
      if !use_subdirs then begin
        let source_dir,_,_ = split_base_subdirs_ext source_name in
        StringMap.fold
          (fun fname _ source_name ->
             let fname' = Filename.concat source_dir fname in
             fname'^" "^source_name
          ) (self#get_current_files ()) ""
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
        (self#get_current_files()) true) ;
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
  method can_insert
      ?(before=false)
      ?(fault_src=false)
      insert_after_sid
      src_sid =
    let src_info, src_gotten_code =
      if fault_src then
        self#get_fault_space_info src_sid, self#get src_sid
      else
        self#get_fix_space_info src_sid, snd (self#get_stmt src_sid)
    in

    let dst_info = self#get_fault_space_info insert_after_sid in

    (* don't insert break/continue if no enclosing loop *)
    (
      let needed  = src_info.syntax_needed in
      let allowed = dst_info.syntax_allowed in
      SyntaxScopeSet.subset needed allowed
    )
    &&

    (* --ignore-untyped-returns: Don't append "return 3.2;" in a function
     * with type void *)
    (if !ignore_untyped_returns then begin
        match src_gotten_code.skind with
        | Return(eo,_) -> begin
            (* find function containing 'insert_after_sid' *)
            (* It is safe to use the fix_funmap to get the return type here,
               since GenProg does not modify function signatures. *)
            let fd = IntMap.find dst_info.in_func !fix_funmap in
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
    ( let liveness =
        if before then dst_info.live_before else dst_info.live_after
      in
      match liveness, src_gotten_code.skind with
      | Some(liveness), Instr[ Set((Var(va),_),rhs,loc) ] ->
        check_available_vars !varmap (IntSet.singleton va.vid) liveness
      | _ -> true)

  (* Return a Set of (atom_ids,fix_weight pairs) that one could append here
   * without violating many typing rules. *)
  method append_sources append_after =
    let dst = self#get_fault_space_info append_after in
    let all_sids =
      match dst.unique_appends with
      | None -> !fix_localization
      | Some(valid) ->
        lfilt (fun (n,_) -> AtomSet.mem n valid) !fix_localization
    in
    let sids =
      lfilt (fun (sid, _) ->
          in_scope_at !varmap dst (self#get_fix_space_info sid)
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
          in_scope_at !varmap here there && in_scope_at !varmap there here
        ) all_sids
    in
    let sids = lfilt (fun (sid, weight) -> sid <> append_after) sids in
    let sids = lfilt (fun (sid, weight) ->
        self#can_insert ~before:true ~fault_src:true sid append_after &&
        self#can_insert ~before:true ~fault_src:true append_after sid
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
          in_scope_at !varmap dst (self#get_fix_space_info sid)
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

  method get_subatoms ~fault_src stmt_id =
    let stmt =
      if fault_src then self#get stmt_id else snd (self#get_stmt stmt_id)
    in
    let output = ref [] in
    let _ = visitCilStmt (my_get_exp output) stmt in
    List.map (fun x -> Exp x) !output

  method get_subatom ~fault_src stmt_id atom_id =
    if fault_src then
      abort "You really have to call get_subatom with fault_src being false\n";
    let subatoms = self#get_subatoms ~fault_src stmt_id in
    List.nth subatoms  atom_id

  method replace_subatom_with_constant stmt_id subatom_id =
    self#replace_subatom stmt_id subatom_id (Exp Cil.zero)

  (** {8 Templates} Templates are implemented for C files and may be loaded,
      typically from [Search]. *)

  method get_template tname = hfind registered_c_templates tname

  method load_templates template_file =
    if template_file <> "" then begin
      let _ = super#load_templates template_file in
      (try
         (* fixme: write out template cache at some point *)
         if !template_cache_file <> "" then begin
           let fin = open_in_bin !template_cache_file in
           let ht = Marshal.from_channel fin in
           hiter (fun k v -> hadd template_cache k v) ht;
           close_in fin
         end
       with _ -> ());
      let file =
        let old_casts = !Cil.insertImplicitCasts in
        Cil.insertImplicitCasts := false;
        let f = cil_parse template_file in
        Cil.insertImplicitCasts := old_casts;
        f
      in
      visitCilFileSameGlobals (new everyVisitor) file ;
      let templateList = ref [] in
      let my_constraints = new collectTemplates templateList in
      visitCilFileSameGlobals my_constraints file;
      liter
        (fun template ->
           hadd registered_c_templates
             template.template_name template) !templateList;
      (* FIXME: this weight is almost certainly wrong *)
      hiter (fun str _ ->  mutations := (Template_mut(str), 1.0) :: !mutations) registered_c_templates;
    end;
    Lasetemplates.configure_templates ()

  method template_available_mutations template_name position =
    let template = hfind registered_c_templates template_name in

    (* Utilities *)
    let get_exp_from_subatom = function
      | Exp e -> e
      | _ -> abort "cilRep: template_available_mutations: non-exp subatom value"
    in

    let iset_of_lst lst =
      lfoldl (fun set item -> IntSet.add item set) IntSet.empty lst
    in
    let fix_stmts () =  iset_of_lst (lmap fst (self#get_fix_source_atoms())) in

    (* utilities *)
    let expr_in_scope position e =
      let pos_info = self#get_fault_space_info position in
      let all_vars = IntSet.union pos_info.local_ids pos_info.global_ids in
      let rec internal_match = function
        | Const _ | SizeOf _ | SizeOfStr _ | AlignOf _ -> true
        | AlignOfE(e) | UnOp(_,e,_) | CastE(_,e) | SizeOfE(e) -> internal_match e
        | BinOp(_,e1,e2,_) -> (internal_match e1) && (internal_match e2)
        | Question(e1,e2,e3,_) ->
          (internal_match e1) && (internal_match e2) && (internal_match e3)
        | Lval(h,o) | AddrOf(h,o) | StartOf(h,o) ->
          begin
            let host = function
              | Var(v) -> IntSet.mem v.vid all_vars
              | Mem(e) -> internal_match e
            in
            let rec offset = function
              | NoOffset -> true
              | Field(_,o) -> offset o
              | Index(e,o) -> (internal_match e) && (offset o)
            in
            (host h) && (offset o)
          end
        | _ -> true (* truly obscure C structures; if you want them, you can deal with them *)
      in
      internal_match e
    in
    let rec get_exp_vars = function
      | Const _ | SizeOf _ | SizeOfStr _ | AlignOf _ -> IntSet.empty
      | SizeOfE(e) | AlignOfE(e) | UnOp(_,e,_) | CastE(_,e)  -> get_exp_vars e
      | BinOp(_,e1,e2,_) -> IntSet.union (get_exp_vars e1) (get_exp_vars e2)
      | Question(e1,e2,e3,_) ->
        IntSet.union (get_exp_vars e1)
          (IntSet.union (get_exp_vars e2)
             (get_exp_vars e3))
      | Lval(h,o) | AddrOf(h,o) | StartOf(h,o) ->
        let host = function
          | Var(v) -> IntSet.singleton v.vid
          | Mem(e) -> get_exp_vars e
        in
        let rec offset = function
          | NoOffset -> IntSet.empty
          | Field(_,o) -> offset o
          | Index(e,o) -> IntSet.union (get_exp_vars e) (offset o)
        in
        (IntSet.union (host h) (offset o))
      | _ -> IntSet.empty (* truly obscure C structures; if you want them, you can deal with them *)
    in
    let get_exp_refs other_hole assignment =
      let ht,sid,sb = StringMap.find other_hole assignment in
      match ht with
        HStmt ->
        (* possible fixme: the assumption that fault_src is false here is not necessarily accurate, but it really shouldn't matter in practice *)
        let subatoms = 0 -- ((llen (self#get_subatoms ~fault_src:false sid)) - 1) in
        let candidates = List.fold_left (fun pairset subatom_id ->
            PairSet.add (sid,subatom_id) pairset) PairSet.empty subatoms
        in
        candidates
      | HLval | HExp -> debug "WARNING: Claire hasn't figured out yet how to implement the Ref constraint for expression holes when Ref refers to an Lval or Expression hole.  If you REALLY need it, ask her to think harder about it.  This code will do weird things otherwise.";
        PairSet.empty
    in
    (* one_hole finds all satisfying assignments for a given template hole *)
    (* I've carefully constructed templates to fulfill dependencies in order *)
    (* fixme: check the sorting to make sure that's true *)
    let process_exp_constraints (position : atom_id) assignment  =
      List.fold_left
        (fun current constrnt ->
           match constrnt with
           | Ref(other_hole) ->
             let other_refs = get_exp_refs other_hole assignment in
             PairSet.inter other_refs current
           | HasType(name) ->
             (* fixme: see my note on hasType for lvals for the fragility of this
                constraint, which must be tested *)
             PairSet.filter
               (fun (stmt_id,subatom_id) ->
                  let all_subatoms = self#get_subatoms ~fault_src:false stmt_id in
                  let candidate_exp = get_exp_from_subatom (List.nth all_subatoms subatom_id) in
                  let candidate_exp = Cil.stripCasts candidate_exp in
                  let candidate_typ = Cil.typeOf candidate_exp in
                  let typ_as_str = Pretty.sprint ~width:80 (printType defaultCilPrinter () candidate_typ) in
                  (* fixme: check this comparison *)
                  typ_as_str = name)
               current
           | HasVar(name) ->
             PairSet.filter
               (fun (stmt_id,subatom_id) ->
                  let all_subatoms = self#get_subatoms ~fault_src:false stmt_id in
                  let candidate_exp = get_exp_from_subatom (List.nth all_subatoms subatom_id) in
                  let candidate_vars = get_exp_vars candidate_exp in
                  IntSet.exists (fun vid ->
                      let varinfo = IntMap.find vid !varmap in
                      (* fixme: check this string comparison *)
                      varinfo.vname = name) candidate_vars)
               current
           | _ ->
             (* IsLocal IsGlobal *)
             debug "WARNING: encountered an exp constraint that either makes no sense or Claire hasn't settled on semantics for, skipping.";
             current)
    in
    let process_lval_constraints (position : atom_id) assignment =
      (* it might make sense to short-circuit when the current viable set goes
         empty, but I've decided the efficiency/readability benefits of the
         fold outweigh the potential gains of that short-circuitry, given that
         the number of constraints on any particular hole is likely small *)
      List.fold_left
        (fun current constrnt ->
           match constrnt with
           | Ref(other_hole) ->
             debug "looking for: %s\n" other_hole;
             let ht,sid,sb = StringMap.find other_hole assignment in
             begin
               match ht with
                 HStmt ->
                 let stmt_info = self#get_fault_space_info sid in
                 (* here is one place where a comparison on name instead of
                    integer might make sense *)
                 IntSet.inter stmt_info.usedvars current
               | HExp ->
                 (* this is annoying, because we have to get the expression in
                    question andwe can only do that by getting all subatoms
                    from the enclosing statement.  Again, the way we handle
                    expressions is unsustainable...*)
                 let all_subatoms = self#get_subatoms ~fault_src:false sid in
                 let this_id = get_opt sb in
                 let this_exp = get_exp_from_subatom (List.nth all_subatoms this_id) in
                 let this_exps_vars = get_exp_vars this_exp in
                 IntSet.inter current this_exps_vars
               | HLval -> IntSet.inter (IntSet.singleton sid) current
             end
           | HasType(str) ->
             (* possible fixme: chances are decent that this comparision will be fragile, because
                it depends on how cil prints out the name internally, but we'll
                test to see how bad it is.  Worst comes to worse, we can
                construct a more complicated constraint that refers to the actual
                type of an actual declared variable in the template, but that's
                complicated so let's see how broken this cheap approach is first. *)
             IntSet.filter
               (fun candidate_vid ->
                  let varinfo = IntMap.find candidate_vid !varmap in
                  let typ_as_str = Pretty.sprint ~width:80 (printType defaultCilPrinter () varinfo.vtype) in
                  (* again, fixme: check the rules for string comparison to make sure this isn't broken.*)
                  typ_as_str = str
               ) current
           | HasVar(str)  ->
             IntSet.filter
               (fun candidate_vid ->
                  let varinfo = IntMap.find candidate_vid !varmap in
                  (* again, fixme: check the rules for string comparison to make sure this isn't broken.*)
                  varinfo.vname = str
               ) current
           | IsLocal ->
             let stmt_info = self#get_fault_space_info position in
             IntSet.inter stmt_info.local_ids current
           | IsGlobal ->
             let stmt_info = self#get_fault_space_info position in
             IntSet.inter stmt_info.global_ids current
        )
    in
    let process_stmt_constraints (position : atom_id) assignment =
      debug "stmt constraints\n";
      List.fold_left
        (fun current constrnt ->
           match constrnt with
           | HasVar(v) ->
             IntSet.filter
               (fun stmt_id ->
                  let stmt_info = self#get_fix_space_info stmt_id in
                  (* this "exists" checks if there's a variable used by the
                     statement we're considering that has the name specified in
                     the HasVar constraint *)
                  IntSet.exists (fun vid -> let vinfo = IntMap.find vid !varmap in
                                  (* possible FIXME: I never remember
                                     if string compare should be
                                     single or double equal *)
                                  vinfo.vname = v) (stmt_info.usedvars)
               ) current
           | _ ->
             (* Ref, HasType, IsLocal, IsGlobal *)
             debug "WARNING: encountered a stmt constraint that either makes no sense or Claire hasn't settled on semantics for, skipping.";
             current )
    in
    let one_hole position hole_name (hole : hole_info) (assignment : filled StringMap.t) =
      (* possible fixme: do the "first constraint" trick for lvals as well,
         because it can probably be much more efficient if the constraint set
         has a HasVar or a Ref in it *)
      match hole.htyp with
      | HExp -> begin
          (* expressions are tricky.  First, they're indexed by pairs instead of
             single ids (statement id, offset).  Fixing that would be desirable
             but a lot of work.  Second, we don't track them for other reasons, as
             we do lvals (which we pay attention to for scoping elsewhere, which
             is convenient here).  Third, there are A LOT of them.  So, we want to
             avoid starting with every fix path expression if possible.  Instead,
             we check if there's a Ref constraint to make our starter set, if possible *)
          let rec single_constraint = function
            | Ref(other_hole) :: rest ->
              let candidates = get_exp_refs other_hole assignment in
              if not (PairSet.is_empty candidates) then
                candidates,rest
              else
                single_constraint rest
            | first :: rest ->
              let exprs,constraints = single_constraint rest in
              exprs, first::constraints
            | [] -> (* either the expression is unconstrained, or we didn't find a ref, so we just enforce that the candidate expressions must be in scope *)
              let get_numbered_subatoms stmt =
                (* this bit of silliness is so that the double-fold below can be
                   tail-recursive.  The list ends up reversed, but it doesn't
                   matter (because of the numbering ). *)
                fst
                  (lfoldl (fun (lst,count) e ->
                       let e = get_exp_from_subatom e in
                       (e,count) :: lst, count + 1)
                      ([],0) (self#get_subatoms ~fault_src:false stmt))
              in
              IntSet.fold
                (fun stmt all_set ->
                   List.fold_left
                     (fun set (e,count) ->
                        if expr_in_scope position e then
                          PairSet.add (stmt,count) set
                        else
                          set
                     ) all_set (get_numbered_subatoms stmt))
                (fix_stmts()) (PairSet.empty), []
          in
          let startset,constraints' = single_constraint (ConstraintSet.elements hole.constraints) in
          let solutions =
            process_exp_constraints
              position
              assignment
              startset
              constraints'
          in
          lmap (fun (atom_id,subatom_id) ->
              StringMap.add hole_name (hole.htyp,atom_id,Some(subatom_id)) assignment)
            (PairSet.elements solutions)
        end
      | HLval ->
        debug "HLval!\n";
        let startset =
          debug "looking for: %d\n" position;
          let stmt_info = self#get_fault_space_info position in
          debug "union!\n";
          IntSet.union stmt_info.local_ids stmt_info.global_ids
        in
        debug "processing constraints\n";
        let solutions =
          process_lval_constraints
            position
            assignment
            startset
            (ConstraintSet.elements hole.constraints)
        in
        debug "before solutions!\n"; flush stdout;
        lmap (fun atom_id ->
            StringMap.add hole_name (hole.htyp,atom_id,None) assignment)
          (IntSet.elements solutions)
      | HStmt ->
        let startset =
          IntSet.filter (fun atom -> self#can_insert ~before:true position atom) (fix_stmts ())
        in
        let solutions =
          process_stmt_constraints
            position
            assignment
            startset
            (ConstraintSet.elements hole.constraints)
        in
        lmap (fun atom_id ->
            StringMap.add hole_name (hole.htyp,atom_id,None) assignment)
          (IntSet.elements solutions)
    in
    (* "instantiation_position" is what was formerly referred to as hole 1 *)
    (* fixme: add constraints on it *)
    let rec one_template (position) (assignment, unassigned) =
      (* FIXME: the weights make me sad *)
      if (llen unassigned) == 0 then [assignment,1.0] else begin
        let name,hole_info = List.hd unassigned in
        let remaining = List.tl unassigned in
        let assignments = one_hole position name hole_info assignment in
        lflatmap
          (fun assignment ->
             one_template position (assignment,remaining))
          assignments
      end
    in
    ht_find template_cache (position, template_name)
      (fun _ ->
         let template_constraints = StringMap.fold (fun k v acc -> (k,v) :: acc) template.hole_constraints [] in
         (* possible fixme: sorting here does the expected thing? *)
         let template_constraints = List.sort (fun (k1,v1) (k2,v2) -> compare k1 k2) template_constraints in
         one_template position ((StringMap.add "instantiation_position" (HStmt,position,None) StringMap.empty),template_constraints))

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

let patchCilRep_fileCache = ref None

(** [patchCilRep] is the default C representation.  The individual is a list of
    edits *)
class patchCilRep = object (self : 'self_type)
  inherit [cilRep_atom edit_history * atom_id] cilRep as super
  (*** State Variables **)

  (** {8 Genome } the [patchCilRep] genome pairs each history element with the
      atom id of the root of the inserted subtree. Note that this is ignored
      for deletes, since no new code is inserted. *)

  (* [genome] and [stmt_overrides] do not need to be serialized. They will be
     regenerated when cilRep's deserialize method calls set_genome. *)

  val mutable genome = ref []

  val mutable stmt_overrides : (int, stmt_info) Hashtbl.t = Hashtbl.create 257

  (**/**)
  method copy () : 'self_type =
    let super_copy : 'self_type = super#copy () in
    genome <- ref !genome;
    stmt_overrides <- Hashtbl.copy stmt_overrides;
    super_copy

  method private get_fault_space_info sid =
    try
      if sid = 0 then
        empty_stmt_info
      else if hmem stmt_overrides sid then
        hfind stmt_overrides sid
      else
        super#get_fault_space_info sid
    with Not_found ->
      failwith (Printf.sprintf "%s: cannot find info for statement %d" (self#name()) sid)

  (**/**)

  method genome_length () = llen !genome

  method get_genome () = !genome

  method private add_gene (h, id) =
    let id =
      if not !do_nested then 0
      else if id <> 0 then id
      else !stmt_count + 1
    in
    genome := !genome @ [(h,id)] ;
    self#get_current_files ()

  method private regen_stmt_info files =
    let reader sid = self#get_fault_space_info sid in
    let writer sid info =
      if (sid <> 0) && (info <> self#get_fault_space_info sid) then
        hrep stmt_overrides sid info
    in
    StringMap.iter (fun _ file ->
        self#internal_collect_stmt_info file reader writer
      ) files

  method set_genome g =
    self#updated();
    genome := [];
    let files = lfoldl (fun _ gene -> self#add_gene gene) StringMap.empty g in
    history := lmap fst !genome;
    self#regen_stmt_info files

  method add_history h =
    let files = self#add_gene (h,0) in
    super#add_history h ;
    self#regen_stmt_info files

  (** Override method in [cachingRepresentation] to accommodate [patchCilRep]'s
      genome. This must be kept in sync with [load_genome_from_string]. *)
  method name () =
    let genome = self#get_genome () in
    if genome = [] then "original"
    else
      let strs =
        lmap (fun (h,n) ->
            if !do_nested then
              (self#history_element_to_str h) ^ "/" ^ (string_of_int n)
            else
              (self#history_element_to_str h)
          ) genome
      in
      String.concat " " strs

  (** @param str history string, such as is printed out by fitness
      @raise Fail("unexpected history element") if the string contains something
      unexpected *)
  method load_genome_from_string str =
    let scan_history_element b =
      Scanf.bscanf b "%c" (fun action -> match action with
          | 'l' -> Scanf.bscanf b "(%s@)"   (fun name -> LaseTemplate(name))
          | 'd' -> Scanf.bscanf b "(%d)"    (fun id -> Delete(id))
          | 'a' -> Scanf.bscanf b "(%d,%d)" (fun dst src -> Append(dst,src))
          | 's' -> Scanf.bscanf b "(%d,%d)" (fun id1 id2 -> Swap(id1,id2))
          | 'r' -> Scanf.bscanf b "(%d,%d)" (fun dst src -> Replace(dst,src))
          | 'e' -> failwith "cannot parse subatoms"
          | _ ->
            (* someone decided that "regular" templates use their name instead of
               an action code, so everything else might just be a template... *)
            failwith "cannot parse templates (not enough info in string)"
        )
    in
    let scan_genome_element b =
      Scanf.bscanf b "%r/%d" scan_history_element (fun h n -> h,n)
    in
    let split_genome = Str.split (Str.regexp " ") str in
    let genome =
      if split_genome = ["original"] then []
      else
        let scanner =
          if !do_nested then scan_genome_element
          else fun b -> ((scan_history_element b), 0)
        in
        List.map (fun x ->
            try
              Scanf.sscanf x "%r" scanner (fun g -> g)
            with End_of_file ->
              failwith (Printf.sprintf "incomplete gene '%s'" x)
          ) split_genome
    in
    self#set_genome genome

  (** {8 internal_calculate_output_xform }

      This is the heart of cilPatchRep -- instead of maintaining the AST for
      this variant, we regenerate it when it's needed.  *)

  method internal_calculate_output_xform (h,n) current : cilVisitor list =
    (* We create [counter] outside [renumber] in case multiple subtrees need to
       be renumbered. This way we won't start renumbering all of them with the
       same ID. *)
    let counter = ref n in
    let do_zero _ _ new_s = visitCilStmt my_zero new_s in
    let renumber _ old_s new_s =
      let info = self#get_fault_space_info old_s.sid in

      let handler s expected funname =
        unexpected_num s expected funname ;
        if not (hmem stmt_overrides s.sid) then
          hrep stmt_overrides s.sid info ;
      in
      visitCilStmt (new numVisitor stmt_count counter handler) new_s
    in
    let update_localization keep_old old_s new_s =
      let empty, combine = 0.0, fun x y -> max x y in

      (* update fault localization, removing the old SIDs if necessary *)
      let sids = ref AtomSet.empty in
      let _ =
        visitCilStmt (object
          inherit nopCilVisitor

          method vstmt s =
            sids := AtomSet.add s.sid !sids ;
            DoChildren
        end) old_s
      in
      let localization, w =
        lfoldl (fun (localization, w) (sid, w') ->
            if AtomSet.mem sid !sids then
              if keep_old then
                (sid,w')::localization, combine w w'
              else
                localization, combine w w'
            else
              (sid,w')::localization, w
          ) ([], empty) !fault_localization
      in
      fault_localization := lrev localization ;

      visitCilStmt (object
        inherit nopCilVisitor
        method vstmt s =
          if s.sid <> 0 then
            fault_localization := (s.sid, w) :: !fault_localization;
          DoChildren
      end) new_s
    in
    let fix_vars _ old_s new_s =
      let info = self#get_fault_space_info old_s.sid in

      (* update the new fragment to use in-scope variables *)

      let available_vars = IntSet.union info.local_ids info.global_ids in
      let convert_var = semantic_equiv_vars !varmap available_vars in
      let _ =
        visitCilStmt (object
          inherit nopCilVisitor

          method vvrbl vi =
            match convert_var vi.vid with
            | [] ->
              let fd = IntMap.find info.in_func !fix_funmap in
              failwith (Printf.sprintf
                          "ERROR: no suitable in scope variable found for %s in %s:%s"
                          vi.vname info.in_file fd.svar.vname)
            | [vid] -> ChangeTo(IntMap.find vid !varmap)
            | vids ->
              let vis = lmap (fun vid -> IntMap.find vid !varmap) vids in
              let candidates =
                lmap (fun vi ->
                    Printf.sprintf "%s (%s)" vi.vname
                      (Pretty.sprint ~width:80 (d_loc () vi.vdecl))
                  ) vis
              in
              failwith (Printf.sprintf
                          "ERROR: too many matching variables for %s: %s"
                          vi.vname (String.concat ", " candidates))
        end) new_s
      in
      new_s
    in
    let internal_update fs keep_old old_s new_s =
      lfoldl (fun new_s f -> f keep_old old_s new_s) new_s fs
    in
    let fs = match h with | LaseTemplate _ -> [] | _ -> [fix_vars] in
    let fs =
      if !do_nested then
        [do_zero; renumber; update_localization] @ fs
      else
        [do_zero; update_localization] @ fs
    in
    let update = Some(internal_update fs) in

    let allow sid = (!partition < 0) || (self#is_in_partition sid !partition) in
    let make_replace label dst get_src =
      if not (allow dst) then []
      else [new replaceVisitor label ~update dst (get_src())]
    in
    let template tname fillins this_id =
      let template = self#get_template tname in
      let block = { template.template_code with battrs = [] }in
      let lval_replace = hcreate 10 in
      let exp_replace = hcreate 10 in
      let stmt_replace : (string,Cil.stmt) Hashtbl.t = hcreate 10 in
      StringMap.iter
        (fun hole (typ,id,idopt) ->
           match typ with
             HStmt ->
             let _,atom = self#get_stmt id in
             hadd stmt_replace hole atom
           | HExp ->
             let exp_id = get_opt idopt in
             let Exp(atom) = self#get_subatom ~fault_src:false id exp_id in
             hadd exp_replace hole atom
           | HLval ->
             let atom = IntMap.find id !varmap in
             hadd lval_replace hole (Var(atom),NoOffset)
        ) fillins;
      let block = mkStmt (Block(visitCilBlock (new templateReplace lval_replace exp_replace stmt_replace) block)) in
      block
    in
    match h with
    | LaseTemplate(name) -> [new laseTemplateVisitor ~update name allow]
    | Delete(id) -> make_replace "del" id mkEmptyStmt
    | Append(dst, src) ->
      if not (allow dst) then []
      else
        let _, src_stmt = self#get_stmt src in
        [new appVisitor ~update dst src_stmt]
    | Swap(id1, id2) ->
      let get_source id =
        let visitor = my_findstmt id in
        try
          StringMap.iter (fun _ file -> visitCilFileSameGlobals visitor file)
            current ;
          raise Not_found
        with Found_Stmt s -> s
      in
      let replaces =
        if !swap_bug then
          [if id1 <= id2 then "swap0", id1, id2 else "swap0", id2, id1]
        else
          ["swap1", id1, id2; "swap2", id2, id1]
      in
      lfoldl (fun visitors (name, dst, src) ->
          try
            make_replace name dst (fun () -> get_source src) @ visitors
          with Not_found -> visitors
        ) [] (lrev replaces)
    | Replace(dst, src) ->
      make_replace "rep" dst (fun () -> snd (self#get_stmt src))
    | Replace_Subatom(id, eid, Exp(subatom)) ->
      if not (allow id) then []
      else [new replaceSubatomVisitor id eid subatom]
    | Replace_Subatom(_, _, Stmt(_)) ->
      failwith "cilrep#replace_atom_subatom"
    | Template(tname,fillins) ->
      let _,dst,_ = StringMap.find "instantiation_position" fillins in
      let newBlock = template tname fillins dst in
      make_replace "template" dst (fun () -> newBlock)
  (*      | e ->
          debug "WARNING: internal_calculate_output_xform: edit %s not currently supported\n"
            (self#history_element_to_str e) ;
          [] *)

  method get_current_files () =
    let gene_to_crumb (h,n) = self#history_element_to_str h, n in
    let rec is_prefix crumbs genes =
      match crumbs, genes with
      | [], _ -> true
      | c::cs, g::gs when c = gene_to_crumb g -> is_prefix cs gs
      | _ -> false
    in
    Stats2.time "rebuild files" (fun () ->
        let result, crumbs, genes =
          match !patchCilRep_fileCache with
          | Some(id,crumbs,files)
            when id = (Oo.id self) && is_prefix crumbs (self#get_genome()) ->
            files, crumbs, snd (split_nth (self#get_genome()) (llen crumbs))
          | _ ->
            (* reset label_counter each time we start over; this method may be
               called hundreds of times while building a population before a
               variant is written...*)
            label_counter := 0;
            fault_localization := !global_ast_info.fault_localization ;
            copy !global_ast_info.code_bank, [], self#get_genome()
        in
        List.iter (fun gene ->
            List.iter (fun xform ->
                StringMap.iter (fun _ file ->
                    visitCilFileSameGlobals xform file
                  ) result
              ) (self#internal_calculate_output_xform gene result)
          ) genes ;
        let crumbs = crumbs @ (lmap gene_to_crumb genes) in
        patchCilRep_fileCache := Some(Oo.id self, crumbs, result) ;
        result
      )()

  (**/**)
  (* computes the source buffers for this variant.
      @return (string option * string option) list pair of filename and string
      source buffer corresponding to that file *)
  method private internal_compute_source_buffers () =
    let make_name n = if !use_subdirs then Some(n) else None in
    let output_list =
      match !min_script with
        Some(difflst, node_map) ->
        let old_file_map = self#get_source_files () in
        let new_file_map =
          lfoldl (fun file_map (filename,diff_script) ->
              let base_file = copy (StringMap.find filename old_file_map) in
              let mod_file =
                Cdiff.usediff base_file node_map diff_script (copy cdiff_data_ht)
              in
              StringMap.add filename mod_file file_map)
            (self#get_current_files ()) difflst
        in
        StringMap.fold
          (fun (fname:string) (cil_file:Cil.file) output_list ->
             let source_string = output_cil_file_to_string cil_file in
             (make_name fname,Some(source_string)) :: output_list
          ) new_file_map []
      | None ->
        StringMap.fold
          (fun (fname:string) (cil_file:Cil.file) output_list ->
             let source_string = output_cil_file_to_string cil_file in
             (make_name fname,Some(source_string)) :: output_list
          ) (self#get_current_files ()) []
    in
    assert((llen output_list) > 0);
    output_list

  method private internal_structural_signature () =
    let final_list, node_map =
      StringMap.fold
        (fun key base (final_list,node_map) ->
           let result = ref StringMap.empty in
           let node_map =
             foldGlobals base (fun node_map g1 ->
                 match g1 with
                 | GFun(fd,l) ->
                   let node_id, node_map = Cdiff.fundec_to_ast node_map fd in
                   result := StringMap.add fd.svar.vname node_id !result;
                   node_map
                 | _ -> node_map
               ) node_map in
           StringMap.add key !result final_list, node_map
        ) (self#get_current_files ()) (StringMap.empty, Cdiff.init_map())
    in
    { signature = final_list ; node_map = node_map}

  method note_success () =
    self#internal_debug_shared_private_size () ;
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
      behavior.  Use [self#get_current_files()] to access. *)

  val base = ref ((StringMap.empty) : Cil.file StringMap.t)

  method get_current_files () =
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
    lmap (fun (atom_id,w) -> Stmt((self#get atom_id).skind), w)
      !fault_localization

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
        output_list := (make_name fname,Some(source_string)) :: !output_list
      ) (self#get_current_files()) ;
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
      let f1,s1 = self#get_stmt stmt_id1 in
      let f2,s2 = self#get_stmt stmt_id2 in
      let base = self#get_current_files () in
      if StringMap.mem f1 base then
        visitCilFileSameGlobals (my_rep stmt_id1 s2) (StringMap.find f1 base);
      if StringMap.mem f2 base then
        visitCilFileSameGlobals (my_rep stmt_id2 s1) (StringMap.find f2 base)
    end
  end

  (* Atomic replace of two statements (atoms) *)
  method replace stmt_id1 stmt_id2 = begin
    let _,replace_with = self#get_stmt stmt_id2 in
    super#replace stmt_id1 stmt_id2 ;
    visitCilFileSameGlobals (my_rep stmt_id1 replace_with) (self#get_file stmt_id1)
  end

  (* application of a named LASE template *)
  method lase_template name =
    let allow sid = (!partition < 0) || (self#is_in_partition sid !partition) in
    let visitor = new laseTemplateVisitor name allow in
    super#lase_template name ;
    StringMap.iter (fun _ cilfile ->
        visitCilFileSameGlobals visitor cilfile
      ) (self#get_current_files ())

  method replace_subatom stmt_id subatom_id atom = begin
    let file = self#get_file stmt_id in
    super#replace_subatom stmt_id subatom_id atom ;
    match atom with
    | Stmt(x) -> failwith "cilRep#replace_atom_subatom"
    | Exp(e) ->
      visitCilFileSameGlobals
        (new replaceSubatomVisitor stmt_id subatom_id e)
        file
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
        ) (self#get_current_files ()) (StringMap.empty, Cdiff.init_map())
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
        let temp_variant = new patchCilRep in

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
