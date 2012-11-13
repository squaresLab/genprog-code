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

     Supports the Patch representation, which is the default for C programs. 
*)

open Printf
open Global
open Cil
open Cilprinter
open Rep
open Pretty
open Minimization

(**/**)
let semantic_check = ref "scope" 

let _ =
  options := !options @
    [
      "--semantic-check", Arg.Set_string semantic_check, 
      "X limit CIL mutations {none,scope}" ;
    ] 
(**/**)

(** {8 High-level CIL representation types/utilities } *)

let cilRep_version = "11" 

type cilRep_atom =
  | Stmt of Cil.stmtkind
  | Exp of Cil.exp 

(** The AST info for the original input Cil file is stored in a global variable
    of type [ast_info].  *)
type ast_info = 
    { code_bank : Cil.file StringMap.t ;
      stmt_map : (string * string) AtomMap.t ;
      localshave : IntSet.t IntMap.t ;
      globalsset : IntSet.t ;
      localsused : IntSet.t IntMap.t ;
      all_source_sids : IntSet.t }

(**/**)
let empty_info () =
  { code_bank = StringMap.empty;
    stmt_map = AtomMap.empty ;
    localshave = IntMap.empty ;
    globalsset = IntSet.empty ;
    localsused = IntMap.empty ;
    all_source_sids = IntSet.empty }
(**/**)

let global_ast_info = ref (empty_info()) 

class collectTypelabels result = object
  inherit nopCilVisitor

  method vstmt stmt = 
    let str,stmt = Cdiff.stmt_to_typelabel stmt in
      result := (IntSet.add str !result);
      SkipChildren
end

(** @param context_sid location being moved to @param moved_sid statement being
    moved @param localshave mapping between statement IDs and sets of variable
    IDs in scope at that statement ID @param localsused mapping between
    statement IDs and sets of variable IDs used at that ID @return boolean
    signifying if all the locals used by [moved_sid] are in scope at location
    [context_sid] *)
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

(** the xformRepVisitor applies a transformation function to a C AST.  Used to
   implement the patch representation for C programs, where the original program
   is transformed by a sequence of edit operations only at compile time. 

    @param xform function that potentially changes a Cil.stmt to return a new
    Cil.stmt
*)
class xformRepVisitor (xform : Cil.stmt -> Cil.stmt) = object(self)
  inherit nopCilVisitor

  method vstmt stmt = ChangeDoChildrenPost(stmt, (fun stmt -> xform stmt))
    
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

(** This visitor walks over the C program AST, numbers the nodes, and builds the
    statement map, while tracking in-scope variables, if desired.

    @param do_semantic boolean; whether to store info for a semantic check 
    @param globalset IntSet.t storing all global variables
    @param localset IntSet.t ref to store in-scope local variables
    @param localshave IntSet.t ref mapping a stmt_id to in scope local variables 
    @param localsused IntSet.t ref mapping stmt_id to vars used by stmt_id
    @param count int ref maximum statement id
    @param add_to_stmt_map function of type int -> (string * string) -> (),
    takes a statement id and a function and filename and should add the info to
    some sort of statement map.
    @param fname string, filename
*)
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

(**/**)
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
(**/**)

(*** Obtaining coverage and Weighted Path Information ***)

(* These are CIL variables describing C standard library functions like
 * 'fprintf'. We use them when we are instrumenting the file to
 * print out statement coverage information for fault localization. *)  
(**/**)
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
(**/**)

(** Visitor for computing statement coverage (for a "weighted path"); walks over
    the C program AST and modifies it so that each statement is preceeded by a
    'printf' that writes that statement's number to the .path file at run-time.
    
    @param coverage_outname path filename
*) 
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

  method vfunc f = 
    if List.mem f.svar.vname do_not_instrument_these_functions then begin 
      debug "cilRep: WARNING: definition of fprintf found at %s:%d\n" 
        f.svar.vdecl.file f.svar.vdecl.line ;
      debug "\tcannot instrument for coverage (would be recursive)\n";
      SkipChildren
    end else begin
      let stmt_str = 
        "if (fout == 0) {\n fout = fopen(%g:fout_g,%g:wb_arg);\n}"
      in
      let ifstmt = cstmt stmt_str 
        [("uniq_array", Fv(!uniq_array_va));("fout_g",Fg coverage_outname);]
      in
        ChangeDoChildrenPost(f,
                             (fun f ->
                               f.sbody.bstmts <- ifstmt :: f.sbody.bstmts;
                               f))
    end

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
let found_atom = ref 0 
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
                found_atom := s.sid ;
                found_dist := this_dist 
              end 
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

(**/**)
let my_get = new getVisitor
let my_get_exp = new getExpVisitor 
let my_findstmt = new findStmtVisitor
let my_find_atom = new findAtomVisitor
(**/**)

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
  (** the underlying code is [cilRep_atom] for all C-based individuals *)
  inherit ['gene, cilRep_atom] faultlocRepresentation as super

  (**/**)
  method virtual get_base : unit -> Cil.file StringMap.t

  val stmt_count = ref 1 

  method copy () : 'self_type =
    let super_copy : 'self_type = super#copy () in 
      super_copy#internal_copy () 

  method internal_copy () : 'self_type =
    {< history = ref (copy !history);
       stmt_count = ref !stmt_count >}

  method atom_to_str atom = 
    let doc = match atom with
      | Exp(e) -> d_exp () e 
      | Stmt(s) -> dn_stmt () (mkStmt s) 
    in 
      Pretty.sprint ~width:80 doc 

  method debug_info () =
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
            debug "cilRep: %s (code bank/base file; atoms [%d,%d])\n" 
              k low high 
        ) (self#get_code_bank ()) ; 
      debug "cilRep: %d file(s) total in representation\n" !file_count ; 

  method max_atom () = !stmt_count 

  (**/**)

  (** 
      {8 Methods that access statements, files, code, etc, in both the base
      representation and the code bank} *)

  method get_stmt_map () = 
    assert(not (AtomMap.is_empty !global_ast_info.stmt_map)) ;
    !global_ast_info.stmt_map 

  method get_code_bank () = 
    assert(not (StringMap.is_empty !global_ast_info.code_bank));
    !global_ast_info.code_bank

  (** gets a statement from the {b code bank}, not the current variant.  
      
      @param stmt_id id of statement we're looking for
      @return (filename,stmtkind) file that statement is in and the statement 
      @raise Fail("stmt_id not found in stmt map")
  *)
  method get_stmt stmt_id =
    try begin 
      let funname, filename =
        AtomMap.find stmt_id (self#get_stmt_map()) 
      in
      let code_bank = self#get_code_bank () in 
        if not (StringMap.mem filename code_bank) then begin
          let code_bank_size = 
            StringMap.fold (fun key elt acc -> acc + 1) code_bank 0 
          in 
          let _ =
            debug "cilrep: code bank size %d\n" 
              code_bank_size 
          in
            abort "cilrep: cannot find stmt id %d in code bank\n%s (function)\n%s (file not found)\n"
              stmt_id funname filename 
        end;
      let file_ast = StringMap.find filename code_bank in 
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
    found_atom := (-1);
    found_dist := max_int;
        StringMap.iter (fun fname file -> 
          visitCilFileSameGlobals (my_find_atom source_file source_line) file)
          (self#get_base ());
      if !found_atom = (-1) then begin
        debug "cilrep: WARNING: cannot convert %s,%d to atom_id\n" source_file
          source_line ;
        0 
      end else !found_atom


  (** {8 Methods for loading and instrumenting source code} *)

  (** loads a CIL AST from a C source file or collection of C source files.
      
      @param filename string with extension .txt, .c, .i, .cu, .cu
      @raise Fail("unexpected file extension") if given anything else
  *)
  method from_source (filename : string) = begin 
    debug "cilrep: from_source: stmt_count = %d\n" !stmt_count ; 
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
      debug "stmt_count: %d\n" !stmt_count;
      stmt_count := pred !stmt_count 
  end 

  (** parses one C file 
      
      @param filename a .c file to parse
      @return Cil.file the parsed file
      @raise Fail("parse failure") can fail in Cil/Frontc parsing *)
  method private internal_parse (filename : string) = 
    debug "cilRep: %s: parsing\n" filename ; 
    let file = Frontc.parse filename () in 
      debug "cilRep: %s: parsed (%g MB)\n" filename (debug_size_in_mb file); 
      file 

  (** parses and then processes one C file.  Collects all necessary data for
      semantic checking and updates the global ast information in
      [global_ast_info].

      @param filename source file
      @return Cil.file parsed/numbered/processed Cil file *)
  method private from_source_one_file (filename : string) : Cil.file =
    let full_filename = filename in
    let file = self#internal_parse full_filename in 
    let globalset = ref !global_ast_info.globalsset in 
    let localshave = ref !global_ast_info.localshave in
    let localsused = ref !global_ast_info.localsused in
    let localset = ref IntSet.empty in
    let stmt_map = ref !global_ast_info.stmt_map in
      visitCilFileSameGlobals (new everyVisitor) file ; 
      visitCilFileSameGlobals (new emptyVisitor) file ; 
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
            Hashtbl.iter (fun str i ->
              source_ids := IntSet.add i !source_ids 
            ) canonical_stmt_ht ;
          global_ast_info := {!global_ast_info with
            stmt_map = !stmt_map;
            localshave = !localshave;
            localsused = !localsused;
            globalsset = !globalset;
            all_source_sids = !source_ids };
          self#internal_post_source filename; file

  (**/**)
  method internal_post_source filename = ()

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
    let Fv(stderr_va) = stderr_va in
    let coverage_out = [GVarDecl(stderr_va,!currentLoc)] in 
    let new_globals = 
      if not globinit then 
        lmap
          (fun glob ->
            match glob with
              GVarDecl(va,loc) -> GVarDecl({va with vstorage = Extern}, loc))
          coverage_out
      else  coverage_out
    in
    let _ = 
      file.globals <- new_globals @ file.globals 
    in
    let cov_visit = new covVisitor coverage_outname in
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

  (**/**)
  method instrument_fault_localization 
    coverage_sourcename coverage_exename coverage_outname = 
    debug "cilRep: instrumenting for fault localization\n";
        StringMap.iter
          (fun fname file ->
            let file = copy file in 
                self#instrument_one_file file ~g:true 
                  coverage_sourcename coverage_outname)
          (self#get_base())

  (*** Atomic mutations ***)

  (* Return a Set of (atom_ids,fix_weight pairs) that one could append here 
   * without violating many typing rules. *) 
  method append_sources append_after =
    let all_sids = !fix_localization in 
    let sids = 
      if !semantic_check = "none" then all_sids
      else  
        lfilt (fun (sid,weight) ->
          in_scope_at append_after sid 
            !global_ast_info.localshave !global_ast_info.localsused 
        ) all_sids
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
            !global_ast_info.localshave !global_ast_info.localsused 
          && in_scope_at append_after sid 
            !global_ast_info.localshave !global_ast_info.localsused 
        ) all_sids 
    in
    let sids = lfilt (fun (sid, weight) -> sid <> append_after) sids in
      lfoldl (fun retval ele -> WeightSet.add ele retval)
        (WeightSet.empty) sids

  (**/**)

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

  method is_max_fitness () = 
    Fitness.test_to_first_failure (self :> ('a, 'b) Rep.representation)

  (** implemented by subclasses.  In our case, generates a fresh version of the
      base representation and calls minimize *)
  method virtual note_success : unit -> unit
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
    let _ = 
      List.iter (fun h -> 
        match h with 
        | Delete(x) | Append(x,_) 
          -> Hashtbl.replace relevant_targets x true 
        | Swap(x,y) -> 
          Hashtbl.replace relevant_targets x true ;
          Hashtbl.replace relevant_targets y true ;
        | Crossover(_,_) -> 
          abort "cilPatchRep: Crossover not supported\n" 
      ) edit_history 
    in
    let edits_remaining = 
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
    let the_xform stmt = 
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
        (* Most statements will not be in the hashtbl. *)  
        if Hashtbl.mem relevant_targets this_id then begin
          (* If the history is [e1;e2], then e1 was applied first, followed by
           * e2. So if e1 is a delete for stmt S and e2 appends S2 after S1, 
           * we should end up with the empty block with S2 appended. So, in
           * essence, we need to apply the edits "in order". *) 
          List.fold_left 
            (fun accumulated_stmt this_edit -> 
              let used_this_edit, resulting_statement = 
                match this_edit with
                | Swap(x,y) when x = this_id  -> swap accumulated_stmt x y
(*                | Swap(y,x) when x = this_id -> swap accumulated_stmt y x*)
                | Delete(x) when x = this_id -> delete accumulated_stmt x
                | Append(x,y) when x = this_id -> append accumulated_stmt x y
                (* Otherwise, this edit does not apply to this statement. *) 
                | _ -> false, accumulated_stmt
              in 
                if used_this_edit then 
                  edits_remaining := 
                    List.filter (fun x -> x <> this_edit) !edits_remaining;
                resulting_statement
            ) stmt !edits_remaining 
        end else stmt 
    in 
      the_xform 

  (**/**)
  (* computes the source buffers for this variant.  @return (string * string)
      list pair of filename and string source buffer corresponding to that
      file *)
  method private internal_compute_source_buffers () = 
    let make_name n =  None in
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
          StringMap.fold
            (fun (fname:string) (cil_file:Cil.file) output_list ->
              let source_string = output_cil_file_to_string ~xform cil_file in
                (make_name fname,source_string) :: output_list 
            ) (self#get_base ()) [] 
    in
      assert((llen output_list) > 0);
      output_list

  method private internal_structural_signature () =
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
(*      orig#load_genome_from_string "";
      (orig#output_source "real_original.c");
      (self#output_source "real_repair.c");*)
      Minimization.do_minimization
        (orig :> ('a,'b) representation)
        (self :> ('a,'b) representation)
(*        (orig#structural_signature())
        (self#structural_signature())*)
        (self#name())

  method structural_signature () = 
(*    match !already_signatured with
      Some(s) -> debug "already signatured\n"; s
      | None -> 
*)      let s = self#internal_structural_signature() in
(*        already_signatured := Some(s); *) s

  (**/**)

end
