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
let semantic_check = ref "scope" 
let multithread_coverage = ref false
let uniq_coverage = ref false
let swap_bug = ref false 

let _ =
  options := !options @
    [
      "--semantic-check", Arg.Set_string semantic_check, 
      "X limit CIL mutations {none,scope}" ;

      "--mt-cov", Arg.Set multithread_coverage, 
      "  instrument for coverage with locks.  Avoid if possible.";

      "--uniq", Arg.Set uniq_coverage, 
      "  print each visited stmt only once";

      "--swap-bug", Arg.Set swap_bug, 
      " swap is implemented as in ICSE 2012 GMB experiments." 
    ] 
(**/**)

(** {8 High-level CIL representation types/utilities } *)

let cilRep_version = "10" 

type cilRep_atom =
  | Stmt of Cil.stmtkind
  | Exp of Cil.exp 

(** The AST info for the original input Cil file is stored in a global variable
    of type [ast_info].  *)
type ast_info = 
    { code_bank : Cil.file StringMap.t ;
      oracle_code : Cil.file StringMap.t ;
      stmt_map : (string * string) AtomMap.t ;
      localshave : IntSet.t IntMap.t ;
      globalsset : IntSet.t ;
      localsused : IntSet.t IntMap.t ;
      varinfo : Cil.varinfo IntMap.t ;
      all_source_sids : IntSet.t }

(**/**)
let empty_info () =
  { code_bank = StringMap.empty;
    oracle_code = StringMap.empty ;
    stmt_map = AtomMap.empty ;
    localshave = IntMap.empty ;
    globalsset = IntSet.empty ;
    localsused = IntMap.empty ;
    varinfo = IntMap.empty ;
    all_source_sids = IntSet.empty }
(**/**)

let global_ast_info = ref (empty_info()) 

(** stores loaded templates *)
let registered_c_templates = hcreate 10

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

(**/**)
let my_put_exp = new putExpVisitor 
let my_get = new getVisitor
let my_get_exp = new getExpVisitor 
let my_findstmt = new findStmtVisitor
let my_find_atom = new findAtomVisitor
let my_del = new delVisitor 
let my_app = new appVisitor 
let my_swap = new swapVisitor 
let my_rep = new replaceVisitor
let my_put = new putVisitor
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
          Marshal.to_channel fout (!global_ast_info.code_bank) [] ;
          Marshal.to_channel fout (!global_ast_info.oracle_code) [] ;
          Marshal.to_channel fout (!global_ast_info.stmt_map) [] ;
          Marshal.to_channel fout (!stmt_count) [] ;
          let triple = !global_ast_info.localshave,
            !global_ast_info.localsused, 
            !global_ast_info.all_source_sids 
          in
            Marshal.to_channel fout triple [] ;
        end;
        Marshal.to_channel fout (self#get_genome()) [] ;
        debug "cilRep: %s: saved\n" filename ; 
        super#serialize ~out_channel:fout ?global_info:global_info filename ;
        debug "cilrep done serialize\n";
        if out_channel = None then close_out fout 
(**/**)

  (** @raise Fail("version mismatch") if the version specified in the binary file is
      different from the current [asmRep_version] *)
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
          let code_bank = Marshal.from_channel fin in
          let oracle_code = Marshal.from_channel fin in
          let stmt_map = Marshal.from_channel fin in
          let _ = stmt_count := Marshal.from_channel fin in
          let var_maps = Marshal.from_channel fin in 
            global_ast_info :=
              { code_bank = code_bank;
                oracle_code = oracle_code;
                stmt_map = stmt_map ;
                localshave = fst3 var_maps ;
                localsused = snd3 var_maps ;
                all_source_sids = trd3 var_maps ;
                globalsset = !global_ast_info.globalsset;
                varinfo = !global_ast_info.varinfo }
        end;
        self#set_genome (Marshal.from_channel fin);
        super#deserialize ~in_channel:fin ?global_info:global_info filename ; 
        debug "cilRep: %s: loaded\n" filename ; 
        if in_channel = None then close_in fin ;
  end 

  (**/**)

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

  (** gets a statement from the {b code bank}, not the current variant.  Will
      also look in the oracle code if available/necessary.
      
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
    let full_filename = 
      if Sys.file_exists filename then filename else (Filename.concat !prefix filename)
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
            Hashtbl.iter (fun str i ->
              source_ids := IntSet.add i !source_ids 
            ) canonical_stmt_ht ;
          global_ast_info := {!global_ast_info with
            stmt_map = !stmt_map;
            localshave = !localshave;
            localsused = !localsused;
            globalsset = !globalset;
            varinfo = !varmap ;
            all_source_sids = !source_ids };
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
    already_signatured := None;
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
          uniq_array_va := makeGlobalVar "___coverage_array" array_typ;
          [GVarDecl(!uniq_array_va,!currentLoc)]
      end else []
    in
    let Fv(stderr_va) = stderr_va in
    let coverage_out = [GVarDecl(stderr_va,!currentLoc)] in 
    let new_globals = 
      if not globinit then 
        lmap
          (fun glob ->
            match glob with
              GVarDecl(va,loc) -> GVarDecl({va with vstorage = Extern}, loc))
          (uniq_globals @ coverage_out)
      else (uniq_globals @ coverage_out)
    in
    let _ = 
      file.globals <- new_globals @ file.globals 
    in
    let cov_visit = new covVisitor coverage_outname in
      visitCilFileSameGlobals cov_visit file;
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
          (self#get_base()) true)

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
            !global_ast_info.localshave !global_ast_info.localsused 
        ) all_sids 
    in
    let sids = lfilt (fun (sid, weight) -> sid <> replace) sids in
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
    let _ = super#load_templates template_file in
    let file = Frontc.parse template_file () in
    let template_constraints_ht = hcreate 10 in
    let template_code_ht = hcreate 10 in
    let template_name = ref "" in
    let my_constraints = new collectConstraints 
      template_constraints_ht template_code_ht template_name
    in
      visitCilFileSameGlobals my_constraints file;
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
        template_constraints_ht;
      (* FIXME: this probability is almost certainly wrong *)
      hiter (fun str _ -> mutations := (Template_mut(str), 1.0) :: !mutations) registered_c_templates
        
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

    let other_hole_is_not_dependent_on_this_one = true in
    (* one_hole finds all satisfying assignments for a given template hole *)
    let rec one_hole (hole : hole_info) (assignment : filled StringMap.t) 
        (holes_left : hole_info StringMap.t) 
        : (filled StringMap.t * hole_info StringMap.t) list = 
      (* CLG: OK, this looks more complicated than it is.  The basic issue is
         that fulfilling one constraint is done very similarly for the different
         hole types, but they vary slightly in how they handle reference/inscope
         constraints and in the underlying element type (expressions are pairs,
         lvals and stmts are not).  one_constraint therefore takes a number of
         parameters to allow the reuse of this constraint-solving code for all
         hole types.  It was worse before, I swear *)
      let rec one_constraint 
          solver
          in_scope_test 
          ref_filter_stmt 
          ref_filter_exp 
          ref_filter_lval 
          candidates assignment holes_left con =
        (* convenience function for the recursive call *)
        let one_constraint = 
          one_constraint solver
            in_scope_test 
            ref_filter_stmt 
            ref_filter_exp 
            ref_filter_lval 
        in
        let cands () = if solver.is_empty candidates then solver.all () else candidates in
          match con with
          | InScope(other_hole) 
          | Ref(other_hole) when other_hole_is_not_dependent_on_this_one && StringMap.mem other_hole holes_left -> 
            (* fill that hole first, then try this one again *)
            let candidates = cands () in
            let other_hole = StringMap.find other_hole holes_left in
            let assignments = one_hole other_hole assignment holes_left in
              lflatmap
                (fun (assignment, holes_left) -> 
                  one_constraint candidates assignment holes_left con)
                assignments
          | _ -> 
            (* otherwise it won't be a recursive call so it's a bit simpler...*)
            let candidates = 
              match con with
              | Fault_path when (solver.is_empty candidates) -> solver.all_fault ()
              | Fix_path when (solver.is_empty candidates) -> solver.all_fix ()
              | Fault_path -> solver.intersect (solver.all_fault()) candidates
              | Fix_path -> solver.intersect (solver.all_fix()) candidates
              | InScope(other_hole) when StringMap.mem other_hole assignment ->
                let candidates = cands () in
                let hole_type,in_other_hole,maybe_exp = 
                  StringMap.find other_hole assignment in
                  assert(hole_type <> Lval_hole); (* I think *)
                  solver.filter (fun ele -> in_scope_test in_other_hole ele) candidates 
              | Ref(other_hole) when StringMap.mem other_hole assignment -> 
                let candidates = cands () in
                let hole_type, in_other_hole, maybe_exp = 
                  StringMap.find other_hole assignment 
                in
                let filter_fun = 
                  match hole_type with
                    Stmt_hole -> 
                      (fun ele -> ref_filter_stmt in_other_hole ele) 
                  | Exp_hole ->
                    (fun ele -> ref_filter_exp in_other_hole maybe_exp ele)
                  | Lval_hole -> 
                    (fun ele -> ref_filter_lval in_other_hole maybe_exp ele)
                in
                  solver.filter filter_fun candidates
              | _ -> solver.empty ()
            in
              [candidates,assignment,holes_left]
      in
      let stmt_solver = 
        {  empty = (fun _ -> IntSet.empty) ;
           is_empty = IntSet.is_empty ;
           all_fault = fault_stmts;
           all_fix = fix_stmts;
           all = all_stmts; 
           filter = IntSet.filter;
           intersect = IntSet.inter }
      in
      let stmt_constraint = 
        one_constraint stmt_solver
          (fun in_other_hole stmt ->
            in_scope_at in_other_hole stmt 
              !global_ast_info.localshave !global_ast_info.localsused)
          (fun _ -> failwith "Not implemented")
          (fun _ -> failwith "Not implemented")
          (fun _ -> failwith "Not implemented")
      in
      let exp_solver = 
        {  empty = (fun _ -> PairSet.empty) ;
           is_empty = PairSet.is_empty ;
           all_fault = fault_exps;
           all_fix = fix_exps;
           all = all_exps; 
           filter = PairSet.filter;
           intersect = PairSet.inter }
      in
      let exp_constraint = 
        one_constraint exp_solver
        (fun in_other_hole (sid,subatom_id) ->
          in_scope_at in_other_hole sid 
            !global_ast_info.localshave !global_ast_info.localsused)
        (fun in_other_hole (sid,subatom_id) ->
          let subatoms_there = self#get_subatoms in_other_hole in
          let this_atom = self#get_subatom sid subatom_id in
            List.mem this_atom subatoms_there)
        (fun in_other_hole maybe_exp (sid,subatom_id) ->
          let exp_id = match maybe_exp with Some(id) -> id in
          let that_exp = self#get_subatom in_other_hole exp_id in
          let this_atom = self#get_subatom sid subatom_id in
            this_atom == that_exp)
          (fun _ -> failwith "unimplemented") 
      in
      let lval_constraint = 
        one_constraint 
          { stmt_solver with all_fault = fault_lvals; all_fix = fix_lvals ; all = all_lvals }
          (fun in_other_hole vid ->
            let locals = 
              IntMap.find in_other_hole !global_ast_info.localshave 
            in
              IntSet.mem vid locals || 
                IntSet.mem vid !global_ast_info.globalsset)

          (fun in_other_hole vid -> 
            let locals_used = IntMap.find in_other_hole !global_ast_info.localsused in 
              IntSet.mem vid locals_used
          )
          (fun _ -> failwith "unimplemented")
          (fun _ -> failwith "unimplemented")

      in
      let constraints = ConstraintSet.elements hole.constraints in
        match hole.htyp with
          Stmt_hole | Lval_hole ->
            let process_func =
              if hole.htyp = Stmt_hole then stmt_constraint else lval_constraint
            in
            let fulfills_constraints =
              lfoldl
                (fun candidates con ->
                  lflatmap
                    (fun (candidate_stmts, assignment, remaining) ->
                      lfilt (fun (foo,_,_) -> not (IntSet.is_empty foo))
                        (process_func candidate_stmts assignment remaining con))
                    candidates 
                ) [(IntSet.empty, assignment,holes_left)] 
                constraints
            in
              lflatmap 
                (fun (candidates,assignment,holes_left) ->
                  lmap (fun id ->
                    StringMap.add hole.hole_id (hole.htyp,id,None) assignment,
                    StringMap.remove hole.hole_id holes_left)
                    (IntSet.elements candidates)) fulfills_constraints
        | Exp_hole ->
          let fulfills_constraints =
            lfoldl
              (fun candidates con ->
                lflatmap
                  (fun (candidate_exps, assignment, remaining) ->
                    lfilt (fun (foo,_,_) -> not (PairSet.is_empty foo))
                      (exp_constraint candidate_exps assignment remaining con))
                  candidates 
              ) [(PairSet.empty, assignment,holes_left)] constraints 
          in
            lflatmap 
              (fun (candidates,assignment,holes_left) ->
                lmap (fun (id,e) ->
                  StringMap.add hole.hole_id (hole.htyp,id,Some(e)) assignment,
                  StringMap.remove hole.hole_id holes_left)
                  (PairSet.elements candidates)) fulfills_constraints
    in
    let template = hfind registered_c_templates template_name in
    (* partially_fulfilled is a list of starting assignments of the location to
       one of the holes and a map of holes that remain to be filled *)
    let partially_fulfilled () =
      hfold
        (fun hole_id _ lst -> 
          let hole_info = StringMap.find hole_id template.hole_constraints in 
          if hole_info.htyp = Stmt_hole && ConstraintSet.mem Fault_path hole_info.constraints then
            (template.template_name, 
             StringMap.add hole_id (Stmt_hole,location_id,None) (StringMap.empty), 
             StringMap.remove hole_id template.hole_constraints) :: lst
          else lst) template.hole_code_ht []
    in
    let rec one_template (template, assignment, unassigned) =
      if StringMap.is_empty unassigned then [template,1.0,assignment] else begin
        let as_lst = 
          StringMap.fold 
            (fun hole_name hole_info lst ->
              (hole_name, hole_info) :: lst) unassigned []
        in
        let name,hole_info = List.hd as_lst in
        let assignments = one_hole hole_info assignment unassigned in
        let cardinal = ref 0 in
          StringMap.iter (fun k v -> incr cardinal) assignment;
          let assignments = 
            if !cardinal > 1 then begin
              lfilt (fun (assignment,remaining) ->
                try 
                  (* CLG: I don't remember why this exists *)
                  StringMap.iter
                    (fun k (t1,id1,eopt1) ->
                      let without = StringMap.remove k assignment in
                        StringMap.iter
                          (fun _ (t2,id2,eopt2) -> 
                            if t1 = t2 && id1 = id2 && eopt1 = eopt2 then raise (FoundIt(k)))
                          without
                    ) assignment; true
                with FoundIt _ -> false) assignments
            end else assignments 
          in 
            lflatmap 
              (fun (assignment, remaining) -> 
                one_template (template,assignment,remaining)) 
              assignments 
      end
    in
      ht_find template_cache (location_id, template_name)
        (fun _ -> lflatmap one_template (partially_fulfilled()))

  (** {8 Structural Differencing} [min_script], [from_source_min], and
      [internal_structural_signature] are used for minimization and partially
      implement the [minimizableObject] interface.  The latter function is
      implemented concretely in the subclasses. *)


  val min_script = ref None

  method from_source_min cilfile_list node_map = 
    self#updated ();
    min_script := Some(cilfile_list, node_map)

  method virtual private internal_structural_signature : unit -> structural_signature

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
  method get_base () = 
    !global_ast_info.code_bank

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
    let _ = 
      List.iter (fun h -> 
        match h with 
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
      ) edit_history 
    in
    let edits_remaining = 
      if !swap_bug then ref edit_history else 
        (* double each swap in the edit history, if you want the correct swap
         * behavior (as compared to the buggy ICSE 2012 behavior); this means
         * each swap is in the list twice and thus will be applied twice (once at
         * each relevant location.) *)
        ref (lflatmap
               (fun edit -> 
                 match edit with Swap _ -> [edit; edit] 
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
      let template accumulated_stmt tname fillins this_id = 
        try
          StringMap.iter
            (fun hole_name (_,id,_) ->
              if id = this_id then raise (FoundIt(hole_name)))
            fillins; 
          false, accumulated_stmt
        with FoundIt(hole_name) -> begin
          let template = self#get_template tname in
          let block = hfind template.hole_code_ht hole_name in 
          let cardinal = ref 0 in
            StringMap.iter (fun k v -> incr cardinal) template.hole_constraints;
            let all_holes = 1 -- !cardinal in
            let arg_list = 
              StringMap.fold
                (fun hole (typ,id,idopt) arg_list ->
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
            let asstr = 
              Pretty.sprint ~width:80 
                (printBlock Cilprinter.noLineCilPrinter () block) 
            in
            let placeholder_regexp = 
              Str.regexp_string " = ___placeholder___.var" 
            in
            let removed_placeholder = 
              Str.global_replace placeholder_regexp "" asstr 
            in
            let spaces =
              lfoldl
                (fun current_str holenum ->
                  let holename = Printf.sprintf "__hole%d__" holenum in
                  let addspace_regexp = Str.regexp (")"^holename) in
                    if any_match addspace_regexp current_str then
                      Str.global_replace addspace_regexp 
                        (") "^holename) current_str
                    else current_str
                ) removed_placeholder all_holes
            in
            let copy = 
              lfoldl
                (fun current_str holenum ->
                  let holename = Printf.sprintf "__hole%d__" holenum in
                  let constraints = 
                    StringMap.find  holename template.hole_constraints 
                  in
                  let typformat = 
                    match constraints.htyp with
                      Stmt_hole -> "%s:"
                    | Exp_hole -> "%e:"
                    | Lval_hole -> "%v:"
                  in
                  let fullformat = typformat^holename in
                  let current_regexp = Str.regexp (holename^".var;") in
                  let rep = 
                    Str.global_replace current_regexp fullformat current_str 
                  in
                  let current_regexp = Str.regexp (holename^".var") in
                    Str.global_replace current_regexp fullformat rep
                ) spaces all_holes
            in
            let new_code = 
              Formatcil.cStmt copy 
                (fun n t -> failwith "This shouldn't make new variables") 
                Cil.locUnknown arg_list
            in
              true, { accumulated_stmt with skind = new_code.skind ; labels = possibly_label accumulated_stmt tname this_id }
        end
      in
        (* Most statements will not be in the hashtbl. *)  
        if Hashtbl.mem relevant_targets this_id then begin
          (* If the history is [e1;e2], then e1 was applied first, followed by
           * e2. So if e1 is a delete for stmt S and e2 appends S2 after S1, 
           * we should end up with the empty block with S2 appended. So, in
           * essence, we need to appliy the edits "in order". *) 
          List.fold_left 
            (fun accumulated_stmt this_edit -> 
              let used_this_edit, resulting_statement = 
                match this_edit with
                | Replace_Subatom(x,subatom_id,atom) when x = this_id -> 
                  abort "cilPatchRep: Replace_Subatom not supported\n" 
                | Swap(x,y) when x = this_id  -> swap accumulated_stmt x y
                | Swap(y,x) when x = this_id -> swap accumulated_stmt y x
                | Delete(x) when x = this_id -> delete accumulated_stmt x
                | Append(x,y) when x = this_id -> append accumulated_stmt x y
                | Replace(x,y) when x = this_id -> replace accumulated_stmt x y
                | Template(tname,fillins) -> 
                  template accumulated_stmt tname fillins this_id
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
      Minimization.do_minimization orig self

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
    base := copy !global_ast_info.code_bank
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

  (* The "get" method's return value is based on the 'current', 'actual'
   * content of the variant and not the 'code bank'. *)

  method get stmt_id = 
    let file = self#get_file stmt_id in
      visitCilFileSameGlobals (my_get stmt_id) file;
      let answer = !gotten_code in
        gotten_code := (mkEmptyStmt()).skind ;
        (Stmt answer) 

  method put stmt_id (stmt : cilRep_atom) =
    let file = self#get_file stmt_id in 
      (match stmt with
      | Stmt(stmt) -> 
        visitCilFileSameGlobals (my_put stmt_id stmt) file;
        visitCilFileSameGlobals (new fixPutVisitor) file;
      | Exp(e) -> failwith "cilRep#put of Exp subatom" );

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
        (* FIXME? super#note_replaced_subatom stmt_id subatom_id atom ; *)
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
      Minimization.do_minimization orig self

end
