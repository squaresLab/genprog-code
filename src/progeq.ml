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

(*
 * An approximate "Program Equivalence" relation for Cilrep. Precise
 * program equivalence is undecideable; we use a series of approximations.
 *
 * Given an approximate program equivalence relation, we can quotient the
 * search space with respect to it (i.e., compute equivalence classes) so
 * that we don't spend time considering edits that are different
 * syntactically but not semantically.
 *
 * One such approximation is "instruction scheduling". The high-level idea
 * is as follows. If the program is:
 *
 *  L1:
 *    x = x + 1;
 *  L2:
 *    y = y + 1;
 *  L3:
 *
 * ... and the possible fix is "Insert z = 5", while there are three places
 * to insert that statement (L1, L2, L3) they all yield equivalent
 * programs, so we should only try one of them.
 *
 * To determine this, we consider the usual notions of "effects" and
 * "dependencies". If you had the repair at L3, instruction scheduling
 * could move it to L2 because "y=y+1" and "z=5" share no relevant
 * (e.e., "read-after-write") dependencies.
 *)
open Printf
open Global
open Cil
open Rep
open Pretty
open Knownfuns

(*
 * To determine if two 'statements' share a dependency (e.g.,
 * read-after-write), we need to know what they read and write.
 *)
type effect =
  | Read_Var of Cil.varinfo
  | Write_Var of Cil.varinfo
  | Read_Mem of Cil.exp
  | Write_Mem of Cil.exp
  | File_IO (* fprintf, fseek, etc. -- cannot move past another File_IO *)
  | Other (* global syscall, printf, etc. -- always a dependency *)

(* debugging *)
let effect_to_str e = match e with
  | Read_Var(v) -> Printf.sprintf "r %s" v.vname
  | Write_Var(v) -> Printf.sprintf "w %s" v.vname
  | Read_Mem(e) -> Printf.sprintf "R %s"
                     (Pretty.sprint ~width:80 (dn_exp () e))
  | Write_Mem(e) -> Printf.sprintf "W %s"
                      (Pretty.sprint ~width:80 (dn_exp () e))
  | File_IO -> "i/o"
  | Other -> "other"

module OrderedEffect =
struct
  type t = effect
  let compare = compare
end
module EffectSet = Set.Make(OrderedEffect)

let unresolved_warnings = ref StringSet.empty

let warn_unresolved_call f =
  let str = (Pretty.sprint ~width:80 (dn_exp () f)) in
  if StringSet.mem str !unresolved_warnings then
    ()
  else begin
    unresolved_warnings := StringSet.add str !unresolved_warnings ;
    debug "progeq: WARNING: cannot resolve call: %s\n" str
  end


(*
 * This Cil visitor collects up sets of effects. For example, "x=y"
 * should yield { write to x, read from y }.
 *
 * Complications:
 *
 * (1) break and continue. Moving them into the middle of a loop changes
 * the control flow.
 *
 * (2) Function calls. A function call may change global variables or
 * system state (e.g., printf). We recursively analyze called functions,
 * but memoize the results.
 *)
class collectEffects  (loop_count : int ref)
    (outset : EffectSet.t ref) (* returned value *)
    (handle_call : EffectSet.t ref -> Cil.exp -> unit)
  = object

    inherit nopCilVisitor
    method vvrbl va =
      outset := EffectSet.add (Read_Var(va)) !outset ;
      DoChildren

    method vstmt s =
      match s.skind with
      | Loop(_) ->
        incr loop_count ;
        ChangeDoChildrenPost(s, (fun s -> decr loop_count ; s))

      (* a statement with an _unguarded_ break or continue changes control
       * flow, and thus always splits up a partition *)
      | Break _
      | Continue _ ->
        if !loop_count <= 0 then begin
          outset := EffectSet.add (Other) !outset
        end ;
        DoChildren

      (* a goto changes control flow and thus always splits up a partition
       * because we are too lazy to do a full CFG analysis *)
      | Goto _
        (* | ComputedGoto _  *)
        -> outset := EffectSet.add (Other) !outset ; DoChildren

      | _ -> DoChildren

    method vlval l =
      ( match l with
        | Var(va),_ ->  outset := EffectSet.add (Read_Var(va)) !outset
        | Mem(e),_ ->   outset := EffectSet.add (Read_Mem(e)) !outset
      ) ; DoChildren

    method vinst i =
      let handle_lhs lhs =
        match lhs with
        | Var(va),_ ->  outset := EffectSet.add (Write_Var(va)) !outset
        | Mem(e),_ ->   outset := EffectSet.add (Write_Mem(e)) !outset
      in
      ( match i with
        | Set(lhs,_,_) ->           handle_lhs lhs
        | Call(Some(lhs),fe,_,_) -> handle_lhs lhs ; handle_call outset fe
        | Call((None),fe,_,_) ->    handle_call outset fe
        | Asm _ ->                  outset := EffectSet.add (Other) !outset
      ) ;
      DoChildren

  end

(* A simple visitor to collect up all of the statements below a given point
 * in the abstract syntax tree. *)
class collectStatements (out : (Cil.stmt list) ref) = object
  inherit nopCilVisitor
  method vstmt v =
    out := v :: !out ;
    DoChildren
end

(* Utility function to find all fundecs with a given name. *)
let collectFundecs (fname : string) file sofar =
  foldGlobals file (fun lst glob ->
      match glob with
      | GFun(fd,_) when fd.svar.vname = fname -> fd :: lst
      | _ -> lst
    ) sofar

(* The effects_cache should be keyed on "fundec.svar", not
 * "fundec.svar.vname", because you can have two different functions with
 * the same name (e.g., C's 'static' scope) but different effects. *)
let effects_cache = Hashtbl.create 255

exception TrustedFunction

(* Compute the effects (e.g., reads and writes to variables) of a function
 * call, given that one function may call another. This ends up being a
 * worklist algorithm (e.g., if A calls B and C, we process A first, and
 * then remove it from the worklist and handle B, ...).
 *
 * To avoid infinite regress, we memoize results. This is safe because we
 * only ever add effects (monotonic transfer function, finite height
 * lattice).
*)
let rec
  handle_call   (files : Cil.file StringMap.t) (* whole program *)
    (outset : EffectSet.t ref) (* output return value *)
    (f : Cil.exp) (* the called function *)
  =
  try
    let fundec_list = match f with
      | Lval(Var(v),NoOffset) when is_pure_function v.vname ->
        raise TrustedFunction
      | Lval(Var(v),NoOffset) when is_io_function v.vname ->
        outset := EffectSet.add (File_IO) !outset ;
        raise TrustedFunction
      | Lval(Var(v),NoOffset) ->
        StringMap.fold (fun filename elt acc ->
            collectFundecs v.vname elt acc
          ) files []
      | (Lval(_,_) as ptr) ->
        (try Ptranal.resolve_funptr ptr with _ -> raise Ptranal.UnknownLocation)
      | _ -> raise Ptranal.UnknownLocation
    in
    (if fundec_list = [] then raise Ptranal.UnknownLocation) ;
    let fundec_list = uniq fundec_list in
    let todo = List.filter (fun fd ->
        (* Don't re-compute the effects of Foo if we have already computed
         * the effects of Foo. *)
        not (Hashtbl.mem effects_cache fd.svar)
      ) fundec_list in
    List.iter (fun fd ->
        (* prevent infinite loops *)
        Hashtbl.add effects_cache fd.svar EffectSet.empty
      ) todo ;
    let finished = ref false in
    (*
     * Worklist algorithm: suppose functions A and B are mutually
     * recursive, and A writes {a} while B writes {b}. We're not done until
     * both A and B write {a,b}.
     *)
    while not !finished do
      finished := true ; (* "repeat until nothing changes" *)
      List.iter (fun fd ->
          let key = fd.svar in
        (*
        debug "progeq: handle_call:\t%s (%s:%d)\n" key.vname
          fd.svar.vdecl.file fd.svar.vdecl.line;
          *)
          let old_result = Hashtbl.find effects_cache key in
          let lc = ref 0 in
          let result = ref EffectSet.empty in
          let _ = visitCilFunction (new collectEffects lc result
                                     (handle_call files)) fd in
          if EffectSet.compare old_result !result <> 0 then begin
        (*
          EffectSet.iter (fun a ->
            debug "\t\told_result: %s\n" (effect_to_str a)
          ) old_result ;
          EffectSet.iter (fun a ->
            debug "\t\t   !result: %s\n" (effect_to_str a)
          ) !result ;
          *)
            finished := false ; (* something changed, so repeat *)
            Hashtbl.replace effects_cache key !result
          end
        ) todo ;
    done ;
    outset := List.fold_left (fun acc elt ->
        let key = elt.svar in
        EffectSet.union (Hashtbl.find effects_cache key) acc
      ) (EffectSet.empty) fundec_list ;
    ()

  with
  | TrustedFunction -> ()
  | Ptranal.UnknownLocation -> begin
      warn_unresolved_call f ;
      outset := EffectSet.add (Other) !outset
    end

(*
 * Given a Cil.block (fault space) and a list of effects associated with
 * the desired edit (e.g., "Insert X=Y" has {Write X, Read Y}), return
 * a partition (list of lists) of Cil.stmts such that all statements in the
 * same partition yield semantically equivalent programs when that edit is
 * applied ot them.
 *
 * Particular edit considered:
 *  Append
 *
 * Even approximating this requires that we handle the "strong update" or
 * "alias" problem. Consider:
 *
 *  L1:
 *      *ptr = 5;
 *  L2:
 *
 * If we want to insert "x = 0" at L1 and L2, those two programs are only
 * semantically equivalent if "ptr" cannot point to "x" (i.e., "ptr" and
 * "&x" cannot have the same value at run-time). We use an off-the-shelf
 * alias analysis to approximate this. If it fails, we just conservatively
 * assume that everything aliases.
 *)
exception CannotPartition

(* Can two expressions "*ptr1" and "*ptr2" alias? *)
let mem_mem_alias exp1 exp2 =
  try
    Ptranal.may_alias exp1 exp2
  with Ptranal.UnknownLocation | Not_found -> true

(* Can a variable "x" and an memory expression "*ptr" alias? *)
let var_mem_alias (varinfo : Cil.varinfo) (memexp : Cil.exp) : bool =
  let varexp = Lval(Var(varinfo),NoOffset) in
  mem_mem_alias varexp memexp

(* Given two effect sets (e.g., {read x, read y} and {write x}) determine
 * if a relevant dependency exists. Two effect sets can both read the
 * same variable without problem, but Read-Write or Write-Write mean
 * the resulting programs won't be equivalent so they can't be put in the
 * same partition (= equivalence class = element of the quotient space).
*)
let dependency_exists es1 es2 =
  EffectSet.exists (fun eff1 ->
      match eff1 with
      | Read_Var(s) ->
        EffectSet.exists (fun eff2 ->
            match eff2 with
            (* Simple case: if you read from X and I write to X, we
             * have a conflict. *)
            | Write_Var(s2) when s.vname = s2.vname -> true

            (* More complicated: if you read from X and I write to *ptr and
             * ptr can point to X, we have a conflict. *)
            | Write_Mem(exp2) when var_mem_alias s exp2 -> true

            | Other -> true
            | _ -> false
          ) es2
      | Write_Var(s) ->
        EffectSet.exists (fun eff2 ->
            match eff2 with
            | Write_Var(s2)
            | Read_Var(s2) when s.vname = s2.vname -> true
            | Write_Mem(exp2)
            | Read_Mem(exp2) when var_mem_alias s exp2 -> true
            | Other -> true
            | _ -> false
          ) es2
      | Read_Mem(exp1) ->
        EffectSet.exists (fun eff2 ->
            match eff2 with
            | Write_Mem(exp2) when mem_mem_alias exp1 exp2 -> true
            | Write_Var(s2) when var_mem_alias s2 exp1 -> true
            | Other -> true
            | _ -> false
          ) es2
      | Write_Mem(exp1) ->
        EffectSet.exists (fun eff2 ->
            match eff2 with
            | Write_Mem(exp2)
            | Read_Mem(exp2) when mem_mem_alias exp1 exp2 -> true
            | Read_Var(s2)
            | Write_Var(s2) when var_mem_alias s2 exp1 -> true
            | Other -> true
            | _ -> false
          ) es2
      | File_IO ->
        EffectSet.exists (fun eff2 ->
            match eff2 with
            | File_IO | Other -> true
            | _ -> false
          ) es2
      | Other -> true
    ) es1

(* Gather up all of the effects of a statement. Since a CIL statement can
 * be an entire 'if' or 'while', this can be more than you might think. *)
let effects_of_stmt files stmt =
  let result = ref EffectSet.empty in
  let lc = ref 0 in
  let _ = visitCilStmt (new collectEffects lc result
                         (handle_call files)) stmt in
  !result

let effects_of_stmtkind files skind =
  let stripped_stmt = {
    labels = [] ; skind = skind ; sid = 0; succs = [] ; preds = [] ;
  } in
  effects_of_stmt files stripped_stmt

(* The workhorse of this program equivalence approximation.
 * Given a CIL block (a list of statements) and an edit (represented by its
 * list of effects), partition that block into equivalence classes (each
 * partition is a list of statements) such that applying the edit to
 * two places within the same equivalence class would yield semantically
 * equivalent programs.
 *
 * High-level algorithm: If the block is the list [s1;s2;s3;s4], just walk
 * down that list. At each statement, check to see if it has a read-write
 * or write-write dependency with the edit. For example, if only s2
 * interferes with the edit, you end up with [s1] [s2;s3;s4]
 * (or [s1;s2] [s3;s4] depending on whether you're considering prepend or
 * append).
 *
 * "if" and "while" are special cases because they include control flow.
 * Because we cannot know which way an 'if' will go statically, we must
 * conservatively consider the dependenceis of both of its branches.
*)
let rec partition_block files block edit_effects
  : Cil.stmt list list =
  let stmts = block.bstmts in
  let final_result = ref [] in
  let current_partition = ref [] in
  let rec walk slist = match slist with
    | [] -> ()
    | stmt :: tl -> begin
        match stmt.skind with
        | Instr(il) ->
          let here_effects = ref EffectSet.empty in
          let lc = ref 0 in
          let _ =visitCilStmt (new collectEffects lc here_effects
                                (handle_call files)) stmt in
          if List.length il > 1 then begin
            debug "progeq: WARNING: instruction list size > 1\n"
          end ;
          if dependency_exists edit_effects !here_effects then begin
            final_result := !current_partition :: !final_result ;
            current_partition := [stmt]
          end else begin
            (* debugging: *)
      (*
        debug "\tNo Dep: %s\n" (Pretty.sprint ~width:80
          (dn_stmt () stmt)) ;
        EffectSet.iter (fun e ->
          debug "\t\tStmt: %s\n" (effect_to_str e)
        ) !here_effects ;
        EffectSet.iter (fun e ->
          debug "\t\tEdit: %s\n" (effect_to_str e)
        ) edit_effects ;
        *)
            (* no dep, add me to the current partition  *)
            current_partition := stmt :: !current_partition
          end ;
          walk tl

        | If(e1,b1,b2,_) ->
          let here_effects = ref EffectSet.empty in
          let lc = ref 0 in
          let _ =visitCilStmt (new collectEffects lc here_effects
                                (handle_call files)) stmt in
          let b1set = partition_block files b1 edit_effects in
          let b2set = partition_block files b2 edit_effects in
          final_result := b1set @ b2set @ !final_result ;
          if dependency_exists edit_effects !here_effects then begin
            final_result := !current_partition :: !final_result ;
            current_partition := [stmt]
          end else begin
            (* no dep, add me to the current partition  *)
            current_partition := stmt :: !current_partition
          end ;
          walk tl

        | Loop(b1,_,_,_) ->
          let here_effects = ref EffectSet.empty in
          let lc = ref 0 in
          let _ =visitCilStmt (new collectEffects lc here_effects
                                (handle_call files)) stmt in
          let b1set = partition_block files b1 edit_effects in
          final_result := b1set @ !final_result ;
          if dependency_exists edit_effects !here_effects then begin
            final_result := !current_partition :: !final_result ;
            current_partition := [stmt]
          end else begin
            (* no dep, add me to the current partition  *)
            current_partition := stmt :: !current_partition
          end ;
          walk tl

        | Block(b) ->
          walk b.bstmts ;
          walk tl

        | Return _
        | Break _
        | Continue _ -> ()

        | _ (* goto, switch, etc. *)
          -> raise CannotPartition

      end in
  try
    walk stmts ;
    final_result := !current_partition :: !final_result ;
    current_partition := [] ; (* documentation only *)
    !final_result
  with CannotPartition ->
    (* everything not already in 'final result' gets
     * put in its own partition! *)
    let all_stmts = ref [] in
    let _ = visitCilBlock (new collectStatements all_stmts) block in
    List.iter (fun s ->
        if List.exists (fun part -> List.mem s part) !final_result then
          ()
        else
          final_result := [s] :: !final_result
      ) !all_stmts ;
    !final_result

let alias_computed = ref false

(* Given an edit, partition the entire program (all the files) into
 * equivalence classes with respect to that edit.
 *
 * This is the primary entry point to this module: the rest of Genprog will
 * call 'partition' once per edit to access our services.
*)
let partition files edit_effects =
  if not !alias_computed then begin
    Ptranal.conservative_undefineds := true ;
    (* Ptranal.no_sub := true ; *)
    debug "progeq: computing alias analysis information\n" ;
    StringMap.iter (fun x f -> Ptranal.analyze_file f) files ;
    alias_computed := true
  end ;
  let partitions = ref [] in
  StringMap.iter (fun x file ->
      iterGlobals file (fun glob ->
          match glob with
          | GFun(fd,_) ->
            let this_partition = partition_block files fd.sbody edit_effects in
            partitions := this_partition @ !partitions
          | _ -> ()
        )
    ) files ;
  (* debugging *)
  (*
  EffectSet.iter (fun eff ->
    debug "\t\t%s\n" (effect_to_str eff)
  ) edit_effects ;
  let debug_partition p =
    List.iter (fun stmt ->
      debug "%s\n" (Pretty.sprint ~width:80 (dn_stmt () stmt))
    ) p
  in
  List.iter (fun part ->
    if part <> [] then begin
      debug "\tPARTITION\n" ;
      debug_partition part
    end
  ) !partitions ;
  *)
  !partitions
