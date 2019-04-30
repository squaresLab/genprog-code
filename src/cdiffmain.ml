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
 * Structural Diff on C Programs
 *
 * --generate: given two C files, produce a data file and a text patch file
 *   that can be used to turn one into the other
 *
 * --use: given the data file and some subset of the text file, apply that
 *   subset of the changes to turn the first file into (something like) the
 *   second file
 *
 * Used by Weimer's prototype GP project to post-mortem minimize a
 * candidate patch. Typically used in conjunction with delta-debugging
 * to produce a 1-minimal subset of the original patch that still has the
 * desired behavior.
 *)
open Pretty
open Printf
open Cil
open Global
open Minimization
open Cdiff

let counter = ref 1
let get_next_count () =
  let count = !counter in
  incr counter ;
  count
class numToZeroVisitor = object
  inherit nopCilVisitor
  method vstmt s = s.sid <- 0 ; DoChildren
end

let my_zero = new numToZeroVisitor

class numVisitor = object
  inherit nopCilVisitor
  method vstmt b =
    let count = get_next_count () in
    b.sid <- count ;
    DoChildren
end
let my_num = new numVisitor

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

let my_every = new everyVisitor

class minimizableCObject = object(self)
  inherit minimizableObject

  val filename = ref ""
  val base = ref None

  method load_file fname =
    Errormsg.hadErrors := false ;
    let file = Frontc.parse fname () in
    visitCilFileSameGlobals my_zero file;
    visitCilFileSameGlobals my_num file;
    visitCilFileSameGlobals my_every file;
    filename := fname;
    base := Some(file)

  method get_base () =
    match !base with
      Some(b) -> b
    | None -> abort "get_base called on empty minimizableCObject"

  method internal_structural_signature () =
    let base = self#get_base () in
    let result = ref StringMap.empty in
    let node_map =
      foldGlobals base (fun node_map g1 ->
          match g1 with
          | GFun(fd,l) ->
            let node_id, node_map = Cdiff.fundec_to_ast node_map fd in
            result := StringMap.add fd.svar.vname node_id !result; node_map
          | _ -> node_map
        ) (Cdiff.init_map())
    in
    (* now we have result, which maps function names to root node ids, and node_map *)
    let signature = StringMap.add !filename !result (StringMap.empty) in
    { signature = signature ; node_map = node_map}

  val min_script = ref None
  val min_patch = ref ""

  method construct_rep patch script =
    match patch with
      Some(p) -> min_patch := p
    | None ->
      begin
        match script with
          Some(cilfile_list,node_map) ->
          min_script := Some(cilfile_list, node_map)
        | None ->
          abort "cilrep#construct_rep called with nothing from which to construct the rep"
      end

  method output_to_disk () : unit = abort "output to disk not implemented"

  method is_max_fitness () =
    self#output_to_disk ();
    match (system "sh compile-run.sh") with
      Unix.WEXITED(0) -> true
    | _ -> false

  method copy () =
    match !base with
      None -> ({< base = ref None >})
    | Some(base) ->
      ({< base = ref (Some(copy base)) >})

end

let main () = begin
  Cil.initCIL () ;
  Random.self_init () ;
  debug_out := open_out "/dev/null";
  let filename = ref [] in
  let generate = ref false in
  let use = ref "" in
  let usageMsg = "Prototype Difference Minimizer\n" in
  let argDescr = [
    "--generate", Arg.Set generate, "generate diff script between two files";
    "--exp-diff", Arg.Set exp_diff_level,
    "perform diffX/delta-debugging at the expression level.  Default: false";
    "--verbose", Arg.Set verbose,
    "verbose printing.  Default: false"
    (* add minimization *)
  ] in
  let handleArg str =
    filename := str :: !filename
  in
  Arg.parse (Arg.align argDescr) handleArg usageMsg ;
  match !filename with
  | [two;one] when !generate -> begin
      let singleFile fname =
        let obj = new minimizableCObject in
        obj#load_file fname;
        obj, obj#structural_signature ()
      in
      let f1,sig1 = singleFile one in
      let f2,sig2 = singleFile two in
      let filemap1 = StringMap.find one sig1.signature in
      let filemap2 = StringMap.find two sig2.signature in
      let node_map = map_union sig1.node_map sig2.node_map in
      let result =
        StringMap.fold
          (fun funname1 t1  result ->
             let t2 = StringMap.find funname1 filemap2 in
             let m = Cdiff.mapping node_map t1 t2 in
             let s = Cdiff.generate_script node_map
                 (Cdiff.node_of_nid node_map t1)
                 (Cdiff.node_of_nid node_map t2) m
             in
             (funname1, s) :: result
          ) filemap1 []
      in
      liter (fun (funname,script) ->
          let as_string =
            lfoldl (fun str elt ->
                let as_string =
                  Printf.sprintf "%s %s %s\n" one funname
                    (Cdiff.edit_action_to_str node_map elt)
                in
                str^as_string
              ) "" script
          in
          if as_string <> "" then
            debug "final script: {%s}\n" as_string
        ) (List.rev result)
    end
  | _ -> ()
end ;;

main () ;;
