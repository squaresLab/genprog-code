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
(** CIL C AST: To-string pretty printer.  *)

open Global
open Cil
open Rep
open Pretty
module E = Errormsg

let width = 32767

let prep_cil_file_for_output cilfile =
  let cilfile =
    if !is_valgrind then begin
      (* CLG: GIANT HACK FOR VALGRIND BUGS *)
      {cilfile with globals =
                      lfilt (fun g ->
                          match g with
                            GVarDecl(vinfo,_) ->
                            (match vinfo.vstorage with
                               Extern when vinfo.vname = "__builtin_longjmp" -> false
                             | _ -> true)
                          | _ -> true) cilfile.globals}
    end else cilfile
  in
  let cilfile =
    {cilfile with globals =
                    lfilt (fun g ->
                        match g with
                        | GVarDecl(vi,l) when
                            (not !printCilAsIs && Hashtbl.mem Cil.builtinFunctions vi.vname) ->
                          (* This prevents the printing of all of those 'compiler built-in'
                           * commented-out function declarations that always appear at the
                           * top of a normal CIL printout file. *)
                          false
                        | _ -> true) cilfile.globals}
  in
  cilfile


let output_cil_file (outfile : string) (cilfile : Cil.file) =
  let cilfile : Cil.file = prep_cil_file_for_output cilfile in
  let fout = open_out outfile in
  let old_directive_style = !Cil.lineDirectiveStyle in
  Cil.lineDirectiveStyle := None ;
  Errormsg.hadErrors := false ;
  begin try
      iterGlobals cilfile (dumpGlobal defaultCilPrinter fout) ;
    with Errormsg.Error ->
      close_out fout ;
      let fout = open_out outfile in
      Printf.fprintf fout "#error \"Cilprinter.output_cil_file failed!\"\n";
      flush fout
  end ;
  Cil.lineDirectiveStyle := old_directive_style;
  close_out fout

(** @param file Cil.file to print to string
    @raise Fail("memory overflow") for very large files, at least in theory. *)
let output_cil_file_to_string (cilfile : Cil.file) =
  let fname, chan = Filename.open_temp_file "" ".c" in
  output_cil_file fname cilfile;
  close_out chan;

  let body = file_to_string fname in
  Sys.remove fname;
  body
