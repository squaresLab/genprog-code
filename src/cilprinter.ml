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
(** CIL C AST: To-string pretty printer.  Unfortunately, a "small flaw in
    CIL's character" means that we have to duplicate a lot of the code from
    Cil.defaultCilPrinterClass in order to override behavior and have it print to
    a Buffer instead of to an out_channel. This separate source file is used to
    keep this copy-and-paste monstrosity in its own little sandbox.  *)

open Global
open Cil
open Rep
open Pretty
module E = Errormsg

let width = 32767 


(** the xformRepVisitor applies a transformation function to a C AST.  Used to
   implement the patch representation for C programs, where the original program
   is transformed by a sequence of edit operations only at compile time. 

    @param xform function that potentially changes a Cil.stmt to return a new
    Cil.stmt
*)
class xformRepVisitor
  (xform : Cil.fundec -> Cil.stmt -> Cil.stmt) 
  (bxform : Cil.fundec -> Cil.fundec) = object(self)
  inherit nopCilVisitor

  val mutable current = dummyFunDec

  method vfunc fd =
    let old_fd = current in
      current <- fd;
      ChangeDoChildrenPost(fd, (fun fd -> current <- old_fd; bxform fd))

  method vstmt stmt = ChangeDoChildrenPost(stmt, (fun stmt -> xform current stmt))
    
end

(**/**)
let nop_xform _ x = x 
let nop_bxform b = b
let my_xform = new xformRepVisitor
(**/**)

let prep_cil_file_for_output xforms bxform final_visitor cilfile =
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
  let cilfile = copy cilfile in
    visitCilFile (my_xform nop_xform bxform) cilfile;
    List.iter (fun xform ->
      visitCilFile (my_xform xform nop_bxform) cilfile
    ) xforms;
    visitCilFile final_visitor cilfile;
    cilfile


let output_cil_file
    ?(xforms = [])
    ?(bxform = nop_bxform)
    ?(final_visitor = new nopCilVisitor)
    (outfile : string)
    (cilfile : Cil.file) = 
  let cilfile : Cil.file =
    prep_cil_file_for_output xforms bxform final_visitor cilfile
  in 
  let fout = open_out outfile in
  let old_directive_style = !Cil.lineDirectiveStyle in
    Cil.lineDirectiveStyle := None ; 
    iterGlobals cilfile (dumpGlobal defaultCilPrinter fout);
    Cil.lineDirectiveStyle := old_directive_style;
    close_out fout

(** @param xforms a list of transformation to apply to the input file;
    optional (default is empty list)
    @param file Cil.file to print to string
    @raise Fail("memory overflow") for very large files, at least in theory. *)
let output_cil_file_to_string
    ?(xforms = [])
    ?(bxform = nop_bxform) 
    ?(final_visitor = new nopCilVisitor)
    (cilfile : Cil.file) = 
  let fname, chan = Filename.open_temp_file "" ".c" in
  output_cil_file ~xforms ~bxform ~final_visitor fname cilfile;
  close_out chan;

  let body = file_to_string fname in
  Sys.remove fname;
  body
