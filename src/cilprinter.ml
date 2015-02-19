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

class toStringCilPrinterClass 
        = object (self) 
  inherit defaultCilPrinterClass
  (**/**)
  val mutable currentFormals : varinfo list = []

  method private pFunDecl () f =
    self#pVDecl () f.svar
  ++  line
  ++ text "{ "
  ++ (align
      (* locals. *)
      ++ (docList ~sep:line (fun vi -> self#pVDecl () vi ++ text ";") 
            () f.slocals)
      ++ line ++ line
      (* the body *)
      ++ ((* remember the declaration *) currentFormals <- f.sformals; 
        let body = self#pBlock () (f.sbody) in
          currentFormals <- [];
          body))
  ++ line
  ++ text "}"

  (* dump initializers to a file. *)
  method bInit (out: Buffer.t) (ind: int) (i: init) = 
    (* Dump an array *)
    let dumpArray (bt: typ) (il: 'a list) (getelem: 'a -> init) = 
      let onALine = (* How many elements on a line *)
        match unrollType bt with TComp _ | TArray _ -> 1 | _ -> 4
      in
      let rec outputElements (isfirst: bool) (room_on_line: int) = function
      [] -> Buffer.add_string out "}"
        | (i: 'a) :: rest -> 
          if not isfirst then Buffer.add_string out ", ";
          let new_room_on_line = 
            if room_on_line == 0 then begin 
              Buffer.add_string out "\n"; 
              Buffer.add_string out (String.make ind ' ');
              onALine - 1
            end else 
              room_on_line - 1
          in
            self#bInit out (ind + 2) (getelem i);
            outputElements false new_room_on_line rest
      in
        Buffer.add_string out "{ ";
        outputElements true onALine il
    in
      match i with 
        SingleInit e -> 
          Buffer.add_string out (Pretty.sprint ~width
                                   (indent ind (self#pExp () e)))
      | CompoundInit (t, initl) -> begin 
        match unrollType t with 
          TArray(bt, _, _) -> 
            dumpArray bt initl (fun (_, i) -> i)
        | _ -> 
          (* Now a structure or a union *)
          Buffer.add_string out 
            (Pretty.sprint ~width (indent ind (self#pInit () i)))
      end

  method bGlobal (out: Buffer.t) (g: global) : unit = 
    (* For all except functions and variable with initializers, use the 
     * pGlobal *)
    match g with 
      GFun (fdec, l) -> 
        (* If the function has attributes then print a prototype because 
         * GCC cannot accept function attributes in a definition *)
        let oldattr = fdec.svar.vattr in
        let proto = 
          if oldattr <> [] then 
            (self#pLineDirective l) ++ (self#pVDecl () fdec.svar) 
            ++ chr ';' ++ line
          else nil in
          Buffer.add_string out 
            (Pretty.sprint ~width 
               (proto ++ (self#pLineDirective ~forcefile:true l)));
         (* Temporarily remove the function attributes *)
          fdec.svar.vattr <- [];
          Buffer.add_string out (Pretty.sprint ~width (self#pFunDecl () fdec));
          fdec.svar.vattr <- oldattr;
          Buffer.add_string out "\n" 

    | GVar (vi, {init = Some i}, l) -> begin
      let str = Pretty.sprint ~width 
        (self#pLineDirective ~forcefile:true l ++
           self#pVDecl () vi
         ++ text " = " 
         ++ (let islong = 
               match i with
                 CompoundInit (_, il) when List.length il >= 8 -> true
               | _ -> false 
             in
               if islong then 
                 line ++ self#pLineDirective l ++ text "  " 
               else nil)) in
        Buffer.add_string out str ; 
        self#bInit out 3 i;
        Buffer.add_string out ";\n" 
    end

    | g -> 
      Buffer.add_string out 
        (Pretty.sprint ~width (self#pGlobal () g))

(**/**)
end 
(* toStringPrinterClass is now noLine via setting of lineDirective *)
(* similarly, we don't need noLineCilPrinterClass, because lineDirective *)

let prep_cil_file_for_output xforms bxform cilfile =
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
    List.iter (fun xform ->
      visitCilFile(my_xform xform bxform) cilfile
    ) xforms;
    cilfile


let output_cil_file ?(xforms = []) ?(bxform = nop_bxform) (outfile : string) (cilfile : Cil.file) = 
  let cilfile : Cil.file = prep_cil_file_for_output xforms bxform cilfile in 
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
let output_cil_file_to_string ?(xforms = []) ?(bxform = nop_bxform) 
    (cilfile : Cil.file) = 
  let cilfile = prep_cil_file_for_output xforms bxform cilfile in
  let old_directive_style = !Cil.lineDirectiveStyle in 
    Cil.lineDirectiveStyle := None;
  let buf = Buffer.create 1024 in   
  let printer = new toStringCilPrinterClass in 
    iterGlobals cilfile (printer#bGlobal buf) ;
    Cil.lineDirectiveStyle := old_directive_style;
    Buffer.contents buf 
