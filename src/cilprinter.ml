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

let nop_xform x = x 
let nop_bxform b = b

class toStringCilPrinterClass 
        (xform : Cil.stmt -> Cil.stmt) 
        (bxform : Cil.fundec -> Cil.fundec) (* fundec body transform *) 
        = object (self) 
  inherit defaultCilPrinterClass as super 
  (**/**)
  val mutable currentFormals : varinfo list = []
  method private getLastNamedArgument (s:string) : exp =
    match List.rev currentFormals with 
      f :: _ -> Lval (var f)
    | [] -> 
      E.s 
        (bug "Cannot find the last named argument when printing call to %s\n" s)

  val mutable printInstrTerminator = ";"

  method pInstr () (i : instr) = 
    match i with
    (* In cabs2cil we have dropped the last argument in the call to 
     * __builtin_va_start and __builtin_stdarg_start. *)
    | Call(None, Lval(Var vi, NoOffset), [marker], l) 
        when ((vi.vname = "__builtin_stdarg_start" ||
            vi.vname = "__builtin_va_start") && not !printCilAsIs) -> 
      if currentFormals <> [] then begin
        let last = self#getLastNamedArgument vi.vname in
          self#pInstr () (Call(None,Lval(Var vi,NoOffset),[marker; last],l))
      end
      else begin
        (* We can't print this call because someone called pInstr outside 
           of a pFunDecl, so we don't know what the formals of the current
           function are.  Just put in a placeholder for now; this isn't 
           valid C. *)
        self#pLineDirective l
        ++ dprintf 
          "%s(%a, /* last named argument of the function calling %s */)"
          vi.vname self#pExp marker vi.vname
        ++ text printInstrTerminator
      end
    (* In cabs2cil we have dropped the last argument in the call to 
     * __builtin_next_arg. *)
    | Call(res, Lval(Var vi, NoOffset), [ ], l) 
        when vi.vname = "__builtin_next_arg" && not !printCilAsIs -> begin
          let last = self#getLastNamedArgument vi.vname in
            self#pInstr () (Call(res,Lval(Var vi,NoOffset),[last],l))
        end
    | _ -> super#pInstr () i 

  method private pStmtNext (next: Cil.stmt) () (s: Cil.stmt) =
    let s = xform s in 
      (* print the labels *)
      ((docList ~sep:line (fun l -> self#pLabel () l)) () s.labels)
      (* print the statement itself. If the labels are non-empty and the
       * statement is empty, print a semicolon  *)
      ++ 
        (if s.skind = Instr [] && s.labels <> [] then
            text ";"
         else
            (if s.labels <> [] then line else nil) 
            ++ self#pStmtKind next () s.skind)

  (* The pBlock will put the unalign itself *)
  method pBlock () (blk: block) = 
    let rec dofirst () = function
    [] -> nil
      | [x] -> self#pStmtNext invalidStmt () x
      | x :: rest -> dorest nil x rest
    and dorest acc prev = function
    [] -> acc ++ (self#pStmtNext invalidStmt () prev)
      | x :: rest -> 
        dorest (acc ++ (self#pStmtNext x () prev) ++ line)
          x rest
    in
      (* Let the host of the block decide on the alignment. The d_block will 
       * pop the alignment as well  *)
      text "{" 
      ++ 
        (if blk.battrs <> [] then 
            self#pAttrsGen true blk.battrs
         else nil)
      ++ line
      ++ (dofirst () blk.bstmts)
      ++ unalign ++ line ++ text "}"

  method private pFunDecl () f =
    let f = bxform f in 
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

  (* A general way of printing lists of attributes *)
  method private pAttrsGen (block: bool) (a: attributes) = 
    (* Scan all the attributes and separate those that must be printed inside 
     * the __attribute__ list *)
    let rec loop (in__attr__: doc list) = function
    [] -> begin 
      match in__attr__ with
        [] -> nil
      | _ :: _->
            (* sm: added 'forgcc' calls to not comment things out
             * if CIL is the consumer; this is to address a case
             * Daniel ran into where blockattribute(nobox) was being
             * dropped by the merger
             *)
        (if block then 
            text (" " ^ (forgcc "/*") ^ " __blockattribute__(")
         else
            text "__attribute__((")
          
        ++ (docList ~sep:(chr ',' ++ break)
              (fun a -> a)) () in__attr__
        ++ text ")"
        ++ (if block then text (forgcc "*/") else text ")")
    end
      | x :: rest -> 
        let dx, ina = self#pAttr x in
          if ina then 
            loop (dx :: in__attr__) rest
          else if dx = nil then
            loop in__attr__ rest
          else
            dx ++ text " " ++ loop in__attr__ rest
    in
    let res = loop [] a in
      if res = nil then
        res
      else
        text " " ++ res ++ text " "

end 


class noLineToStringCilPrinterClass 
        (xform : Cil.stmt -> Cil.stmt) 
        (bxform : Cil.fundec -> Cil.fundec) 
        = object
  inherit toStringCilPrinterClass xform bxform as super 
  method pGlobal () (g:global) : Pretty.doc = 
    match g with 
    | GVarDecl(vi,l) when
        (not !printCilAsIs && Hashtbl.mem Cil.builtinFunctions vi.vname) -> 
      (* This prevents the printing of all of those 'compiler built-in'
       * commented-out function declarations that always appear at the
       * top of a normal CIL printout file. *) 
      Pretty.nil 
    | _ -> super#pGlobal () g

  method pLineDirective ?(forcefile=false) l = 
    Pretty.nil
end 

class noLineCilPrinterClass = object
  inherit defaultCilPrinterClass as super 
  method pGlobal () (g:global) : Pretty.doc = 
    match g with 
    | GVarDecl(vi,l) when
        (not !printCilAsIs && Hashtbl.mem Cil.builtinFunctions vi.vname) -> 
      (* This prevents the printing of all of those 'compiler built-in'
       * commented-out function declarations that always appear at the
       * top of a normal CIL printout file. *) 
      Pretty.nil 
    | _ -> super#pGlobal () g

  method pLineDirective ?(forcefile=false) l = 
    Pretty.nil
end 

let noLineCilPrinter = new noLineCilPrinterClass 
let toStringCilPrinter = new toStringCilPrinterClass  
let noLineToStringCilPrinter = new noLineToStringCilPrinterClass 

let output_cil_file_to_channel (fout : out_channel) (cilfile : Cil.file) = 
  iterGlobals cilfile (dumpGlobal noLineCilPrinter fout) 

let output_cil_file (outfile : string) (cilfile : Cil.file) = 
  let fout = open_out outfile in
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
	end else cilfile in

    output_cil_file_to_channel fout cilfile ;
    close_out fout

(** @param xform a transformation to apply to the input file; optional (default
    is nop)
    @param file Cil.file to print to string
    @raise Fail("memory overflow") for very large files, at least in theory. *)
let output_cil_file_to_string ?(xform = nop_xform) ?(bxform = nop_bxform) 
    (cilfile : Cil.file) = 
  (* Use the Cilprinter.ml code to output a Cil.file to a Buffer *) 
  let cilfile = 
    (* CLG: GIANT HACK FOR VALGRIND BUGS *)
	if !is_valgrind then begin
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
  let buf = Buffer.create 10240 in   
  let printer = noLineToStringCilPrinter xform bxform in 
    iterGlobals cilfile (printer#bGlobal buf) ;
    Buffer.contents buf 
