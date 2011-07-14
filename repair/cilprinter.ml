(* 
 * Program Repair Prototype (v2) 
 *
 * Program Representation -- CIL C AST: To-String Pretty Printer
 *
 * Unfortunately, a "small flaw in CIL's character" means that we have to
 * duplicate a lot of the code from Cil.defaultCilPrinterClass in order
 * to override behavior and have it print to a Buffer instead of to an
 * out_channel. This separate source file is used to keep this
 * copy-and-paste monstrosity in its own little sandbox. 
 *)

open Global
open Cil
open Rep
open Pretty
module E = Errormsg

let width = 32767 

class toStringCilPrinterClass = object (self) 
  inherit defaultCilPrinterClass as super 
  val mutable currentFormals : varinfo list = []
  method private getLastNamedArgument (s:string) : exp =
    match List.rev currentFormals with 
      f :: _ -> Lval (var f)
    | [] -> 
        E.s (bug "Cannot find the last named argument when printing call to %s\n" s)

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
                let body = self#pBlock () f.sbody in
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

end 


class noLineToStringCilPrinterClass = object
  inherit toStringCilPrinterClass as super 
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
