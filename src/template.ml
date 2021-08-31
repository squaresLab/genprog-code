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
(** this implements support for template-based repairs.  Initially templates
    were only supported in [Cilrep.cilRep], though any representation can be
    extended to handle them.  However, this means much of the concrete code in
    this module is CIL-specific *)
open Cil
open Global


let templates_file = ref ""
let template_cache_file = ref ""

let _ =
  options := !options @ [
      "--templates", Arg.Set_string templates_file,
      " Use repair templates; read from file X.  Default: none";
      "--template-cache", Arg.Set_string template_cache_file,
      "save the template computations to avoid wasting time." ;
    ]

(* Note that lval is used improperly, here, since an lval is technically the
   left-hand-side of any expression, and I use it to mean a variable name.
   fixme: possibly fix that at some point?
   Additionally, I deal with the lvals by manipulating them as sets of integer
   ids instead of the more general names, mostly for convenience, but that may
   be worth a fixme too at some point.
*)
type hole_type = HStmt | HExp | HLval
(* differences from original: eliminated fix_path and fault_path constraints.
   If it's a hole for an atom, it needs to come from the fix atom pool; if it's a
   template, it needs to be instantiated in the fault space.  The only question
   is how to refer to statements, variables, etc that are in those places (like
   the position at which a template is being instantiated).  *)
(* I might add these back as the code comes together, or someone else can,
   they're easy to implement *)
(* also eliminated inscope, which now implicitly applies to anything, and means
   that all variables in a hole must be in scope at the instantiation position *)
(* Ref: referenced in the hole named by the string.*)
type constraints =  Ref of string
                 | HasType of string (* name *)
                 | IsLocal | IsGlobal | HasVar of string

module OrderedConstraint =
struct
  type t = constraints
  (* possible FIXME: it might make sense to make this a real comparision *)
  let compare = compare
end

module ConstraintSet = Set.Make(OrderedConstraint)

type hole = hole_type * ConstraintSet.t

module OrderedHole =
struct
  type t = hole
  let compare h1 h2 =
    match h1, h2 with
      (ht1,cons1),(ht2,cons2) ->
      match ht1,ht2 with
        HStmt, HStmt
      | HExp, HExp
      | HLval, HLval -> compare cons1 cons2
      | HStmt, HExp
      | HStmt, HLval
      | HExp, HLval -> 1
      | HExp, HStmt
      | HLval,HStmt
      | HLval, HExp -> -1
end

module HoleSet = Set.Make(OrderedHole)

type filled = hole_type * int * int option

type hole_info =
  { (* holes are implicitly named, in the sense that they're keyed in the map
       by their names, so there's no need to track the name here *)
    htyp : hole_type;
    constraints : ConstraintSet.t
  }

type 'a template =
  {
    template_name : string;
    (* fixme: add possible constraints on instantiation position besides
       type.  Not sure what this will do if you try... *)
    (* another possible fixme: right now, position hole_type must be a stmt *)
    position : hole_type ;
    hole_constraints : hole_info StringMap.t;
    template_code : 'a;
  }

(* a instantiated template is instantiated at a location,
   and whatever was already there is replaced by the result of filling in the
   holes appropriately *)

(*** CilRep-specific template stuff *)


let hole_regexp = Str.regexp "__hole[0-9]+__"

exception FoundIt of string

class collectTemplates returnTemplates = object
  inherit nopCilVisitor

  val mutable template_name = ""
  val mutable hole_info = StringMap.empty
  val mutable ptyp = HStmt
  val mutable pname = ""

  method vfunc fundec =
    let gettyp attrs =
      let [Attr(_,[AStr(typ)])] =
        filterAttributes "type" attrs in
      match typ with
        "stmt" -> HStmt
      | "lval" -> HLval
      | "exp" -> HExp
      | _ -> failwith "Unexpected type value"
    in
    let _ = (* init *)
      template_name <- fundec.svar.vname;
      hole_info <- StringMap.empty;
      ptyp <- HStmt;
      pname = "";
    in
    let [position] =
      lfilt (fun varinfo ->
          match varinfo.vtype with
          (* possible FIXME: check that this works *)
            TNamed(tinfo,_) -> tinfo.tname = "position"
          | _ -> false) fundec.slocals in
    pname <- position.vname;
    debug "pname: %s\n" pname;
    ptyp <- gettyp position.vattr;

    let holes =
      lfilt (fun varinfo ->
          match varinfo.vtype with
            TNamed(tinfo,_) -> tinfo.tname = "hole"
          | _ -> false) fundec.slocals
    in
    liter
      (fun varinfo ->
         let htyp = gettyp varinfo.vattr in
         let constraints =
           lfoldl
             (fun constraints attr ->
                match attr with
                | Attr("reference", [AStr(v)]) ->
                  ConstraintSet.add (Ref(v)) constraints
                | Attr("hastype",[AStr(v)]) ->
                  ConstraintSet.add(HasType(v)) constraints
                | Attr("local",[]) ->
                  ConstraintSet.add (IsLocal) constraints
                | Attr("global",[]) ->
                  ConstraintSet.add (IsGlobal) constraints
                | Attr("hasvar",[AStr(v)]) ->
                  ConstraintSet.add (HasVar(v)) constraints
                | Attr("type",_) ->
                  (* we dealt with this one already when we got type, so ignore it *)
                  constraints
             ) ConstraintSet.empty varinfo.vattr
         in
         hole_info <- StringMap.add varinfo.vname
             ({htyp=htyp; constraints=constraints}) hole_info)
      holes;
    DoChildren

  (* I kind of think it may be possible to do something like, all code b/f the
     instantiation position should match the surrounding context.  But that will
     be hard to implement for now so I'm ignoring the thought *)
  method vblock block =
    match block.battrs with
      [] -> DoChildren
    | lst ->
      let posattr = filterAttributes pname block.battrs in
      if (llen posattr) > 0 then begin
        let newattrs = dropAttribute pname block.battrs in
        let template =
          { template_name = template_name;
            position = ptyp;
            hole_constraints = hole_info;
            template_code = {block with battrs=newattrs} }
        in
        returnTemplates := template :: !returnTemplates;
        SkipChildren
      end else DoChildren

end
