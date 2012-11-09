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
(** this implements support for template-based repairs.  Initially templates
    were only supported in [Cilrep.cilRep], though any representation can be
    extended to handle them.  However, this means much of the concrete code in
    this module is CIL-specific *)
open Cil
open Global

type hole_type = Stmt_hole | Exp_hole | Lval_hole
(* Ref: referenced in the hole named by the string.  InScope: all variables at
   this hole must be in scope at the hole named by the string *)
(* matches: statement looks like this! *)
type constraints =  Fault_path | Fix_path | Ref of string | InScope of string 
                    | ExactMatches of string | FuzzyMatches of string | HasType of string (* name? *)
                    | IsLocal | IsGlobal | HasVar of string

module OrderedConstraint = 
struct
  type t = constraints
  let compare c1 c2 = 
    if c1 = c2 then 0 else 
      match c1,c2 with
      | Fault_path,_ -> -1
      | _,Fault_path -> 1
      | Fix_path,_ -> -1
      | _,Fix_path -> 1
      | HasVar _ ,_ -> -1
      | _,HasVar _ -> 1
      | Ref(i1),Ref(i2)
      | Ref(i1),InScope(i2)
      | InScope(i1),Ref(i2)
      | InScope(i1),InScope(i2)  -> compare i1 i2
      | _,_ -> 0
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
          Stmt_hole, Stmt_hole
        | Exp_hole, Exp_hole
        | Lval_hole, Lval_hole -> compare cons1 cons2
        | Stmt_hole, Exp_hole
        | Stmt_hole, Lval_hole
        | Exp_hole, Lval_hole -> 1
        | Exp_hole, Stmt_hole
        | Lval_hole,Stmt_hole
        | Lval_hole, Exp_hole -> -1
end

module HoleSet = Set.Make(OrderedHole)

type filled = hole_type * int * int option

type hole_info =
    {
      hole_id : string;
      htyp : hole_type;
      constraints : ConstraintSet.t
    }

type 'a template = 
    {
      template_name : string;
      hole_constraints : hole_info StringMap.t;
      hole_code_ht : (string, 'a) Hashtbl.t ;
    }


(*** CilRep-specific template stuff *)


let hole_regexp = Str.regexp "__hole[0-9]+__"

exception FoundIt of string

(** collectConstraints constraints_ht code_ht template_file_name processes the
    code in template_file_name to construct the constraints and code hashtable,
    used when templates are instantiated during the mutation process *)
(* this can fail if the input template file is corrupted or has unexpected
   elements in the template specifications *)

class collectConstraints 
  template_constraints_ht template_code_ht template_name = object
    inherit nopCilVisitor

    method vfunc fundec =
      let hole_ht = hcreate 10 in
      let holes = 
        lfilt (fun varinfo -> Str.string_match hole_regexp varinfo.vname 0) 
          fundec.slocals in
        liter
          (fun varinfo ->
            let htyp =
              let [Attr(_,[AStr(typ)])] = 
                filterAttributes "holetype" varinfo.vattr in
                match typ with
                  "stmt" -> Stmt_hole
                | "lval" -> Lval_hole
                | "exp" -> Exp_hole
                | _ -> failwith "Unexpected value in htype value"
            in
            let constraints = 
              lfoldl
                (fun constraints attr ->
                  match attr with
                    Attr("constraint", [AStr("fault_path")]) -> 
                      ConstraintSet.add Fault_path constraints
                  | Attr("constraint", [AStr("fix_path")]) -> 
                    ConstraintSet.add Fix_path constraints
                  | Attr("inscope", [AStr(v)]) -> 
                    ConstraintSet.add (InScope(v)) constraints
                  | Attr("reference", [AStr(v)]) -> 
                    ConstraintSet.add (Ref(v)) constraints
                  | Attr("exactmatches", [AStr(v)]) ->
                    ConstraintSet.add (ExactMatches(v)) constraints
                  | Attr("fuzzymatches", [AStr(v)]) ->
                    ConstraintSet.add (FuzzyMatches(v)) constraints
                  | Attr("hasvar",[AStr(v)]) ->
                    ConstraintSet.add(HasVar(v)) constraints
                  | Attr("hastype",[AStr(v)]) ->
                    ConstraintSet.add(HasType(v)) constraints
                  | _ -> constraints
                ) ConstraintSet.empty varinfo.vattr
            in
              hrep hole_ht varinfo.vname 
                { hole_id=varinfo.vname; htyp=htyp; constraints=constraints})
          holes;
        template_name := fundec.svar.vname;
        hadd template_constraints_ht !template_name hole_ht;
        DoChildren
          
    method vblock block =
      match block.battrs with
        [] -> DoChildren
      | lst ->
        let hole_ht = hfind template_constraints_ht !template_name in
        let holes = 
          hfold (fun k -> fun v -> fun lst -> k :: lst) hole_ht [] in
          try
            liter
              (fun attr ->
                match attr with
                  Attr(name,_) ->
                    liter (fun hole -> 
                      if ("__"^name^"__") = hole then 
                        raise (FoundIt(hole))
                    ) holes
              ) block.battrs;
            DoChildren
          with FoundIt(holename) ->
            begin
              let newattrs = dropAttribute ("__"^holename^"__") block.battrs in
              let code_ht = ht_find template_code_ht !template_name 
                (fun _ -> hcreate 10) in
                hadd code_ht holename 
                  ({ block with battrs=newattrs });
                hrep template_code_ht !template_name code_ht;
                DoChildren
            end
  end

(* convenience record types, also for cilrep template solving *)

type ('a,'b) solver = {
  empty : unit -> 'a ;
  is_empty : 'a -> bool ;
  all_fault : unit -> 'a ;
  all_fix : unit -> 'a ;
  all : unit -> 'a;
  filter : ('b -> bool) -> 'a -> 'a ;
  intersect : 'a -> 'a -> 'a
}

