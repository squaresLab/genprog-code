open Batteries
open Set
open Ref
open Map
open Utils
open Cil

let printer = Cilprinter.noLineCilPrinter

let stmt_str stmt = Pretty.sprint ~width:80 (printStmt printer () stmt) 
let exp_str exp = Pretty.sprint ~width:80 (printExp printer () exp) 

module OrderedExp = struct
  type t = Cil.exp
  let compare e1 e2 = 
    let e1' = exp_str e1 in
    let e2' = exp_str e2 in
      compare e1' e2'
end

module OrderedStmt = struct
  type t = Cil.stmt
  let compare s1 s2 = 
    let s1' = stmt_str s1 in 
    let s2' = stmt_str s2 in 
      compare s1' s2'
end
                                                         

module ExpSet = Set.Make(OrderedExp)
module StmtMap = Map.Make(OrderedStmt)
module StmtSet = Set.Make(OrderedStmt)


module OrderedStmtPair = struct
  type t = Cil.stmt * Cil.stmt
  let compare (s11,s12) (s21,s22) = 
    let s11' = stmt_str s11 in
    let s21' = stmt_str s21 in
      if s11' = s21' then begin
        let s12' = stmt_str s12 in
        let s22' = stmt_str s22 in
          compare s12' s22'
      end else compare s11' s21'
end

module StmtPairSet = Set.Make(OrderedStmtPair)

type predicate = Cil.exp
type predicates = ExpSet.t
type stmt_node = Cil.stmt
(*type guards = IfPreds of predicates | IfAlso of predicates | NoCondition*)
(* list * list, first is Do, second is insteadof? *)
type change = stmt_node list * stmt_node list 

type change_node =
    { change : change;
      guards : predicates ;
    }

let rec change_node_str node =
  let dothis,insteadof = node.change in 
    (* Hm, should be indented under IF, right? *)
  let str1 = 
    if not (ExpSet.is_empty node.guards) then begin
      (Printf.sprintf "IF: \n") ^
      (ExpSet.fold (fun exp accum -> Printf.sprintf "%s%s &&" accum (exp_str exp)) node.guards "\t\t")^"\n"
    end
    else "UNDER ALL CONDITIONS:"
  in
  let str2 =
    if (llen dothis) > 0 then 
      (Printf.sprintf "\tDO: \n")^
      (lfoldl (fun accum stmt -> Printf.sprintf "\t%s\t%s\n" accum (stmt_str stmt)) "" dothis)^"\n"
    else "\tDO: NOTHING\n"
  in
  let str3 = 
    if (llen insteadof) > 0 then 
      (Printf.sprintf "\tINSTEAD OF: \n")^
      (lfoldl (fun accum stmt -> Printf.sprintf "\t%s\t%s\n" accum (stmt_str stmt)) "" insteadof )^"\n"
    else "\tINSTEAD OF: NOTHING\n"
  in
    str1^str2^str3

let new_node c g = { change = c; guards = g}

type concrete_delta = change_node list 
    
type full_diff = {
  mutable fullid : int;
  rev_num : int;
  msg : string;
  mutable changes : (string * (change_node list StringMap.t)) list ; (* functions to change trees *)
  dbench : string
}

(* diff type and initialization *)

let diff_ht_counter = ref 0
let diffid = ref 0
let changeid = ref 0

let new_diff revnum msg changes benchmark = 
  {fullid = (post_incr diffid);rev_num=revnum;msg=msg; changes = changes; dbench = benchmark }
(*
let template_id = ref 0 
let new_template () = Ref.post_incr template_id

type template =
    { template_id : int ;
      diff : full_diff;
      change : change ;
      linestart : int ;
      lineend : int ;
      edits : change list ;
      names : StringSet.t ;
    }
*)
