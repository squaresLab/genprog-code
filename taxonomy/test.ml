open Set
open Cil
open Global

type hole_type = Stmt_hole | Exp_hole | Lval_hole
(* Ref: referenced in the hole named by the string.  InScope: all variables at
   this hole must be in scope at the hole named by the string *)
(* matches: statement looks like this! *)
type constraints =  Fault_path | Fix_path | Ref of string | InScope of string | Matches of int
module StringMap = Map.Make(OrderedString)

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
      | Ref(i1),Ref(i2)
      | Ref(i1),InScope(i2)
      | InScope(i1),Ref(i2)
      | InScope(i1),InScope(i2) -> compare i1 i2
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
      hole_code_ht : (string, 'a) Hashtbl.t
    }


(*** CilRep-specific template stuff *)


let hole_regexp = Str.regexp "__hole[0-9]+__"

let exp_str exp = Pretty.sprint ~width:80 (dn_exp () exp) 

module OrderedExp = struct
  type t = Cil.exp
  let compare e1 e2 = 
    let e1' = exp_str e1 in
    let e2' = exp_str e2 in
      compare e1' e2'
end
module ExpSet = Set.Make(OrderedExp)

type predicates = ExpSet.t
type stmt_node = int * Cil.stmt

type change_node =
    {
      change_id : int;
      file_name : string;
      function_name : string;
      add : stmt_node list;
      delete : stmt_node list;
      guards : predicates ;
    }

class replaceLvalsVisitor lvalholes = object
  inherit nopCilVisitor
    
  method vexpr exp = 
    match exp with
    | Lval(Var(vinfo),o) when (Hashtbl.mem lvalholes vinfo.vname) ->
      let str = Printf.sprintf "__hole%d___.var" (Hashtbl.find lvalholes vinfo.vname) in
      let newexp = Formatcil.cExp str [] in
        ChangeTo(newexp)
    | _ -> DoChildren

end 

let alpha_ht = Hashtbl.create 10
let name_id = ref 0

class alphaRenameVisitor = object
  inherit nopCilVisitor

  method vinst i =
    match i with
      Call(l,e1,elist,l2) ->
        let copy = copy e1 in
          ChangeDoChildrenPost([i], 
                               (fun i ->
                                 match i with
                                   [Call(l,foo,elist,l2)] -> 
                                     [Call(l,copy,elist,l2)]
                                 | _ -> i))
    | _ -> DoChildren

  method vvrbl varinfo =
    let new_name = 
      if Hashtbl.mem alpha_ht varinfo.vname then
        Hashtbl.find alpha_ht varinfo.vname 
      else begin
        incr name_id; 
        "___alpha"^(string_of_int !name_id)
      end
    in
      debug "adding vname: %s new_name: %s\n" varinfo.vname new_name;
      Hashtbl.add alpha_ht varinfo.vname new_name;
      varinfo.vname <- new_name; 
      SkipChildren
end

let my_rename = new alphaRenameVisitor

(*
let alpha_rename change =
  Hashtbl.clear alpha_ht ;
  let predicates = ExpSet.elements change.guards in 
  let predicates = List.map (fun exp -> visitCilExpr my_alpha exp) predicates in
  let adds = List.map (fun (n,stmt) -> n,visitCilStmt my_alpha stmt) change.add in
  let dels = List.map (fun (n,stmt) -> n,visitCilStmt my_alpha stmt) change.delete in
(*    {change with guards = (ExpSet.of_enum (List.enum predicates)); add = adds; delete = dels }*) ()*)
  
let convert_change_to_template change = 
  let hole_num = ref 2 in 
  let hole_ht = Hashtbl.create 10 in
    Hashtbl.add hole_ht "__hole1__"
      {hole_id = "__hole1__"; 
       htyp = Stmt_hole; 
       constraints = (ConstraintSet.singleton Fault_path)};


    let all_add_vars =
      let add_vars = ref (StringSet.empty) in
      let myv = new collectVarVisitor add_vars in 
      List.iter 
        (fun (_,stmt) -> ignore(visitCilStmt myv stmt))
        change.add; !add_vars in

    (* all variables in guards are holes, only those that overlap with
       statements are replaced in the statement *)

    let guards = ExpSet.elements change.guards in

    let guards =

_vars = 
      List.map (fun g ->
        let constraint_vars = ref (StringSet.empty) in
        let myv = new collectVarVisitor constraint_vars in
          ignore(visitCilExpr myv g);
          g,!constraint_vars) guards in

    let new_lval_holes =
      StringSet.elements
        (StringSet.filter
           (fun add_var -> 
             List.exists (fun (g,vars) -> StringSet.mem add_var vars) guard_vars)
           all_add_vars)
    in
    let alpha_res = ref [] in
    let myalpha = new alphaRenameVisitor new_lval_holes in 

    let allexps = 
      List.map 
        (fun exp -> visitCilExpr myalpha exp) guards
    in
    (* left off here - need to replace alpha-renamed, not original names, in strings*)
    let lvalholes = Hashtbl.create 10 in
    let regular_expressions =
      lfoldl (fun res varname -> 
        if hmem alpha_ht varname then begin
          let alpha_name = hfind alpha_ht varname in 
          let id = Printf.sprintf "__hole%d__" (!hole_num) in
            Hashtbl.add lvalholes varname !hole_num;
            let constraints = ConstraintSet.add (Fault_path) (ConstraintSet.singleton (InScope("__hole1__"))) in
            (* FIXME: maybe "reference hole1" ? *)
            Hashtbl.replace 
              hole_ht 
              id 
              ({ hole_id = id; 
                 htyp = Lval_hole; 
                 constraints=constraints});
            Printf.printf "making a thing for %s\n" varname;
            let matches_this_var = Str.regexp_string alpha_name in 
              (matches_this_var, (Printf.sprintf "(%s.var)" id)) :: res
        end else res
      ) [] new_lval_holes 
    in

      let new_constraints = 
        List.map (fun g -> 
          let str = Pretty.sprint ~width:80 (dn_exp () g) in
            Printf.printf "Str pre fold: {%s}\n" str;
            let res = List.fold_left (fun str (vregexp,newstr) ->
              Str.global_replace vregexp newstr str) str
              regular_expressions
            in
              Printf.printf "Str post fold: {%s}\n" res; res
) guards in

      let as_stmt_str = 
        List.fold_left (fun str (_,stmt) -> str^(Pretty.sprint ~width:80 (dn_stmt () stmt))^"\n") "" change.add
      in
      let as_stmt_str = 
        List.fold_left 
          (fun str (vregexp,newstr) ->
            Str.global_replace vregexp newstr str) 
          as_stmt_str regular_expressions
      in
      let rec all_ifs exps = 
        match exps with
        | [last] -> last  ^") { " ^ as_stmt_str ^ "}"
        | hd :: rest -> hd ^ " && " ^ (all_ifs rest)
        | [] -> ") \n { "^ as_stmt_str ^ "}"
      in
      let new_stmt = 
        if (List.length new_constraints) > 0 then
          "if ( " ^ (all_ifs new_constraints) 
        else "{"^as_stmt_str ^ "}"
      in
        Printf.printf "new_stmt: \n {%s} \n" new_stmt
    (* now replace all uses of those lvals in the added statements with __holenum__.var *)
      (* and all the adds are just one hole, because it's a block *)

let main () = begin
  let fname = Sys.argv.(1) in 
    Printf.printf "fname: %s\n" fname;
    let fin = open_in_bin fname in 
    let num_medoids = Marshal.from_channel fin in
      Printf.printf "num medoids: %d\n" num_medoids;
      let res = ref [] in
      let i = ref 0 in 
        while !i < num_medoids do
          incr i;
          let change = Marshal.from_channel fin in
            res := change :: !res
        done;
        List.iter (fun change -> Printf.printf "Change id: %d, file name: %s\n" change.change_id change.file_name) !res;
        List.iter convert_change_to_template !res
end ;;

main () ;;
