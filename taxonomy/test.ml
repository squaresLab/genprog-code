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

class collectVarVisitor holes = object
  inherit nopCilVisitor
  method vvrbl varinfo =
    holes := StringSet.add varinfo.vname !holes; DoChildren
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

let my_alpha = new alphaRenameVisitor

(*
let alpha_rename change =
  Hashtbl.clear alpha_ht ;
  let predicates = ExpSet.elements change.guards in 
  let predicates = List.map (fun exp -> visitCilExpr my_alpha exp) predicates in
  let adds = List.map (fun (n,stmt) -> n,visitCilStmt my_alpha stmt) change.add in
  let dels = List.map (fun (n,stmt) -> n,visitCilStmt my_alpha stmt) change.delete in
(*    {change with guards = (ExpSet.of_enum (List.enum predicates)); add = adds; delete = dels }*) ()*)
let template_count = ref 0 

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
      lmap (fun guard -> visitCilExpr my_alpha guard) guards 
    in
    let lval_holes = 
      let constraint_vars = ref (StringSet.empty) in
      let my_v = new collectVarVisitor constraint_vars in
      let _ = 
        liter (fun guard -> ignore(visitCilExpr my_v guard)) guards in
        StringSet.elements (!constraint_vars)
    in
    let regular_expressions = 
      lmap (fun varname -> 
        let id = Printf.sprintf "__hole%d__" (!hole_num) in
          incr hole_num;
          let constraints = 
            (* FIXME: maybe "reference hole1" ? *)
            ConstraintSet.add (Fault_path) (ConstraintSet.singleton (InScope("__hole1__"))) 
          in
          let _ =
            Hashtbl.replace 
              hole_ht id
              ({ hole_id = id; 
                 htyp = Lval_hole; 
                 constraints=constraints})
          in
          let matches_this_var = Str.regexp_string varname in 
            matches_this_var, Printf.sprintf "(%s.var)" id)
        lval_holes
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
      ) guards 
    in

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
      else as_stmt_str
    in
      debug "#include \"templates.h\"\n";
      debug "void * template%d() {\n" !template_count; incr template_count ;
      hiter 
        (fun id holeinfo ->
          let decl = Printf.sprintf "hole %s " id in
          let typ = 
            Printf.sprintf "__attribute__ (holetype(\"%s\")) "
              (match holeinfo.htyp with 
                Stmt_hole -> "stmt"
              | Exp_hole -> "exp"
              | Lval_hole -> "lval")
          in
          let attributes = 
            ConstraintSet.fold 
              (fun cons str -> 
                let this_const = 
                  Printf.sprintf "__attribute__((%s)) "
                    (match cons with 
                      Fault_path -> "constraint(\"fault_path\")"
                    | Fix_path -> "constraint(\"fix_path\")"
                    | Ref(str) -> Printf.sprintf "reference(\"%s\")" str
                    | InScope(str) -> Printf.sprintf "inscope(\"%s\")" str
                    | Matches(i) -> Printf.sprintf "matches(%d)" i)
                in
                  str^this_const
              ) holeinfo.constraints "" 
          in
            debug "%s%s%s;\n" decl typ attributes
        ) hole_ht ;
      debug "{ __blockattribute__(__hole1__)\n";
      debug "%s" new_stmt;
      debug "}\n";
      try 
      ignore(Formatcil.cStmt new_stmt (fun _ -> failwith "new varinfos?") !currentLoc [])
      with e -> debug "error: %s\n" (Printexc.to_string e)

let main () = begin
  let medoids = Sys.argv.(1) in 
(*  let files = Sys.argv.(2) in*)
    let fin = open_in_bin medoids in 
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
        List.iter (fun change -> debug "FOO\n"; ignore(convert_change_to_template change); debug "BAR\n") !res
end ;;

main () ;;
