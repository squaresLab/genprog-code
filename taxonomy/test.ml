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
module OrderedGlobal =
  struct
    type t = global
    let compare g1 g2 = 
      let str1 = Pretty.sprint ~width:80 (dn_global () g1) in
      let str2 = Pretty.sprint ~width:80 (dn_global () g2) in
        compare str1 str2
  end
module GlobalSet = Set.Make(OrderedGlobal)

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
      file_name1 : string;
      file_name2 : string;
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
      Hashtbl.add alpha_ht varinfo.vname new_name;
      varinfo.vname <- new_name; 
      SkipChildren
end

let my_alpha = new alphaRenameVisitor
let template_num = ref 0 

class lvalRenameVisitor lvals = object
  inherit nopCilVisitor
  method vlval lval =
    match lval with
      Var(vinfo),_ when StringMap.mem vinfo.vname lvals ->
        let vinfo = StringMap.find vinfo.vname lvals in
          ChangeTo(Formatcil.cLval "%v:holename.var" [("holename", Fv(vinfo))])
    | _ -> DoChildren
end
let cil_files = hcreate 10

let my_rename = new lvalRenameVisitor

exception FoundIt of varinfo 
class findDeclVisitor lookingfor = object
  inherit nopCilVisitor
  method vvdec vdecl =
    if vdecl.vname = lookingfor then raise (FoundIt(vdecl))
    else DoChildren
end
let convert_change_to_template change = 
  let allglobals = 
    hfold (fun k v  lst -> lst@v.globals) cil_files []
  in
  let hole_num = ref 2 in 
  let hole_ht = Hashtbl.create 10 in
    Hashtbl.add hole_ht "__hole1__"
      {hole_id = "__hole1__"; 
       htyp = Stmt_hole; 
       constraints = (ConstraintSet.singleton Fault_path)};

    (* all variables in guards are holes, only those that overlap with
       statements are replaced in the statement *)

    let guards = 
      lmap (fun guard -> visitCilExpr my_alpha guard) (ExpSet.elements change.guards)
    in

    let lval_holes = 
      let constraint_vars = ref (StringSet.empty) in
      let my_v = new collectVarVisitor constraint_vars in
      let _ = 
        liter (fun guard -> ignore(visitCilExpr my_v guard)) guards in
        StringSet.elements (!constraint_vars)
    in
    let hole_names = 
      lfoldl (fun strmap varname -> 
        let id = Printf.sprintf "__hole%d__" (!hole_num) in
          incr hole_num;
          let constraints = 
            ConstraintSet.add (Fault_path) (ConstraintSet.singleton (InScope("__hole1__"))) 
          in
          let _ =
            Hashtbl.replace 
              hole_ht id
              ({ hole_id = id; 
                 htyp = Lval_hole; 
                 constraints=constraints})
          in
            StringMap.add id  varname strmap)
        (StringMap.empty) lval_holes
    in
    (* construct the function etc *)
    let templates = Frontc.parse "templates.c" () in
    let hole_typ = 
      let res = List.find (fun g -> match g with 
        GType(t,_) -> t.tname = "hole"
      | _ -> false) templates.globals
      in match res with GType(t,_) -> t.ttype
    in
    let fundec = 
      let fun_typ = Formatcil.cType ("void * ()()") [] in
      let function_name = Printf.sprintf "template%d" !template_num in
      let varinfo = makeVarinfo true function_name fun_typ in
        incr template_num;
        {svar = varinfo ;
         sformals = [] ;
         slocals= [];
         smaxid = 0;
         sbody = {battrs = []; bstmts = []};
         smaxstmtid = None ;
         sallstmts = [] }
    in
    let stmt_holetype_attr = Attr("holetype",[AStr("stmt")]) in
    let lval_holetype_attr = Attr("holetype",[AStr("lval")]) in
    let exp_holetype_attr = Attr("holetype",[AStr("exp")]) in
    let constraint_fault_attr = Attr("constraint",[AStr("fault_path")]) in
    let constraint_fix_attr = Attr("constraint",[AStr("fix_path")]) in
    let holes = 
      hfold
        (fun id holeinfo all_holes ->
          let typ_attr = 
            match holeinfo.htyp with 
              Stmt_hole -> stmt_holetype_attr
            | Exp_hole -> exp_holetype_attr
            | Lval_hole -> lval_holetype_attr
          in
          let attributes = 
            ConstraintSet.fold 
              (fun cons attrs -> 
                let this_const = 
                  match cons with 
                    Fault_path -> constraint_fault_attr
                  | Fix_path -> constraint_fix_attr
                  | Ref(str) -> Attr("reference",[AStr(str)])
                  | InScope(str) -> Attr("InScope", [AStr(str)]) 
                  | Matches(i) -> Attr("Matches", [AInt(i)])
                in
                  this_const :: attrs
              ) holeinfo.constraints [typ_attr]
          in
          let typ = typeAddAttributes attributes hole_typ in
          let newhole = makeLocalVar fundec id typ in
            debug "looking for %s\n" id;
            if id <> "__hole1__" then begin
              let original_name = StringMap.find id hole_names in
                debug "found %s\n" original_name;
                StringMap.add original_name newhole all_holes
            end else all_holes
        ) hole_ht (StringMap.empty)
    in
    let new_constraints = 
      List.map (fun g -> visitCilExpr (my_rename holes) g) guards
    in
    let stmts =
      lmap (fun (_,stmt) -> visitCilStmt (my_rename holes) stmt) change.add 
    in
    let add_vars = 
      let res = ref (StringSet.empty) in
      let my_v = new collectVarVisitor res in
      liter (fun stmt -> ignore(visitCilStmt my_v stmt)) stmts;
        lfilt (fun var -> not (hmem hole_ht var))
        (StringSet.elements (!res))
    in
    let decls = 
      lmap
        (fun var -> 
          try
            liter (fun g -> ignore(visitCilGlobal (new findDeclVisitor var) g)) allglobals;
            makeGlobalVar var intType            
          with FoundIt(v) -> v 
        ) add_vars 
    in
    let locals = lfilt (fun d -> not d.vglob) decls in
      fundec.slocals <- fundec.slocals @ locals;
    let globals = lfilt (fun d -> d.vglob) decls in 
    let new_stmtkind = 
      let then_block = mkBlock stmts in
      if (List.length new_constraints) > 0 then begin
        let rec all_ifs exps =
          match exps with
            [last] -> last
          | hd :: rest -> BinOp(LAnd,hd,all_ifs rest,intType)
        in
        let conditional = all_ifs new_constraints in
        If(conditional,then_block,{bstmts=[];battrs=[]},!currentLoc)
      end
      else Block(then_block)
    in
    let new_stmt = mkStmt new_stmtkind in
    let block = mkBlock [new_stmt] in
    let block = {block with battrs = [Attr("hole1",[])]} in
    let gfun = GFun({fundec with sbody = block}, !currentLoc) in
      lfoldl (fun set g -> GlobalSet.add (GVarDecl(g,!currentLoc))  set) (GlobalSet.empty) globals, gfun

let main () = begin
  let medoids_file = Sys.argv.(1) in 
  let fin = open_in_bin medoids_file in 
  let num_medoids = Marshal.from_channel fin in
  let medoids = ref [] in
  let i = ref 0 in 
  let _ =
    while !i < num_medoids do
      incr i;
      let change = Marshal.from_channel fin in
        medoids := change :: !medoids
    done;
    close_in fin
  in
  let files = 
    lmap (fun change -> 
      hadd cil_files change.file_name1 (Frontc.parse change.file_name1 ())) !medoids
  in
  let globals,gfun = 
    lfoldl (fun (globals,funs) change ->
      let globals',gfun = convert_change_to_template change in
        GlobalSet.union globals globals', funs@[gfun]) (GlobalSet.empty,[])
      !medoids
  in
  let globals = (GlobalSet.elements globals) @ gfun in
    assert((llen globals) > 0);
  let file = 
    { fileName = "templates.c";
      globals = globals;
      globinit = None;
      globinitcalled = false } in
    dumpFile defaultCilPrinter stdout "test" file 
end ;;

main () ;;
