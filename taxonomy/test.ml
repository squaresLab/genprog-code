open Set
open Cil
open Pretty
open Global

class myCilPrinterClass : cilPrinter = object(self)
  inherit defaultCilPrinterClass as super
  method private pStmtNext (next: stmt) () (s: stmt) =
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
                text (" __blockattribute__(")
               else
                 text "__attribute__((")

                ++ (docList ~sep:(chr ',' ++ break)
                      (fun a -> a)) () in__attr__
                ++ text ")"
                ++ (text ")")
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

let dn_obj (func: unit -> 'a -> doc) : (unit -> 'a -> doc) =
begin
  (* construct the closure to return *)
  let theFunc () (obj:'a) : doc =
  begin
    let prevStyle = !lineDirectiveStyle in
    lineDirectiveStyle := None;
    let ret = (func () obj) in    (* call underlying printer *)
    lineDirectiveStyle := prevStyle;
    ret
  end in
  theFunc
end

let _ = lineDirectiveStyle := None

module OrderedGlobal =
  struct
    type t = global
    let compare g1 g2 = 
      let str1 = Pretty.sprint ~width:80 (dn_global () g1) in
      let str2 = Pretty.sprint ~width:80 (dn_global () g2) in
        compare str1 str2
  end
module GlobalSet = Set.Make(OrderedGlobal)

type hole_type = Stmt_hole | Exp_hole | Lval_hole
type constraints = 
    Fault_path 
  | Fix_path 
  | Ref of string 
  | InScope of string 
  | Matches of int

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

(*** CilRep-specific template stuff *)

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

class lvalRenameVisitor lvals = object
  inherit nopCilVisitor
  method vlval lval =
    match lval with
      Var(vinfo),_ when StringMap.mem vinfo.vname lvals ->
        let vinfo = StringMap.find vinfo.vname lvals in
          ChangeTo(Formatcil.cLval "%v:holename.var" [("holename", Fv(vinfo))])
    | _ -> DoChildren
end

exception FoundIt of varinfo 

class findDeclVisitor lookingfor = object
  inherit nopCilVisitor
  method vvdec vdecl =
    if vdecl.vname = lookingfor then raise (FoundIt(vdecl))
    else DoChildren
end

let my_rename = new lvalRenameVisitor

let all_globals = ref [] 
let template_num = ref 0 

(* convenience CIL constructs to clean up the code below *)
let templates = Frontc.parse "templates.c" ()
let hole_typ = 
  let res = List.find (fun g -> match g with 
      GType(t,_) -> t.tname = "hole"
    | _ -> false) templates.globals
  in match res with GType(t,_) -> t.ttype

let mkFundec () = 
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

let stmt_holetype_attr = Attr("holetype",[AStr("stmt")])
let lval_holetype_attr = Attr("holetype",[AStr("lval")]) 
let exp_holetype_attr = Attr("holetype",[AStr("exp")]) 
let constraint_fault_attr = Attr("constraint",[AStr("fault_path")]) 
let constraint_fix_attr = Attr("constraint",[AStr("fix_path")])

let convert_change_to_template change = 
  let fundec = mkFundec() in
  let hole_num = ref 2 in 
  let hole_ht = Hashtbl.create 10 in
  let _ =
    (* the location for the change; hole 1 *)
    hadd hole_ht "__hole1__"
      {hole_id = "__hole1__"; 
       htyp = Stmt_hole; 
       constraints = (ConstraintSet.singleton Fault_path)};
  in
  (* all variables in guards are holes, only those that overlap with
     statements are replaced in the statement. *) 
  let lval_holes = 
    let constraint_vars = ref (StringSet.empty) in
    let my_v = new collectVarVisitor constraint_vars in
    let _ = 
      liter (fun guard -> ignore(visitCilExpr my_v guard)) 
        (ExpSet.elements change.guards) in
      StringSet.elements (!constraint_vars)
  in
  (* collect the holes *)
  let hole_names = 
    lfoldl (fun strmap varname -> 
      let id = Printf.sprintf "__hole%d__" (!hole_num) in
        incr hole_num;
        let constraints = 
          ConstraintSet.add (Fault_path) 
            (ConstraintSet.singleton (InScope("__hole1__"))) 
        in
        let _ =
          hrep hole_ht id
            ({ hole_id = id; 
               htyp = Lval_hole; 
               constraints=constraints})
        in
          StringMap.add id  varname strmap)
      (StringMap.empty) lval_holes
  in
  (* make local variable declarations for the holes *)
  let holes = 
    hfold (fun id holeinfo all_holes ->
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
              | InScope(str) -> Attr("inscope", [AStr(str)]) 
              | Matches(i) -> Attr("matches", [AInt(i)])
            in
              this_const :: attrs
          ) holeinfo.constraints [typ_attr]
      in
      let newhole = makeLocalVar fundec id hole_typ in
        newhole.vattr <- attributes;
        if id <> "__hole1__" then begin
          let original_name = StringMap.find id hole_names in
            StringMap.add original_name newhole all_holes
        end else all_holes
    ) hole_ht (StringMap.empty)
  in

  (* replace the variables in the expressions and stmts with their hole
     counterparts *)
  let guards =
    lmap (fun g -> visitCilExpr (my_rename holes) g) (ExpSet.elements change.guards)
  in
  let stmts =
    lmap (fun (_,stmt) -> visitCilStmt (my_rename holes) stmt) change.add 
  in

  (* now that the holes are taken care of, time to attend to the rest of the
     variables used in the code (esp functions and such) to be sure they're
     declared and the resulting file will parse *)
  (* step one: figure out which variables used in the statements are not holes.
     FIXME: do the same for expressions in case they reference functions!  And
     make sure we take care of that above...*)
  let add_vars = 
    let res = ref (StringSet.empty) in
    let my_v = new collectVarVisitor res in
      liter (fun stmt -> ignore(visitCilStmt my_v stmt)) stmts;
      lfilt (fun var -> not (hmem hole_ht var))
        (StringSet.elements (!res))
  in
  (* step two: look for each of those variables in the files we've already
     parsed.  If we fail to find it, make up a global and pray *)
  let decls = 
    lmap
      (fun var -> 
        try
          liter (fun g -> ignore(visitCilGlobal (new findDeclVisitor var) g)) !all_globals;
          makeGlobalVar var intType            
        with FoundIt(v) -> v 
      ) add_vars 
  in
  (* for the locals we found, add them to the function's locals *)
  let _ = 
    let locals = lfilt (fun d -> not d.vglob) decls in
      fundec.slocals <- fundec.slocals @ locals
  in
  (* the rest are globals *)
  let globals = lfilt (fun d -> d.vglob) decls in 

  (* phew, now we can finally make the body of the function *)
  let stmtkind = 
    (* the add stmts become the block for hole1 *)
    let then_block = mkBlock stmts in
      (* if there are predicates, build those into an if *)
      if (llen guards) > 0 then begin
        let rec all_ifs exps =
          match exps with
            [last] -> last
          | hd :: rest -> BinOp(LAnd,hd,all_ifs rest,intType)
        in
        let conditional = all_ifs guards in
          If(conditional,then_block,{bstmts=[];battrs=[]},!currentLoc)
      end
      else Block(then_block)
  in
  (* make it into a block and attribute it accordingly *)
  let block = { (mkBlock [mkStmt stmtkind]) with battrs = [Attr("hole1",[])]} in
  (* the block is the body of the function *)
  let gfun = GFun({fundec with sbody = block}, !currentLoc) in
    (* this set thing is not going to work, ultimately, but one thing at a
       time. FIXME *)
    lfoldl (fun set g -> GlobalSet.add (GVarDecl(g,!currentLoc)) set) (GlobalSet.empty) globals, gfun

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
  let _ = 
    liter (fun change -> 
      let parsed = Frontc.parse change.file_name1 () in
        all_globals := !all_globals @ parsed.globals)
      !medoids
  in
    let hole_typ = 
      List.find (fun g -> match g with 
      | GCompTag(ci,_) -> ci.cname = "_hole"
      | _ -> false) templates.globals
    in
  let globals,gfun = 
    lfoldl (fun (globals,funs) change ->
      let globals',gfun = convert_change_to_template change in
        GlobalSet.union globals globals', funs@[gfun]) (GlobalSet.singleton hole_typ,[])
      !medoids
  in
  let globals = (GlobalSet.elements globals) @ gfun in
    assert((llen globals) > 0);

  let file = 
    { fileName = "templates.c";
      globals = globals;
      globinit = None;
      globinitcalled = false } in
    dumpFile (new myCilPrinterClass) stdout "test" file 
end ;;

main () ;;
