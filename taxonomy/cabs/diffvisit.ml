
(* cabsvisit.ml *)
(* tree visitor and rewriter for cabs *)

open Diffabs
open Cabs
open Cabsvisit
open Cabshelper
open Trace
open Pretty
module E = Errormsg

(* basic interface for a visitor object *)

(* Different visiting actions. 'a will be instantiated with exp, instr, etc. *)
type 'a visitAction = 
    SkipChildren                        (* Do not visit the children. Return 
                                         * the node as it is *)
  | ChangeTo of 'a                      (* Replace the expression with the 
                                         * given one *)
  | DoChildren                          (* Continue with the children of this 
                                         * node. Rebuild the node on return 
                                         * if any of the children changes 
                                         * (use == test) *)
  | ChangeDoChildrenPost of 'a * ('a -> 'a) (* First consider that the entire 
                                          * exp is replaced by the first 
                                          * paramenter. Then continue with 
                                          * the children. On return rebuild 
                                          * the node if any of the children 
                                          * has changed and then apply the 
                                          * function on the node *)

type nameKind = 
    NVar                                (* Variable or function prototype 
                                           name *)
  | NFun                                (* A function definition name *)
  | NField                              (* The name of a field *)
  | NType                               (* The name of a type *)

(* All visit methods are called in preorder! (but you can use 
 * ChangeDoChildrenPost to change the order) *)
class diffVisitor = object(self)
  inherit nopCabsVisitor 

  method vdirective (d : directive node) : directive node visitAction = DoChildren
  method vtree (t : tree) : tree visitAction = DoChildren
  method vtreenode (tn : tree_node) : tree_node visitAction = DoChildren
end

let rec childrenTreeNode vis (tn : tree_node) : tree_node = 
  match tn with
	Globals(defs) -> Globals(List.flatten(List.map (fun d -> (visitCabsDefinition vis d)) defs))
  | Stmts(s) ->Stmts(List.flatten(List.map (fun s -> (visitCabsStatement vis s)) s))
  | Exps(e) ->Exps(List.map (fun e -> visitCabsExpression vis e) e)
  | Syntax(s) -> tn 
let doVisit (vis: cabsVisitor)
    (startvisit: 'a -> 'a visitAction) 
    (children: cabsVisitor -> 'a -> 'a) 
    (node: 'a) : 'a = 
  let action = startvisit node in
  match action with
    SkipChildren -> node
  | ChangeTo node' -> node'
  | _ ->  
      let nodepre = match action with
        ChangeDoChildrenPost (node', _) -> node'
      | _ -> node
      in
      let nodepost = children vis nodepre in
      match action with
        ChangeDoChildrenPost (_, f) -> f nodepost
      | _ -> nodepost
            
let visitDirective (vis : diffVisitor) (d : directive ) : directive = d
(*  match directive with 
  | PREINCLUDE of string * cabsloc*) 

let visitTreeNode vis (tn: tree_node) : tree_node =  
  let coerce v = (v : diffVisitor :> cabsVisitor) in
	doVisit (coerce vis) vis#vtreenode childrenTreeNode tn

let visitTree vis ((fname, f): tree) : tree = 
  (fname, (List.map (visitTreeNode vis) f))


