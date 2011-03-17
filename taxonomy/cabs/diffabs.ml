(* 
 * Stolen wholesale from CIL.
 *
 *)

(** This file was originally part of Hugues Casee's frontc 2.0, and has been 
 * extensively changed since. 
**
** 1.0	3.22.99	Hugues Cassé	First version.
** 2.0  George Necula 12/12/00: Many extensions
 **)

(*
** Types
*)

(* AST nodes can have unique ids, except for strings/similar
   constants, which are terminals.  In practice we only number
   expressions, statements, definitions, and tree nodes, because
   that's all we need. *)

open Cabs
open Cabshelper
open Cil
open Cprint
open Pretty

type 'a nodeact = NODE of 'a | MODSITE of int 

type 'a node = 
{
  mutable node : 'a nodeact;
  id : int; 
  mutable typelabel : int;
  mutable tl_str : string;
} 

type directive = 
  | PREINCLUDE of string * cabsloc
  (* FIXME: fix the others *)

and tree_node = 
  | Globals of definition list
  | Stmts of statement list
  | Exps of expression list
  | Syntax of string
  | Directive of directive 

and tree = string * tree_node list

let node_number = ref 0

let nd (node : 'a) = { node = NODE(node); id = (incr node_number; !node_number); typelabel = (-1); tl_str = "UNINITIALIZED"}
					   
let dn (node : 'a node) : 'a = match node.node with NODE(node) -> node | MODSITE(_) -> failwith "dn of modification site!\n"

let setnode (node : 'a node) (value : 'a) = node.node <- NODE(value)

let mod_number = ref 0 

let newmod () = incr mod_number; MODSITE(!mod_number)

(*type cil_tree_node = 
  | CilGlobals of global list
  | CilStmts of stmt list
  | CilExp of exp list
(* FIXME: directives *)

type diff_code = 
	{ mutable file_name : string;
	  mutable elements : cil_tree_node list }*)
(* tempted to force snippets into legal CIL *)

