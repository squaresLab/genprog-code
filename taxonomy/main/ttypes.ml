open Batteries
open Utils
open Cabs
open Cprint
open Difftypes

type 'a lifted = STAR | MAYBE of 'a list | ATLEAST of 'a list | LNOTHING | UNUNIFIED of 'a list 
				 | PARTIALMATCH of 'a

and ops = Modify_value | Arithmetic | Bitwise | Logic | OnNumbers | OnBits | 
	Bop_op of bop_gen | Uop_op of uop_gen | Lifted_ops of ops lifted

and bop_gen =  Modify_assign | BitTruth | NotBitTruth | Shift
				| Bgen of ops | Bop of binary_operator | Bop_gen of bop_gen lifted

and uop_gen = Sizeof | Sign_modifier | Memory_operator | Not_operator | Alignof
			   | Pre_operator | Post_operator | Increment | Decrement | Uop of unary_operator   
			   | Ugen of ops | Uop_gen of uop_gen lifted

and exp_gen = EXPBASE of expression node
			   | ELIFTED of exp_gen lifted
			   | CONSTGEN of constant lifted
			   | UNARYOP of uop_gen * exp_gen
			   | BINOP of bop_gen * exp_gen * exp_gen
			   | QUESTOP of exp_gen * exp_gen * exp_gen
			   | CASTOP of (spec_gen * dt_gen) * ie_gen 
			   | CALLOP of exp_gen * exp_gen list
			   | COMMAOP of exp_gen list
			   | PARENOP of exp_gen
			   | EXPSIZEOFOP of exp_gen
			   | TYPESIZEOFOP of spec_gen * dt_gen
			   | EXPALIGNOFOP of exp_gen 
			   | TYPEALIGNOFOP of spec_gen * dt_gen
			   | INDEXOP of exp_gen * exp_gen
			   | MEMBEROFOP of exp_gen * string 
			   | MEMBEROFPTROP of exp_gen * string
			   | ADDROFEXP of exp_gen
			   | OPERATION of ops  * exp_gen
			   | SOMEMEMBER of exp_gen * string 
			   | VALUE of exp_gen 
			   | GNUGEN of block_gen

and spec_gen = Spec_list of se_gen list | Spec_lifted of spec_gen lifted | Spec_base of specifier

and se_gen = Spec_elem of spec_elem
			 | Se_attr of attr_gen
			 | Se_type of typeSpec_gen
			 | Se_lifted of se_gen lifted
			 | Se_CV of cvspec lifted
			 | Se_storage of storage lifted

and attr_gen = ATTRBASE of attribute | ATTRLIFTED of attr_gen lifted | ATTRGEN of string * exp_gen list
and storun = Struct | Union | Something
and typeSpec_gen = 
  | TSBASE of typeSpecifier | TSTYPEOFE of exp_gen | TSTYPEOFT of spec_gen * dt_gen
  | TSSORU of string * storun * fg_gen list option * attr_gen list 
  | TSLIFTED of typeSpec_gen lifted (* FIXME: no enums?? *)
and fg_gen = FGBASE of field_group | FGGEN of spec_gen * (name_gen * exp_gen option) list | FGLIFTED of fg_gen lifted
and sn_gen = SNBASE of single_name | SNGEN of spec_gen * name_gen | SNLIFTED of sn_gen lifted

and dt_gen = 
  | DTBASE of decl_type
  | DTLIFTED of dt_gen lifted
  | DTPAREN of attr_gen list * dt_gen * attr_gen list
  | DTARRAY of dt_gen * attr_gen list * exp_gen
  | DTPTR of attr_gen list * dt_gen
  | DTPROTO of dt_gen * sn_gen list 
  | DTCOMPLEX of dt_gen * attr_gen list

and ie_gen = 
  | IEBASE of init_expression
  | GENSINGLE of exp_gen
  | GENCOMPOUND of (iw_gen * ie_gen) list
  | IELIFTED of ie_gen lifted

and iw_gen = 
  | IWBASE of initwhat
  | IWINFIELD of string * iw_gen
  | IWATINDEX of exp_gen * iw_gen
  | IWATINDEXRANGE of exp_gen * exp_gen
  | IWLIFTED of iw_gen lifted
  | IWSOME of exp_gen * iw_gen

and stmt_gen = 
  | STMTBASE of statement node
  | SLIFTED of stmt_gen lifted
  | STMTCOMP of exp_gen
  | STMTBLOCK of block_gen
  | STMTSEQ of stmt_gen * stmt_gen
  | STMTIF of exp_gen * stmt_gen * stmt_gen
  | STMTFOR of fc_gen * exp_gen * exp_gen * stmt_gen
  | STMTLOOP of loop_type * exp_gen * stmt_gen
  | STMTCONTROL (* break or continue *)
  | STMTRET of exp_gen
  | STMTSWITCH of exp_gen * stmt_gen
  | STMTCASE of exp_gen * stmt_gen 
  | STMTCASERANGE of exp_gen * exp_gen * stmt_gen 
  | STMTDEFAULT of stmt_gen 
  | STMTLABEL of string * stmt_gen
  | STMTCOMPGOTO of exp_gen 
  | STMTDEF of def_gen (* FIXME: ommitting ASM for now *)
  | STMTTRYE of block_gen * exp_gen * block_gen
  | STMTTRYF of block_gen * block_gen 
and block_gen = Reg of stmt_gen list | BLKLIFTED of block_gen lifted | BLOCKBASE of block

and fc_gen = FCBASE of for_clause | FCLIFTED of fc_gen lifted | FCEXP of exp_gen | FCDECL of def_gen
and def_gen = DLIFTED of def_gen lifted
			  | DBASE of definition node
			  | DFUNDEF of sn_gen * block_gen
			  | DDECDEF of ing_gen
			  | DTYPEDEF of ng_gen
			  | DONLYTD of spec_gen
			  | DPRAGMA of exp_gen
			  | DLINK of string * def_gen list 
			  | DGENERICTYPE of spec_gen * name_gen list
			  | DGENERICDEC of spec_gen * name_gen

and loop_type = Any | While | DoWhile | AnyWhile	  
and ng_gen = NGBASE of name_group | NGGEN of spec_gen * name_gen list | NGLIFTED of ng_gen lifted
and name_gen = NAMEBASE of name | NAMEGEN of string * dt_gen * attr_gen list | NAMELIFTED of name_gen lifted
and ing_gen = INGBASE of init_name_group | INGGEN of spec_gen * in_gen list | INGLIFTED of ing_gen lifted
and in_gen = INBASE of init_name | INGEN of name_gen * ie_gen | INLIFTED of in_gen lifted

type tn_gen = 
  | TNLIFTED of tn_gen lifted
  |	GENDEFS of def_gen list
  | GENSTMTS of stmt_gen list
  | GENEXPS of exp_gen list
  | TNBASE of tree_node node

type tree_gen = TNS of tn_gen list | TREELIFTED of tree_gen lifted | TBASE of tree

type dummy_gen = 
  | TREEGEN of tree_gen
  | STMTGEN of stmt_gen
  | EXPGEN of exp_gen
  | TNGEN of tn_gen
  | DEFGEN of def_gen
  | DUMLIFTED of dummy_gen lifted
  | DUMBASE of dummyNode

type change_gen = (* potential FIXME: I lost the "which child" we're inserting
					 into because I think the context info is enough, but we may
					 want to put it back in? *)
  |	InsertGen of dummy_gen 
  | MoveGen of dummy_gen 
  | DeleteGen of dummy_gen
  | ReplaceGen of dummy_gen * dummy_gen
  | ChangeLifted of change_gen lifted
  | ChangeBase of change

type changes_gen = BASECHANGES of change_gen list | CHANGEATLEAST of change_gen list

(* types for generalized AST nodes *)
 
type guard = EXPG | CATCH | GUARDLIFTED of guard lifted

type context = 
	{
	  pdef : def_gen option;
	  pstmt : stmt_gen option;
	  pexp : exp_gen option;
	  sding : dummy_gen Set.t;
	  gby : (guard * exp_gen) list;
	  ging : dummy_gen Set.t;
(*	  mutable renamed : (string,string) Map.t;*)
	}


module OrderedDum =
  struct 
	type t = dummyNode
	let compare dt1 dt2 = 
	  let hash1 = dummy_node_to_str dt1 in
	  let hash2 = dummy_node_to_str dt1 in
		Pervasives.compare hash1 hash2 
  end
module DumSet = Set.Make(OrderedDum)

type init_context = 
	{
	  parent_definition : definition node option;
	  parent_statement : statement node option;
	  parent_expression : expression node option;
	  surrounding : DumSet.t;
	  guarded_by: (guard * expression node) list;
	  guarding: DumSet.t;
(*	  mutable alpha : (string,string) Map.t*)
	}

let make_icontext def s e sur gby ging = 
  {
	  parent_definition=def;
	  parent_statement=s;
	  parent_expression=e;
	  surrounding=sur;
	  guarded_by=gby;
	  guarding=ging;
(*	  alpha = Map.empty;*)
  }

let make_context def s e sur gby ging = 
  {
	  pdef=def;
	  pstmt=s;
	  pexp=e;
	  sding=sur;
	  gby=gby;
	  ging=ging;
(*	  renamed = Map.empty;*)
  }

type init_template = init_context * changes

type template = context * changes_gen

(* a template is one change to one location in code, meaning a treediff converts
   into a list of templates *)
