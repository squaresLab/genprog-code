open Batteries
open Set
open Utils
open Cil
open Difftypes

type 'a lifted = STAR | MAYBE of 'a list | ATLEAST of 'a list | LNOTHING | UNUNIFIED of 'a list 
				 | PARTIALMATCH of 'a

and ops = Modify_value | Arithmetic | Bitwise | Logic | OnNumbers | OnBits | 
	Bop_op of bop_gen | Uop_op of uop_gen | Lifted_ops of ops lifted

and bop_gen =  Modify_assign | BitTruth | NotBitTruth | Shift
			   | Bgen of ops | Bop of binop | Bop_gen of bop_gen lifted

and uop_gen = Not_operator
			  | Ugen of ops | Uop_gen of uop_gen lifted | Uop_base of unop

and ('a,'b) gen = BASE of 'a | LIFTED of 'b lifted

and expbase = 	
  | CONSTGEN of constant lifted (* FIXME: maybe *)
  | LVALGEN of lval_gen
  | EXPSIZEOFOP of exp_gen
  | TYPESIZEOFOP of typ_gen
  | GENSIZEOF
  | TYPESTRSIZEOF of string lifted
  | EXPALIGNOF of exp_gen 
  | TYPEALIGNOF of typ_gen
  | UNARYOP of uop_gen * exp_gen
  | BINOP of bop_gen * exp_gen * exp_gen
  | CASTOP of typ_gen * exp_gen
  | ADDROF of lval_gen
  | STARTOF of lval_gen
and lvalbase = lhost_gen * offset_gen
and lhostbase = | VARGEN of varinfo_gen
                | MEMGEN of exp_gen
and offsetbase = | FIELDGEN of fieldinfo_gen * offset_gen
                 | INDEXGEN of exp_gen * offset_gen
and typbase = 
  | TPTRGEN of typ_gen
  | TARRAYGEN of typ_gen * exp_gen
  | TFUNGEN of typ_gen * typ_gen list
  | TNAMEDGEN of typeinfo_gen
  | TCOMPGEN of compinfo_gen
  | TENUMGEN of enuminfo_gen

and varinfobase = string * typ_gen * bool (* FIXME: maybe string/bool lifted? *)
and fieldinfobase = string lifted * typ_gen * string lifted (* second one is compinfo *)
and typeinfobase = string lifted * typ_gen
and compinfobase = string lifted * fieldinfo_gen list 
and enuminfobase = string lifted * (string lifted * exp_gen) list

and stmtbase = 
  | INSTRGEN of instr_gen list
  | RETURNGEN of exp_gen option 
(* break and continue are both control flow stuff *)
  | BREAKORCONTINUE
  | UNSTRUCTURED_CONTROL_FLOW
  | IFGEN of exp_gen * block_gen * block_gen
  | SWITCHGEN of exp_gen * block_gen * (stmt_gen list)
  | LOOP of block_gen
  | CONDITIONALGEN of exp_gen * block_gen list
  | BLOCKGEN of block_gen
  | TRYFINALLYGEN of block_gen * block_gen
  | TRYEXCEPTGEN of block_gen * instr_gen list * exp_gen * block_gen
  | GENERICTRY of block_gen * block_gen

and instrbase = 
    SETGEN of lval_gen * exp_gen
  | CALLGEN of lval_gen * exp_gen * exp_gen list
(* FIXME: ommitting asm for now *)

and changebase = stmt_gen list * stmt_gen list * exp_gen list
  
and lval_gen = (lval,lvalbase) gen
and exp_gen = (exp,expbase) gen
and varinfo_gen = (varinfo, varinfobase) gen
and lhost_gen = (lhost,lhostbase) gen
and offset_gen = (offset,offsetbase) gen
and typ_gen = (typ,typbase) gen
and fieldinfo_gen = (fieldinfo,fieldinfobase) gen
and typeinfo_gen = (typeinfo,typeinfobase) gen
and compinfo_gen = (compinfo,compinfobase) gen
and enuminfo_gen = (enuminfo,enuminfobase) gen
and stmt_gen = (stmtkind,stmtbase) gen 
and instr_gen = (instr,instrbase) gen
and block_gen = (block,stmt_gen list) gen
and change_gen = (change_node,changebase) gen
and ci_gen = (compinfo,string * fieldinfo_gen list) gen

let template_id = ref 0 

type template =
    { template_id : int ;
      t_adds : stmt_gen list ;
      t_dels : stmt_gen list ;
      t_guards : exp_gen list ;
      overall_stmts : stmt_gen list ;
      overall_exps : exp_gen list ;
      names : StringSet.t ;
    }
let new_template adds dels guards o1 o2 names = 
  let id = Ref.post_incr template_id in
    { template_id = id ;
      t_adds = adds;
      t_dels = dels;
      t_guards = guards ;
      overall_stmts = o1 ;
      overall_exps = o2 ;
      names = names 
    }

(*and storun = Struct | Union | Something
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

type change_gen = (* potential FIXME: I lost the "which child" we're inserting
					 into because I think the context info is enough, but we may
					 want to put it back in? 
  |	InsertGen of dummy_gen 
  | MoveGen of dummy_gen 
  | DeleteGen of dummy_gen
  | ReplaceGen of dummy_gen * dummy_gen
  | ChangeLifted of change_gen lifted*)
  | ChangeBase of edit

type changes_gen = BASECHANGES of change_gen list | CHANGEATLEAST of change_gen list

(* types for generalized AST nodes *)
 
type guard = EXPG | CATCH | GUARDLIFTED of guard lifted

type context = 
	{
	  pdef : def_gen option;
	  pstmt : stmt_gen option;
	  pexp : exp_gen option;
	  sding :  int Set.t; (* fixme *)
	  gby : (guard * exp_gen) list;
	  ging : int Set.t; (* fixme *)
(*	  mutable renamed : (string,string) Map.t;*)
	}

type init_context = 
	{
	  parent_definition : definition node option;
	  parent_statement : statement node option;
	  parent_expression : expression node option;
	  surrounding : int Set.t; (* fixme *)
	  guarded_by: (guard * expression node) list;
	  guarding: int Set.t; (* fixme *)
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
*)
(* a template is one change to one location in code, meaning a treediff converts
   into a list of templates *)


