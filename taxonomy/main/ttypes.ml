open Batteries
open Utils
open Cabs
open Cprint
open Difftypes

type 'a lifted = STAR | MAYBE of 'a list | ATLEAST of 'a list | LNOTHING | EXACT of 'a

type ops = Modify_value | Arithmetic | Bitwise | Logic | OnNumbers | OnBits | 
	Bop_op of bop_gen | Uop_op of uop_gen | Lifted_ops of ops lifted

and bop_gen =  Modify_assign | BitTruth | NotBitTruth | Shift
				| Bgen of ops | Bop of binary_operator | Bop_gen of bop_gen lifted

and uop_gen = Sizeof | Sign_modifier | Memory_operator | Not_operator | Alignof
			   | Pre_operator | Post_operator | Increment | Decrement | Uop of unary_operator   
			   | Ugen of ops | Uop_gen of uop_gen lifted

and exp_gen = EXPBASE of expression node
			   | ELIFTED of exp_gen lifted
			   | UNARYOP of uop_gen * exp_gen
			   | BINOP of bop_gen * exp_gen * exp_gen
			   | QUESTOP of exp_gen * exp_gen * exp_gen
			   | CASTOP of (spec_gen * dt_gen) * ie_gen 
			   | CALLOP of exp_gen * exp_gen list
			   | COMMAOP of exp_gen list
			   | PARENOP of exp_gen
			   | EXPSIZOFOP of exp_gen
			   | TYPESIZOFOP of spec_gen * dt_gen
			   | EXPALIGNOFOP of exp_gen 
			   | TYPEALIGNOFOP of spec_gen * dt_gen
			   | INDEXOP of exp_gen * exp_gen
			   | MEMBEROFOP of exp_gen * string 
			   | MEMBEROFPTROP of exp_gen * string
			   | ADDROFEXP of exp_gen
			   | OPERATION of ops  * exp_gen
			   | SOMEMEMBER of exp_gen * string 
			   | VALUE of exp_gen 

and spec_gen = Spec_list of se_gen list | Spec_lifted of spec_gen lifted | Spec_base of specifier

and se_gen = Spec_elem of spec_elem
			 | Se_attr of attr_gen
			 | Se_type of typeSpec_gen
			 | Se_lifted of se_gen lifted
and attr_gen = unit
and typeSpec_gen = unit
and sn_gen = unit
and dt_gen = 
  | DTBASE of decl_type
  | DTLIFTED of dt_gen lifted
  | DTPAREN of attr_gen list * dt_gen * attr_gen list
  | DTARRAY of dt_gen * attr_gen list * exp_gen
  | DTPTR of attr_gen list * dt_gen
  | DTPROTO of dt_gen * sn_gen list * bool 
  | DTWITHPAREN of dt_gen * attr_gen list
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
and block_gen = Reg of stmt_gen list | BLKLIFTED of block_gen lifted

and fc_gen = unit
and def_gen = DLIFTED of def_gen lifted
			  | DBASE of definition node

and loop_type = Any | While | DoWhile | AnyWhile	  

type tn_gen = 
  | TNLIFTED of tn_gen lifted
  |	GENDEFS of def_gen list
  | GENSTMTS of stmt_gen list
  | GENEXPS of exp_gen list
  | TNBASE of tree_node node

type tree_gen = TNS of tn_gen list | TREELIFTED of tree_gen lifted | TBASE of tree

type contextNode = 
	TREEGEN of tree_gen
  | STMTGEN of stmt_gen
  | EXPGEN of exp_gen
  | TNGEN of tn_gen
  | DEFGEN of def_gen

let dum_to_context = function
  | TREE(t) -> TREEGEN(TBASE(t))
  | STMT(s) -> STMTGEN(STMTBASE(s))
  | EXP(e) -> EXPGEN(EXPBASE(e))
  | TREENODE(tn) -> TNGEN(TNBASE(tn))
  | DEF(d) -> DEFGEN(DBASE(d))

let lifted printer = function 
  | STAR -> "STAR"
  | MAYBE(alist) -> "MAYBE(" ^ (lfoldl (fun res -> fun a -> res ^ (printer a)) "" alist) ^ ")"
  | ATLEAST(alist) -> "ATLEAST(" ^ (lfoldl (fun res -> fun a -> res ^ (printer a)) "" alist) ^ ")"
  | LNOTHING -> "LNOTHING"
  | EXACT(a) -> "EXACT[" ^ (printer a)^ "]"

let lst_str printer printed = 
  lfoldl
	(fun res -> 
	  fun ele ->
		res ^ " , " ^ (printer ele)) "" printed

let rec exp_str = function
  | EXPBASE(exp) -> "EXPBASE:" ^ (Pretty.sprint ~width:80 (d_exp () exp))
  | ELIFTED(elifted) -> "ELIFTED(" ^ (lifted exp_str elifted) ^ ")"
  | UNARYOP(uop,exp) -> "UNARYOPERATION(" ^ (uop_str uop) ^ " " ^ (exp_str exp) ^ ")"
  | BINOP(bop,exp1,exp2) -> "BINOP(" ^ (exp_str exp1) ^ " " ^ (bop_str bop) ^ " " ^ (exp_str exp2) ^ ")"
  | QUESTOP(exp1,exp2,exp3) -> "QUESTOP(" ^ (exp_str exp1) ^ " ? " ^ (exp_str exp2) ^ " : " ^ (exp_str exp3) ^ ")"
  | CASTOP((sg,dt),ie) -> "CASTOP"
  | CALLOP(exp1,elist) -> "CALLOP: "^ (exp_str exp1) ^ "(" ^ (lst_str exp_str elist) ^ ")"
  | COMMAOP(elist) -> "COMMAOP: " ^ "(" ^ (lst_str exp_str elist) ^ ")" 
  | PARENOP(exp) -> "PAREN (" ^ (exp_str exp) ^ ")"
  | EXPSIZOFOP(exp) -> "EXPSIZEOF( " ^ (exp_str exp) ^ ")"
  | TYPESIZOFOP(sg,dt) -> "TYPESIZEOF"
  | EXPALIGNOFOP(exp) -> "EXPALIGNOF(" ^ (exp_str exp) ^ ")"
  | TYPEALIGNOFOP(sg,dt) -> "TYPEALIGNOF"
  | INDEXOP(exp1,exp2) -> "INDEX: " ^ (exp_str exp1) ^ "[" ^ (exp_str exp2) ^ "]"
  | MEMBEROFOP(exp,str) -> "MEMBEROF: " ^ (exp_str exp) ^ "-> " ^ str
  | MEMBEROFPTROP(exp,str) -> "MEMBEROFPTR: *" ^ (exp_str exp) ^ "-> " ^ str
  | ADDROFEXP(exp) -> "ADDROF: &" ^ (exp_str exp)
  | OPERATION(op,exp) -> "OPPERATION: "^ (op_str op) ^ " on " ^ (exp_str exp)
  | SOMEMEMBER(exp,str) -> "SOMEMEMBER: " ^ (exp_str exp) ^ "-> " ^ str
  | VALUE(exp) -> "VALUE(" ^ (exp_str exp) ^ ")"

and uop_str = function
  | Sizeof -> "Sizeof"
  | Sign_modifier -> "Sign_modifier"
  | Memory_operator -> "Memory_operator"
  | Not_operator -> "Not_operator"
  | Alignof -> "Alignof"
  | Pre_operator -> "Pre_operator"
  | Post_operator -> "Post_operator"
  | Increment -> "Increment"
  | Decrement -> "Decrement"
  | Uop(u) -> 
	"Uop("^
	  begin
		match u with
		  MINUS -> "MINUS"
		| PLUS -> "PLUS"
		| NOT -> "NOT"
		| BNOT -> "BNOT"
		| MEMOF -> "MEMOF"
		| ADDROF -> "ADDROF"
		| PREINCR -> "PREINCR"
		| PREDECR -> "PREDECR"
		| POSINCR -> "POSINCR"
		| POSDECR -> "POSDECR"
	end ^ ")"
  | Ugen(ops) -> "Uop(" ^ (op_str ops) ^ ")"
 | Uop_gen(ul) -> "LiftedUop(" ^ (lifted uop_str ul) ^ ")"

and bop_str = function
  | Modify_assign -> "Modify_assign"
  | BitTruth -> "BitTruth"
  | NotBitTruth -> "NotBitTruth"
  | Shift -> "Shift"
  | Bgen(o) -> "BGen(" ^ (op_str o) ^ ")"
  | Bop(b) ->
	"Bop(" ^
	  begin
		match b with 
		  ADD -> "ADD"
		| SUB -> "SUB"
		| MUL -> "MUL"
		| DIV -> "DIV"
		| MOD -> "MOD"
  		| AND -> "AND"
		| OR -> "OR"
  		| BAND -> "BAND"
		| BOR -> "BOR"
		| XOR -> "XOR"
		| SHL -> "SHL"
		| SHR-> "SH"
  		| EQ -> "EQ"
		| NE -> "NE"
		| LT -> "LT"
		| GT -> "GT"
		| LE -> "LE"
		| GE -> "GE"
  		| ASSIGN -> "ASSIGN"
  		| ADD_ASSIGN -> "ADD_ASSIGN"
		| SUB_ASSIGN -> "SUB_ASSIGN"
		| MUL_ASSIGN -> "MUL_ASSIGN"
		| DIV_ASSIGN -> "DIV_ASSIGN"
		| MOD_ASSIGN -> "MOD_ASSIGN"
  		| BAND_ASSIGN -> "BAND_ASSIGN"
		| BOR_ASSIGN -> "BOR_ASSIGN"
		| XOR_ASSIGN -> "XOR_ASSIGN"
		| SHL_ASSIGN -> "SHL_ASSIGN"
		| SHR_ASSIGN -> "SHR_ASSIG"
	  end ^ ")"
 | Bop_gen(b) -> "LiftedBop(" ^ (lifted bop_str b) ^ ")"

and op_str = function
  | Modify_value -> "Modify_value"
  | Arithmetic -> "Arithmetic"
  | Bitwise -> "Bitwise"
  | Logic -> "Logic"
  | OnNumbers -> "OnNumbers"
  | OnBits -> "OnBits"
  | Bop_op(bop) -> "BopOp("^ (bop_str bop) ^ ")"
  | Uop_op(uop) -> "UopOp("^ (uop_str uop) ^ ")"
  | Lifted_ops(ol) -> "LiftedOp(" ^ (lifted op_str ol) ^ ")"
and def_str = function 
  | DLIFTED(dl) -> "LiftedDef(" ^ (lifted def_str dl) ^ ")"
  | DBASE(d) -> "DBASE: " ^ (Pretty.sprint ~width:80 (d_def () d))

and tn_str = function
  | TNLIFTED(tn) -> "LiftedTN(" ^ (lifted tn_str tn) ^ ")"
  |	GENDEFS(def_gen) ->
	"GENDEFS [" ^ (lst_str def_str def_gen) ^ "]"
  | GENSTMTS(stmt_gen) ->
	"GENSTMTS [" ^ (lst_str stmt_str stmt_gen) ^ "]"
  | GENEXPS(exp_gen) ->
	"GENEXPS [" ^ (lst_str exp_str exp_gen) ^ "]"
  | TNBASE(tn) -> "TNBASE: " ^ (Pretty.sprint ~width:80 (d_tree_node () tn))

and block_str = function
  | Reg(b) -> "{" ^ (lst_str stmt_str b)  ^ "}"
  | BLKLIFTED(b) -> "LiftedBlk(" ^ (lifted block_str b) ^ ")"

and stmt_str = function
  | STMTBASE(s) -> "STMTBASE:" ^  (Pretty.sprint ~width:80 (d_stmt () s))
  | SLIFTED(sl) -> "LiftedStmt("^ (lifted stmt_str sl) ^ ")"
  | STMTCOMP(e) -> "STMTCOMP(" ^ (exp_str e) ^ ")"
  | STMTBLOCK(b) -> "STMTBLOCK{" ^ block_str b ^ "}"
  | STMTSEQ(s1,s2) -> "STMTSEQ(" ^ (stmt_str s1) ^ " ; " ^ (stmt_str s2) ^ ")"
  | STMTIF(e1,s1,s2) -> "STMTIF( " ^ (exp_str e1) ^ " then " ^ (stmt_str s1) ^ " else " ^ (stmt_str s2)
  | STMTFOR(fc,e1,e2,s1) -> "STMTFOR( FCNI," ^ (exp_str e1) ^", "^(exp_str e2) ^") {" ^ (stmt_str s1) ^ " }"
  | STMTLOOP(lt, e1,s1) -> 
	"LOOP( " ^ (match lt with | Any -> "Any" | While -> "While" | DoWhile -> "DoWhile" | AnyWhile -> "AnyWhile") ^ ", " ^ (exp_str e1) ^ " : " ^ (stmt_str s1) ^ ")"	  
  | STMTCONTROL -> "STMTCONTROL"
  | STMTRET(e1) -> "STMTRET(" ^ (exp_str e1) ^ ")"
  | STMTSWITCH(e1,s1) -> "STMTSW( " ^ (exp_str e1) ^ " ) { " ^ (stmt_str s1) ^ " }"
  | STMTCASE(e1,s1) -> "STMTCASE( case " ^ (exp_str e1) ^ ": " ^ (stmt_str s1) ^ " )"
  | STMTCASERANGE(e1,e2,s1) -> "STMTCASERANGE( case " ^ (exp_str e1) ^ " ... "  ^ (exp_str e2) ^ ":" ^ (stmt_str s1) ^ ")"
  | STMTDEFAULT(s1) -> "STMTDEFAULT( " ^ (stmt_str s1) ^ ")"
  | STMTLABEL(str,s1) -> "STMTLABEL( " ^ str ^ ": " ^ (stmt_str s1) ^ ")" 
  | STMTCOMPGOTO(e1) -> "STMTCOMPGOTO( "^(exp_str e1) ^ ")"
  | STMTDEF(d) -> "STMTDEF(" ^ (def_str d) ^ ")"
  | STMTTRYE(b1,e1,b2) -> "STMTTRYE( try { " ^ block_str b1 ^ " } except ( " ^ (exp_str e1) ^ " ) { " ^ block_str b2 ^ " } )"
  | STMTTRYF(b1,b2) -> "STMTTRYF( try { " ^ block_str b1 ^ " } finally { " ^ block_str b2 ^ " } ) "

let print_tn_gen tn = pprintf "%s\n" (tn_str tn)
let print_def_gen def = pprintf "%s\n" (def_str def)
let print_stmt_gen stmt = pprintf "%s\n" (stmt_str stmt)
let print_exp_gen exp = pprintf "%s\n" (exp_str exp)

