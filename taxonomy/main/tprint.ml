open Batteries
open Utils
open Cabs
open Cprint
open Cabswalker
open Ttypes
open Difftypes

let addparen rest = rest ^ " )"
let addbrack rest = rest ^ " ]"
let basecomb str = CombineChildrenPost(str,addparen)
let basebrack str = CombineChildrenPost(str,addbrack)

let lifted printer = function 
  | STAR -> "STAR"
  | MAYBE(alist) -> "MAYBE(" ^ (lfoldl (fun res -> fun a -> res ^ (printer a)) "" alist) ^ ")"
  | ATLEAST(alist) -> "ATLEAST(" ^ (lfoldl (fun res -> fun a -> res ^ (printer a)) "" alist) ^ ")"
  | LNOTHING -> "LNOTHING"
  | PARTIALMATCH(a) -> "PARTIAL(" ^ printer a ^ ")"
  | UNUNIFIED(alist) -> "UNUNIFIED("^ (lfoldl (fun res -> fun a -> res ^ (printer a)) "" alist) ^ ")"

let lst_str printer printed = 
  lfoldl
	(fun res -> 
	  fun ele ->
		res ^ " , " ^ (printer ele)) "" printed

let rec uop_str = function
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


class ttypesPrintWalker = object(self)
  inherit [string,typeSpec_gen,se_gen,spec_gen,dt_gen,ng_gen,ing_gen,name_gen,in_gen,
		   sn_gen,def_gen,block_gen,stmt_gen,exp_gen,ie_gen,attr_gen,tn_gen,tree_gen] singleWalker as super 

  method combine val1 val2 = val1^val2
  method default_res () = ""

  method wTypeSpecifier = function
  | TSBASE(_) -> basecomb "TSBASE( "
  | TSTYPEOFE(_) -> basecomb "TSTYPEOFE( "
  | TSTYPEOFT(_) -> basecomb "TSTYPEOFT( "
  | TSSORU(_) -> basecomb "TSSORU( "
  | TSLIFTED(_) -> basecomb "TSLIFTED( "

  method childrenTypeSpecifier ts =
	let strorun = function
	  | Struct -> "Struct"
	  | Union -> "Union"
	  | Something -> "Something"
	in
	let rec fg_str = function
	  | FGBASE(fg) -> Pretty.sprint ~width:80 (d_field_group () fg)
	  | FGGEN(spec,lst) -> self#walkSpecifier spec ^ 
		(lst_str (fun (name,eno) -> self#walkName name ^ " " ^ (match eno with Some(e) -> self#walkExpression e | None -> ""))
		   lst)
	  | FGLIFTED(fg) -> lifted fg_str fg
	in
	  match ts with
	  | TSBASE(ts) -> Pretty.sprint ~width:80 (d_type_spec () ts)
	  | TSTYPEOFE(exp) -> self#walkExpression exp
	  | TSTYPEOFT(spec,dt) -> self#walkSpecifier spec ^ ", " ^ self#walkDeclType dt
	  | TSSORU(str,storun,Some(fgs),attrs) ->
		(strorun storun) ^ " " ^ str ^ "{ " ^ lst_str fg_str fgs ^ " } (" ^ self#walkAttributes attrs ^ " )"
	  | TSSORU(str,storun,None,attrs) ->
		(strorun storun) ^ " " ^ str ^ "{ } (" ^ self#walkAttributes attrs ^ " )"
	  | TSLIFTED(ts) -> lifted self#walkTypeSpecifier ts

  method wSpecifier = function
  | Spec_list(ses) -> basecomb "SPECLIST( " 
  | Spec_lifted(sg) -> basecomb "SPECLIFTED( "
  | Spec_base(spec) -> basecomb "SPECBASE( "

  method childrenSpecifier = function
  | Spec_list(ses) -> lst_str self#walkSpecElem ses
  | Spec_lifted(sg) -> lifted self#walkSpecifier sg
  | Spec_base(spec) -> Pretty.sprint ~width:80 (d_specifier () spec)

  method wDeclType = function
  | DTBASE(_) -> basecomb "DTBASE( "
  | DTLIFTED(_) -> basecomb "DTLIFTED( "
  | DTPAREN(_) -> basecomb "DTPAREN( "
  | DTARRAY(_) -> basecomb "DTARRAY( "
  | DTPTR(_) -> basecomb "DTPTR( "
  | DTPROTO(_) -> basecomb "DTPROTO( "
  | DTCOMPLEX(_) -> basecomb "DTCOMPLEX( "

  method childrenDeclType = function
  | DTBASE(dt) -> Pretty.sprint ~width:80 (d_decl_type () dt)
  | DTLIFTED(dt) -> lifted self#walkDeclType dt
  | DTPAREN(attrs1,dt,attrs2) -> self#walkAttributes attrs1 ^ " ( " ^ self#walkDeclType dt ^ " ) " ^ self#walkAttributes attrs2
  | DTARRAY(dt,attrs,exp) -> self#walkDeclType dt ^ " ( "^ self#walkAttributes attrs ^ " ) [ " ^ self#walkExpression exp ^ " ]"
  | DTPTR(attrs,dt) -> self#walkAttributes attrs ^ " *" ^ self#walkDeclType dt
  | DTPROTO(dt,sns) -> self#walkDeclType dt ^ " ( " ^ lst_str self#walkSingleName sns ^ " )"
  | DTCOMPLEX(dt,attrs) -> self#walkDeclType dt ^ "(" ^ self#walkAttributes attrs ^ " )"

  method wExpression = function
  | EXPBASE(exp) -> basecomb "EXPBASE( "
  | ELIFTED(elifted) -> basecomb "ELIFTED( "
  | GNUGEN(b_gen) -> basecomb "GNU_BODY_GEN( "
  | CONSTGEN(c) -> basecomb "CONST_GEN( "
  | UNARYOP(uop,exp) -> basecomb "UNARYOPERATION( "
  | BINOP(bop,exp1,exp2) -> basecomb "BINOP( "
  | QUESTOP(exp1,exp2,exp3) -> basecomb "QUESTOP( "
  | CASTOP((sg,dt),ie) -> basecomb "CASTOP( "
  | CALLOP(exp1,elist) -> basecomb "CALLOP( "
  | COMMAOP(elist) -> basecomb "COMMAOP( "
  | PARENOP(exp) -> basecomb "PAREN( "
  | EXPSIZEOFOP(exp) -> basecomb "EXPSIZEOF( "
  | TYPESIZEOFOP(sg,dt) -> basecomb "TYPESIZEOF( "
  | EXPALIGNOFOP(exp) -> basecomb "EXPALIGNOF( "
  | TYPEALIGNOFOP(sg,dt) -> basecomb "TYPEALIGNOF( "
  | INDEXOP(exp1,exp2) -> basecomb "INDEX( "
  | MEMBEROFOP(exp,str) -> basecomb "MEMBEROF( "
  | MEMBEROFPTROP(exp,str) -> basecomb "MEMBEROFPTR( *"
  | ADDROFEXP(exp) -> basecomb "ADDROF( &"
  | OPERATION(op,exp) -> basecomb "OPPERATION( "
  | SOMEMEMBER(exp,str) -> basecomb "SOMEMEMBER( "
  | VALUE(exp) -> basecomb "VALUE( "

  method childrenExpression = function
  | EXPBASE(exp) -> Pretty.sprint ~width:80 (d_exp () exp)
  | ELIFTED(elifted) -> lifted self#walkExpression elifted
  | GNUGEN(b_gen) -> self#walkBlock b_gen
  | CONSTGEN(c) -> lifted (fun c -> Pretty.sprint ~width:80 (d_const () c)) c
  | UNARYOP(uop,exp) -> (uop_str uop) ^ " " ^ self#walkExpression exp
  | BINOP(bop,exp1,exp2) -> self#walkExpression exp1 ^ " " ^ (bop_str bop) ^ " " ^ self#walkExpression exp2
  | QUESTOP(exp1,exp2,exp3) -> self#walkExpression exp1 ^ " ? " ^ self#walkExpression exp2 ^ " : " ^ self#walkExpression exp3
  | CASTOP((sg,dt),ie) -> "( " ^ self#walkSpecifier sg ^ " " ^ self#walkDeclType dt ^ " ) " ^ self#walkInitExpression ie
  | CALLOP(exp1,elist) -> self#walkExpression exp1 ^ ") (" ^ (lst_str self#walkExpression elist)
  | COMMAOP(elist) -> lst_str self#walkExpression elist
  | PARENOP(exp)
  | EXPSIZEOFOP(exp)
  | ADDROFEXP(exp)
  | VALUE(exp)
  | EXPALIGNOFOP(exp) -> self#walkExpression exp
  | TYPESIZEOFOP(sg,dt)
  | TYPEALIGNOFOP(sg,dt) -> self#walkSpecifier sg ^ " ," ^ self#walkDeclType dt
  | INDEXOP(exp1,exp2) -> self#walkExpression exp1 ^ "[" ^ self#walkExpression exp2
  | SOMEMEMBER(exp,str)
  | MEMBEROFOP(exp,str)
  | MEMBEROFPTROP(exp,str) -> self#walkExpression exp ^ "-> " ^ str
  | OPERATION(op,exp) -> (op_str op) ^ " on " ^ self#walkExpression exp

  method wInitExpression = function
  | IEBASE(_) -> basecomb "IEBASE( "
  | GENSINGLE(_) -> basecomb "IESINGLE( "
  | GENCOMPOUND(_) -> basecomb "IECOMPOUND( "
  | IELIFTED(_) -> basecomb "IELIFTED( "

  method childrenInitExpression = function
  | IEBASE(ie) -> Pretty.sprint ~width:80 (d_init_expression () ie)
  | GENSINGLE(exp) -> self#walkExpression exp
  | GENCOMPOUND(iwies) -> failwith "NOT IMPLEMENTED"
  | IELIFTED(ie) -> lifted self#walkInitExpression ie

  method wStatement = function
  | STMTBASE(s) -> basecomb "STMTBASE:" 
  | SLIFTED(sl) -> basecomb "LiftedStmt("
  | STMTCOMP(e) -> basecomb "STMTCOMP(" 
  | STMTBLOCK(b) -> basecomb "STMTBLOCK{" 
  | STMTSEQ(s1,s2) -> basecomb "STMTSEQ(" 
  | STMTIF(e1,s1,s2) -> basecomb "STMTIF( " 
  | STMTFOR(fc,e1,e2,s1) -> basecomb "STMTFOR( FCNI," 
  | STMTLOOP(lt, e1,s1) -> basecomb "LOOP( " 
  | STMTCONTROL -> Result("STMTCONTROL")
  | STMTRET(e1) -> basecomb "STMTRET(" 
  | STMTSWITCH(e1,s1) -> CombineChildrenPost("STMTSW( ", (fun res -> res ^ " }"))
  | STMTCASE(e1,s1) -> basecomb "STMTCASE( case " 
  | STMTCASERANGE(e1,e2,s1) -> basecomb "STMTCASERANGE( case " 
  | STMTDEFAULT(s1) -> basecomb "STMTDEFAULT( " 
  | STMTLABEL(str,s1) -> basecomb "STMTLABEL( " 
  | STMTCOMPGOTO(e1) -> basecomb "STMTCOMPGOTO( "
  | STMTDEF(d) -> basecomb "STMTDEF(" 
  | STMTTRYE(b1,e1,b2) -> basecomb "STMTTRYE( try { " 
  | STMTTRYF(b1,b2) -> basecomb "STMTTRYF( try { " 

  method childrenStatement = function
  | STMTBASE(s) -> Pretty.sprint ~width:80 (d_stmt () s)
  | SLIFTED(sl) -> lifted self#walkStatement sl
  | STMTCOMP(e) -> self#walkExpression e
  | STMTBLOCK(b) -> self#walkBlock b
  | STMTSEQ(s1,s2) -> self#walkStatement s1 ^ " ; " ^ self#walkStatement s2
  | STMTIF(e1,s1,s2) -> self#walkExpression e1 ^ " then " ^ self#walkStatement s1 ^ " else " ^ self#walkStatement s2
(* FIXME: fix For clause! *)
  | STMTFOR(fc,e1,e2,s1) -> self#walkExpression e1 ^", "^self#walkExpression e2 ^") {" ^ self#walkStatement s1 ^ " }"
  | STMTLOOP(lt, e1,s1) -> 
	 (match lt with | Any -> "Any" | While -> "While" | DoWhile -> "DoWhile" | AnyWhile -> "AnyWhile") ^ ", " ^ self#walkExpression e1 ^ " : " ^ self#walkStatement s1
  | STMTRET(e1) ->  self#walkExpression e1
  | STMTSWITCH(e1,s1) -> self#walkExpression e1 ^ " ) { " ^ self#walkStatement s1 ^ " }"
  | STMTCASE(e1,s1) -> self#walkExpression e1 ^ ": " ^ self#walkStatement s1
  | STMTCASERANGE(e1,e2,s1) -> "STMTCASERANGE( case " ^ self#walkExpression e1 ^ " ... "  ^ self#walkExpression e2 ^ ":" ^ self#walkStatement s1 ^ ")"
  | STMTDEFAULT(s1) -> self#walkStatement s1
  | STMTLABEL(str,s1) -> str ^ ": " ^ self#walkStatement s1
  | STMTCOMPGOTO(e1) -> self#walkExpression e1
  | STMTDEF(d) -> self#walkDefinition d
  | STMTTRYE(b1,e1,b2) -> self#walkBlock b1 ^ " } except ( " ^ self#walkExpression e1 ^ " ) { " ^ self#walkBlock b2 ^ " }"
  | STMTTRYF(b1,b2) -> self#walkBlock b1 ^ " } finally { " ^ self#walkBlock b2 ^ " } "
  | STMTCONTROL -> ""

  method wBlock = function
  | Reg(b) -> CombineChildrenPost("REGBLK {", (fun res -> res^ " }"))
  | BLKLIFTED(b) -> basecomb "LiftedBlk("
  | BLOCKBASE(b) -> basecomb "BaseBlk( "

  method childrenBlock = function
  | Reg(b) -> lst_str self#walkStatement b
  | BLKLIFTED(b) -> lifted self#walkBlock b
  | BLOCKBASE(b) -> Pretty.sprint ~width:80 (d_block () b)

  method wDefinition = function
  | DLIFTED(dl) -> basecomb "LiftedDef( "
  | DBASE(d) -> basecomb "DBASE( "
  | DFUNDEF(sn,b) -> basecomb "DFUNDEF( "
  | DDECDEF(ing) -> basecomb "DDECDEF( "
  | DTYPEDEF(ng) -> basecomb "DTYPDEF( "
  | DONLYTD(spec) -> basecomb "DONLYTD( "
  | DPRAGMA(e) -> basecomb "DPRAGMA( "
  | DLINK(str,defs) -> basecomb "DLINK( "
  | DGENERICTYPE(spec,names) -> basecomb "DGENERICTYPE( "
  | DGENERICDEC(spec,name) -> basecomb "DGENERICDEC( "

  method childrenDefinition = function
  | DLIFTED(dl) -> lifted self#walkDefinition dl
  | DBASE(d) -> Pretty.sprint ~width:80 (d_def () d)
  | DFUNDEF(sn,b) -> self#walkSingleName sn ^ " ) " ^ self#walkBlock b
  | DDECDEF(ing) -> self#walkInitNameGroup ing
  | DTYPEDEF(ng) -> self#walkNameGroup ng
  | DONLYTD(spec) -> self#walkSpecifier spec
  | DPRAGMA(e) -> self#walkExpression e
  | DLINK(str,defs) -> str ^ " ( " ^ lst_str self#walkDefinition defs
  | DGENERICTYPE(spec,names) -> self#walkSpecifier spec ^ " ( " ^ lst_str self#walkName names
  | DGENERICDEC(spec,name) -> self#walkSpecifier spec ^ "( " ^ self#walkName name ^ ")"

  method wName = function
  | NAMEBASE(name) -> Result("NAMEBASE( " ^ Pretty.sprint ~width:80 (d_name () name) ^ ")")
  | NAMEGEN(str,dt,attrs) -> basecomb ("NAMEGEN( " ^ str)
  | NAMELIFTED(name) -> Result("NAMELIFTED( " ^ lifted self#walkName name ^ " )")

  method childrenName = function
  | NAMEBASE(name) -> Pretty.sprint ~width:80 (d_name () name)
  | NAMEGEN(str,dt,attrs) -> self#walkDeclType dt ^ "( " ^ self#walkAttributes attrs ^ " )"
  | NAMELIFTED(name) -> lifted self#walkName name

  method wAttribute = function
  | ATTRBASE(attr) -> basecomb "ATTRBASE( "
  | ATTRLIFTED(attr) -> basecomb "ATTRLIFTED( "
  | ATTRGEN(str,elist) -> basecomb "ATTRGEN( "

  method childrenAttribute = function
  | ATTRBASE(attr) -> Pretty.sprint ~width:80 (d_attr () attr)
  | ATTRLIFTED(attr) -> lifted self#walkAttribute attr 
  | ATTRGEN(str,elist) -> str ^ ", " ^ lst_str self#walkExpression elist

  method wTreenode = function
  | TNLIFTED(tn) -> basecomb "LiftedTN("
  |	GENDEFS(def_gen) -> basebrack "GENDEFS ["
  | GENSTMTS(stmt_gen) -> basebrack	"GENSTMTS [" 
  | GENEXPS(exp_gen) -> basebrack "GENEXPS ["
  | TNBASE(tn) -> basecomb "TNBASE: "

  method childrenTreenode = function
  | TNLIFTED(tn) -> lifted self#walkTreenode tn 
  |	GENDEFS(def_gen) -> lst_str self#walkDefinition def_gen
  | GENSTMTS(stmt_gen) -> lst_str self#walkStatement stmt_gen
  | GENEXPS(exp_gen) -> lst_str self#walkExpression exp_gen
  | TNBASE(tn) ->Pretty.sprint ~width:80 (d_tree_node () tn)

  method wTree = function
  | TNS(tns) -> basecomb "TNS( "
  | TREELIFTED(t) -> basecomb "TREELIFTED( "
  | TBASE(t) -> basecomb "TBASE( "

  method childrenTree = function
  | TNS(tns) -> lst_str self#walkTreenode tns
  | TREELIFTED(t) -> lifted self#walkTree t
  | TBASE(t) -> Pretty.sprint ~width:80 (d_tree () t) 

  method wSpecElem = function
  | Spec_elem(se) -> basecomb "SEBASE( "
  | Se_attr(attr) -> basecomb "SEATTR( "
  | Se_type(ts) -> basecomb "SETYPE( "
  | Se_lifted(se) -> basecomb "SELIFTED( "
  | Se_CV(_) -> Result("SECVLIF(FIXME)")
  | Se_storage(_) -> Result("SESTORAGELIF(FIXME)")

  method childrenSpecElem = function
  | Spec_elem(se) -> Pretty.sprint ~width:80 (d_spec_elem () se)
  | Se_attr(attr) -> self#walkAttribute attr
  | Se_type(ts) -> self#walkTypeSpecifier ts
  | Se_lifted(se) -> lifted self#walkSpecElem se
  | _ -> ""

  method wSingleName = function
  | SNBASE(sn) -> basecomb "SNBASE( " 
  | SNGEN(spec,name) -> basecomb "SNGEN(" 
  | SNLIFTED(sn) -> basecomb "SNLIFTED( " 

  method childrenSingleName = function 
  | SNBASE(sn) ->  Pretty.sprint ~width:80 (d_single_name () sn)
  | SNGEN(spec,name) -> self#walkSpecifier spec ^ " " ^ self#walkName name
  | SNLIFTED(sn) -> lifted self#walkSingleName sn

  method wNameGroup = function
  | NGBASE(ng) -> basecomb "NGBASE( "
  | NGGEN(spec,names) -> basecomb "NGEN( "
  | NGLIFTED(ng) -> basecomb "NGLIFTED( "

  method childrenNameGroup = function
  | NGBASE(ng) -> Pretty.sprint ~width:80 (d_name_group () ng)
  | NGGEN(spec,names) -> self#walkSpecifier spec ^ "," ^ lst_str self#walkName names
  | NGLIFTED(ng) -> lifted self#walkNameGroup ng

  method wInitNameGroup = function
  | INGBASE(_) -> basecomb "INGBASE( "
  | INGGEN(_) -> basecomb "INGGEN( "
  | INGLIFTED(_) -> basecomb "INGLIFTED( "

  method childrenInitNameGroup = function
  | INGBASE(ing) -> Pretty.sprint ~width:80 (d_init_name_group () ing)
  | INGGEN(spec,inn) -> self#walkSpecifier spec ^ ", " ^ lst_str self#walkInitName inn 
  | INGLIFTED(ing) -> lifted self#walkInitNameGroup ing

  method wInitName = function
  | INBASE(iname) -> basecomb "INBASE( "
  | INGEN(name,ie) -> basecomb "INGEN( "
  | INLIFTED(ilifted) -> basecomb "INLIFTED( "

  method childrenInitName = function
  | INBASE(iname) -> Pretty.sprint ~width:80 (d_init_name () iname)
  | INGEN(name,ie) -> failwith "FIXME TYPES" (*self#walkName name ^ ", " ^ self#walkInitExpression ie*)
  | INLIFTED(ilifted) -> lifted self#walkInitName ilifted

  method wDummyGen = function
  | TREEGEN(t) -> basecomb "TREEGEN( "
  | STMTGEN(s) -> basecomb "STMTGEN( "
  | EXPGEN(e) -> basecomb "EXPGEN( "
  | TNGEN(tn) -> basecomb "TNGEN( "
  | DEFGEN(def) -> basecomb "DEFGEN( "
  | DUMLIFTED(dl) -> basecomb "DUMLIFTED( "
  | DUMBASE(dum) -> basecomb "DUMBASE( "

  method childrenDummyGen = function
  | TREEGEN(t) -> self#walkTree t
  | STMTGEN(s) -> self#walkStatement s
  | EXPGEN(e) -> self#walkExpression e
  | TNGEN(tn) -> self#walkTreenode tn
  | DEFGEN(def) -> self#walkDefinition def
  | DUMLIFTED(dl) -> lifted self#walkDummyGen dl
  | DUMBASE(dum) -> dummy_node_to_str dum

  method wChangeGen = function
  |	InsertGen(dg) -> basecomb "Insert("
  | MoveGen(dg) -> basecomb "Move("
  | DeleteGen(dg) -> basecomb "Delete("
  | ReplaceGen(dg1,dg2) -> basecomb "Replace("
  | ChangeLifted(cg) -> basecomb "ChangeLifted("
  | ChangeBase(c) -> basecomb "ChangeBase("

  method childrenChangeGen = function
  |	InsertGen(dg)
  | MoveGen(dg)
  | DeleteGen(dg) -> self#walkDummyGen dg 
  | ReplaceGen(dg1,dg2) -> self#walkDummyGen dg1 ^ " with "^ self#walkDummyGen dg2
  | ChangeLifted(cg) -> lifted self#walkChangeGen cg
  | ChangeBase(c) -> standard_eas_to_str c

  method wChangesGen = function
  | BASECHANGES(cgs) -> basecomb "BASECHANGES( "
  | CHANGEATLEAST(cgs) -> basecomb "ATLEAST( "

  method childrenChangesGen = function
  | BASECHANGES(cgs)
  | CHANGEATLEAST(cgs) -> lst_str self#walkChangeGen cgs

  method private walkAttributes attrs = lst_str self#walkAttribute attrs
  method walkChangeGen cg = doWalk self#combine self#wChangeGen self#childrenChangeGen cg
  method walkChangesGen cg = doWalk self#combine self#wChangesGen self#childrenChangesGen cg
  method walkDummyGen dg = doWalk self#combine self#wDummyGen self#childrenDummyGen dg
end

let genprinter = new ttypesPrintWalker

let print_tn_gen tn = genprinter#walkTreenode tn
let print_def_gen def = genprinter#walkDefinition def 
let print_stmt_gen stmt = genprinter#walkStatement stmt
let print_exp_gen exp = genprinter#walkExpression exp
let print_name_gen name = genprinter#walkName name
let print_se_gen se = genprinter#walkSpecElem se
let print_sn_gen sn = genprinter#walkSingleName sn
let print_attr_gen attr = genprinter#walkAttribute attr
let print_change_gen change = genprinter#walkChangeGen change
let print_dummy_gen dum = genprinter#walkDummyGen dum

let print_guard = function
  | EXPG,e -> "EXPG: " ^ Pretty.sprint ~width:80 (d_exp () e)
  | CATCH,e -> "CATCH: " ^ Pretty.sprint ~width:80 (d_exp () e)

let print_guard_gen = function
  | EXPG,e -> "EXPG: " ^ print_exp_gen e
  | CATCH,e -> "CATCH: " ^ print_exp_gen e

let get_opt pfunc = function
    Some(o) -> pfunc o
  | None -> "None"


let itemplate_to_str (con,changes) =
  "*****Context*****\n" ^
  "parent_treenode: " ^
  get_opt (fun tn -> Pretty.sprint ~width:80 (d_tree_node () tn)) con.parent_treenode ^
  "\nparent_definition: " ^
  get_opt (fun def -> Pretty.sprint ~width:80 (d_def () def)) con.parent_definition ^
  "\nparent_statement: " ^
  get_opt (fun s -> Pretty.sprint ~width:80 (d_stmt () s))  con.parent_statement ^
  "\nparent_expression: " ^
  get_opt (fun e -> Pretty.sprint ~width:80 (d_exp () e)) con.parent_expression ^
  "\nsurrounding: " ^
  lst_str dummy_node_to_str (List.of_enum (DumSet.enum con.surrounding)) ^
  "\nguarded_by: " ^
  lst_str
	(fun guard -> 
	  match guard with 
		EXPG,e -> "EXPG: " ^ Pretty.sprint ~width:80 (d_exp () e)
	  | CATCH,e -> "CATCH: " ^ Pretty.sprint ~width:80 (d_exp () e)
	  ) 
	con.guarded_by ^
  "\nguarding: " ^
  lst_str dummy_node_to_str (List.of_enum (DumSet.enum con.guarding)) ^
  "\n*****END CONTEXT*****\n" ^
  "*****CHANGES*****\n" ^
	lst_str standard_eas_to_str changes ^
	"*****END CHANGES*****\n"

let template_to_str (context,changes) =
	"parent_treenode: " ^
	get_opt (genprinter#walkTreenode) context.ptn ^
	"\nparent_definition: " ^
	get_opt (genprinter#walkDefinition) context.pdef ^
  "\nparent_statement: " ^
  get_opt (genprinter#walkStatement) context.pstmt ^
  "\nparent_expression: " ^
  get_opt (genprinter#walkExpression) context.pexp ^
	"\nSurrounding: " ^
	lst_str (genprinter#walkDummyGen) (List.of_enum (Set.enum context.sding)) ^
"\n"^
   "\n*****END CONTEXT*****\n" ^
   "*****CHANGES*****\n" ^
	genprinter#walkChangesGen changes ^
   "\n*****END CHANGES*****\n"

let print_template t = pprintf "%s\n" (template_to_str t); flush stdout
let print_itemplate it = pprintf "%s\n" (itemplate_to_str it); flush stdout

