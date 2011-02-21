open Batteries
open Utils
open Cabs
open Cprint
open Ttypes
open Cabswalker
open Distance
open Tprint

let unify_exp_ht = hcreate 10
let unify_stmt_ht = hcreate 10
let hash_exp exp = Pretty.sprint ~width:80 (d_exp () exp)
let hash_stmt stmt = Pretty.sprint ~width:80 (d_stmt () stmt)
let def_ht = hcreate 10
let spec_ht = hcreate 10
let ie_ht = hcreate 10
let exps_ht = hcreate 10
let stmts_ht = hcreate 10
let tn_ht = hcreate 10
let iw_ht = hcreate 10
let dts_ht = hcreate 10
let se_ht = hcreate 10

let str_hash = hcreate 10

let unify_string str1 str2 = ht_find str_hash (str1,str2) (fun _ -> Distance.unify_string str1 str2)
(*  (fun _ -> String.of_list(StringDistance.gcs (String.to_list str1) (String.to_list str2)))*)


let sign = function MINUS | PLUS -> true | _ -> false 
let notm = function NOT | BNOT -> true | _ -> false 
let mem = function ADDROF | MEMOF -> true | _ -> false 
let pre = function PREDECR | PREINCR -> true | _ -> false 
let post = function POSDECR | POSINCR -> true | _ -> false 
let incr = function POSINCR | PREINCR -> true | _ -> false
let decr = function POSDECR | PREDECR -> true | _ -> false
let uop_mod uop = incr uop || decr uop 
let uop_num uop = uop_mod uop || sign uop 

let bop_arithmod = function
	ADD_ASSIGN | SUB_ASSIGN | MUL_ASSIGN | DIV_ASSIGN | MOD_ASSIGN -> true
  | _ -> false
let bop_bitmod = function
	BAND_ASSIGN | BOR_ASSIGN | XOR_ASSIGN | SHL_ASSIGN | SHR_ASSIGN -> true
  | _ -> false 
let bop_modify bop = bop_arithmod bop || bop_bitmod bop 
let bop_notbittruth = function AND | OR | EQ | NE | LT | GT | LE | GE -> true | _ -> false
let bop_bittruth = function BAND | BOR | XOR -> true | _ -> false
let bop_truth bop = bop_notbittruth bop || bop_bittruth bop 
let bop_onnumbers bop =
  match bop with
	ADD | SUB | MUL | DIV | MOD | BAND |BOR | XOR |SHL | SHR -> true
  | _ -> bop_modify bop
let bop_bit_assign = function BAND_ASSIGN | BOR_ASSIGN | XOR_ASSIGN -> true | _ -> false
let bop_logic = function AND | OR | EQ | NE | LT | GT | LE | GE -> true | _ -> false
let bop_commut = function ADD | SUB | MUL | DIV | OR | BOR | EQ | NE -> true | _ -> false
let bop_bits bop = bop_bitmod bop || bop_bittruth bop || bop_bit_assign bop 
let pair_match one two three four =
  (one = three && two = four) || (one = four && two = three)

let unify_constant c1 c2 = failwith "Not implemented unify_constant"
(*  if c1 = c2 then EXPBASE(c1) else 
  match c1.node,c2.node with 
  | CONST_INT(i1),CONST_INT(i2) -> 
	let int1 = int_of_string i1 in
	let int2 = int_of_string i2 in 
	  UNUNIFIED([c1;c2])
  | CONST_INT(i1),CONST_FLOAT(f2) 
  | CONST_FLOAT(f2),CONST_INT(i1) ->
	let int1 = int_of_string i1 in 
	let float1 = float_of_int int1 in 
	  UNUNIFIED([CONST_FLOAT(string_of_float float1);CONST_FLOAT(f2)])
  | _ -> failwith "Not implemented unify_constant"
  | CONST_INT(i1),CONST_CHAR(cs1)
  | CONST_CHAR(cs1),CONST_INT(i1) 
  | CONST_INT(i1),CONST_WCHAR(cs2)
  | CONST_WCHAR(cs2),CONST_INT(i1)
  | CONST_INT(i1),CONST_STRING(s2)
  | CONST_STRING(s2),CONST_INT(i1)
  | CONST_INT(i1),CONST_WSTRING(strs2)
  | CONST_WSTRING(strs2),CONST_INT(i1)
  | CONST_FLOAT(f1),CONST_FLOAT(f2) 
  | CONST_FLOAT(f1),CONST_CHAR(cs1)
  | CONST_CHAR(cs1),CONST_FLOAT(f1)
  | CONST_FLOAT(f1),CONST_WCHAR(cs1)
  | CONST_WCHAR(cs1),CONST_FLOAT(f1)
  | CONST_FLOAT(f1),CONST_STRING(str2)
  | CONST_STRING(str2),CONST_FLOAT(f1)
  | CONST_FLOAT(f1),CONST_WSTRING(strs2)
  | CONST_WSTRING(strs2),CONST_FLOAT(f1)
  | CONST_CHAR(cs1),CONST_CHAR(cs2) 
  | CONST_CHAR(cs1),CONST_WCHAR(cs1)
  | CONST_WCHAR(cs1),CONST_CHAR(cs1)
  | CONST_CHAR(cs1),CONST_STRING(str2)
  | CONST_STRING(str2),CONST_CHAR(cs1)
  | CONST_CHAR(cs1),CONST_WSTRING(strs2)
  | CONST_WSTRING(strs2),CONST_CHAR(cs1)
  | CONST_WCHAR(cs1),CONST_WCHAR(cs2) 
  | CONST_WCHAR(cs1),CONST_STRING(str2)
  | CONST_STRING(str2),CONST_WCHAR(cs1)
  | CONST_WCHAR(cs1),CONST_WSTRING(cs2)
  | CONST_WSTRING(cs2),CONST_WCHAR(cs1)
  | CONST_STRING(str1),CONST_STRING(str2) 
  | CONST_STRING(str1),CONST_WSTRING(strs2)
  | CONST_WSTRING(strs2),CONST_STRING(str1)
  | CONST_WSTRING(strs1),CONST_WSTRING(strs2) -> () *)

let atleast res = ATLEAST([res])
(* is this actually what I want? I *think* so, but I worry the "ATLEAST" will throw things off in comparisons.  Hmmm...FIXME *)

let check_hash ht key1 key2 hash ifeq ifnot = 
  let hash1,hash2 = hash key1,hash key2 in
	ht_find ht (hash1,hash2) 
	  (fun _ -> if hash1 = hash2 then ifeq key1 else ifnot ())

let combo_ht = hcreate 10
let info_ht = hcreate 10

let distance  (ht1 : 'a -> string) (ht2 : 'b -> string) (fn : 'a * 'a -> 'b) (val1 : 'a) (val2 : 'a) : float =
  let hash1,hash2 = ht1 val1, ht1 val2 in 
  let hash1,hash2 = if hash1 < hash2 then hash1,hash2 else hash2,hash1 in 
	ht_find combo_ht (hash1,hash2) 
	  (fun _ -> 
		pprintf "hash1: %s\nhash2: %s\n" hash1 hash2;
		let combination : 'b = fn (val1,val2) in
		let info1 = ht_find info_ht hash1 (fun _ -> measure_info val1) in 
		let info2 = ht_find info_ht hash2 (fun _ -> measure_info val2) in 
		let hash3 = ht2 combination in 
		  pprintf "hash3: %s\n" hash3; flush stdout;
		let synth_info = ht_find info_ht hash3 (fun _ -> measure_info combination) in
		let maxinfo = 2.0 /. ((1.0 /. float_of_int(info1)) +. (1.0 /. (float_of_int(info2)))) in
		let retval = (maxinfo -. float_of_int(synth_info)) /. maxinfo in
		let retval = if retval < 0.0 then 0.0 else retval in
(*		  pprintf "Info1: %d, info2: %d, maxinfo: %g synth_info: %d distance: %g\n" info1 info2 maxinfo synth_info retval; *)retval)

let compare (val1 : 'a) (val2 : 'a) : 'a =
  let comp1 = Objsize.objsize val1 in 
  let comp2 = Objsize.objsize val2 in 
	if comp1 > comp2 then val1 else val2

let pretty printer toprint = Pretty.sprint ~width:80 (printer () toprint)

let ifmatch constructor k = Result(constructor k)

let wGeneric (val1,val2) hashtbl printfun mtch partial = check_hash hashtbl val1 val2 printfun (ifmatch mtch) partial

class templateDoubleWalker = object(self)
  inherit [tree_gen,typeSpec_gen,se_gen,spec_gen,dt_gen,ng_gen,ing_gen,name_gen,in_gen,sn_gen,def_gen,block_gen,stmt_gen,exp_gen,ie_gen,attr_gen,tn_gen] doubleCabsWalker as super

  method combine res1 res2 = compare res1 res2

  method default_res() = TREELIFTED(STAR)
  method default_exp() = ELIFTED(STAR)
  method default_stmt() = SLIFTED(STAR)
  method default_def() = DLIFTED(STAR)
  method default_tn() = TNLIFTED(STAR)
  method default_ts() = TSLIFTED(STAR)
  method default_spec() = Spec_lifted (STAR)
  method default_sn() = SNLIFTED(STAR)
  method default_se() = Se_lifted(STAR)
  method default_ng() = NGLIFTED(STAR)
  method default_name() = NAMELIFTED(STAR)
  method default_init_name() = INLIFTED(STAR)
  method default_ing() = INGLIFTED(STAR)
  method default_ie() = IELIFTED(STAR)
  method default_dt() = DTLIFTED(STAR)
  method default_block() = BLKLIFTED(STAR)
  method default_attr() = ATTRLIFTED(STAR)
  (* OK: the point of "children" is to see if there's a better match between exp1
	 and exp2's children or exp2 and exp1's children than there was between exp1
	 and exp2 *)

  method private distance_def = distance (pretty d_definition) print_def_gen self#walkDefinition
  method private distance_stmt = distance (pretty d_stmt) print_stmt_gen self#walkStatement
  method private distance_name = distance (pretty d_name) print_name_gen self#walkName
  method private distance_se = distance (pretty d_spec_elem) print_se_gen self#walkSpecElem
  method private distance_exp = distance (pretty d_exp) print_exp_gen self#walkExpression
  method private distance_tn = distance (pretty d_tree_node) print_tn_gen self#walkTreenode
  method private distance_sn = distance (pretty d_single_name) print_sn_gen self#walkSingleName
  method private distance_attr = distance (pretty d_attr) print_attr_gen self#walkAttribute
 
  method wTreenode (tn1,tn2) = 
	wGeneric (tn1,tn2) tn_ht (pretty d_tree_node) (fun k -> TNBASE(k))
	  (fun _ ->
		match tn1.node,tn2.node with
		| Globals(dlist1),Globals(dlist2) ->
		  Result(GENDEFS(lmap
						   (fun (d1,d2) -> 
							 self#walkDefinition (d1,d2)) (best_mapping self#distance_def dlist1 dlist2)))
		| Stmts(slist1),Stmts(slist2) ->
		  Result(GENSTMTS(lmap
							(fun (s1,s2) -> 
							  self#walkStatement (s1,s2)) (best_mapping self#distance_stmt slist1 slist2)))
		| Exps(elist1),Exps(elist2) ->
		  Result(GENEXPS(self#walkExpressions (elist1,elist2)))
		| _,_ -> Result(TNLIFTED(STAR))) (* the question is: is it worth comparing the internals of Exps/Stmts? wTree takes care of the full list *)

  method wDefinition (def1,def2) =
	wGeneric (def1,def2) def_ht (pretty d_def) (fun d -> DBASE(d))
	  (fun _ ->
		match def1.node,def2.node with
		  FUNDEF(sn1,b1,_,_),FUNDEF(sn2,b2,_,_) -> Result(DFUNDEF(self#walkSingleName (sn1,sn2), self#walkBlock(b1,b2)))
		| DIRECTIVE(d1),DIRECTIVE(d2) ->
		  (match d1.node,d2.node with
			 PREINCLUDE(str1,loc), PREINCLUDE(str2,_) -> Result(DBASE(nd(DIRECTIVE(nd(PREINCLUDE(unify_string str1 str2,loc)))))))
		| DECDEF(ing1,_),DECDEF(ing2,_) -> Result(DDECDEF(self#walkInitNameGroup (ing1,ing2)))
		| TYPEDEF(ng1,_),TYPEDEF(ng2,_) -> Result(DTYPEDEF(self#walkNameGroup (ng1,ng2)))
		| ONLYTYPEDEF(spec1,_),ONLYTYPEDEF(spec2,_) -> Result(DONLYTD(self#walkSpecifier(spec1,spec2)))
		| GLOBASM(str1,loc),GLOBASM(str2,_) -> Result(DBASE(nd(GLOBASM(unify_string str1 str2,loc))))
		| PRAGMA(exp1,_),PRAGMA(exp2,_) -> Result(DPRAGMA(self#walkExpression (exp1,exp2)))
		| LINKAGE(str1,_,dlist1),LINKAGE(str2,_,dlist2) ->
		  Result(DLINK(unify_string str1 str2, 
					   lmap (fun (d1,d2) -> self#walkDefinition (d1,d2)) (best_mapping self#distance_def dlist1 dlist2)))
		| FUNDEF(sn1,b1,_,_),DECDEF(ing1,_)
		| DECDEF(ing1,_),FUNDEF(sn1,b1,_,_) ->
		  let dspec,ins = ing1 in
		  let fspec,nme1 = sn1 in
			Result(DGENERICDEC(self#walkSpecifier (dspec,fspec),
							   (walklist2 (NAMELIFTED(LNOTHING)) (fun (nme1,(nme2,_)) -> self#walkName (nme1,nme2)) nme1 ins)))
		| FUNDEF(sn1,b1,_,_), TYPEDEF(ng,_)
		| TYPEDEF(ng,_), FUNDEF(sn1,b1,_,_) ->
		  let fspec,nme1 = sn1 in
		  let tspec,nmes = ng in
			Result(DGENERICDEC(self#walkSpecifier (fspec,tspec), walklist2 (self#default_name()) self#walkName nme1 nmes))
		| FUNDEF(sn1,b1,_,_), ONLYTYPEDEF(spec1,_)
		| ONLYTYPEDEF(spec1,_), FUNDEF(sn1,b1,_,_) ->
		  let fspec,nme1 = sn1 in 
			Result(DGENERICDEC(self#walkSpecifier (spec1,fspec), NAMELIFTED(LNOTHING)))
		| DECDEF(ing,_), TYPEDEF(ng,_)
		| TYPEDEF(ng,_),DECDEF(ing,_) ->
		  let dspec,ins = ing in
		  let tspec,nmes = ng in
			Result(DGENERICTYPE(self#walkSpecifier(dspec,tspec),
								  lmap (fun (name1,name2) -> self#walkName (name1,name2)) (best_mapping self#distance_name nmes
									 (lmap (fun (n,_) -> n) ins))))
		| DECDEF(ing,_), ONLYTYPEDEF(spec,_)
		| ONLYTYPEDEF(spec,_),DECDEF(ing,_) ->
		  let dspec,ins = ing in
			Result(DGENERICDEC(self#walkSpecifier (dspec,spec), NAMELIFTED(LNOTHING)))
		| TYPEDEF(ng,_), ONLYTYPEDEF(spec,_)
		| ONLYTYPEDEF(spec,_),TYPEDEF(ng,_) ->
		  let tspec,_ = ng in 
			Result(DGENERICDEC(self#walkSpecifier (tspec,spec), NAMELIFTED(LNOTHING)))
		| _ -> ChildrenPost(fun res -> DLIFTED(PARTIALMATCH(res)))
	  )

  method wStatement (stmt1,stmt2) =
	let post s = SLIFTED(PARTIALMATCH(s)) in
	  wGeneric (stmt1,stmt2) unify_stmt_ht (pretty d_stmt) (fun s -> STMTBASE(s))
		(fun _ ->
		  match stmt1.node,stmt2.node with
		  | COMPUTATION(exp1,_),COMPUTATION(exp2,_) ->  Result(STMTCOMP(self#walkExpression(exp1,exp2)))
		  | BLOCK(b1,_),BLOCK(b2,_) -> Result(STMTBLOCK(self#walkBlock (b1,b2)))
		  | SEQUENCE(s1,s2,_),SEQUENCE(s3,s4,_) ->
			let bestHere = 
			  compare
				(STMTSEQ(self#walkStatement (s1,s3), self#walkStatement(s2,s4))) 
				(STMTSEQ(self#walkStatement(s1,s4), self#walkStatement(s2,s3)))
			in
			  Result(bestHere)
		  | IF(e1,s1,s2,_), IF(e2,s3,s4,_) ->
			Result(
			  STMTIF(self#walkExpression (e1,e2), self#walkStatement (s1,s3), self#walkStatement (s2,s4)))
		  | WHILE(e1,s1,_), WHILE(e2,s2,_) ->
			Result(STMTLOOP(While, self#walkExpression (e1,e2), self#walkStatement(s1,s2)))
		  | DOWHILE(e1,s1,_), DOWHILE(e2,s2,_) ->
			Result(STMTLOOP(DoWhile, self#walkExpression (e1,e2), self#walkStatement(s1,s2)))
		  | FOR(fc1,e1,e2,s1,_), FOR(fc2,e3,e4,s2,_) ->
			Result(STMTFOR(self#walkForClause (fc1,fc2), self#walkExpression (e1,e3), self#walkExpression (e2,e4),
										self#walkStatement(s1,s2)))
		  | RETURN(e1,_), RETURN(e2,_) -> Result(STMTRET(self#walkExpression(e1,e2)))
		  | SWITCH(e1,s1,_),SWITCH(e2,s2,_) ->
			Result(STMTSWITCH(self#walkExpression (e1,e2), self#walkStatement (s1,s2)))
		  | CASE(e1,s1,_),CASE(e2,s2,_) ->
			Result(STMTCASE(self#walkExpression (e1,e2), self#walkStatement (s1,s2)))
		  | CASERANGE(e1,e2,s1,_), CASERANGE(e3,e4,s2,_) ->
			Result(STMTCASERANGE(self#walkExpression (e1,e3), self#walkExpression (e2,e4),
											  self#walkStatement (s1,s2)))
		  | DEFAULT(s1,_), DEFAULT(s2,_) -> Result(STMTDEFAULT(self#walkStatement (s1,s2)))
		  | LABEL(str1,s1,_),LABEL(str2,s2,_) -> Result(STMTLABEL(unify_string str1 str2, self#walkStatement (s1,s2)))
		  | GOTO(str1,c),GOTO(str2,_) -> Result(STMTBASE(nd(GOTO(unify_string str1 str2,c))))
		  | COMPGOTO(e1,_),COMPGOTO(e2,_) -> Result(STMTCOMPGOTO(self#walkExpression(e1,e2)))
		  | DEFINITION(def1), DEFINITION(def2) -> Result(STMTDEF(self#walkDefinition (def1,def2)))
		  (* FIXME: Ommitting ASM *)
		  | TRY_EXCEPT(b1,e1,b2,_), TRY_EXCEPT(b3,e2,b4,_) ->
			Result(STMTTRYE(self#walkBlock(b1,b3), self#walkExpression(e2,e2), self#walkBlock(b2,b4)))
		  | TRY_FINALLY(b1,b2,_), TRY_FINALLY(b3,b4,_) ->
			Result(STMTTRYF(self#walkBlock(b1,b3),self#walkBlock(b2,b4)))
		  | _ -> ChildrenPost(post)) (* FIXME? *)

  method wExpression (exp1,exp2) = 
	let postf e = ELIFTED(PARTIALMATCH(e)) in
	  wGeneric (exp1,exp2) unify_exp_ht (pretty d_exp) (fun e -> EXPBASE(e))
		(fun _ ->
		  let unify_uop uop1 uop2 = 
			let both fn = fn uop1 && fn uop2 in
			  if uop1 = uop2 then Uop(uop1)
			  else if both sign then Sign_modifier
			  else if both notm then Not_operator
			  else if both mem then Memory_operator
			  else if both pre then Pre_operator
			  else if both post then Post_operator
			  else if both incr then Increment
			  else if both decr then Decrement
			  else if both uop_mod then Ugen(Modify_value)
			  else if both uop_num then Ugen(OnNumbers)
			  else Uop_gen(STAR) in
		  let unary_labeladdr str uop exp = 
			match uop with
			  ADDROF -> Result(ADDROFEXP(self#walkExpression (nd(VARIABLE(str)), exp)))
			| MEMOF -> Result(UNARYOP(Memory_operator, self#walkExpression (nd(VARIABLE(str)),exp)))
			| _ -> Result(OPERATION(Uop_op(unify_uop ADDROF uop), self#walkExpression (nd(VARIABLE(str)),exp)))
		  in
		  let unary_binary uop unexp bop binexp1 binexp2 = 
			let constant1 = nd(CONSTANT(CONST_INT("1"))) in
			let const_binop op = 
			  Result(BINOP(op, self#walkExpression (unexp,binexp1), self#walkExpression (binexp2,constant1)))
			in
			let const_op op = 
			  Result(OPERATION(op, compare (self#walkExpression (unexp,binexp1)) (self#walkExpression (unexp,binexp2))))
			in
			  if (incr uop) && bop = ADD_ASSIGN then const_binop (Bop(ADD_ASSIGN))
			  else if (uop_mod uop) && (bop_modify bop) then const_binop (Bgen(Modify_value))
			  (* FIXME: the bitwise operators? *)
			  else if (decr uop) && bop = SUB_ASSIGN then const_binop (Bop(SUB_ASSIGN))
			  else if uop = NOT && bop_notbittruth bop then const_op (Bop_op(NotBitTruth))
			  else if uop = BNOT && bop_bittruth bop then const_op (Bop_op(BitTruth))
			  else if (uop = NOT && bop_bittruth bop) || (uop = BNOT && bop_notbittruth bop) then const_op Logic
			  else if bop_onnumbers bop && uop_num uop then const_op OnNumbers
			  else if uop = BNOT && bop_bit_assign bop then const_op OnBits
			  else ChildrenPost(fun res -> OPERATION(Lifted_ops(STAR),ELIFTED(ATLEAST([res]))))
		  in
		  let unary_question uexp qexp1 qexp2 qexp3 = function
			| NOT
			| BNOT -> ChildrenPost(fun res -> OPERATION(Lifted_ops(ATLEAST[Logic]), res)) (*(OPERATION(ATLEAST([Truth]), ATLEAST([self#combine (self#walkExpression uexp qexp1)
																							(self#combine (self#walkExpression uexp qexp2)
																							(self#walkExpression uexp qexp3))]))) That may not work? *)
			| _ -> ChildrenPost(postf)
		  in

		  let unary_value uexp exp1 = function 
			| MINUS | PLUS | NOT | BNOT | MEMOF | ADDROF -> Result(VALUE(self#walkExpression (uexp,exp1)))
			| _ -> Children (* FIXME: maybe? *)
		  in
			match exp1.node,exp2.node with
			| GNU_BODY(b1),GNU_BODY(b2) -> Result(GNUGEN(self#walkBlock (b1,b2)))
			| GNU_BODY(b),_
			| _, GNU_BODY(b) -> Children
			| UNARY(uop1,exp3),UNARY(uop2,exp4) -> Result(UNARYOP(unify_uop uop1 uop2,self#walkExpression (exp3,exp4)))
			| LABELADDR(str1),LABELADDR(str2) -> Result(EXPBASE(nd(LABELADDR(unify_string str1 str2))))
			| BINARY(bop1,exp3,exp4),BINARY(bop2,exp5,exp6) ->
 			  let pair_match = pair_match bop1 bop2 in 
			  let binole bop = Bop(bop) in
			  let binsla bops = Bop_gen(ATLEAST([Bop(bops)])) in
			  let bopsla bops = Bop_gen(ATLEAST[bops]) in
			  let commut bop =
				Result(compare (BINOP(bop, self#walkExpression (exp3,exp5), self#walkExpression (exp4,exp6)))
									  (BINOP(bop, self#walkExpression (exp3,exp6), self#walkExpression (exp4,exp5))))
			  in
			  let not_commut bop =
				Result(compare (BINOP(bop, self#walkExpression (exp3,exp5), self#walkExpression (exp4,exp6)))
									  (BINOP(bop, self#walkExpression (exp3,exp6), self#walkExpression (exp4,exp5))))
			  in
			  let not_commut_bin_a lst = not_commut (binsla lst) in
			  let not_commut_bop_a lst = not_commut (bopsla lst) in
				if bop1 = bop2 then
				  if bop_commut bop1 then commut (binole bop1)
				  else not_commut (binole bop1)
				else begin
				  if pair_match ADD ADD_ASSIGN then not_commut_bin_a ADD
				  else if pair_match SUB SUB_ASSIGN then not_commut_bin_a SUB
				  else if pair_match MUL MUL_ASSIGN then not_commut_bin_a MUL
				  else if pair_match DIV DIV_ASSIGN then not_commut_bin_a DIV
				  else if pair_match MOD MOD_ASSIGN then not_commut_bin_a MOD
				  else if pair_match BAND BAND_ASSIGN then not_commut_bin_a BAND
				  else if pair_match BOR BOR_ASSIGN then not_commut_bin_a BOR
				  else if pair_match SHL SHL_ASSIGN then not_commut_bin_a SHL
				  else if pair_match SHR SHR_ASSIGN then not_commut_bin_a SHR
				  else if pair_match SHR SHL_ASSIGN || pair_match SHL SHR_ASSIGN || pair_match SHR SHL then
					not_commut_bop_a Shift
				  else if ((bop1 = ASSIGN) && (bop_modify bop2)) || (bop_modify bop1 && bop2 = ASSIGN) then not_commut_bin_a ASSIGN
				  else if (bop_arithmod bop1) && (bop_arithmod bop2) then not_commut (Bop_gen(ATLEAST[Modify_assign;Bgen(Arithmetic)]))
				  else if (bop_bitmod bop1) && (bop_bitmod bop2) then not_commut (Bop_gen(ATLEAST[Modify_assign;Bgen(Bitwise)]))
				  else if (bop_modify bop1) && (bop_modify bop2) then
					Result(compare (BINOP(Bgen(Arithmetic), self#walkExpression (exp3, exp5), self#walkExpression (exp4, exp6)))
										  (BINOP(Bgen(Arithmetic), self#walkExpression (exp3, exp6), self#walkExpression (exp4, exp5))))
				  else if pair_match AND BAND || pair_match OR BOR || pair_match OR XOR then 
					Result(compare (BINOP(Bgen(Logic), self#walkExpression (exp3, exp5), self#walkExpression (exp4, exp6)))
										  (BINOP(Bgen(Arithmetic), self#walkExpression (exp3, exp6), self#walkExpression (exp4, exp5))))
				  else if pair_match AND BAND_ASSIGN || pair_match OR BOR_ASSIGN || pair_match XOR XOR_ASSIGN then
					Result(compare (BINOP(Bgen(Logic), self#walkExpression (exp3, exp5), self#walkExpression (exp4, exp6)))
										  (BINOP(Bgen(Arithmetic), self#walkExpression (exp3, exp6), self#walkExpression (exp4, exp5))))
				  (* fixme: shl/shr relationship to add/multiply/etc *)
				  else if pair_match LT LE then not_commut_bin_a LT
				  else if pair_match GT GE then not_commut_bin_a GT
				  else if pair_match LE EQ || pair_match GE EQ then not_commut_bin_a EQ
				  else if pair_match GE EQ then not_commut_bin_a EQ
				  else if (bop_logic bop1) && (bop_logic bop2) then
					if bop_commut bop1 || bop_commut bop2 then commut (Bgen(Logic))
					else not_commut (Bgen(Logic))
				  else if bop_bits bop1 && bop_bits bop2 then commut  (Bgen(Bitwise))
				  else if bop_onnumbers bop1 && bop_onnumbers bop2 then commut (Bgen(Arithmetic))
				  else commut (Bop_gen(STAR))
				end
			| QUESTION(exp1,exp2,exp3), QUESTION(exp4,exp5,exp6) -> 
			  Result(QUESTOP(self#walkExpression (exp1,exp4),
										  self#walkExpression (exp2,exp5),
										  self#walkExpression (exp3, exp6)))
			| CAST((spec1,dt1),ie1),CAST((spec2,dt2), ie2) ->
			  Result(CASTOP((self#walkSpecifier (spec1,spec2), self#walkDeclType (dt1,dt2)),
										 self#walkInitExpression (ie1,ie2)))
			| CALL(e1,elist1), CALL(e2,elist2) ->
			  let funname = self#walkExpression (e1,e2) in
			  let arglist = self#walkExpressions (elist1,elist2) in
			  Result(CALLOP(funname, arglist))
			| COMMA(elist1), COMMA(elist2) -> Result(COMMAOP(self#walkExpressions (elist1,elist2)))
			| CONSTANT(c1),CONSTANT(c2) -> Result(CONSTGEN(STAR)) (* FIXME *)
			| PAREN(exp1),PAREN(exp2) -> Result(PARENOP(self#walkExpression (exp1, exp2)))
			| VARIABLE(str1),VARIABLE(str2) -> Result(EXPBASE(nd(VARIABLE(unify_string str1 str2))))
			| EXPR_SIZEOF(exp1),EXPR_SIZEOF(exp2) -> Result(EXPSIZEOFOP(self#walkExpression (exp1,exp2)))
			| TYPE_SIZEOF(spec1,dt1),TYPE_SIZEOF(spec2,dt2) -> Result(TYPESIZEOFOP(self#walkSpecifier (spec1,spec2),
																								self#walkDeclType (dt1,dt2)))
			| EXPR_ALIGNOF(exp1),EXPR_ALIGNOF(exp2) -> Result(EXPALIGNOFOP(self#walkExpression (exp1,exp2)))
			| TYPE_ALIGNOF(spec1,dt1),TYPE_ALIGNOF(spec2,dt2) -> Result(TYPEALIGNOFOP(self#walkSpecifier (spec1,spec2),
																					  self#walkDeclType (dt1,dt2)))
			| INDEX(exp1,exp2),INDEX(exp3,exp4) -> Result(INDEXOP(self#walkExpression (exp1,exp3),
																  self#walkExpression (exp2,exp4)))
			| MEMBEROF(exp1,str1),MEMBEROF(exp2,str2) -> Result(MEMBEROFOP(self#walkExpression (exp1,exp2),
																		   unify_string str1 str2))
			| MEMBEROFPTR(exp1,str1),MEMBEROFPTR(exp2,str2) ->
			  Result(MEMBEROFPTROP(self#walkExpression (exp1,exp2),
								   unify_string str1 str2))
			| EXPR_PATTERN(str1),EXPR_PATTERN(str2) -> Result(EXPBASE(nd(EXPR_PATTERN(unify_string str1 str2))))
			  
			(* those were the direct (easy) matches; now the harder stuff *)
			(* Unary expressions first *)
			| UNARY(uop1,exp3),LABELADDR(str) 
			| LABELADDR(str),UNARY(uop1,exp3) -> unary_labeladdr str uop1 exp3
			| UNARY(uop,exp3),BINARY(bop,exp4,exp5)
			| BINARY(bop,exp4,exp5),UNARY(uop,exp3) -> unary_binary uop exp3 bop exp4 exp5
			| UNARY(uop,uexp),QUESTION(qexp1,qexp2,qexp3)
			| QUESTION(qexp1,qexp2,qexp3),UNARY(uop,uexp) -> unary_question uexp qexp1 qexp2 qexp3 uop
(*			| UNARY(uop,uexp),CAST((spec,dt),ie)
			| CAST((spec,dt),ie), UNARY(uop,uexp) -> failwith "Not implemented" (* sort of accesses the value *)*)
			| UNARY(_,uexp),VARIABLE(str)
			| VARIABLE(str), UNARY(_,uexp) -> Result(UNARYOP(Uop_gen(LNOTHING),self#walkExpression (uexp,nd(VARIABLE(str)))))
			| UNARY(uop,uexp),CALL(fn,args)
			| CALL(fn,args),UNARY(uop,uexp) -> 
			  (match uop with
			  | PREINCR | PREDECR | POSINCR | POSDECR -> 
				CombineChildrenPost(OPERATION(Lifted_ops(MAYBE([Modify_value])),
											  lfoldl (fun result -> fun exp -> compare (self#walkExpression (uexp,exp)) result) (self#default_exp()) args),postf)
			  | _ -> Children
			  )
			(* we need a special case for calls, to deal with function names *)
			| UNARY(_), EXPR_SIZEOF(_)
			| EXPR_SIZEOF(_),UNARY(_) 
			| UNARY(_),TYPE_SIZEOF(_)
			| TYPE_SIZEOF(_),UNARY(_) 
			| UNARY(_),EXPR_ALIGNOF(_)
			| EXPR_ALIGNOF(_),UNARY(_) ->
			  ChildrenPost((fun res -> UNARYOP(Uop_gen(STAR),res))) (* this may not work *)
			| UNARY(uop,uexp), INDEX(iexp1,iexp2) 
			| INDEX(iexp1,iexp2),UNARY(uop,uexp) -> unary_value uexp iexp1 uop (* also sort of accesses the value *)
			(*		  | UNARY(uop,uexp),MEMBEROF(mexp,str)
					  | MEMBEROF(mexp,str),UNARY(uop,uexp)
					  | UNARY(uop,uexp),MEMBEROFPTR(mexp,str) (* should we distinguish between pointer dereferences here? *)
					  | MEMBEROFPTR(mexp,str),UNARY(uop,uexp) -> failwith "Not implemented13" *)(* unary_member uexp mexp uop*)  (* also sort of accesses the value *)
			  
			(* Binary expressions *)
			| BINARY(_),QUESTION(_)
			| QUESTION(_),BINARY(_) -> CombineChildrenPost(OPERATION(Lifted_ops(ATLEAST[Logic]),ELIFTED(STAR)),postf)
			(* LEFT OFF HERE *)
			| _,_ -> ChildrenPost(postf))

  method wBlock (b1,b2) =
	wGeneric (b1,b2) stmts_ht (pretty d_block) (fun d -> BLOCKBASE(d))
	  (fun _ ->
		let res = 
		  lmap
			(fun (s1,s2) -> 
			  self#walkStatement (s1,s2)) (best_mapping self#distance_stmt b1.bstmts b2.bstmts) in
		  if (llen b1.bstmts) <> (llen b2.bstmts) then Result(BLKLIFTED(ATLEAST([Reg(res)]))) else Result(Reg(res)))

  method wSpecElem ((se1,se2) : (spec_elem * spec_elem)) =
	wGeneric (se1,se2) se_ht (pretty d_spec_elem) (fun k -> Spec_elem(k))
	  (fun _ ->
		match se1,se2 with
		| SpecCV(cv1),SpecCV(cv2) -> Result(Se_CV(STAR))
		| SpecAttr(attr1),SpecAttr(attr2) -> Result(Se_attr (self#walkAttribute (attr1,attr2)))
		| SpecStorage(st1),SpecStorage(st2) -> Result(Se_storage(STAR))
		| SpecType(ts1),SpecType(ts2) -> Result(Se_type(self#walkTypeSpecifier (ts1,ts2)))
		| SpecPattern(str1),SpecPattern(str2) -> Result(Spec_elem(SpecPattern(unify_string str1 str2)))
		| _,_ -> CombineChildren(Se_lifted(STAR)))

  method wSpecifier (spec1,spec2) = 
	wGeneric (spec1,spec2) spec_ht (pretty d_specifier) (fun s -> Spec_base(s))
	  (fun _ ->
		Result(Spec_list(
		  lmap
			(fun(spec1,spec2) -> self#walkSpecElem (spec1,spec2)) (best_mapping self#distance_se spec1 spec2))))
	
  method childrenSpecifier blah = failwith "We shouldn't call children on specifier in doublewalk!"

  method walkIwIes (iwies1,iwies2) = failwith "Not implemented IWIES"
	
  method wInitWhat (iw1,iw2) =
	wGeneric (iw1,iw2) iw_ht (pretty d_init_what) 
	  (fun k -> IWBASE(k))
	  (fun _ ->
		match iw1,iw2 with
		  NEXT_INIT,_ 
		| _,NEXT_INIT -> Result(IWLIFTED(STAR)) (* FIXME: what does next_init mean? *)
		| INFIELD_INIT(str1,iw1),INFIELD_INIT(str2,iw2) -> CombineChildren(IWINFIELD(unify_string str1 str2, self#walkInitWhat (iw1,iw2)))
		| INFIELD_INIT(str1,iw1), ATINDEX_INIT(exp1,iw2)
		| ATINDEX_INIT(exp1,iw2), INFIELD_INIT(str1,iw1) -> CombineChildren(IWSOME(self#walkExpression (nd(VARIABLE(str1)), exp1), self#walkInitWhat (iw1,iw2) ))
		| INFIELD_INIT(str1,iw1), ATINDEXRANGE_INIT(exp1,exp2) 
		| ATINDEXRANGE_INIT(exp1,exp2), INFIELD_INIT(str1,iw1) -> CombineChildren(IWLIFTED(STAR))
		| ATINDEX_INIT(exp1,iw1), ATINDEX_INIT(exp2,iw2) -> Result(IWATINDEX(self#walkExpression (exp1,exp2), self#walkInitWhat (iw1,iw2)))
		| ATINDEX_INIT(exp3,iw1), ATINDEXRANGE_INIT(exp1,exp2)   (* We're missing many cases here *)
		| ATINDEXRANGE_INIT(exp1,exp2),ATINDEX_INIT(exp3,iw1) -> 
		  CombineChildren(IWATINDEX(compare (self#walkExpression (exp1,exp3)) (self#walkExpression (exp2,exp3)),IWLIFTED(LNOTHING)))
		| _,_ -> CombineChildrenPost(IWLIFTED(STAR),(fun res -> IWLIFTED(PARTIALMATCH(res))))
	  )
  method private childrenInitWhat (iw1,iw2) = failwith "children initwhat not implemented"

  method private walkInitWhat (iw1,iw2) = 
	doWalk compare self#wInitWhat self#childrenInitWhat (iw1,iw2)

  method wInitExpression (ie1,ie2) = 
	wGeneric (ie1,ie2) ie_ht (pretty d_init_expression) 
	  (fun k -> IEBASE(k))
	  (fun _ ->
		match ie1,ie2 with
		| SINGLE_INIT(exp1),SINGLE_INIT(exp2) -> Result(GENSINGLE(self#walkExpression (exp1,exp2)))
		| COMPOUND_INIT(iwies1),COMPOUND_INIT(iwies2) -> 
		  CombineChildren(GENCOMPOUND(self#walkIwIes (iwies1,iwies2)))
		| NO_INIT,_
		| _,NO_INIT -> CombineChildren(IELIFTED(LNOTHING))
		| SINGLE_INIT(exp1),COMPOUND_INIT(iwies1)
		| COMPOUND_INIT(iwies1),SINGLE_INIT(exp1) -> CombineChildren(IELIFTED(STAR)))

  method wExpressions (elist1,elist2) =
	wGeneric (elist1,elist2) exps_ht 
	  (fun elist -> lst_str (fun e -> Pretty.sprint ~width:80 (d_exp () e)) elist)
	  (fun elist1 ->  lmap (fun e -> EXPBASE(e)) elist1)
	  (fun _ ->
		Result(
		  lmap
		  (fun (e1,e2) -> 
			self#walkExpression (e1,e2)) (best_mapping self#distance_exp elist1 elist2)))

  method walkExpressions blah = 
	doWalk compare self#wExpressions (fun _ -> failwith "Shouldn't call children on expression list in doublewalker!") blah

  method wDeclType (dt1,dt2) = 
	  wGeneric (dt1,dt2) dts_ht (pretty d_decl_type) (fun dt1 -> DTBASE(dt1))
	  (fun _ -> 
		match dt1,dt2 with 
		| JUSTBASE, _
		| _, JUSTBASE ->
		  CombineChildrenPost(DTLIFTED(STAR),(fun d -> DTLIFTED(PARTIALMATCH(d))))
		| PARENTYPE(alist1,dt3,alist2),PARENTYPE(alist3,dt4,alist4) ->
		  Result(DTPAREN(self#walkAttributes (alist1,alist3), self#walkDeclType (dt3,dt4), self#walkAttributes (alist2,alist4)))
		| PARENTYPE(alist1,dt3,alist2),ARRAY(dt4,alist3,exp)
		| ARRAY(dt4,alist3,exp),PARENTYPE(alist1,dt3,alist2) ->
		  CombineChildrenPost(DTCOMPLEX(self#walkDeclType (dt3,dt4), compare (self#walkAttributes (alist1,alist2)) (self#walkAttributes (alist2,alist3))),
							  (fun d -> DTLIFTED(atleast d))) (* FIXME: make these partial matches instead of atleast *)
			
		| PARENTYPE(alist1,dt3,alist2),PTR(alist3,dt4) 
		| PTR(alist3,dt4),PARENTYPE(alist1,dt3,alist2) ->
		  CombineChildrenPost(DTCOMPLEX(self#walkDeclType(dt3,dt4), compare (self#walkAttributes (alist1,alist3)) (self#walkAttributes(alist1,alist2))),
							  (fun d -> DTLIFTED(atleast d)))
		| PARENTYPE(alist1,dt3,alist2),PROTO(dt4,sns,_) 
		| PROTO(dt4,sns,_), PARENTYPE(alist1,dt3,alist2) ->
		  CombineChildrenPost(DTCOMPLEX(self#walkDeclType(dt3,dt4), []), (fun d -> DTLIFTED(atleast d)))
		| ARRAY(dt3,alist1,exp1),ARRAY(dt4,alist2,exp2) ->
		  Result(DTARRAY(self#walkDeclType (dt3,dt4), self#walkAttributes (alist1,alist2), self#walkExpression (exp1,exp2)))
		| ARRAY(dt3,alist1,exp), PTR(alist2,dt4)
		| PTR(alist2,dt4),ARRAY(dt3,alist1,exp) ->
		  CombineChildrenPost(DTCOMPLEX(self#walkDeclType(dt3,dt4), self#walkAttributes (alist1,alist2)), (fun d -> DTLIFTED(atleast d)))
		| ARRAY(dt3,alist,exp),PROTO(dt4,sns,_) 
		| PROTO(dt4,sns,_),ARRAY(dt3,alist,exp) ->
		  CombineChildrenPost(DTLIFTED(ATLEAST([self#walkDeclType(dt3,dt4)])), (fun d -> DTLIFTED(atleast d)))
		| PTR(alist1,dt3), PTR(alist2,dt4) ->
		  Result(DTPTR(self#walkAttributes (alist2,alist2), self#walkDeclType(dt3,dt4)))
		| PTR(alist1,dt3), PROTO(dt4,sns,_) 
		| PROTO(dt4,sns,_),PTR(alist1,dt3) -> 
		  CombineChildrenPost(DTLIFTED(ATLEAST([self#walkDeclType(dt3,dt4)])), (fun d -> DTLIFTED(atleast d)))
		| PROTO(dt3,sns1,_),PROTO(dt4,sns2,_) ->
		  Result(DTPROTO(self#walkDeclType(dt3,dt4), self#walkSingleNames (sns1,sns2)))
	  )


  method walkForClause (fc1,fc2) = failwith "Not implemented15"

  method childrenTree ((_,tns1),(_,tns2)) = 
	TNS(lmap (fun (tn1,tn2) -> self#walkTreenode (tn1,tn2)) (best_mapping self#distance_tn tns1 tns2))
	  
  method walkSingleNames (sn1,sn2) =
	lmap
	  (fun (a1,a2) ->
		self#walkSingleName (a1,a2)) (best_mapping self#distance_sn sn1 sn2)

  method walkAttributes (attrs1,attrs2) =
	lmap
	  (fun (a1,a2) ->
		self#walkAttribute (a1,a2)) (best_mapping self#distance_attr attrs1 attrs2)

end
