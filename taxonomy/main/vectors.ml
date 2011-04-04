open Batteries
open Utils
open Cabswalker
open Ttypes
module C=Cabs

let vector_hash = hcreate 10

(* OK, what are the interesting nodes for cabs? *)
(* everything rooted at statement, including expressions?  I think so. *)

type tIndex = {
  computation : int; if_ind : int; while_ind : int; dowhile  : int;
  for_ind : int; break : int; continue : int; return : int; 
  switch : int; case : int; caserange : int; default : int;
  label : int; goto : int; compgoto : int; definition : int;
  asm : int; try_except  : int; try_finally : int; uminus : int;
  uplus : int; unot : int; ubnot  : int; umemof  : int; uaddrof : int;
  upreincr : int; upredecr : int; uposincr : int; uposdecr : int;
  labeladdr : int; binary : int; question : int; cast : int; 
  call : int; comma : int; constant : int; paren : int; variable : int;
  expr_sizeof : int; type_sizeof : int; expr_alignof : int;
  type_alignof : int; index : int; memberof : int; memberofptr : int;
  gnu_body  : int; expr_pattern : int; badd : int; bsub : int; 
  bmul : int; bdiv : int; bmod : int; band : int; bor  : int;
  bband : int; bbor : int; bxor : int; bshl : int; bshr : int;
  beq : int; bne : int; blt : int; bgt : int; ble : int; bge : int;
  bassign : int; badd_assign : int; bsub_assign : int; bmul_assign : int;
  bdiv_assign : int; bmod_assign : int; bband_assign  : int;
  bbor_assign  : int; bxor_assign  : int; bshl_assign  : int;
  bshr_assign : int;
}

let i = {
  computation=0; if_ind=1; while_ind=2; dowhile=3; for_ind=4; break=5;
  continue=6; return=7; switch=8; case=9; caserange=10; default=11;
  label=12; goto=13; compgoto=14; definition=15; asm=16; try_except=17;
  try_finally=18; uminus=19; uplus=20; unot=21; ubnot=22; umemof=23;
  uaddrof=24; upreincr=25; upredecr=26; uposincr=27; uposdecr=28;
  labeladdr=29; binary=30; question=31; cast=32; call=33; comma=34; 
  constant=35; paren=36; variable=37; expr_sizeof=38; type_sizeof=39;
  expr_alignof=40; type_alignof=41; index=42; memberof=43; memberofptr=44;
  gnu_body=45; expr_pattern=46; badd=47; bsub=48; bmul=49; bdiv=50;
  bmod=51; band=52; bor=53; bband=54; bbor=55; bxor=56; bshl=57; bshr=58;
  beq=59; bne=60; blt=61; bgt=62; ble=63; bge=64; bassign=65; 
  badd_assign=66; bsub_assign=67; bmul_assign=68; bdiv_assign=69;
  bmod_assign=70; bband_assign=71; bbor_assign=72; bxor_assign=73;
  bshl_assign=74; bshr_assign=75;
}

let max_size = 75
(* we need to do everything in postorder *)
let array_incr array index =
  let currval = array.(index) in
	Array.set array index (currval + 1)
	     
let array_sum array1 array2 = (* for each i in array1, array1.(i) = array1.(i) + array2.(i) *)
  Array.iteri
	(fun index ->
	  fun val1 -> 
		Array.set array1 index (array1.(index) + array2.(index))) array1; array1
  
class vectorGenWalker = object(self)
  inherit [int Array.t] singleCabsWalker

  method default_res () = Array.make max_size 0 (* FIXME: init to zero! *)
  method combine array1 array2 = array_sum array1 array2
  method wExpression exp =
	if not (hmem vector_hash exp.C.id) then begin
	let exp_array = Array.make max_size 0 in
	let incr = array_incr exp_array in
	  (match C.dn exp with
	  | C.UNARY(uop,exp1) -> 
		let uop_index = 
		  match uop with
		  | C.MINUS -> i.uminus
		  | C.PLUS -> i.uplus 
		  | C.NOT -> i.unot
		  | C.BNOT -> i.ubnot
		  | C.MEMOF -> i.umemof 
		  | C.ADDROF -> i.uaddrof
		  | C.PREINCR -> i.upreincr
		  | C.PREDECR -> i.upredecr
		  | C.POSINCR -> i.uposincr
		  | C.POSDECR -> i.uposdecr 
		in
		  incr uop_index
	  | C.BINARY(bop,exp1,exp2) ->
		let bop_index = 
		  match bop with 
		  | C.ADD -> i.badd
		  | C.SUB -> i.bsub
		  | C.MUL -> i.bmul
		  | C.DIV -> i.bdiv
		  | C.MOD -> i.bmod
		  | C.AND -> i.band
		  | C.OR -> i.bor 
		  | C.BAND -> i.bband
		  | C.BOR -> i.bbor
		  | C.XOR -> i.bxor
		  | C.SHL -> i.bshl
		  | C.SHR -> i.bshr
		  | C.EQ -> i.beq
		  | C.NE -> i.bne
		  | C.LT -> i.blt
		  | C.GT -> i.bgt
		  | C.LE -> i.ble
		  | C.GE -> i.bge
		  | C.ASSIGN -> i.bassign
		  | C.ADD_ASSIGN -> i.badd_assign
		  | C.SUB_ASSIGN -> i.bsub_assign
		  | C.MUL_ASSIGN -> i.bmul_assign
		  | C.DIV_ASSIGN -> i.bdiv_assign
		  | C.MOD_ASSIGN -> i.bmod_assign
		  | C.BAND_ASSIGN -> i.bband_assign
		  | C.BOR_ASSIGN -> i.bbor_assign 
		  | C.XOR_ASSIGN -> i.bxor_assign
		  | C.SHL_ASSIGN -> i.bshl_assign
		  | C.SHR_ASSIGN -> i.bshr_assign 
		in
		  incr bop_index
	  | C.LABELADDR(str) -> incr i.labeladdr
	  | C.QUESTION(exp1,exp2,exp3) -> incr i.question
	  | C.CAST((spec,dt),ie) -> incr i.cast
	  | C.CALL(exp,elist) -> incr i.call
	  | C.CONSTANT(const) -> incr i.constant
	  (* GIANT QUESTION FIXME TODO: how to deal with info in variable names? *)
	  | C.VARIABLE(str) -> incr i.variable
	  | C.EXPR_SIZEOF(exp) -> incr i.expr_sizeof
	  | C.TYPE_SIZEOF(spec,dt) -> incr i.type_sizeof
	  | C.EXPR_ALIGNOF(exp) -> incr i.expr_alignof
	  | C.TYPE_ALIGNOF(spec,dt) -> incr i.type_alignof
	  | C.INDEX(e1,e2) -> incr i.index
	  | C.MEMBEROF(exp,str) -> incr i.memberof
	  | C.MEMBEROFPTR(exp,str) -> incr i.memberofptr
	  | C.GNU_BODY(b) -> incr i.gnu_body
	  | C.EXPR_PATTERN(str) -> incr i.expr_pattern
	  | _ -> ());
	  CombineChildrenPost(exp_array, 
						  (fun child_arrays -> 
							let exp_array = array_sum exp_array child_arrays in
							hadd vector_hash exp.C.id exp_array;
							exp_array))
	end else Result(hfind vector_hash exp.C.id) 

  method wStatement stmt =
	if not (hmem vector_hash stmt.C.id) then begin
	let stmt_array = Array.make max_size 0 in 
	let incr = array_incr stmt_array in
	  (match C.dn stmt with 
	  | C.COMPUTATION _ -> incr i.computation
	  | C.IF _ -> incr i.if_ind
	  | C.WHILE _ -> incr i.while_ind
	  | C.DOWHILE _ -> incr i.dowhile
	  | C.FOR _ -> incr i.for_ind
	  | C.BREAK _ -> incr i.break
	  | C.CONTINUE _ -> incr i.continue
	  | C.RETURN _ -> incr i.return
	  | C.SWITCH _ -> incr i.switch
	  | C.CASE _ -> incr i.case
	  | C.CASERANGE _ -> incr i.caserange
	  | C.DEFAULT _ -> incr i.default
	  | C.LABEL _ -> incr i.label
	  | C.GOTO _ -> incr i.goto
	  | C.COMPGOTO _ -> incr i.compgoto
	  | C.DEFINITION _ -> incr i.definition
	  | C.ASM _ ->  incr i.asm
	  | C.TRY_EXCEPT _ -> incr i.try_except
	  | C.TRY_FINALLY _ -> incr i.try_finally
	  | _ -> ()
	  );
	  CombineChildrenPost(stmt_array, 
						  (fun child_arrays -> 
							let stmt_array = array_sum stmt_array child_arrays in
							hadd vector_hash stmt.C.id stmt_array;
							stmt_array))
	end else Result(hfind vector_hash stmt.C.id)
end

let template_to_vectors (template : init_template) = 
  let con,changes = template in 
  let vector_gen = new vectorGenWalker in
  let walk_opt func ele = 
	match ele with
	  None -> Array.make max_size 0 
	| Some(ele) -> func ele
  in
  let vector_def = walk_opt vector_gen#walkDefinition con.parent_definition in
  let vector_stmt = walk_opt vector_gen#walkStatement con.parent_statement in
  let vector_exp = walk_opt vector_gen#walkExpression con.parent_expression in
	(* fixme: do the rest? *)
	(vector_def, vector_stmt, vector_exp)
 
