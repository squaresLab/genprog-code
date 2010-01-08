(* Cheap Bug Isolation *)

open Printf
open Cil


let fprintf_va = makeVarinfo true "fprintf" (TVoid [])
let fopen_va = makeVarinfo true "fopen" (TVoid [])
let fflush_va = makeVarinfo true "fflush" (TVoid [])
let stderr_va = makeVarinfo true "_cbi_fout" (TPtr(TVoid [], []))
let fprintf = Lval((Var fprintf_va), NoOffset)
let fopen = Lval((Var fopen_va), NoOffset)
let fflush = Lval((Var fflush_va), NoOffset)
let stderr = Lval((Var stderr_va), NoOffset)

let counter = ref 0
let site = ref 0

let variables : (Cil.varinfo, Cil.typsig) Hashtbl.t ref = ref (Hashtbl.create 10)
let global_variables = Hashtbl.create 10

(* maps counter numbers to a string describing the predicate *)

let pred_ht = Hashtbl.create 10

(* maps site numbers to location, associated expression, and a list of counter
 * numbers associated with that site. *)

let site_ht : (int, (Cil.location * string * int list)) Hashtbl.t = Hashtbl.create 10

(* maps counter numbers to site numbers *)
let pred_to_site_ht = Hashtbl.create 10

let get_next_site () = 
  let count = !site in
    incr site ;
    count

let get_next_count str site = 
  let count = !counter in 
    Hashtbl.add pred_ht count str;
    Hashtbl.add pred_to_site_ht count site;
    incr counter ;
    count 

let print_str_stmt pred_num = begin
  let str = Printf.sprintf "%d\n" pred_num in 
  let str_exp = Const(CStr(str)) in 
  let instr = Call(None,fprintf,[stderr; str_exp],!currentLoc) in
  let instr2 = Call(None,fflush,[stderr],!currentLoc) in
  let skind = Instr([instr;instr2]) in
    mkStmt skind
end

let print_str_b pred_num = mkBlock [(print_str_stmt pred_num)] 

let instrument_branch_block b site_num = 
  let bs = b.bstmts in
    { b with bstmts = (print_str_stmt site_num) :: bs } 

let get_num_and_b str site = 
  let counter = get_next_count str site in
    (counter, (print_str_b counter))
	  
let if_else_if_else e comp_lval scheme =
  let site_num = get_next_site () in
  let (lt, lt_b) = get_num_and_b "default_string" site_num in
  let (eq, eq_b) = get_num_and_b "default_string" site_num in
  let (gt, gt_b) = get_num_and_b "default_string" site_num in
  let ret_typ = TInt(IInt,[]) in
  let lt_cond, gt_cond = BinOp(Lt,e,comp_lval,ret_typ),BinOp(Gt,e,comp_lval,ret_typ) in
  let inner_if_b = 
	mkBlock [mkStmt (If(gt_cond,gt_b, eq_b,!currentLoc))] in
	Hashtbl.add site_ht site_num (!currentLoc,scheme,[lt;eq;gt]);
	mkStmt (If(lt_cond,lt_b,inner_if_b,!currentLoc))

let conditionals_for_one_var myvarinfo mylval =
  let my_typ = typeSig myvarinfo.vtype in
  let to_compare : Cil.varinfo list = 
	Hashtbl.fold
	  (fun vi ->
		 fun vtypsig -> 
		   fun list_to_add ->
			 if (vtypsig = my_typ) && (not (vi.vname = myvarinfo.vname)) then
			   vi :: list_to_add
	   else list_to_add) !variables [] in
	List.map (* turns vars to add into a list of stmts. How convenient *)
	  (fun var_to_add -> if_else_if_else (Lval(Var(var_to_add),NoOffset)) mylval "scalar-pairs")
	  to_compare 

let visit_instr (instr : instr) : stmt list = 
  match instr with
	  Set((Var(vi), off), e1, l) -> 
		conditionals_for_one_var vi (Lval(Var(vi),off))
	| Call(Some(Var(vi), off), e1, elist, l) ->
		conditionals_for_one_var vi (Lval(Var(vi), off))
	| _ -> []

class instrumentVisitor = object
  inherit nopCilVisitor

  method vstmt s = 
    ChangeDoChildrenPost
      (s, 
       fun s -> 
		 match s.skind with 
		   | If(e1,b1,b2,l) -> 
			   let site_num = get_next_site () in
			   let branch_true = get_next_count "default_string" site_num in (* FIXME *)
			   let branch_false = get_next_count "default_string" site_num in
			   let b1' = instrument_branch_block b1 branch_true in
			   let b2' = instrument_branch_block b2 branch_false in
				 Hashtbl.add site_ht site_num (l,"branches",[branch_true;branch_false]);
				 {s with skind=If(e1,b1',b2',l)}
	       | Return(Some(e), l) -> 
			   let if_stmt = if_else_if_else e zero "returns" in
			   let new_block = (Block(mkBlock [if_stmt;s])) in
				 mkStmt new_block
		   | Instr(ilist) -> (* Partial.callsEndBasicBlocks *should* (by its 
								own documentation?) put calls in their own blocks.
								If not, more hackery will be required. *)
			   let new_stmts : stmt list = 
				 List.fold_left
				   (fun (accum : stmt list) ->
					  fun (i : instr) ->
						(visit_instr i) @ accum
				   ) [] ilist 
			   in
			   let new_block = (Block(mkBlock (s::new_stmts))) in
				 mkStmt new_block
		   | _ -> s
      )

  method vfunc fdec =
	(* first, get a fresh version of the variables hash table *)
	variables := Hashtbl.copy global_variables;
	(* next, replace all variables in the hashtable with their name and type from here *)
	List.iter 
	  (fun v -> Hashtbl.replace !variables v (typeSig v.vtype))
	  (fdec.sformals @ fdec.slocals);
	DoChildren
end

let my_visitor = new instrumentVisitor

let main () = begin
  let usageMsg = "Prototype Cheap Bug Isolation Instrumentation\n" in
  let filenames = ref [] in
  let argDescr = [ ] in
  let handleArg str = filenames := str :: !filenames in
    Arg.parse (Arg.align argDescr) handleArg usageMsg ;

    Cil.initCIL();

	let files = List.map 
	  (fun filename -> 
		 let file = Frontc.parse filename () in
		   Partial.calls_end_basic_blocks file;
		   Cfg.computeFileCFG file;
		   file) !filenames in
	  List.iter
		(fun file ->
		   List.iter 
			 (fun g ->
				match g with 
				  | GVarDecl(vi, l) -> Hashtbl.add global_variables vi (typeSig vi.vtype)
				  | GVar(vi, ii, l) -> Hashtbl.add global_variables vi (typeSig vi.vtype)
				  | _ -> ()) 
			 file.globals) files;

    List.iter 
      (fun file -> 
		 begin
	       visitCilFileSameGlobals my_visitor file;
		   
		   let new_global = GVarDecl(stderr_va,!currentLoc) in 
			 file.globals <- new_global :: file.globals ;
			 
			 let fd = Cil.getGlobInit file in 
			 let lhs = (Var(stderr_va),NoOffset) in 
			 let data_str = file.fileName ^ ".preds" in 
			 let str_exp = Const(CStr(data_str)) in 
			 let str_exp2 = Const(CStr("wb")) in 
			 let instr = Call((Some(lhs)),fopen,[str_exp;str_exp2],!currentLoc) in 
			 let new_stmt = Cil.mkStmt (Instr[instr]) in 
			   fd.sbody.bstmts <- new_stmt :: fd.sbody.bstmts ; 
			   iterGlobals file (fun glob ->
								   dumpGlobal defaultCilPrinter stdout glob ;
								) ; 
			   let sites = file.fileName ^ ".sites" in
			   let fout = open_out_bin sites in
				 Marshal.to_channel fout (pred_ht,site_ht,pred_to_site_ht) [] ;
				 close_out fout ;
		 end
      ) files;
end ;;

main () ;;
    
