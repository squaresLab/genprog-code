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

(* maps counter numbers to a string describing the predicate *)

let pred_ht = Hashtbl.create 10

(* maps site numbers to location, associated expression, and a list of counter
 * numbers associated with that site. *)

let site_ht : (int, (Cil.location * Cil.exp * int list)) Hashtbl.t = Hashtbl.create 10

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

(* separate now for separate testing; combine later for
 * faster instrumentation? *)

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
				 Hashtbl.add site_ht site_num (l,e1, [branch_true;branch_false]);
				 {s with skind=If(e1,b1',b2',l)}
	       | Return(Some(e), l) -> 
			   let site_num : int = get_next_site () in
			   let (lt, lt_b) = get_num_and_b "default_string" site_num in
			   let (eq, eq_b) = get_num_and_b "default_string" site_num in
			   let (gt, gt_b) = get_num_and_b "default_string" site_num in
			   let ret_typ = TInt(IInt,[]) in
			   let lt_cond, gt_cond = BinOp(Lt,e,zero,ret_typ),BinOp(Gt,e,zero,ret_typ) in
			   let inner_if_b = 
				 mkBlock [mkStmt (If(gt_cond,gt_b, eq_b,!currentLoc))] in
			   let outer_if_s = 
				 mkStmt (If(lt_cond,lt_b,inner_if_b,!currentLoc)) in
			   let new_block = (Block(mkBlock [outer_if_s;s])) in
				 Hashtbl.add site_ht site_num (l,e,[lt;eq;gt]); (* NTS: I don't think I need to add the exp to this table *)
				 mkStmt new_block
		   | _ -> s
      )
end


class scalarPairsVisitor = object
  inherit nopCilVisitor

  method vinst i = 
    ChangeDoChildrenPost
      ([i],
       fun i -> i
      (*	 match i with
	     Set(lv, e1, l) -> i
	   | Call(lvalopt, e1, elist, l) -> i
	   | _ -> i *)
      )
end

let my_visitor = new instrumentVisitor
let my_sp_visitor = new scalarPairsVisitor

let main () = begin
  let usageMsg = "Prototype Cheap Bug Isolation\n" in
  let filenames = ref [] in
  let argDescr = [ ] in
  let handleArg str = filenames := str :: !filenames in
    Arg.parse (Arg.align argDescr) handleArg usageMsg ;

    Cil.initCIL() ;

    List.iter 
      (fun arg -> 
	 begin
	   let file = Frontc.parse arg () in

	     visitCilFileSameGlobals my_visitor file;

	     (* TODO: serialize site information to disk *)

	     let new_global = GVarDecl(stderr_va,!currentLoc) in 
	       file.globals <- new_global :: file.globals ;
	       
	       let fd = Cil.getGlobInit file in 
	       let lhs = (Var(stderr_va),NoOffset) in 
	       let data_str = arg ^ ".preds" in 
	       let str_exp = Const(CStr(data_str)) in 
	       let str_exp2 = Const(CStr("wb")) in 
	       let instr = Call((Some(lhs)),fopen,[str_exp;str_exp2],!currentLoc) in 
	       let new_stmt = Cil.mkStmt (Instr[instr]) in 
		 fd.sbody.bstmts <- new_stmt :: fd.sbody.bstmts ; 
		 iterGlobals file (fun glob ->
				     dumpGlobal defaultCilPrinter stdout glob ;
				  ) ; 
		 let sites = arg ^ ".sites" in
		 let fout = open_out_bin sites in
		   Marshal.to_channel fout (pred_ht,site_ht,pred_to_site_ht) [] ;
		   close_out fout ;

	 end
      ) !filenames ;
end ;;

main () ;;
    
