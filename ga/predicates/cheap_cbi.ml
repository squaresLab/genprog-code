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

let site_ht = Hashtbl.create 10

(* maps counter numbers to site numbers *)
let pred_to_site_ht = Hashtbl.create 10

let get_next_site () = 
  let count = !site in
    incr site ;
    count

let get_next_count () = 
  let count = !counter in 
  incr counter ;
  count 

(* separate now for separate testing; combine later for
 * faster instrumentation? *)

let instrument_branch_block b site_num = begin
  let str = Printf.sprintf "%d\n" site_num in 
  let str_exp = Const(CStr(str)) in 
  let instr = Call(None,fprintf,[stderr; str_exp],!currentLoc) in
  let instr2 = Call(None,fflush,[stderr],!currentLoc) in
  let skind = Instr([instr;instr2]) in
  let newstmt = mkStmt skind in
  let bs = b.bstmts in
    { b with bstmts = newstmt :: bs }
end

class branchesVisitor = object
  inherit nopCilVisitor
    
  method vstmt s = 
    ChangeDoChildrenPost
      (s, 
       fun s -> 
	 match s.skind with 
	   | If(e1,b1,b2,l) -> 
	       let site_num = get_next_site () in
	       let branch_true = get_next_count () in
	       let branch_false = get_next_count () in 
	       let b1' = instrument_branch_block b1 branch_true in
	       let b2' = instrument_branch_block b2 branch_false in
		 Hashtbl.add pred_ht branch_true "default_string"; (* FIXME *)
		 Hashtbl.add pred_ht branch_false "default_string"; (* FIXME *)
		 Hashtbl.add pred_to_site_ht branch_true site_num;
		 Hashtbl.add pred_to_site_ht branch_false site_num;
		 Hashtbl.add site_ht site_num (l,e1, [branch_true;branch_false]);
		 {s with skind=If(e1,b1',b2',l)}
	   | _ -> s
      )
end

class returnsVisitor = object
  inherit nopCilVisitor
  method vstmt s = 
    ChangeDoChildrenPost
      (s,
       fun s ->
	 match s.skind with
	     Return(Some(e), l) -> s
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

let my_b_visitor = new branchesVisitor
let my_r_visitor = new returnsVisitor
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

	     visitCilFileSameGlobals my_b_visitor file;
	     visitCilFileSameGlobals my_r_visitor file;
	     visitCilFileSameGlobals my_sp_visitor file;

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
    
