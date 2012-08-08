open Useract
open Cabs
open Diffparser

let handcoded_diffParserUserActions = {
  reductionAction = (fun prodId svals -> ( 
    (* this is how ocamlyacc does it, so I assume it's the fastest way *)
    let actions : (tSemanticValue array -> tSemanticValue) array = [|
      (fun svals ->
        let top = (Obj.obj svals.(0) : int) in
          (Obj.repr ( 
			top
           ))
      );
      (fun svals ->
        let e1 = (Obj.obj svals.(0) : int) in
        let e2 = (Obj.obj svals.(2) : int) in
          (Obj.repr (
			e1 + e2
           ))
      );
      (fun svals ->
        let e1 = (Obj.obj svals.(0) : int) in
        let e2 = (Obj.obj svals.(2) : int) in
          (Obj.repr (
			e1 - e2
           ))
      );
      (fun svals ->
        let e1 = (Obj.obj svals.(0) : int) in
        let e2 = (Obj.obj svals.(2) : int) in
          (Obj.repr (
			e1 * e2
           ))
      );
      (fun svals ->
        let e1 = (Obj.obj svals.(0) : int) in
        let e2 = (Obj.obj svals.(2) : int) in
          (Obj.repr (
			e1 / e2
           ))
      );
      (fun svals ->
        let n = (Obj.obj svals.(0) : int) in
          (Obj.repr (
			n
           ))
      );
      (fun svals ->
        let p = (Obj.obj svals.(0) : int) in
          (Obj.repr (
			p
           ))
      );
      (fun svals ->
        let e = (Obj.obj svals.(1) : int) in
          (Obj.repr (
			e
           ))
      )
																   |] in
      (actions.(prodId) svals)
  ));
  
  duplicateTerminalValue = (fun termId sval -> sval);
  duplicateNontermValue =  (fun termId sval -> sval);
  
  deallocateTerminalValue =  (fun termId sval -> ());
  deallocateNontermValue = (fun termId sval -> ());
  
  mergeAlternativeParses = 
	(fun nontermId sval1 sval2 -> 
	  match nontermId with 
	  | 52 -> (* BlockElementList *)
		let containscall exp = 
		  match dn exp with
			CALL(_) -> true
		  | _ -> false
		in
		let (top1: (Cabs.statement node list * int)) = 
		  ((Obj.obj sval1) : Cabs.statement node list * int) in
		let (top2: (Cabs.statement node list * int)) =
		  ((Obj.obj sval2) : Cabs.statement node list * int) in
		  if (List.length (fst top1) > 0) && (List.length (fst top2) > 0) then
		  (match dn (List.hd (fst top1)),dn (List.hd (fst top2)) with
			DEFINITION(_),COMPUTATION(exp,_) -> if containscall exp then sval2 else sval1
		  | COMPUTATION(exp,_),DEFINITION(_) -> if containscall exp then sval1 else sval2
		  | _ -> sval1)
		  else if (List.length (fst top1)) > 0 then sval1
		  else sval2
	  | 29 -> (* Expression *)
		let (top1 : (Cabs.expression node * cabsloc * int)) = 
		  ((Obj.obj sval1) : Cabs.expression node * cabsloc * int) in
		let (top2: (Cabs.expression node * cabsloc * int)) =
		  ((Obj.obj sval2) : Cabs.expression node * cabsloc * int) in
		let exp1,_,_ = top1 in
		let exp2,_,_ = top2 in
		  (match dn exp1, dn exp2 with
			EXPDIRECTIVE _, EXPDIRECTIVE _ -> sval1
		  | EXPDIRECTIVE _, _ -> sval2
		  | _, EXPDIRECTIVE _ -> sval1
		  | _,_ -> sval1)
	  | 57 -> (* statement *)
		let (top1 : (Cabs.statement node * cabsloc * int)) = 
		  ((Obj.obj sval1) : Cabs.statement node * cabsloc * int) in
		let (top2: (Cabs.statement node * cabsloc * int)) =
		  ((Obj.obj sval2) : Cabs.statement node * cabsloc * int) in
		let stmt1,_,_ = top1 in
		let stmt2,_,_ = top2 in
		  (match dn stmt1, dn stmt2 with
			STMTDIRECTIVE _, STMTDIRECTIVE _ -> sval1
		  | STMTDIRECTIVE _, COMPUTATION _ -> sval2
		  | COMPUTATION _ , STMTDIRECTIVE _ -> sval1
		  | _,_ -> sval1)

	  (* Statement *)
	  | _ -> sval1);
  
  keepNontermValue = (fun nontermId sval -> true);

  terminalDescription =  (fun termId sval -> "TODO");
  nonterminalDescription = (fun termId sval -> "TODO");

  terminalName = (fun termId -> "TODO");
  nonterminalName =	(fun termId -> "TODO");
}
