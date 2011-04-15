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
	  match (nonterminalNameFunc nontermId) with 
		"MiddleStatements" ->
		  let count_partial_exps = 
			List.fold_left 
			  (fun accum -> fun tn ->
				match dn tn with
				  Exps(_) -> accum + 1
				| _ -> accum) 0 
		  in
		  let (top1 : (Cabs.tree_node node list * int)) =
			((Obj.obj sval1) : Cabs.tree_node node list * int) in
		  let count1 = count_partial_exps (fst top1) in
(*			Printf.printf "sval1 numToks: %d, exps: %d\n" (snd top1) count1; flush stdout;*)
			let (top2: (Cabs.tree_node node list * int)) =
			  ((Obj.obj sval2) : Cabs.tree_node node list * int) in
		  let count2 = count_partial_exps (fst top2) in
(*			  Printf.printf "sval1 numToks: %d, exps: %d\n" (snd top2) count2; flush stdout;*)
			  if count1 > count2 then sval1
			  else if count2 > count1 then sval2 
			  else if (snd top1) > (snd top2) then sval1
			  else sval2
	  | "BlockElementList" ->
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
	  | _ -> sval1);
  
  keepNontermValue = (fun nontermId sval -> true);

  terminalDescription =  (fun termId sval -> "TODO");
  nonterminalDescription = (fun termId sval -> "TODO");

  terminalName = (fun termId -> "TODO");
  nonterminalName =	(fun termId -> "TODO");
}
