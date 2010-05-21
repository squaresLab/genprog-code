open Cil

(* this naming scheme is probably a really crap idea because the two words
   actually mean the same thing, but I'm using one to be just like "truth-valued
   statement" and one to be "Slightly more general." The three type definitions
   are also completely hideous. *)

type predicate = CilExp of exp | ReturnVal of exp | RunFailed | RunSucceeded
type invariant = General of predicate 
		 | Specific of predicate * location 
type predicatable = Pred of predicate | Inv of invariant

let opposite pable =
  let oppp pred = 
    match pred with 
      CilExp(e) -> failwith "Not implemented"
    | ReturnVal(e) -> failwith "Not implemented"
    | RunFailed -> RunSucceeded
    | RunSucceeded -> RunFailed
  in
  let oppi inv = 
    match inv with
      General(p) -> failwith "Not implemented"
    | Specific(p, l) -> failwith "Not implemented"
  in
    match pable with
      Pred(p) -> Pred(oppp p)
    | Inv(i) -> Inv(oppi i)

