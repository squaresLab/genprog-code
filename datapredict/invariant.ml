open Cil
open Pretty
open Globals

(* this naming scheme is probably a really crap idea because the two words
   actually mean the same thing, but I'm using one to be just like "truth-valued
   statement" and one to be "Slightly more general." The three type definitions
   are also completely hideous. *)

type predicate = CilExp of exp 
		 | ReturnVal of exp 
		 | RunFailed
		 | RunSucceeded 
		 | Undefined

type invariant = General of predicate 
		 | Specific of predicate * location 
type predicatable = Pred of predicate | Inv of invariant

let d_pred p =
  match p with
    CilExp(e) -> 
      let exp_str = Pretty.sprint 80 (d_exp () e) in
	pprintf "cilexp: %s\n" exp_str;
	flush stdout
  | ReturnVal(e) -> pprintf "returnval\n"; flush stdout
  | RunFailed -> pprintf "Run Failed\n"; flush stdout
  | RunSucceeded -> pprintf "Run Succeeded\n"; flush stdout
let d_inv i = failwith "Not implemented"

let d_pable pable  = 
  match pable with 
    Pred(p) -> d_pred p 
  | Inv(i) -> d_inv i

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

