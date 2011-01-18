(* useract.ml *)
(* interface for user-defined reduction (etc.) actions *)
(* based on elkhound/useract.h *)

(* for now, some actual user actions *)


(* secret to type casting in OCaml: the Obj module *)
type tSemanticValue = Obj.t
let cNULL_SVAL = (Obj.repr 0)


(* collection of actions for use during parsing *)
(* again, see elkhound/useract.h for more info *)
type tUserActions = {
  (* action to perform upon performing a reduction *)
  reductionAction:
    (*context?*)
    int ->                     (* production being used to reduce *)
    tSemanticValue array ->    (* array of svals for RHS symbols *)
    (*loc?*)
    tSemanticValue;            (* sval for the reduction *)

  (* duplicate a semantic value *)
  duplicateTerminalValue:
    (*context?*)
    int ->                     (* terminal id *)
    tSemanticValue ->          (* sval being yielded *)
    tSemanticValue;            (* sval to yield next time *)
  duplicateNontermValue:
    (*context?*)
    int ->                     (* nonterminal id *)
    tSemanticValue ->          (* sval being yielded *)
    tSemanticValue;            (* sval to yield next time *)

  (* deallocate an sval that didn't get used *)
  deallocateTerminalValue:
    (*context?*)
    int ->                     (* terminal id *)
    tSemanticValue ->          (* sval being dropped *)
    unit;
  deallocateNontermValue:
    (*context?*)
    int ->                     (* nonterminal id *)
    tSemanticValue ->          (* sval being dropped *)
    unit;

  (* merge svals for alternative derivations of the same nonterminal *)
  mergeAlternativeParses:
    int ->                     (* nonterminal with two derivations *)
    tSemanticValue ->          (* sval from derivation 1 *)  
    tSemanticValue ->          (* sval from derivation 2 *)
    tSemanticValue;            (* merged sval *)
    
  (* choose whether to keep or drop a reduced value *)
  keepNontermValue:
    int ->                     (* reduced nonterm id *)
    tSemanticValue ->          (* sval that 'reductionAction' yielded *)
    bool;                      (* if false, drop the sval on the floor *)
    
  (* reclassification goes here *)
  
  (* debugging support; see useract.h for more info *)
  terminalDescription: int -> tSemanticValue -> string;
  nonterminalDescription: int -> tSemanticValue -> string;
  terminalName: int -> string;
  nonterminalName: int -> string;
} 

