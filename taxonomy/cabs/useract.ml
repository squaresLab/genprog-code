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

(* ---------------- sample reduction actions -------------- *)
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
	   Printf.printf "merging alternative parses!\n"; flush stdout;
	   match nontermId with 
	   | 5 (* Globals *) ->
		   begin
			 let (top1 : (Cabs.tree_node list * int)) =
			   ((Obj.obj sval1) : Cabs.tree_node list * int) in
			 let (top2: (Cabs.tree_node list * int)) =
			   ((Obj.obj sval2) : Cabs.tree_node list * int) in
			   if (fst top1) > (fst top2) then sval1
			   else sval2
		   end
	   | _ -> sval1 
	);
  
  keepNontermValue = (fun nontermId sval -> true);

  terminalDescription =  (fun termId sval -> "TODO");
  nonterminalDescription = (fun termId sval -> "TODO");

  terminalName = (fun termId -> "TODO");
  nonterminalName =	(fun termId ->
	   match termId with
	  	 0 -> "empty"
	   | 1 -> "__EarlyStartSymbol"
	   | 2 -> "File"
	   | 3 -> "EnterScope"
	   | 4 -> "LeaveScope"
	   | 5 -> "Globals"
	   | 6 -> "RandomTokens"
	   | 7 -> "Expressions"
	   | 8 -> "Statements"
	   | 9 -> "RealGlobals"
	   | 10 -> "Global"
	   | 11 -> "ExternDeclaration"
	   | 12 -> "IdOrTypeName"
	   | 13 -> "MaybeComma"
	   | 14 -> "PrimaryExpression"
	   | 15 -> "PostfixExpression"
	   | 16 -> "OffsetofMemberDesignator"
	   | 17 -> "UnaryExpression"
	   | 18 -> "CastExpression"
	   | 19 -> "BinaryExpression"
	   | 20 -> "BinaryOp"
	   | 21 -> "ConditionalExpression"
	   | 22 -> "AssignmentExpression"
	   | 23 -> "AssignmentOp"
	   | 24 -> "Expression"
	   | 25 -> "Constant"
	   | 26 -> "StringConstant"
	   | 27 -> "OneStringConstant"
	   | 28 -> "StringList"
	   | 29 -> "WStringList"
	   | 30 -> "OneString"
	   | 31 -> "InitExpression"
	   | 32 -> "InitializerList"
	   | 33 -> "InitializerListOpt"
	   | 34 -> "Initializer"
	   | 35 -> "EqOpt"
	   | 36 -> "InitDesignators"
	   | 37 -> "InitDesignatorsOpt"
	   | 38 -> "GccInitDesignators"
	   | 39 -> "Arguments"
	   | 40 -> "ExpressionOpt"
	   | 41 -> "CommaExpression"
	   | 42 -> "CommaExpressionOpt"
	   | 43 -> "Block"
	   | 44 -> "BlockAttrs"
	   | 45 -> "BlockElementList"
	   | 46 -> "LocalLabels"
	   | 47 -> "LocalLabelNames"
	   | 48 -> "Statement"
	   | 49 -> "ForClause"
	   | 50 -> "Declaration"
	   | 51 -> "InitDeclaratorList"
	   | 52 -> "InitDeclarator"
	   | 53 -> "DeclSpecList"
	   | 54 -> "DeclSpecListOpt"
	   | 55 -> "DeclSpecListOptNoNamed"
	   | 56 -> "TypeSpecifier"
	   | 57 -> "StructDeclList"
	   | 58 -> "FieldDeclList"
	   | 59 -> "FieldDecl"
	   | 60 -> "EnumList"
	   | 61 -> "Enumerator"
	   | 62 -> "Declarator"
	   | 63 -> "DirectDeclarator"
	   | 64 -> "ParameterDeclList"
	   | 65 -> "RestParList1"
	   | 66 -> "ParameterDecl"
	   | 67 -> "OldProtoDecl"
	   | 68 -> "DirectOldProtoDecl"
	   | 69 -> "OldParameterListNE"
	   | 70 -> "OldPardefList"
	   | 71 -> "OldPardef"
	   | 72 -> "Pointer"
	   | 73 -> "PointerOpt"
	   | 74 -> "TypeName"
	   | 75 -> "AbstractDeclarator"
	   | 76 -> "DirectAbstractDeclarator"
	   | 77 -> "DirectAbstractDeclaratorOpt"
	   | 78 -> "FunctionDefinition"
	   | 79 -> "FunctionDefStart"
	   | 80 -> "CVSpec"
	   | 81 -> "Attributes"
	   | 82 -> "AttributesWithASM"
	   | 83 -> "AttributeNoCV"
	   | 84 -> "AttributeNoCVList"
	   | 85 -> "Attribute"
	   | 86 -> "JustAttribute"
	   | 87 -> "JustAttributes"
	   | 88 -> "Pragma"
	   | 89 -> "PrimaryAttr"
	   | 90 -> "PostfixAttr"
	   | 91 -> "UnaryAttr"
	   | 92 -> "CastAttr"
	   | 93 -> "BinaryAttr"
	   | 94 -> "ConditionalAttr"
	   | 95 -> "Attr"
	   | 96 -> "AttrListNE"
	   | 97 -> "AttrList"
	   | 98 -> "ASMAttr"
	   | 99 -> "ASMTemplate"
	   | 100 -> "ASMOutputs"
	   | 101 -> "ASMOperands"
	   | 102 -> "ASMOperandsNE"
	   | 103 -> "ASMOperand"
	   | 104 -> "ASMInputs"
	   | 105 -> "ASMOpName"
	   | 106 -> "ASMClobber"
	   | 107 -> "ASMCloberLstNE");
}


(* EOF *)
