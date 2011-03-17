open Useract
open Cabs
open Diffabs
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
				match tn with
				  Exps(_) -> accum + 1
				| _ -> accum) 0 
		  in
		  let (top1 : (Diffabs.tree_node list * int)) =
			((Obj.obj sval1) : Diffabs.tree_node list * int) in
		  let count1 = count_partial_exps (fst top1) in
(*			Printf.printf "sval1 numToks: %d, exps: %d\n" (snd top1) count1; flush stdout;*)
			let (top2: (Diffabs.tree_node list * int)) =
			  ((Obj.obj sval2) : Diffabs.tree_node list * int) in
		  let count2 = count_partial_exps (fst top2) in
(*			  Printf.printf "sval1 numToks: %d, exps: %d\n" (snd top2) count2; flush stdout;*)
			  if count1 > count2 then sval1
			  else if count2 > count1 then sval2 
			  else if (snd top1) > (snd top2) then sval1
			  else sval2
	  | "BlockElementList" ->
		let containscall exp = 
		  match exp with
			CALL(_) -> true
		  | _ -> false
		in
		let (top1: (Cabs.statement list * int)) = 
		  ((Obj.obj sval1) : Cabs.statement list * int) in
		let (top2: (Cabs.statement list * int)) =
		  ((Obj.obj sval2) : Cabs.statement list * int) in
		  if (List.length (fst top1) > 0) && (List.length (fst top2) > 0) then
		  (match (List.hd (fst top1)),(List.hd (fst top2)) with
			DEFINITION(_),COMPUTATION(exp,_) -> if containscall exp then sval2 else sval1
		  | COMPUTATION(exp,_),DEFINITION(_) -> if containscall exp then sval1 else sval2
		  | _ -> sval1)
		  else if (List.length (fst top1)) > 0 then sval1
		  else sval2
	  | _ -> sval1);
  (*		"TreeNodes" -> 
			begin
			let (top1 : (Cabs.tree_node list * int)) =
			((Obj.obj sval1) : Cabs.tree_node list * int) in
			Printf.printf "sval1 numToks: %d\n" (snd top1); flush stdout;
			let (top2: (Cabs.tree_node list * int)) =
			((Obj.obj sval2) : Cabs.tree_node list * int) in
			Printf.printf "sval1 numToks: %d\n" (snd top2); flush stdout;
			if (snd top1) > (snd top2) then sval1
			else sval2
			end
			| "StatementsNE" ->
			| "Globals" ->
			| "MiddleStatements" ->
			| "MiddleSwitch" ->
			| "FPGlobal" ->
			| "FPGlobals" ->
			| "SPGlobals" ->
			| "SPGlobal" ->
			| "StmtOrFPStmt" ->
			| "StmtOrSPStmt" ->
			| "ExpOrFPExp" ->
			| "ExpOrSPExp" ->
			| "ExpOptOrFPExpOpt" ->
			| "ExpOptOrSPExpOpt" ->
			| "CEorFPCE" ->
			| "CEorSPCE" ->
			| "ArgsOrFPArgs" ->
			| "ArgsOrSPArgs" ->
			| "BlockOrFPBlock" ->
			| "BlockOrSPBlock" ->
			| "TypeNameOrFPTypeName" ->
			| "TypeNameOrSPTypeName" ->
			| "OoMDorFPOoMD" ->
			| "OoMDorSPOoMD" ->
			| "FCorFPFC" ->
			| "FCorSPFC" ->
			| "FPExternDeclaration" ->
			| "SPExternDeclaration" ->
			| "FPIdOrTypeName" ->
			| "SPIdOrTypeName" ->
			| "FPPrimaryExpression" ->
			| "SPPrimaryExpression" ->
			| "FPPostfixExpression" ->
			| "SPPostfixExpression" ->
			| "FPOffsetofMemberDesignator" ->
			| "SPOffsetofMemberDesignator" ->
			| "FPUnaryExpression" ->
			| "SPUnaryExpression" ->
			| "FPCastExpression" ->
			| "SPCastExpression" ->
			| "FPBinaryExpression" ->
			| "SPBinaryExpression" ->
			| "FPConditionalExpression" ->
			| "SPConditionalExpression" ->
			| "FPAssignmentExpression" ->
			| "SPAssignmentExpression" ->
			| "FPExpression" ->
			| "SPExpression" ->
			| "FPInitExpression" ->
			| "SPInitExpression" ->
			| "FPInitializerList" ->
			| "SPInitializerList" ->
			| "FPInitializer" ->
			| "SPInitializer" ->
			| "FPInitDesignators" ->
			| "SPInitDesignators" ->
			| "FPGccInitDesignators" ->
			| "FPArguments" ->
			| "SPArguments" ->
			| "FPExpressionOpt" ->
			| "SPExpressionOpt" ->
			| "FPCommaExpression" ->
			| "SPCommaExpression" ->
			| "FPBlock" ->
			| "SPBlock" ->
			| "FPBlockAttrs" ->
			| "FPBlockElementList" ->
			| "SPBlockElementList" ->
			| "FPLocalLabels" ->
			| "SPLocalLabels" ->
			| "FPLocalLabelNames" ->
			| "SPLocalLabelNames" ->
			| "FPStatement" ->
			| "SPStatement" ->
			| "FPForClause" ->
			| "SPForClause" ->
			| "FPDeclaration" ->
			| "SPDeclaration" ->
			| "FPInitDeclaratorList" ->
			| "SPInitDeclaratorList" ->
			| "FPInitDeclarator" ->
			| "SPInitDeclarator" ->
			| "FPDeclSpecList" ->
			| "SPDeclSpecList" ->
			| "FPTypeSpecifier" ->
			| "SPTypeSpecifier" ->
			| "FPStructDeclList" ->
			| "SPStructDeclList" ->
			| "FPFieldDeclList" ->
			| "SPFieldDeclList" ->
			| "FPFieldDecl" ->
			| "SPFieldDecl" ->
			| "FPEnumList" ->
			| "SPEnumList" ->
			| "FPEnumerator" ->
			| "SPEnumerator" ->
			| "FPDeclarator" ->
			| "SPDeclarator" ->
			| "FPDirectDeclarator" ->
			| "SPDirectDeclarator" ->
			| "FPParameterDeclList" ->
			| "SPParameterDeclList" ->
			| "FPRestParList1" ->
			| "FPParameterDecl" ->
			| "SPParameterDecl" ->
			| "FPOldProtoDecl" ->
			| "SPOldProtoDecl" ->
			| "FPDirectOldProtoDecl" ->
			| "SPDirectOldProtoDecl" ->
			| "FPOldParameterListNE" ->
			| "SPOldParameterListNE" ->
			| "FPOldPardefList" ->
			| "SPOldPardefList" ->
			| "FPOldPardef" ->
			| "SPOldPardef" ->
			| "FPPointer" ->
			| "SPPointer" ->
			| "FPTypeName" ->
			| "SPTypeName" ->
			| "FPAbstractDeclarator" ->
			| "SPAbstractDeclarator" ->
			| "FPDirectAbstractDeclarator" ->
			| "SPDirectAbstractDeclarator" ->
			| "FPFunctionDefinition" ->
			| "SPFunctionDefinition" ->
			| "FPFunctionDefStart" ->
			| "SPFunctionDefStart" ->
			| "FPAttributes" ->
			| "SPAttributes" ->
			| "FPAttributesWithASM" ->
			| "SPAttributesWithASM" ->
			| "FPAttributeNoCV" ->
			| "SPAttributeNoCV" ->
			| "FPAttributeNoCVList" ->
			| "SPAttributeNoCVList" ->
			| "FPAttribute" ->
			| "SPAttribute" ->
			| "FPJustAttribute" ->
			| "FPJustAttributes" ->
			| "FPPragma" ->
			| "SPPragma" ->
			| "FPPrimaryAttr" ->
			| "SPPrimaryAttr" ->
			| "FPPostfixAttr" ->
			| "SPPostfixAttr" ->
			| "FPUnaryAttr" ->
			| "SPUnaryAttr" ->
			| "FPCastAttr" ->
			| "SPCastAttr" ->
			| "FPBinaryAttr" ->
			| "SPBinaryAttr" ->
			| "FPConditionalAttr" ->
			| "SPConditionalAttr" ->
			| "FPAttr" ->
			| "SPAttr" ->
			| "FPAttrList" ->
			| "SPAttrList" ->
			| "FPASMOutputs" ->
			| "SPASMOutputs" ->
			| "SPASMOperands" ->
			| "FPASMOperands" ->
			| "SPASMOperandsNE" ->
			| "FPASMOperand" ->
			| "SPASMOperand" ->
			| "FPASMInputs" ->
			| "SPASMInputs" ->
			| "FPASMOpName" ->
			| "SPASMOpName" ->
			| "FPASMClobber" ->
			| "SPASMClobber" ->
			| "FPASMCloberLstNE" ->
			| "SPASMCloberLstNE" ->
			| "Global" ->
			| "ExternDeclaration" ->
			| "IdOrTypeName" ->
			| "MaybeComma" ->
			| "PrimaryExpression" ->
			| "PostfixExpression" ->
			| "OffsetofMemberDesignator" ->
			| "UnaryExpression" ->
			| "CastExpression" ->
			| "BinaryExpression" ->
			| "BinaryOp" ->
			| "ConditionalExpression" ->
			| "AssignmentExpression" ->
			| "AssignmentOp" ->
			| "Expression" ->
			| "Constant" ->
			| "StringConstant" ->
			| "OneStringConstant" ->
			| "StringList" ->
			| "WStringList" ->
			| "InitExpression" ->
			| "InitializerList" ->
			| "InitializerListOpt" ->
			| "Initializer" ->
			| "EqOpt" ->
			| "InitDesignators" ->
			| "InitDesignatorsOpt" ->
			| "GccInitDesignators" ->
			| "Arguments" ->
			| "ExpressionOpt" ->
			| "CommaExpression" ->
			| "CommaExpressionOpt" ->
			| "ParenCommaExpression" ->
			| "Block" ->
			| "BlockAttrs" ->
			| "BlockAttrsNE" ->
			| "BlockElementList" ->
			| "LocalLabels" ->
			| "LocalLabelsNE" ->
			| "LocalLabelNames" ->
			| "Statement" ->
			| "ForClause" ->
			| "Declaration" ->
			| "InitDeclaratorList" ->
			| "InitDeclarator" ->
			| "DeclSpecList" ->
			| "DeclSpecListOpt" ->
			| "DeclSpecListOptNoNamed" ->
			| "TypeSpecifier" ->
			| "StructDeclListNE" ->
			| "StructDeclList" ->
			| "FieldDeclList" ->
			| "FieldDecl" ->
			| "EnumList" ->
			| "Enumerator" ->
			| "Declarator" ->
			| "DirectDeclarator" ->
			| "ParameterDeclList" ->
			| "RestParList1" ->
			| "ParameterDecl" ->
			| "OldProtoDecl" ->
			| "DirectOldProtoDecl" ->
			| "OldParameterListNE" ->
			| "OldPardefList" ->
			| "OldPardefListNE" ->
			| "OldPardef" ->
			| "Pointer" ->
			| "PointerOpt" ->
			| "TypeName" ->
			| "AbstractDeclarator" ->
			| "DirectAbstractDeclarator" ->
			| "DirectAbstractDeclaratorOpt" ->
			| "FunctionDefinition" ->
			| "FunctionDefStart" ->
			| "CVSpec" ->
			| "Attributes" ->
			| "AttributesWithASM" ->
			| "AttributeNoCV" ->
			| "AttributeNoCVList" ->
			| "AttributeNoCVListNE" ->
			| "Attribute" ->
			| "JustAttribute" ->
			| "JustAttributes" ->
			| "Pragma" ->
			| "PrimaryAttr" ->
			| "PostfixAttr" ->
			| "UnaryAttr" ->
			| "CastAttr" ->
			| "BinaryAttr" ->
			| "ConditionalAttr" ->
			| "Attr" ->
			| "AttrListNE" ->
			| "AttrList" ->
			| "ASMAttr" ->
			| "ASMTemplate" ->
			| "ASMOutputs" ->
			| "ASMOperands" ->
			| "ASMOperandsNE" ->
			| "ASMOperand" ->
			| "ASMInputs" ->
			| "ASMOpName" ->
			| "ASMClobber" ->
			| "ASMCloberLstNE" ->
			| _ -> sval1 
			);*)
  
  keepNontermValue = (fun nontermId sval -> true);

  terminalDescription =  (fun termId sval -> "TODO");
  nonterminalDescription = (fun termId sval -> "TODO");

  terminalName = (fun termId -> "TODO");
  nonterminalName =	(fun termId -> "TODO");
}
