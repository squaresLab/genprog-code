(* cprint -- pretty printer of C program from abstract syntax
**
** Project:	FrontC
** File:	cprint.ml
** Version:	2.1e
** Date:	9.1.99
** Author:	Hugues Cassé
**
**	1.0		2.22.99	Hugues Cassé	First version.
**	2.0		3.18.99	Hugues Cassé	Compatible with Frontc 2.1, use of CAML
**									pretty printer.
**	2.1		3.22.99	Hugues Cassé	More efficient custom pretty printer used.
**	2.1a	4.12.99	Hugues Cassé	Correctly handle:
**									char *m, *m, *p; m + (n - p)
**	2.1b	4.15.99	Hugues Cassé	x + (y + z) stays x + (y + z) for
**									keeping computation order.
**	2.1c	7.23.99	Hugues Cassé	Improvement of case and default display.
**	2.1d	8.25.99	Hugues Cassé	Rebuild escape sequences in string and
**									characters.
**	2.1e	9.1.99	Hugues Cassé	Fix, recognize and correctly display '\0'.
*)

(* George Necula: I changed this pretty dramatically since CABS changed *)
open Cabs
open Pretty
open Escape
open Whitetrack

let version = "Cprint 2.1e 9.1.99 Hugues Cassé"

type loc = { line : int; file : string }

let lu = {line = -1; file = "loc unknown";}
let cabslu = {lineno = -10; 
			  filename = "cabs loc unknown"; 
			  byteno = -10;
              ident = 0;}

let curLoc = ref cabslu
let printLn = ref true
let printLnComment = ref false

(*
** FrontC Pretty printer
*)
let out = ref stdout
let width = ref 80
let tab = ref 2
let max_indent = ref 60

let line = ref ""
let line_len = ref 0
let current = ref ""
let current_len = ref 0
let spaces = ref 0
let follow = ref 0
let roll = ref 0
    

(* sm: for some reason I couldn't just call print from frontc.... ? *)
let print_unescaped_string str = print str

let print_string (s:string) =
  print ("\"" ^ escape_string s ^ "\"")

let set_tab t = tab := t
let set_width w = width := w

(* Printers for pretty-printing Cabs statements when we don't want to write to
 stdout.  Modeled on pretty.ml and the d_exp () etc functions in CIL.  Why am I
 the first person to write this nonsense?
*)

class type cabsPrinter = object
  method pSpecElem : unit -> spec_elem -> doc
  method pAsmDetails : unit -> asm_details option -> doc
  method pForClause : unit -> for_clause -> doc
  method pSpecifier : unit -> specifier -> doc
  method pTypeSpecifier : unit -> typeSpecifier -> doc
  method pDeclType : unit -> string -> decl_type -> doc
  method pEnumItems : unit -> enum_item  list -> doc
  method pName : unit -> name  -> doc
  method pInitName : unit -> init_name  -> doc
  method pNameGroup : unit -> name_group  -> doc
  method pFieldGroup : unit -> field_group  -> doc
  method pField : unit -> (name  * expression node option) -> doc
  method pInitNameGroup : unit -> init_name_group  -> doc
  method pSingleName : unit -> single_name  -> doc
  method pInitwhat : unit -> initwhat  -> doc
  method pInitExpression : unit -> init_expression  -> doc
  method pExpression : unit -> expression node -> doc
  method pStatement : unit -> statement node -> doc
  method pBlock : unit -> block  -> doc
  method pAttribute : unit -> attribute  -> doc
  method pAttributes : unit -> attribute  list -> doc
  method pDefinition : unit -> definition node -> doc
  method pDefinitions : unit -> definition node list -> doc
  method pTreeNode : unit -> tree_node node -> doc
  method pTree : unit -> tree -> doc
  method pFile : unit -> file -> doc
  method pDirective : unit -> directive node -> doc

  method dDefinition : out_channel -> definition node -> unit

  method dStatement : out_channel -> int -> statement node -> unit
  method dExpression : out_channel -> int -> expression node -> unit
  method dBlock : out_channel -> int -> block  -> unit
  method dFile : out_channel -> file -> unit
  method dTree : out_channel -> tree -> unit
  method dTreeNode : out_channel -> tree_node node -> unit
end

let get_operator exp =
  match exp with
    NOTHING -> ("", 16)
  | PAREN exp -> ("", 16)
  | UNARY (op, _) ->
	  (match op with
		 MINUS -> ("-", 13)
	   | PLUS -> ("+", 13)
	   | NOT -> ("!", 13)
	   | BNOT -> ("~", 13)
	   | MEMOF -> ("*", 13)
	   | ADDROF -> ("&", 13)
	   | PREINCR -> ("++", 13)
	   | PREDECR -> ("--", 13)
	   | POSINCR -> ("++", 14)
	   | POSDECR -> ("--", 14))
  | LABELADDR s -> ("", 16)  (* Like a constant *)
  | BINARY (op, _, _) ->
	  (match op with
		 MUL -> ("*", 12)
	   | DIV -> ("/", 12)
	   | MOD -> ("%", 12)
	   | ADD -> ("+", 11)
	   | SUB -> ("-", 11)
	   | SHL -> ("<<", 10)
	   | SHR -> (">>", 10)
	   | LT -> ("<", 9)
	   | LE -> ("<=", 9)
	   | GT -> (">", 9)
	   | GE -> (">=", 9)
	   | EQ -> ("==", 8)
	   | NE -> ("!=", 8)
	   | BAND -> ("&", 7)
	   | XOR -> ("^", 6)
	   | BOR -> ("|", 5)
	   | AND -> ("&&", 4)
	   | OR -> ("||", 3)
	   | ASSIGN -> ("=", 1)
	   | ADD_ASSIGN -> ("+=", 1)
	   | SUB_ASSIGN -> ("-=", 1)
	   | MUL_ASSIGN -> ("*=", 1)
	   | DIV_ASSIGN -> ("/=", 1)
	   | MOD_ASSIGN -> ("%=", 1)
	   | BAND_ASSIGN -> ("&=", 1)
	   | BOR_ASSIGN -> ("|=", 1)
	   | XOR_ASSIGN -> ("^=", 1)
	   | SHL_ASSIGN -> ("<<=", 1)
	   | SHR_ASSIGN -> (">>=", 1))
  | QUESTION _ -> ("", 2)
  | CAST _ -> ("", 13)
  | CALL _ -> ("", 15)
  | COMMA _ -> ("", 0)
  | CONSTANT _ -> ("", 16)
  | VARIABLE name -> ("", 16)
  | EXPR_SIZEOF exp -> ("", 16)
  | TYPE_SIZEOF _ -> ("", 16)
  | EXPR_ALIGNOF exp -> ("", 16)
  | TYPE_ALIGNOF _ -> ("", 16)
  | INDEX (exp, idx) -> ("", 15)
  | MEMBEROF (exp, fld) -> ("", 15)
  | MEMBEROFPTR (exp, fld) -> ("", 15)
  | GNU_BODY _ -> ("", 17)
  | EXPR_PATTERN _ -> ("", 16)     (* sm: not sure about this *)

let d_const () = function
  CONST_INT i -> text i
  | CONST_FLOAT r -> text r
  | CONST_CHAR c -> text ("'" ^ escape_wstring c ^ "'")
  | CONST_WCHAR c -> text ("L'" ^ escape_wstring c ^ "'")
  | CONST_STRING s -> text ("\"" ^ escape_string s ^ "\"")
  | CONST_WSTRING ws -> text ("L\"" ^ escape_wstring ws ^ "\"")

class defaultCabsPrinterClass : cabsPrinter = object (self)

  method pForClause () fc =
	match fc with
	  FC_EXP exp1 -> self#pExpressionLevel 0 exp1 ++ chr ';'
	| FC_DECL dec1 -> self#pDefinition () dec1

  method pAsmDetails () details = 
	let d_asm_operand () (identop,cnstr, e) =
      text cnstr ++ chr ' ' ++ self#pExpressionLevel 100 e
	in
	  match details with
	  | None -> nil
	  | Some d ->
		  let { aoutputs = outs; ainputs = ins; aclobbers = clobs } = d in
			text ": "
			++ (docList ~sep:(text ",\n") (d_asm_operand ()) () outs)
			++ 
			  if ins <> [] || clobs <> [] then begin
				text ": "
				++ (docList ~sep:(text ",\n") (d_asm_operand ()) () ins)
				++ (if clobs <> [] then begin
					  text ": "
					  ++ (docList ~sep:(text ",\n") text () clobs)
					end else nil)
			  end else nil

  method pSpecElem () se = 
	match se with
	  SpecTypedef -> text "typedef"
	| SpecInline -> text "__inline__"
	| SpecStorage sto ->
		text (match sto with
				NO_STORAGE -> ""
			  | AUTO -> "auto"
			  | STATIC -> "static"
			  | EXTERN -> "extern"
			  | REGISTER -> "register")
	| SpecCV cv -> 
		text (match cv with
			  | CV_CONST -> "const"
			  | CV_VOLATILE -> "volatile"
			  | CV_RESTRICT -> "restrict")
	| SpecAttr al -> self#pAttribute () al ++ chr ' '
	| SpecPattern name -> text "@specifier(" ++ text name ++ chr ')'
	| SpecType bt -> self#pTypeSpecifier () bt

  method pSpecifier () (specs:specifier ) = 
	((docList ~sep:(chr ' ' ++ break) (self#pSpecElem ())) () specs)

  method pTypeSpecifier () ts =
	match ts with
      Tvoid -> text "void "
	| Tchar -> text "char "
	| Tshort -> text "short "
	| Tint -> text "int "
	| Tlong -> text "long "
	| Tint64 -> text "__int64 "
	| Tfloat -> text "float "
	| Tdouble -> text "double "
	| Tsigned -> text "signed "
	| Tunsigned -> text "unsigned "
	| Tnamed s -> text s ++ chr ' '
	| Tstruct (n, None, _) -> text "struct" ++ text n 
	| Tstruct (n, Some flds, extraAttrs) ->
		(self#pStructNameAttr "struct" n extraAttrs)
		++ self#pFields () flds
	| Tunion (n, None, _) -> text "union" ++ text n ++ chr ' '
	| Tunion (n, Some flds, extraAttrs) ->
		(self#pStructNameAttr "union" n extraAttrs)
		++ self#pFields () flds 
	| Tenum (n, None, _) -> text "enum" ++ text n 
	| Tenum (n, Some enum_items, extraAttrs) ->
		(self#pStructNameAttr "enum" n extraAttrs)
		++ self#pEnumItems () enum_items
	| TtypeofE e -> text "__typeof__ (" ++ self#pExpression () e ++ text ") "
	| TtypeofT (s,d) -> text "__typeof__(" ++ self#pOnlyType () (s, d) ++ text ") "

  method private pStructNameAttr (keyword: string) (name: string) (extraAttrs: attribute  list) =
	if extraAttrs = [] then (text keyword ++ text name)
	else begin
	  text keyword 
	  ++ self#pAttributes () extraAttrs
	  ++ text name
	end

  method pDeclType () (n : string) d = (* FIXME: I don't know if that'll work for Pretty.doc purposes *)
	match d with
	| JUSTBASE -> if n <> "___missing_field_name" then text n else nil
	| PARENTYPE (al1, d, al2) ->
		chr '('
		++ self#pAttributes () al1
		++ chr ' '
		++ self#pDeclType () n d
		++ chr ' '
		++ self#pAttributes () al2
		++ chr ')'
	| PTR (al, d) ->
		text "* "
		++ self#pAttributes () al
		++ chr ' '
		++ self#pDeclType () n d
	| ARRAY (d, al, e) ->
		self#pDeclType () n d
		++ chr '['
		++ self#pAttributes () al
		++ self#pExpression () e
		++ chr ']'
	| PROTO(d, args, isva) ->
		self#pDeclType () n d 
		++ chr '('
		++ self#pParams () args isva
		++ chr ')'

  method private pFields () (flds : field_group   list) =
	if flds = [] then text " { } "
	else begin
      text " {"
	  ++ text "  "
	  ++ (align
		  ++ ((docList ~sep:(chr ' ' ++ break) (self#pFieldGroup ())) () flds)
		  ++ unalign)
	  ++ chr '}'
	end

  method pEnumItems () items =
    text " {\n"
	++ (align
		++ (docList ~sep:(chr ',')
			  (fun en -> 
				 let (n,i, loc) = en in
				   text (n ^ " = ") 
				   ++ self#pExpression () i)
			  () items)
		++ unalign) ++ text "\n} " 
	  
  method private pOnlyType () (specs, dt) = self#pSpecifier () specs ++ self#pDeclType () "" dt
    
  method pName () (name : name ) =
	let (n,decl,attrs,_) = name in
	  self#pDeclType () n decl
	  ++ chr ' '
	  ++ self#pAttributes () attrs

  method pInitName () (inname : init_name ) =
	let (n, i) = inname in
	  self#pName () n ++
		if i <> NO_INIT then begin
		  text " = "
		  ++ self#pInitExpression () i
		end
		else nil
		  
  method pNameGroup () ng =
	let (specs, names) = ng in
	  self#pSpecifier () specs ++
		((docList ~sep:(chr ',') (self#pName ())) () names)
		
  method pFieldGroup () fg = 
	let (specs, fields) = fg in
	  self#pSpecifier () specs 
	  ++ (docList ~sep:(chr ',') (self#pField()) () fields)

  method pField () (name, widtho) = 
	self#pName () name 
	++ 
	  (match widtho with 
		 None -> nil
	   | Some w -> text " : " ++ self#pExpression () w)

  method pInitNameGroup () ing = 
	let (specs, names) = ing in
	  self#pSpecifier () specs 
	  ++ ((docList ~sep:(chr ',') (self#pInitName ())) () names)
		
  method pSingleName () sn =
	let (specs, name) = sn in
	  self#pSpecifier () specs ++ self#pName () name

  method private pParams () (pars : single_name  list) (ell : bool) =
	let e = 
	  if ell then 
		if pars = [] then text "..." else text ",..."
	  else nil
	in
	  (docList ~sep:(chr ',') (self#pSingleName ()) () pars) ++ e
		
  method private pOldParams () pars ell =
	let e = 
	  if ell then 
		if pars = [] then text "..." else text ",..."
	  else nil
	in
	  (docList ~sep:(chr ',') text () pars) ++ e

  method pInitwhat () iw =
	match iw with
      NEXT_INIT -> nil
	| INFIELD_INIT (fn, i) -> chr '.' ++ text fn ++ self#pInitwhat () i
	| ATINDEX_INIT (e, i) -> 
		chr '[' 
		++ self#pExpression () e 
		++ chr ']' 
		++ self#pInitwhat () i
	| ATINDEXRANGE_INIT (s, e) -> 
		chr '['
		++ self#pExpression () s
		++ text " ... "
		++ self#pExpression () e
		++ chr ']'

  method pInitExpression () (iexp: init_expression ) = 
	match iexp with 
      NO_INIT -> nil
	| SINGLE_INIT e -> self#pExpression () e
	| COMPOUND_INIT  initexps ->
		let doinitexp () = function
			n, e when n = NEXT_INIT -> self#pInitExpression () e
          | i, e -> 
              let rec doinit iw =
				match iw with
                  NEXT_INIT -> nil
				| INFIELD_INIT (fn, i) -> chr '.' ++ text fn ++ doinit i
				| ATINDEX_INIT (e, i) -> 
					chr '[' ++ self#pExpression () e ++ chr ']' ++ doinit i
				| ATINDEXRANGE_INIT (s, e) -> 
					chr '[' ++ self#pExpression () s  
					++ text " ... "
					++ self#pExpression () e
					++ chr ']'
              in
				doinit i ++ text " = " ++ self#pInitExpression () e
		in
		  chr '{'
		  ++ (docList ~sep:(chr ',') (doinitexp ()) () initexps)
		  ++ chr '}'

  method pExpression () (exp: expression node) = self#pExpressionLevel 1 exp

  method private pExpressionLevel lvl exp =
	let (txt, lvl') = get_operator (dn exp) in
	  match (dn exp) with
		NOTHING -> nil
	  | PAREN exp -> 
		  chr '(' 
		  ++ self#pExpression () exp 
		  ++ chr ')'
	  | UNARY (op, exp') -> 
		  (match op with
			 POSINCR | POSDECR ->
			   (self#pExpressionLevel lvl' exp')
			   ++ text txt
		   | _ ->
			   text txt 
			   ++ chr ' '
			   ++ self#pExpressionLevel lvl' exp')
	  | LABELADDR l -> text "&&" ++ text l
	  | BINARY (op, exp1, exp2) ->
		  align
		  ++ (self#pExpressionLevel lvl' exp1)
		  ++ chr ' '
		  ++ text txt 
		  ++ chr ' '
		  ++ (self#pExpressionLevel (lvl' + 1) exp2)
		  ++ unalign
	  | QUESTION (exp1, exp2, exp3) ->
		  align
		  ++ (self#pExpressionLevel 2 exp1)
		  ++ text " ? " 
		  ++ (self#pExpressionLevel 2 exp2)
		  ++ text " : "
		  ++ (self#pExpressionLevel 2 exp3)
		  ++ unalign
	  | CAST (typ, iexp) ->
		  chr '('
		  ++ self#pOnlyType () typ
		  ++ chr ')'
		  ++
			(match iexp with
			   SINGLE_INIT e -> self#pExpressionLevel 15 e
			 | COMPOUND_INIT _ -> self#pInitExpression () iexp
			 | NO_INIT -> text "<NO_INIT in cast. Should never arise>")
	  | CALL (fn, args) ->
(*		Printf.printf "CALL\n"; flush stdout;*)
		  if (dn fn) == VARIABLE "__builtin_va_arg" then
			(match args with 
			   [node1;node2] ->
				 (match (dn node2) with 
					TYPE_SIZEOF (bt, dt) -> 
					  text "__builtin_va_arg" 
					  ++ chr '(' 
					  ++ self#pExpressionLevel 1 node1
					  ++ chr ','
					  ++ self#pOnlyType () (bt,dt)
					  ++ chr ')'
				  | _ -> (self#pExpressionLevel 16 fn)
					  ++ chr '('
					  ++ (docList ~sep:(chr ',') (self#pExpression ()) () args)
					  ++ chr ')')
			 |  _ -> (self#pExpressionLevel 16 fn)
				  ++ chr '('
				  ++ (docList ~sep:(chr ',') (self#pExpression ()) () args)
				  ++ chr ')')
		  else begin
			(self#pExpressionLevel 16 fn)
			++ chr '('
			++ (docList ~sep:(chr ',') (self#pExpression ()) () args)
			++ chr ')'
		  end
	  | COMMA exps -> (docList ~sep:(chr ',') (self#pExpression ()) () exps)
	  | CONSTANT cst -> d_const () cst
	  | VARIABLE name -> text name
	  | EXPR_SIZEOF exp -> text "sizeof" ++ self#pExpressionLevel 0 exp
	  | TYPE_SIZEOF (bt,dtn) ->
		  text "sizeof"
		  ++ chr '('
		  ++ self#pOnlyType () (bt, dtn)
		  ++ chr ')'
	  | EXPR_ALIGNOF exp ->
		  text "alignof"
		  ++ chr '('
		  ++ self#pExpressionLevel 0 exp
		  ++ chr ')'
	  | TYPE_ALIGNOF (bt,dtn) ->
		  text "__alignof__"
		  ++ chr '('
		  ++ self#pOnlyType () (bt, dtn)
		  ++ chr ')'
	  | INDEX (exp, idx) ->
		  self#pExpressionLevel 16 exp
		  ++ chr '['
		  ++ self#pExpressionLevel 0 idx
		  ++ chr ']'
	  | MEMBEROF (exp, fld) ->
		  self#pExpressionLevel 16 exp
		  ++ chr '.'
		  ++ text fld
	  | MEMBEROFPTR (exp, fld) ->
		  self#pExpressionLevel 16 exp
		  ++ text "->"
		  ++ text fld
	  | GNU_BODY (blk) ->
		  chr '('
		  ++ self#pBlock () blk
		  ++ chr ')'
	  | EXPR_PATTERN (name) ->
		  text "@expr(" ++ text name ++ chr ')'

  method pStatement () stat =
	match (dn stat) with
      NOP (loc) -> chr ';'
	| COMPUTATION (exp, loc) ->
		self#pExpression () exp
		++ chr ';'
		++ text "\n" (* fixme: how else to do newline? *)
	| BLOCK (blk, loc) ->  self#pBlock () blk
	| SEQUENCE (s1, s2, loc) -> (*Printf.printf "SEQUENCE\n"; flush stdout;*) self#pStatement () s1 ++ self#pStatement () s2
	| IF (exp, s1, s2, loc) ->
(*	  Printf.printf "IF\n"; flush stdout;*)
		text "if ("
		++ self#pExpressionLevel 0 exp
		++ chr ')'
		++ self#pSubStatement () s1
		++
		  (match (dn s2) with
		   | NOP(_) -> nil
		   | _ -> begin
			   text "else" ++
				 self#pSubStatement () s2
			 end)
	| WHILE (exp, stat, loc) ->
		text "while ("
		++ self#pExpressionLevel 0 exp
		++ chr ')'
		++ self#pSubStatement () stat
	| DOWHILE (exp, stat, loc) ->
		text "do"
		++ self#pSubStatement () stat
		++ text "while ("
		++ self#pExpressionLevel 0 exp
		++ text ");\n"
	| FOR (fc1, exp2, exp3, stat, loc) ->
		text "for (" ++ self#pForClause () fc1
		++ chr ' '
		++ self#pExpressionLevel 0 exp2
		++ chr ';'
		++ chr ' '
		++ self#pExpressionLevel 0 exp3
		++ chr ')'
		++ self#pSubStatement () stat
	| BREAK (loc)-> text "break;\n"
	| CONTINUE (loc) -> text "continue;\n"
	| RETURN (exp, loc) -> 
		text "return"
		++ if (dn exp) = NOTHING then nil
		else begin
		  chr ' ' ++
			self#pExpressionLevel 1 exp
		end ++ text ";\n"
	| SWITCH (exp, stat, loc) ->
		text "switch ("
		++ self#pExpressionLevel 0 exp
		++ chr ')'
		++ self#pSubStatement () stat
	| CASE (exp, stat, loc) ->
		(*		unindent (); FIXME? *)
		text "case "
		++ self#pExpressionLevel 1 exp
		++ text ":  "
		++ (align ++ self#pSubStatement () stat ++ unalign)
	| CASERANGE (expl, exph, stat, loc) ->
		text "case "
		++ self#pExpression () expl
		++ text " ... "
		++ self#pExpression () exph 
		++ chr ':'
		++ text "  "
		++ (align ++ self#pSubStatement () stat ++ unalign)
	| DEFAULT (stat, loc) -> text "default :  " ++ (align ++ self#pSubStatement () stat ++ unalign)
	| LABEL (name, stat, loc) ->
		text "name"
		++ text ":  "
		++ (align ++ self#pSubStatement () stat ++ unalign)
	| GOTO (name, loc) ->
		text "goto"
		++ text name
		++ text ";\n"
	| COMPGOTO (exp, loc) -> 
		text "goto *"
		++ self#pExpression () exp 
		++ text ";\n" 
	| DEFINITION d -> self#pDefinition () d
	| ASM (attrs, tlist, details, loc) ->
		text "__asm__ "
		++ self#pAttributes () attrs
		++ chr '('
		++ (docList ~sep:(text "\n" ) text () tlist) 
		++ self#pAsmDetails () details 
		++ text ");\n"
	| TRY_FINALLY (b, h, loc) -> 
		text "__try "
		++ self#pBlock () b
		++ text "__finally "
		++ self#pBlock () h
	| TRY_EXCEPT (b, e, h, loc) -> 
		text "__try "
		++ self#pBlock () b
		++ text "__except ("
		++ self#pExpression () e
		++ chr ')'
		++ self#pBlock () h
		  
  method pBlock () blk = 
	text "{  "
	++ (align ++
		  if blk.blabels <> [] then begin
			text "__label__ "
			++ (docList ~sep:(text ",\n") text () blk.blabels)
			++ text ";\n"
		  end else nil
			++
			if blk.battrs <> [] then begin
			  Printf.printf "ATTRS\n"; flush stdout;
			  (docList ~sep:(text ",\n") (self#pAttribute ()) () blk.battrs)
			  ++ text "\n"
			end else nil
			  ++ ((*(Printf.printf "PSTATEMENT\n"; flush stdout);*) (docList ~sep:(text ",\n") (self#pStatement ()) () blk.bstmts))
			  ++ unalign) 
	++ text "}\n"
	  
  method private pSubStatement () stat = 
	match (dn stat) with
	  IF _
	| SEQUENCE _
	| DOWHILE _ ->
		text "\n{  " 
		++ (align ++
			  self#pStatement () stat ++ unalign)
		++ text "}\n"
	| BLOCK _ -> self#pStatement () stat
	| _ ->
		text "  " ++ (align ++ self#pStatement () stat ++ unalign)

  method pAttribute () attr = 
	let (name,args) = attr in
	  if args = [] then text name
	  else begin
		text name
		++ text "("
		++ (if name = "__attribute__" then text "(" else nil)
		++
		  (match args with
			 [node] when node.node == VARIABLE "aconst" -> text "const"
		   | [node] when node.node == VARIABLE "restrict" -> text "restrict"
	  	   | _ -> (docList ~sep:(text ",\n") (self#pExpression ()) () args))
		++ text ")" ++ if name = "__attribute__" then text ")" else nil
	  end

  method pAttributes () attrs =
	docList ~sep:(text " \n") (self#pAttribute ()) () attrs

  method pDefinitions () defs = (* FIXME?: not what was originally in cprint but it's complicated and stupid so whateer *)
	docList ~sep:(text "\n") (self#pDefinition ()) () defs

  method pDefinition () def =
	match (dn def) with
	  FUNDEF (proto, body, loc, _) ->
(*		Printf.printf "PRINTING FUNDEF\n"; flush stdout;*)
		self#pSingleName () proto
		++ self#pBlock () body
		++ text "\n"
	| DECDEF (names, loc) ->
(*	  Printf.printf "DECDEF\n"; flush stdout;*)
		self#pInitNameGroup () names ++ text ";\n"
	| TYPEDEF (names, loc) ->
(*	  Printf.printf "TYPEDEF\n"; flush stdout;*)
		self#pNameGroup () names 
		++ text ";\n\n" 
	| ONLYTYPEDEF (specs, loc) ->
(*	  Printf.printf "ONLYTYPEDEF\n"; flush stdout;*)
		self#pSpecifier () specs 
		++ text ";\n\n"
	| GLOBASM (asm, loc) ->
		text "__asm__ ("
		++ text asm 
		++ text ");\n\n"
	| PRAGMA (a,loc) ->
		text "\n#pragma\n"
		++ self#pExpression () a 
		++ text "\n"
		  (* FIXME: do not wrap pragmas? *)
	| LINKAGE (n, loc, dl) -> 
		text "\nextern"
		++ text n 
		++ text "  { "
		++ (docList (self#pDefinition ()) () dl)
		++ text "}\n"
	| DIRECTIVE (d) -> text "DIRECTIVE [" ++ self#pDirective () d ++ text "]\n"

  method pTreeNode () tn = 
	match tn.node with
	| Globals(dlist) -> text "GLOBALS (" ++ self#pDefinitions () dlist ++ text ")\n"
	| Stmts(slist) -> text "STMTS (" ++ (docList ~sep:(chr ';') (self#pStatement ()) () slist) ++ text ")\n"
	| Exps(elist) -> text "EXPS (" ++ (docList ~sep:(chr ';') (self#pExpression ()) () elist) ++ text ")\n"
	| Syntax(s) -> text "SYNTAX (" ++ text s ++ text ")\n"

  method pDirective () d = 
	match (dn d) with
	  PREINCLUDE(str,loc) -> text "#include " ++ text str 

  method pFile () (fname,defs) = self#pDefinitions () defs

  method pTree () ((fname, nodes) : tree) = docList ~sep:(text " \n") (self#pTreeNode ()) () nodes
	
  method dDefinition out def = 
	fprint out 80 (self#pDefinition () def)

  method dStatement out ind s = 
	fprint out 80 (indent ind (self#pStatement () s))

  method dExpression out ind e = 
	fprint out 80 (indent ind (self#pExpression () e))

  method dBlock out ind block =  (* does this align still make sense? *)
	fprint out 80 (indent ind (align ++ self#pBlock () block))

  method dFile out file = fprint out 80 (self#pFile () file)

  method dTree out tree = fprint out 80 (self#pTree () tree)
  method dTreeNode out treenode = fprint out 80 (self#pTreeNode () treenode)
end (* class defaultCabsPrinterClass *)

let defaultCabsPrinter = new defaultCabsPrinterClass

(* Top-level printing functions *)
  
let printExp (pp: cabsPrinter) () (e: expression node) : doc = 
  pp#pExpression () e

let printDefinition (pp: cabsPrinter) () (g: definition node) : doc = 
  pp#pDefinition () g

let dumpDefinition (pp: cabsPrinter) (out: out_channel) (g: definition node) : unit = 
  pp#dDefinition out g

let printAttr (pp: cabsPrinter) () (a: attribute ) : doc = pp#pAttribute () a

let printAttrs (pp: cabsPrinter) () (a: attribute  list) : doc = 
  pp#pAttributes () a

let printStmt (pp: cabsPrinter) () (s: statement node) : doc = 
  pp#pStatement () s

let printBlock (pp: cabsPrinter) () (b: block ) : doc = pp#pBlock () b
  (* We must add the alignment ourselves, becuase pBlock will pop it *)

let printSpecElem (pp: cabsPrinter) () (sc: spec_elem ) : doc = pp#pSpecElem () sc
let printForClause (pp: cabsPrinter) () (fc: for_clause ) : doc = pp#pForClause () fc
let printAsmDetails (pp: cabsPrinter) () (asm: asm_details option ) : doc = pp#pAsmDetails () asm
let printDeclType (pp: cabsPrinter) () (dt: decl_type ) : doc = pp#pDeclType () "" dt
let printInitNameGroup (pp: cabsPrinter) () (ing: init_name_group ) : doc = pp#pInitNameGroup () ing
let printInitName (pp: cabsPrinter) () (ing: init_name ) : doc = pp#pInitName () ing
let printInitWhat (pp: cabsPrinter) () (what: initwhat ) : doc = pp#pInitwhat () what
let printName (pp: cabsPrinter) () (n : name ) : doc = pp#pName () n
let printNameGroup (pp: cabsPrinter) () (ng: name_group ) : doc = pp#pNameGroup () ng
let printSingleName (pp: cabsPrinter) () (sn: single_name ) : doc = pp#pSingleName () sn
let printSpecifier (pp: cabsPrinter) () (s: specifier ) : doc = pp#pSpecifier () s
let printInitExpression (pp: cabsPrinter) () (ie: init_expression ) : doc = pp#pInitExpression () ie
let printFieldGroup (pp: cabsPrinter) () (ie: field_group ) : doc = pp#pFieldGroup () ie
let printEnumItem (pp: cabsPrinter) () (ie: enum_item ) : doc = pp#pEnumItems () [ie]
let printTypeSpec (pp : cabsPrinter) () (tc : typeSpecifier ) : doc = pp#pTypeSpecifier () tc
let printTreeNode (pp: cabsPrinter) () (tn: tree_node node) : doc = pp#pTreeNode () tn
let printTree (pp : cabsPrinter) () (tree : tree) : doc = pp#pTree () tree

let dumpStmt (pp: cabsPrinter) (out: out_channel) (ind: int) (s: statement node) : unit = 
  pp#dStatement out ind s

let dumpExpression (pp: cabsPrinter) (out: out_channel) (ind: int) (s: expression node) : unit = 
  pp#dExpression out ind s

let dumpBlock (pp: cabsPrinter) (out: out_channel) (ind: int) (b: block ) : unit = 
  pp#dBlock out ind b

let dumpFile (pp : cabsPrinter) (out: out_channel) (f:file) : unit = pp#dFile out f

let dumpTree (pp : cabsPrinter) (out: out_channel) (t:tree) : unit = pp#dTree out t

let dumpTreeNode (pp : cabsPrinter) (out: out_channel) (t:tree_node node) : unit = pp#dTreeNode out t

(* Now define some short cuts *)
let d_type_spec () tc = printTypeSpec defaultCabsPrinter () tc
let d_tree () tree = printTree defaultCabsPrinter () tree
let d_tree_node () tn = printTreeNode defaultCabsPrinter () tn
let d_spec_elem () d = printSpecElem defaultCabsPrinter () d
let d_name () name = printName defaultCabsPrinter () name
let d_exp () e = printExp defaultCabsPrinter () e
let d_definition () g = printDefinition defaultCabsPrinter () g
let d_attrlist () a = printAttrs defaultCabsPrinter () a 
let d_attr () a = printAttr defaultCabsPrinter () a
let d_stmt () s = printStmt defaultCabsPrinter () s
let d_block () b = printBlock defaultCabsPrinter () b
let d_fc () fc = printForClause defaultCabsPrinter () fc
let d_asm_det () asm = printAsmDetails defaultCabsPrinter () asm
let d_decl_type () dt = printDeclType defaultCabsPrinter () dt
let d_init_name_group () ng = printInitNameGroup defaultCabsPrinter () ng
let d_init_name () inn = printInitName defaultCabsPrinter () inn
let d_init_what () what = printInitWhat defaultCabsPrinter () what
let d_name_group () ng = printNameGroup defaultCabsPrinter () ng
let d_single_name () ng = printSingleName defaultCabsPrinter () ng
let d_specifier () sps = printSpecifier defaultCabsPrinter () sps
let d_init_expression () ie = printInitExpression defaultCabsPrinter () ie
let d_def () def = printDefinition defaultCabsPrinter () def
let d_field_group () fg = printFieldGroup defaultCabsPrinter () fg
let d_enum_item () fg = printEnumItem defaultCabsPrinter () fg
