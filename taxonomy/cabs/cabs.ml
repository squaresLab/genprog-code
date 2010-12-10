(* 
 * Stolen wholesale from CIL.
 *
 *)

(** This file was originally part of Hugues Casee's frontc 2.0, and has been 
 * extensively changed since. 
**
** 1.0	3.22.99	Hugues Cassé	First version.
** 2.0  George Necula 12/12/00: Many extensions
 **)

(*
** Types
*)

type cabsloc = {
 lineno : int;
 filename: string;
 byteno: int;
 ident : int;
}

type typeSpecifier = (* Merge all specifiers into one type *)
    Tvoid                             (* Type specifier ISO 6.7.2 *)
  | Tchar
  | Tshort
  | Tint
  | Tlong
  | Tint64
  | Tfloat
  | Tdouble
  | Tsigned
  | Tunsigned
  | Tnamed of string
	  (* each of the following three kinds of specifiers contains a field 
	   * or item list iff it corresponds to a definition (as opposed to
	   * a forward declaration or simple reference to the type); they
	   * also have a list of __attribute__s that appeared between the
	   * keyword and the type name (definitions only) *)
  | Tstruct of string * field_group list option * attribute list
  | Tunion of string * field_group list option * attribute list
  | Tenum of string * enum_item list option * attribute list
  | TtypeofE of expression                      (* GCC __typeof__ *)
  | TtypeofT of specifier * decl_type       (* GCC __typeof__ *)

and storage =
    NO_STORAGE | AUTO | STATIC | EXTERN | REGISTER

and funspec = 
    INLINE | VIRTUAL | EXPLICIT

and cvspec =
    CV_CONST | CV_VOLATILE | CV_RESTRICT

(* Type specifier elements. These appear at the start of a declaration *)
(* Everywhere they appear in this file, they appear as a 'spec_elem list', *)
(* which is not interpreted by cabs -- rather, this "word soup" is passed *)
(* on to the compiler.  Thus, we can represent e.g. 'int long float x' even *)
(* though the compiler will of course choke. *)
and spec_elem =
    SpecTypedef          
  | SpecCV of cvspec            (* const/volatile *)
  | SpecAttr of attribute       (* __attribute__ *)
  | SpecStorage of storage
  | SpecInline
  | SpecType of typeSpecifier
  | SpecPattern of string       (* specifier pattern variable *)

(* decided to go ahead and replace 'spec_elem list' with specifier *)
and specifier = spec_elem list


(* Declarator type. They modify the base type given in the specifier. Keep
 * them in the order as they are printed (this means that the top level
 * constructor for ARRAY and PTR is the inner-level in the meaning of the
 * declared type) *)
and decl_type =
  | JUSTBASE                               (* Prints the declared name *)
  | PARENTYPE of attribute list * decl_type * attribute list
      (* Prints "(attrs1 decl attrs2)".
       * attrs2 are attributes of the
       * declared identifier and it is as
       * if they appeared at the very end
       * of the declarator. attrs1 can
       * contain attributes for the
       * identifier or attributes for the
       * enclosing type.  *)
  | ARRAY of decl_type * attribute list * expression
      (* Prints "decl [ attrs exp ]".
       * decl is never a PTR. *)
  | PTR of attribute list * decl_type      (* Prints "* attrs decl" *)
  | PROTO of decl_type * single_name list * bool 
      (* Prints "decl (args[, ...])".
       * decl is never a PTR.*)

(* The base type and the storage are common to all names. Each name might
 * contain type or storage modifiers *)
(* e.g.: int x, y; *)
and name_group = specifier * name list

(* The optional expression is the bitfield *)
and field_group = specifier * (name * expression option) list

(* like name_group, except the declared variables are allowed to have initializers *)
(* e.g.: int x=1, y=2; *)
and init_name_group = specifier * init_name list

(* The decl_type is in the order in which they are printed. Only the name of
 * the declared identifier is pulled out. The attributes are those that are
 * printed after the declarator *)
(* e.g: in "int *x", "*x" is the declarator; "x" will be pulled out as *)
(* the string, and decl_type will be PTR([], JUSTBASE) *)
and name = string * decl_type * attribute list * cabsloc

(* A variable declarator ("name") with an initializer *)
and init_name = name * init_expression

(* Single names are for declarations that cannot come in groups, like
 * function parameters and functions *)
and single_name = specifier * name


and enum_item = string * expression * cabsloc

(*
 ** Declaration definition (at toplevel)
 *)
and definition =
	FUNDEF of single_name * block * cabsloc * cabsloc
  | DECDEF of init_name_group * cabsloc        (* global variable(s), or function prototype *)
  | TYPEDEF of name_group * cabsloc
  | ONLYTYPEDEF of specifier * cabsloc
  | GLOBASM of string * cabsloc
  | PRAGMA of expression * cabsloc
  | LINKAGE of string * cabsloc * definition list (* extern "C" { ... } *)


and file = string * definition list

(*
 ** statements
 *)

(* A block contains a list of local label declarations ( GCC's ({ __label__ 
 * l1, l2; ... }) ) , a list of definitions and a list of statements  *)
and block = 
    { blabels: string list;
      battrs: attribute list;
      bstmts: statement list
    } 

(* GCC asm directives have lots of extra information to guide the optimizer *)
and asm_details =
    { aoutputs: (string option * string * expression) list; (* optional name, constraints and expressions for outputs *)
      ainputs: (string option * string * expression) list; (* optional name, constraints and expressions for inputs *)
      aclobbers: string list (* clobbered registers *)
    }

and statement =
	NOP of cabsloc
  | COMPUTATION of expression * cabsloc
  | BLOCK of block * cabsloc
  | SEQUENCE of statement * statement * cabsloc
  | IF of expression * statement * statement * cabsloc
  | WHILE of expression * statement * cabsloc
  | DOWHILE of expression * statement * cabsloc
  | FOR of for_clause * expression * expression * statement * cabsloc
  | BREAK of cabsloc
  | CONTINUE of cabsloc
  | RETURN of expression * cabsloc
  | SWITCH of expression * statement * cabsloc
  | CASE of expression * statement * cabsloc
  | CASERANGE of expression * expression * statement * cabsloc
  | DEFAULT of statement * cabsloc
  | LABEL of string * statement * cabsloc
  | GOTO of string * cabsloc
  | COMPGOTO of expression * cabsloc (* GCC's "goto *exp" *)
  | DEFINITION of definition (*definition or declaration of a variable or type*)

  | ASM of attribute list * (* typically only volatile and const *)
      string list * (* template *)
      asm_details option * (* extra details to guide GCC's optimizer *)
      cabsloc

  (** MS SEH *)
  | TRY_EXCEPT of block * expression * block * cabsloc
  | TRY_FINALLY of block * block * cabsloc
	  
and for_clause = 
	FC_EXP of expression
  | FC_DECL of definition

(*
 ** Expressions
 *)
and binary_operator =
    ADD | SUB | MUL | DIV | MOD
  | AND | OR
  | BAND | BOR | XOR | SHL | SHR
  | EQ | NE | LT | GT | LE | GE
  | ASSIGN
  | ADD_ASSIGN | SUB_ASSIGN | MUL_ASSIGN | DIV_ASSIGN | MOD_ASSIGN
  | BAND_ASSIGN | BOR_ASSIGN | XOR_ASSIGN | SHL_ASSIGN | SHR_ASSIGN

and unary_operator =
    MINUS | PLUS | NOT | BNOT | MEMOF | ADDROF
  | PREINCR | PREDECR | POSINCR | POSDECR

and expression =
    NOTHING
  | UNARY of unary_operator * expression
  | LABELADDR of string  (* GCC's && Label *)
  | BINARY of binary_operator * expression * expression
  | QUESTION of expression * expression * expression

  (* A CAST can actually be a constructor expression *)
  | CAST of (specifier * decl_type) * init_expression

  (* There is a special form of CALL in which the function called is
     __builtin_va_arg and the second argument is sizeof(T). This 
     should be printed as just T *)
  | CALL of expression * expression list
  | COMMA of expression list
  | CONSTANT of constant
  | PAREN of expression
  | VARIABLE of string
  | EXPR_SIZEOF of expression
  | TYPE_SIZEOF of specifier * decl_type
  | EXPR_ALIGNOF of expression
  | TYPE_ALIGNOF of specifier * decl_type
  | INDEX of expression * expression
  | MEMBEROF of expression * string
  | MEMBEROFPTR of expression * string
  | GNU_BODY of block
  | EXPR_PATTERN of string     (* pattern variable, and name *)

and constant =
  | CONST_INT of string   (* the textual representation *)
  | CONST_FLOAT of string (* the textual representaton *)
  | CONST_CHAR of int64 list
  | CONST_WCHAR of int64 list
  | CONST_STRING of string
  | CONST_WSTRING of int64 list 
      (* ww: wstrings are stored as an int64 list at this point because
       * we might need to feed the wide characters piece-wise into an 
       * array initializer (e.g., wchar_t foo[] = L"E\xabcd";). If that
       * doesn't happen we will convert it to an (escaped) string before
       * passing it to Cil. *) 

and init_expression =
  | NO_INIT
  | SINGLE_INIT of expression
  | COMPOUND_INIT of (initwhat * init_expression) list

and initwhat =
    NEXT_INIT
  | INFIELD_INIT of string * initwhat
  | ATINDEX_INIT of expression * initwhat
  | ATINDEXRANGE_INIT of expression * expression
	  

(* Each attribute has a name and some
 * optional arguments *)
and attribute = string * expression list

and ('a, 'b) part = REAL of 'a | PART of 'b | EMPTY

and partial_init_name = namep * init_expressionp
and partial_init_name_group = specifier * partial_init_name list
and partial_field_group = specifier * (namep * expp option) list

(* enum_itemp doesn't follow the usual part type, but the usual is kind of 
 * unecessary *)
and enum_itemp = string * expp * cabsloc

and name_groupp = (name_group, partial_name_group) part

and namep = string * decl_typep * attributep list * cabsloc
and decl_typep = (decl_type, partial_decl_type) part

and single_namep = (single_name, partial_single_name) part
and field_groupp = (field_group, partial_field_group) part
and init_namep = (init_name, partial_init_name) part
and init_name_groupp = (init_name_group, partial_init_name_group) part
and attributep = (attribute, partial_attribute) part
and stmtp = (statement, partial_statement) part 
and expp = (expression, partial_expression) part 
and forp = (for_clause, partial_fc) part
and defp = (definition, partial_definition) part
and initwhatp = (initwhat, partial_initwhat) part
and init_expressionp = (init_expression, partial_init_expression) part
and blockp = (block, partial_block) part
and asm_detailsp = (asm_details, partial_asm_details) part

and type_name = spec_elem list * decl_type

and partial_type_name = spec_elem list * decl_typep
and typep = (type_name, partial_type_name) part

(* FIXME: partial_type_name doesn't exist net *)
and partial_attribute = string * expp list

and partial_fc = PART_FC_EXP of partial_expression
				 | PART_FC_DECL of partial_definition

and partial_single_name = specifier * namep
and partial_name_group = specifier * namep list

and partial_asm_details =
    { aoutputsp: (string option * string * expp) list; (* optional name, constraints and expressions for outputs *)
      ainputsp: (string option * string * expp) list; (* optional name, constraints and expressions for inputs *)
      aclobbersp: string list (* clobbered registers *)
    }

and partial_decl_type =
  | PJUSTBASE
  | PPARENTYPE of attributep list * decl_typep * attributep list
  | PARRAY of decl_typep * attributep list * expp list 
  | PPTR of attributep list * decl_typep 
  | PPROTO of decl_typep * single_namep list * bool 

and partial_type_specifier = 
  | PTstruct of string * field_groupp list * attributep list
  | PTunion of string * field_groupp list * attributep list
  | PTenum of string * enum_itemp list * attributep list
  | PTtypeofE of expp                      (* GCC __typeof__ *)
  | PTtypeofT of specifier * decl_typep       (* GCC __typeof__ *)												 

and partial_init_expression = 
  | PNO_INIT
  | PSINGLE_INIT of partial_expression
  | PCOMPOUND_INIT of (initwhatp * init_expressionp) list

and partial_initwhat =
    PNEXT_INIT
  | PINFIELD_INIT of string * initwhatp
  | PATINDEX_INIT of expp * initwhatp
  | PATINDEXRANGE_INIT of expp * expp

and partial_block = 
	{ pblabels : string list ;
      pbattrs : attributep list ;
      pbstmts : stmtp list }

and partial_statement = 
  | PARTCOMPUTATION of expp list * cabsloc
  | PARTBLOCK of partial_block * cabsloc * cabsloc
  | PARTDEFINITION of partial_definition
  | PARTIF of expp list * stmtp * stmtp * cabsloc
  | PARTSWITCH of expp list * stmtp * cabsloc
  | PARTWHILE of expp list * stmtp * cabsloc
  | PARTDOWHILE of stmtp * expp list * cabsloc
  | PARTDOWHILEASM
  | PARTCASE of expp * expp * stmtp * cabsloc
  | PARTDEFAULT of cabsloc
  | PARTBREAK of cabsloc
  | PARTCONTINUE of cabsloc
  | PARTRETURN of expp list * cabsloc
  | PARTFOR of forp * expp * expp * stmtp
  | PARTLABEL of string * stmtp
  | PARTGOTO of string * cabsloc
  | PARTCOMPGOTO of expp list * cabsloc
  | PARTASM of attributep list * string list * asm_details option * cabsloc
  | PARTTRYEXCEPT of blockp * expp list * blockp * cabsloc
  | PARTTRYFINALLY of blockp * blockp * cabsloc
	  
and partial_expression =
  | PARTNOTHING
  | PARTUNARY of unary_operator * expp
  | PARTLABELADDR of string  (* GCC's && Label *)
  | PARTBINARY of binary_operator * expp * expp
  | PARTQUESTION of expp * expp * expp

  (* A CAST can actually be a constructor expression *)
  | PARTCAST of (specifier * decl_typep) option * init_expressionp

  (* There is a special form of CALL in which the function called is
     __builtin_va_arg and the second argument is sizeof(T). This 
     should be printed as just T *)
  | PARTCALL of expp * expp list
  | PARTCOMMA of expp list
  | PARTCONSTANT of constant
  | PARTPAREN of expp list 
  | PARTVARIABLE of string
  | PARTEXPR_SIZEOF of expp
  | PARTTYPE_SIZEOF of specifier * decl_typep
  | PARTEXPR_ALIGNOF of expp
  | PARTTYPE_ALIGNOF of specifier * decl_typep
  | PARTINDEX of expp * expp list
  | PARTMEMBEROF of expp * string 
  | PARTMEMBEROFPTR of expp * string
  | PARTGNU_BODY of blockp
  | PARTEXPR_PATTERN of string option

and partial_definition = 
	PARTFUNDEF of single_namep * blockp * cabsloc * cabsloc
  | PARTDECDEF of init_name_groupp * cabsloc        (* global variable(s), or function prototype *)
  | PARTTYPEDEF of name_groupp * cabsloc
  | PARTONLYTYPEDEF of specifier * cabsloc
  | PARTGLOBASM of string * cabsloc
  | PARTPRAGMA of expp * cabsloc
  | PARTLINKAGE of string * cabsloc * defp list (* extern "C" { ... } *)

and tree_node = 
  | Global of definition
  | Stmt of statement
  | Exp of expression
  | PartialStmt of partial_statement
  | PartialExp of partial_expression 
  | PartialGlobal of partial_definition
  | Syntax of string

and tree = string * tree_node list

let cabslu = {lineno = -10; 
			  filename = "cabs loc unknown"; 
			  byteno = -10;
              ident = 0}

let dummyPartialFunction : namep = ("<PARTIAL FUNCTION WITHOUT A PROTO>", PART(PPROTO(PART(PJUSTBASE), [], false)), [], cabslu)
