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

(* AST nodes can have unique ids, except for strings/similar
   constants, which are terminals. *)

type 'a node = {
  mutable node : 'a;
  id : int; 
} 

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
  | Tstruct of string * field_group node list option * attribute node list
  | Tunion of string * field_group node list option * attribute node list
  | Tenum of string * enum_item node list option * attribute node list
  | TtypeofE of expression node                     (* GCC __typeof__ *)
  | TtypeofT of specifier node * decl_type node       (* GCC __typeof__ *)

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
  | SpecCV of cvspec node       (* const/volatile *)
  | SpecAttr of attribute node     (* __attribute__ *)
  | SpecStorage of storage
  | SpecInline
  | SpecType of typeSpecifier node
  | SpecPattern of string       (* specifier pattern variable *)

(* decided to go ahead and replace 'spec_elem list' with specifier *)
and specifier = spec_elem node list

(* Declarator type. They modify the base type given in the specifier. Keep
 * them in the order as they are printed (this means that the top level
 * constructor for ARRAY and PTR is the inner-level in the meaning of the
 * declared type) *)
and decl_type =
  | JUSTBASE                               (* Prints the declared name *)
  | PARENTYPE of attribute node list * decl_type node * attribute node list
      (* Prints "(attrs1 decl attrs2)".
       * attrs2 are attributes of the
       * declared identifier and it is as
       * if they appeared at the very end
       * of the declarator. attrs1 can
       * contain attributes for the
       * identifier or attributes for the
       * enclosing type.  *)
  | ARRAY of decl_type node * attribute node list * expression node
      (* Prints "decl [ attrs exp ]".
       * decl is never a PTR. *)
  | PTR of attribute node list * decl_type node      (* Prints "* attrs decl" *)
  | PROTO of decl_type node * single_name node list * bool 
      (* Prints "decl (args[, ...])".
       * decl is never a PTR.*)

(* The base type and the storage are common to all names. Each name might
 * contain type or storage modifiers *)
(* e.g.: int x, y; *)
and name_group = specifier node * name node list

(* The optional expression is the bitfield *)
and field_group = specifier node * (name node * expression node option) list

(* like name_group, except the declared variables are allowed to have initializers *)
(* e.g.: int x=1, y=2; *)
and init_name_group = specifier node * init_name node list

(* The decl_type is in the order in which they are printed. Only the name of
 * the declared identifier is pulled out. The attributes are those that are
 * printed after the declarator *)
(* e.g: in "int *x", "*x" is the declarator; "x" will be pulled out as *)
(* the string, and decl_type will be PTR([], JUSTBASE) *)
and name = string * decl_type node * attribute node list * cabsloc

(* A variable declarator ("name") with an initializer *)
and init_name = name node * init_expression node

(* Single names are for declarations that cannot come in groups, like
 * function parameters and functions *)
and single_name = specifier node * name node


and enum_item = string * expression node * cabsloc

(*
 ** Declaration definition (at toplevel)
 *)
and definition =
	FUNDEF of single_name node * block node * cabsloc * cabsloc
  | DECDEF of init_name_group node * cabsloc        (* global variable(s), or function prototype *)
  | TYPEDEF of name_group node * cabsloc
  | ONLYTYPEDEF of specifier node * cabsloc
  | GLOBASM of string * cabsloc
  | PRAGMA of expression node * cabsloc
  | LINKAGE of string * cabsloc * definition node list (* extern "C" { ... } *)


and file = string * definition node list

(*
 ** statements
 *)

(* A block contains a list of local label declarations ( GCC's ({ __label__ 
 * l1, l2; ... }) ) , a list of definitions and a list of statements  *)
and block = 
    { blabels: string list;
      battrs: attribute node list;
      bstmts: statement node list
    } 

(* GCC asm directives have lots of extra information to guide the optimizer *)
and asm_details =
    { aoutputs: (string option * string * expression node) list; (* optional name, constraints and expressions for outputs *)
      ainputs: (string option * string * expression node) list; (* optional name, constraints and expressions for inputs *)
      aclobbers: string list (* clobbered registers *)
    }

and statement =
	NOP of cabsloc
  | COMPUTATION of expression node * cabsloc
  | BLOCK of block node * cabsloc
  | SEQUENCE of statement node * statement node * cabsloc
  | IF of expression node * statement node * statement node * cabsloc
  | WHILE of expression node * statement node * cabsloc
  | DOWHILE of expression node * statement node * cabsloc
  | FOR of for_clause node * expression node * expression node * statement node * cabsloc
  | BREAK of cabsloc
  | CONTINUE of cabsloc
  | RETURN of expression node * cabsloc
  | SWITCH of expression node * statement node * cabsloc
  | CASE of expression node * statement node * cabsloc
  | CASERANGE of expression node * expression node * statement node * cabsloc
  | DEFAULT of statement node * cabsloc
  | LABEL of string * statement node * cabsloc
  | GOTO of string * cabsloc
  | COMPGOTO of expression node * cabsloc (* GCC's "goto *exp" *)
  | DEFINITION of definition node (*definition or declaration of a variable or type*)
  | ASM of attribute node list * (* typically only volatile and const *)
      string list * (* template *)
      asm_details option node * (* extra details to guide GCC's optimizer *)
      cabsloc

  (** MS SEH *)
  | TRY_EXCEPT of block node * expression node * block node * cabsloc
  | TRY_FINALLY of block node * block node * cabsloc
	  
and for_clause = 
	FC_EXP of expression node
  | FC_DECL of definition node

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
  | UNARY of unary_operator * expression node
  | LABELADDR of string  (* GCC's && Label *)
  | BINARY of binary_operator * expression node * expression node
  | QUESTION of expression node * expression node * expression node

  (* A CAST can actually be a constructor expression *)
  | CAST of (specifier node * decl_type node) * init_expression node

  (* There is a special form of CALL in which the function called is
     __builtin_va_arg and the second argument is sizeof(T). This 
     should be printed as just T *)
  | CALL of expression node * expression node list
  | COMMA of expression node list
  | CONSTANT of constant
  | PAREN of expression node
  | VARIABLE of string
  | EXPR_SIZEOF of expression node
  | TYPE_SIZEOF of specifier node * decl_type node
  | EXPR_ALIGNOF of expression node
  | TYPE_ALIGNOF of specifier node * decl_type node
  | INDEX of expression node * expression node
  | MEMBEROF of expression node * string
  | MEMBEROFPTR of expression node * string
  | GNU_BODY of block node
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
  | SINGLE_INIT of expression node
  | COMPOUND_INIT of (initwhat node * init_expression node) list

and initwhat =
    NEXT_INIT
  | INFIELD_INIT of string * initwhat node
  | ATINDEX_INIT of expression node * initwhat node
  | ATINDEXRANGE_INIT of expression node * expression node
	  

(* Each attribute has a name and some
 * optional arguments *)
and attribute = string * expression node list

and tree_node = 
  | Globals of definition node list
  | Stmts of statement node list
  | Exps of expression node list
  | Syntax of string

and tree = string * tree_node node list

let node_number = ref 0

let nd (node : 'a) = { node = node; id = (incr node_number; !node_number) }
let dn (node : 'a node) : 'a = node.node
