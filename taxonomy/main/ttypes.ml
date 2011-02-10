open Batteries
open Utils
open Cabs


type 'a lifted = STAR | MAYBE of 'a list | ATLEAST of 'a list | LNOTHING | EXACT of 'a

type ops = Modify_value | Arithmetic | Bitwise | Logic | OnNumbers | OnBits | 
	Bop_op of bop_gen | Uop_op of uop_gen | Lifted_ops of ops lifted

and bop_gen =  Modify_assign | BitTruth | NotBitTruth | Shift
				| Bgen of ops | Bop of binary_operator | Bop_gen of bop_gen lifted

and uop_gen = Sizeof | Sign_modifier | Memory_operator | Not_operator | Alignof
			   | Pre_operator | Post_operator | Increment | Decrement | Uop of unary_operator   
			   | Ugen of ops | Uop_gen of uop_gen lifted

and exp_gen = EXPBASE of expression node
			   | LIFTED of exp_gen lifted
			   | UNARYOP of uop_gen * exp_gen
			   | BINOP of bop_gen * exp_gen * exp_gen
			   | QUESTOP of exp_gen * exp_gen * exp_gen
			   | CASTOP of (spec_gen * dt_gen) * ie_gen 
			   | CALLOP of exp_gen * exp_gen list
			   | COMMAOP of exp_gen list
			   | PARENOP of exp_gen
			   | EXPSIZOFOP of exp_gen
			   | TYPESIZOFOP of spec_gen * dt_gen
			   | EXPALIGNOFOP of exp_gen 
			   | TYPEALIGNOFOP of spec_gen * dt_gen
			   | INDEXOP of exp_gen * exp_gen
			   | MEMBEROFOP of exp_gen * string 
			   | MEMBEROFPTROP of exp_gen * string
			   | ADDROFEXP of exp_gen
			   | OPERATION of ops  * exp_gen
			   | SOMEMEMBER of exp_gen * string 
and spec_gen = unit
and dt_gen = unit
and ie_gen = unit
