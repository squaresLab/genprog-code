(* main.ml *)
(* driver for an Elkhound parser written in OCaml *)
open Pretty
open Cabsvisit
open Lexerint      (* tLexerInterface *)
open Lrparse       (* parse *)
open Glr           (* tGLR, makeGLR, glrParse *)
open Useract       (* tSemanticValue *)
open Parsetables   (* tParseTables *)
open Useract       (* tUserActions *)
open Cparser
open Clexer         (* token, readToken *)
open Cparse

let usageMsg = "A test driver for C and C-diff parsing"
let parse_type = ref "c"

let options =  [
  "-type", Arg.Set_string parse_type, "\t Diff or C parsing? Default: .c";
] 

let main () = 
  let files_to_parse = ref [] in
  let handleArg str = files_to_parse := str :: !files_to_parse in 
  let aligned = Arg.align options in 
	Arg.parse aligned handleArg usageMsg ; 
	List.iter 
	  (fun filename ->
		 Printf.printf "Parsing %s:\n\n" filename ;
		 let ast = Cparse.parse_file filename in
		   Printf.printf "Ast parsed; printing tree:\n"; flush stdout;
		   Cprint.printFile 
			 Pervasives.stdout 
			 (filename,ast);
		   Printf.printf "Done printing file!\n"; flush stdout)
	  !files_to_parse;
    Printf.printf "Done!"; flush stdout
;;

main () ;;
