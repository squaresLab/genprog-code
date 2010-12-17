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
open Cprint

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
		 match (String.lowercase !parse_type) with
		 | "c" -> let ast = Cparse.parse_file filename in 
			 Printf.printf "Done parsing; about to print\n"; flush stdout;
			 dumpFile defaultCabsPrinter stdout (filename,ast)
		 | "diff" -> let ast,count = Diffparse.parse_file filename in
			 Printf.printf "Done parsing; about to print\n"; flush stdout;
			 dumpTree defaultCabsPrinter stdout (filename,ast)
		 | _ -> 
			 let s = Printf.sprintf "Unrecognized file type to parse: %s\n" (String.lowercase !parse_type) in
			   failwith s) !files_to_parse;
    Printf.printf "Done!"; flush stdout
;;

main () ;;
