(* main.ml *)
(* driver for an Elkhound parser written in OCaml *)
open Pretty
open Cabs
open Diffabs
(*open Mycabsvisit*)
open Lexerint      (* tLexerInterface *)
open Lrparse       (* parse *)
open Glr           (* tGLR, makeGLR, glrParse *)
open Useract       (* tSemanticValue *)
open Parsetables   (* tParseTables *)
open Useract       (* tUserActions *)
open Mycparser
open Difflexer         (* token, readToken *)
open Mycparse
open Diffprint

let usageMsg = "A test driver for C and C-diff parsing"
let parse_type = ref "c"
let long = ref false

let options =  [
  "-type", Arg.Set_string parse_type, "\t Diff or C parsing? Default: .c";
  "-long", Arg.Set long, "\t a long file as printed by diffs"
] 

let main () = 
  let files_to_parse = ref [] in
  let handleArg str = files_to_parse := str :: !files_to_parse in 
  let aligned = Arg.align options in 
	Arg.parse aligned handleArg usageMsg ; 
	List.iter 
	  (fun filename ->
		 match (String.lowercase !parse_type) with
		 | "c" -> let ast = Mycparse.parse_file filename in 
					diff_lexing := false;
			 Printf.printf "Done parsing; about to print\n";
			 Diffprint.dumpFile defaultCabsPrinter stdout (filename,ast)
		 | "diff" -> 
		   Printf.printf "one\n"; Pervasives.flush stdout;
			 if not !long then begin
			   Printf.printf "Parsing: %s\n" filename; Pervasives.flush stdout;
			   let ast,count = Diffparse.parse_file filename in
				 Printf.printf "Done parsing; about to print\n";Pervasives.flush stdout;
				 dumpTree defaultCabsPrinter stdout (filename,ast);
				 Printf.printf "before conversion\n"; Pervasives.flush stdout;
				 let ascil = Diff2cil.convTree (filename,ast) in
				   Printf.printf "After conversion, printing again\n"; Pervasives.flush stdout;
				   Cil.iterGlobals ascil (fun glob ->
					 Cil.dumpGlobal Cil.defaultCilPrinter stdout glob ;
				   ) ; 

			 end else begin
			   let num_succeed,num_fail = ref 0, ref 0 in
			   let reg = Str.regexp_string "SEPSEPSEPSEP" in
			   let fin = open_in filename in 
				 try
				   let current_snippet = ref "" in
					 while true do
					   let str = input_line fin in 
						 if Str.string_match reg str 0 then begin
						   Printf.printf "Current snippet: %s\n" !current_snippet; flush stdout;
						   (try
							  let ast,count = Diffparse.parse_from_string !current_snippet in 
								Printf.printf "Done parsing; about to print\n"; flush stdout;
								dumpTree defaultCabsPrinter stdout (filename,ast);
								incr num_succeed; Printf.printf "Succeeded; %d successes so far\n" !num_succeed; flush stdout
							with _ -> 
							  begin 
								incr num_fail; Printf.printf "Failed to parse. %d failures so far\n" !num_fail; flush stdout
							  end);
						   Printf.printf "Resetting current snippet\n"; flush stdout;
						   current_snippet := ""
						 end else
						   current_snippet := !current_snippet ^ "\n" ^ str
					 done
				 with End_of_file -> ()
			 end
		 | _ -> 
			 let s = Printf.sprintf "Unrecognized file type to parse: %s\n" (String.lowercase !parse_type) in
			   failwith s) !files_to_parse;
    Printf.printf "Done!\n\n"; flush stdout
;;

main () ;;
