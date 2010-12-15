open Batteries
open Utils
open Globals
open Diffparse
(* TODO: add alpha-renaming to diffs *)

module Treediff =
struct
  (* process_diff takes the syntactic diff returned by svn diff and
   * splits it into old_file and new file, parses them, and them diffs
   * them to produce tree-based representations of the changes suitable
   * for comparison to other diffs *)

  exception ParseError of string
		
  let parse_to_tree stringbuff =
(*	try*)
	  Diffparse.parse_from_string stringbuff
(*      Errormsg.hadErrors := false;
      let lexbuf = Clexer.init stringbuff in
      let cabs = Diffparser.interpret (Whitetrack.wraplexer clexer) lexbuf in
		Whitetrack.setFinalWhite (Clexer.get_white ());
		Clexer.finish (); cabs
	with (Sys_error msg) -> begin
      ignore (pprintf "Cannot open stringbuff : %s\n" msg);
      Clexer.finish ();
      raise (ParseError("Cannot open stringbuff: " ^ msg ^ "\n"))
	end
	| Parsing.Parse_error -> begin
		ignore (pprintf "Parsing error\n");
		Clexer.finish ();
		raise (ParseError("Parse error"))
	  end
	with e -> begin
		ignore (pprintf "Caught %s while parsing\n" (Printexc.to_string e));
		Clexer.finish ();
		raise e
	  end*)

  let tree_diff tree1 tree2 = failwith "Not implemented" 

  let process_diff (syntactic : string list) =
	let old_file_str,new_file_str = 
	  lfoldl
		(fun (oldf,newf) ->
		   fun str ->
			 if Str.string_match at_regexp str 0 then oldf,newf
			 else if Str.string_match plus_regexp str 0 then oldf,(String.lchop str)^"\n"^newf
			 else if Str.string_match minus_regexp str 0 then (String.lchop str)^"\n"^oldf,newf
			 else (str^"\n"^oldf),(str^"\n"^newf)
		) ("","") syntactic 
	in 
	let old_file_tree,new_file_tree = (* will the diff files be backwards?  Double-check! *)
	  parse_to_tree old_file_str,
	  parse_to_tree new_file_str
	in
	  tree_diff old_file_tree new_file_tree

end
