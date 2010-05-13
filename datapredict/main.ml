open List
open Globals
open State
open Graph

let cbi_hash_tables = ref ""
let runs_in = ref ""

let usageMsg = "Giant Predicate Processing Program of Doom\n"
let options = ref [
  "-cbi-hin", Arg.Set_string cbi_hash_tables, 
  "\t File containing serialized hash tables from my implementation \
                of CBI." ;
  "-rs", Arg.Set_string runs_in,
  "\t File listing names of files containing runs, followed by a passed \
                or failed on the same line to delineate runs." ;
] 

(* Utility function to read 'command-line arguments' from a file. 
 * This allows us to avoid the old 'ldflags' file hackery, etc. *) 
let parse_options_in_file (file : string) : unit =
  let args = ref [ Sys.argv.(0) ] in 
  try
    let fin = open_in file in 
    (try while true do
      let line = input_line fin in
      let words = Str.bounded_split space_regexp line 2 in 
      args := !args @ words 
    done with _ -> close_in fin) ;
    Arg.current := 0 ; 
    Arg.parse_argv (Array.of_list !args) 
      (Arg.align !options) 
      (fun str -> debug "%s: unknown option %s\n"  file str) usageMsg 
  with _ -> () 

module DynamicExecGraph = ExecutionGraph(DynamicState)

let main () = begin

  let handleArg str = parse_options_in_file str in
	Arg.parse (Arg.align !options) handleArg usageMsg ;

	(* compile list of files containing output of instrumented program runs *)

	let fin = open_in !runs_in in
	let file_list = ref [] in
	  begin
		try
		  while true do
			let line = input_line fin in
			let split = Str.split whitespace_regexp line in 
			  file_list := ((hd split), (hd (tl split))) :: !file_list
		  done
		with _ -> close_in fin
	  end;

	let graph = DynamicExecGraph.build_execution_graph !file_list in
	  ()
end ;;

main () ;;
