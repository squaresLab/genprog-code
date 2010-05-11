open Globals
open File_process

let main () = begin

  let handleArg str = parse_options_in_file str in
	Arg.parse (Arg.align !options) handleArg usageMsg ;

	(* compile list of files containing output of instrumented program runs *)

	let file_list = begin
	  let flist = ref [] in
		while true do
		  let line = input_line fin in
		  let split = Str.split whitespace_regexp line in 
			flist := ((hd split), (hd (tl split))) :: !flist
		done
	with _ -> close_in fin; !flist
	end in
	let graph = build_execution_graph file_list in
	  ()
end ;;

main () ;;
