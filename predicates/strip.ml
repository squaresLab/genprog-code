open Printf
open Cil
open List
open Str

let claire_str = regexp_string "claire"

class stripVisitor = object 
  inherit nopCilVisitor

  method vstmt s = 
	ChangeDoChildrenPost
	  (s,
	   fun s ->
		 let remove = 
		   (not (s.labels = [])) &&
			 (List.fold_left
				(fun accum ->
				   fun lab ->
					 match lab with
						 Label(lab,_,_) -> string_match claire_str lab 0
					   | _ -> accum) false s.labels)
		 in
		   if remove then begin
			 (mkEmptyStmt())
		   end else s
	  )
end

let strip_v = new stripVisitor

let main () = begin
  let argDescr = [] in
  let files = ref [] in
  let handleArg a = files := a :: !files in
	Arg.parse argDescr handleArg "" ;

	Cil.initCIL() ;

	let cil_files = 
	  List.map
		(fun filename ->
		   let file = Frontc.parse filename() in 
			 Partial.calls_end_basic_blocks file;
			 Cfg.computeFileCFG file; file) !files in
	  List.iter
		(fun file ->
		   begin
			 visitCilFileSameGlobals strip_v file;
			 iterGlobals file
			   (fun glob -> dumpGlobal defaultCilPrinter stdout glob);
		   end
		) cil_files
end ;;

main () ;;
