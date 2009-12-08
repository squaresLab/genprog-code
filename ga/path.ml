open Printf
open Str
open Cil

let comma_regexp = regexp_string ","
let whitespace_regexp= regexp "[ \t\n]+"

let ht_file = ref ""
let goodpath_file = ref ""
let badpath_file = ref ""
let modify_input = ref ""
let good_path_factor = ref 0.01

let uniq lst = (* return a copy of 'lst' where each element occurs once *) 
  let ht = Hashtbl.create 255 in 
  let lst = List.filter (fun elt ->
    if Hashtbl.mem ht elt then false
    else begin
      Hashtbl.add ht elt () ;
      true 
    end 
  ) lst in
  lst 

let build_importance_table cbi_file loc_ht : ('a, float) Hashtbl.t = 
  let retval_ht : ('a, float) Hashtbl.t = Hashtbl.create 10 in
  let fin = open_in cbi_file in
  let loc_to_imp = Hashtbl.create 10 in
  let _ =
    try 
      while true do
		let (filename::lineno::importance::rest) = 
		  (split comma_regexp (input_line fin)) in
		let lineno = int_of_string lineno in
		let importance = float_of_string importance in
		let loc = {file = filename; line = lineno; byte = 0} in
		  Printf.printf "File name is: %s\n"; flush stdout;
		  if Hashtbl.mem loc_to_imp loc then begin
			if Hashtbl.find loc_to_imp loc < importance then
			  Hashtbl.replace loc_to_imp loc importance
		  end else
			Hashtbl.add loc_to_imp loc importance
      done
    with _ -> ()
  in
    Hashtbl.iter 
      (fun stmt ->
		 fun loc1 ->
		   let loc2 = {file=loc1.file; line=loc1.line+1;byte=loc1.byte} in
		   let loc3 = {file=loc1.file; line=loc1.line-1;byte=loc1.byte} in
		   let (imp1, imp2, imp3) =
			 let one = 
	       try 
		 Hashtbl.find loc_to_imp loc1 
	       with Not_found -> 0.0 in
	     let two = 
	       try 
		 Hashtbl.find loc_to_imp loc2
	       with Not_found -> 0.0 in
	     let three = 
		 try 
		   Hashtbl.find loc_to_imp loc3
		 with Not_found -> 0.0  in
	       (one, two, three)
	   in
	   let add =
	     if imp1 > imp2 then
	       if imp1 > imp3 then imp1 else imp3
	     else if imp2 > imp3 then imp2 else imp3
	   in
	     if add > 0.0 then
	       Hashtbl.add retval_ht stmt add
      ) loc_ht;
    retval_ht

let my_int_of_string str =
  try 
    let res = ref 0 in 
    Scanf.sscanf str " %i" (fun i -> res := i) ;
    !res
  with _ -> begin 
    if String.lowercase str = "true" then 1
    else if String.lowercase str = "false" then 0 
    else failwith ("cannot convert to an integer: " ^ str)
  end 

let main () = begin
  let usageMsg = "Path analyzer\n" in
    
  let argDescr = [
    "-ht", Arg.Set_string ht_file, " file with location hashtable information";
    "-gp", Arg.Set_string goodpath_file, "file with good path";
    "-bp", Arg.Set_string badpath_file, "file with bad path";
    "-mi", Arg.Set_string modify_input, "file with cbi info for modify";
  ] in
  let handleArg str = () in
    Arg.parse (Arg.align argDescr) handleArg usageMsg ;
    let ht_fin = open_in_bin !ht_file in
    let loc_ht = Marshal.from_channel ht_fin in
      close_in ht_fin;
      let imp_ht = 
	  if not (!modify_input = "") then begin
		build_importance_table !modify_input loc_ht 
	  end else Hashtbl.create 10 in
      let gpath_ht = Hashtbl.create 255 in
      let gpath = ref [] in
	(try
	   let gpath_fin = open_in !goodpath_file in
	     while true do
	       let line = input_line gpath_fin in
	       let i = my_int_of_string line in
	       let (cbi_prob, not_cbi_prob) = 
		 let one = 
		   if Hashtbl.mem imp_ht i then
		     Hashtbl.find imp_ht i
		   else 
		     !good_path_factor
		 in
		 let two = 
		   if Hashtbl.mem gpath_ht i then
		     !good_path_factor
		   else
		     1.0
		 in 
		   (one, two)
	       in 
	       let num = (my_int_of_string line) in
	       let loc = Hashtbl.find loc_ht num in
		 gpath := (loc, num, not_cbi_prob, cbi_prob) :: !gpath;
		 Hashtbl.add gpath_ht i ()
	     done ;
	 with _ -> ()
	) ;
	
	let path_fin = open_in !badpath_file in 
	let path = ref [] in
	  (try
	     while true do
	       let line = input_line path_fin in
	       let i = my_int_of_string line in
	       let (cbi_prob, not_cbi_prob) = 
		 let one = 
		   if Hashtbl.mem imp_ht i then
		     Hashtbl.find imp_ht i
		   else 
		     if Hashtbl.mem gpath_ht i
		     then !good_path_factor
		     else 0.3 
		 in
		 let two = 
		   if Hashtbl.mem gpath_ht i then
		     !good_path_factor
		   else
		     1.0
		 in 
		   (one, two)
	       in 
	       let num = (my_int_of_string line) in
	       let loc = Hashtbl.find loc_ht num in
		 path := (loc, num, not_cbi_prob, cbi_prob) :: !path 
	     done 
	   with _ -> close_in path_fin) ; 
	  
	  let path = uniq( List.rev !path) in 
	  let path = 
	    List.sort
	      (fun (l1,n1,ncp1,cp1) ->
		 (fun (l2,n2,ncp2,cp2) ->
		    n1 - n2)) path in
	    Printf.printf "Stmt_num,file,line,not_cbi_prob,cbi_prob\n";
	    List.iter
	      (fun (l,n,ncp,cp) ->
		 Printf.printf "%d,%s,%d,%g,%g\n"
		   n l.file l.line ncp cp)
	      path;
	    flush stdout
end ;;

main () ;;
