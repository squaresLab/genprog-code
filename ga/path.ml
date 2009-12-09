open Printf
open Str
open Cil

let good_path_factor = ref 0.01
let mod_input = ref 0

let comma_regexp = regexp_string ","
let whitespace_regexp= regexp "[ \t\n]+"

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

let compare_paths bpath_ht gpath_ht imp_ht loc_ht stmt_cov_ht print_info =
  let badpath = ref [] in
  let stmt_head = ref "" in
    Hashtbl.iter 
      (fun stmt ->
		 fun _ -> 
		   let (cov,cbi,fp,cp,ip1,ip2) =
			 let cov = 
			   if Hashtbl.mem gpath_ht stmt then
				 !good_path_factor
			   else 1.0 in
			 let cbi = 
			   if Hashtbl.mem imp_ht stmt then
				 Hashtbl.find imp_ht stmt
			   else 
				 if Hashtbl.mem gpath_ht stmt
				 then !good_path_factor
				 else 0.3 in
			 let fp,cp,ip1,ip2 = 
			   if Hashtbl.mem stmt_cov_ht stmt then
				 Hashtbl.find stmt_cov_ht stmt 
			   else (-1.0,-1.0,-1.0,-1.0) in
			   (cov,cbi,fp,cp,ip1,ip2) in
			 let loc = Hashtbl.find loc_ht stmt in 
			   badpath := (stmt,loc,cov,cbi,fp,cp,ip1,ip2) :: !badpath)
      bpath_ht;
    let badpath = uniq (List.rev !badpath) in 
    let badpath = 
      List.sort
		(fun (stmt1,l1,cov1,cbi1,fp1,cp1,ip11,ip21) ->
		   (fun (stmt2,l2,cov2,cbi2,fp2,cp2,ip12,ip22) ->
			  stmt1 - stmt2)) badpath in
      List.iter print_info badpath; flush stdout

let build_count_ht flist =
  let ht = Hashtbl.create 255 in
    List.iter 
      (fun file ->
	 let path = ref [] in
	 try
	   let fin = open_in file in
	     while true do
	       let line = input_line fin in
	       let i = my_int_of_string line in
		 path := i :: !path
	     done ;
	 with _ -> ();
	   let path = uniq (!path) in 
	     List.iter
	       (fun stmt ->
		  let count = 
		    try Hashtbl.find ht stmt with _ -> 0
		  in
		    Hashtbl.add ht stmt (count + 1)
	       ) path) flist ;
    ht

let calculate_importance gpath_ht bpath_ht num_g_runs num_b_runs = 
  let ht = Hashtbl.create 100 in
  Hashtbl.iter
    (fun stmt ->
       fun bcount ->
	 let gcount = try Hashtbl.find gpath_ht stmt with Not_found -> 0 in
	 let failure_p = float(bcount) /. (float(gcount) +. float(bcount)) in
	   (* CLG: note to sober self: how to deal with the fact that we can't
	    * actually calculate failure_p on these statements? It's never the case
	    * that we observe a statement and have the associated preciate
	    * (coverage) not be true 
	    * 
	    * Initial hack implemented below. 
	    *)
	 let context_p = num_b_runs /. (num_g_runs +. num_b_runs) in
	 let increase_p = failure_p -. context_p in 
	 let importance_p = 
	   2.0 /. ((1.0 /. increase_p) +. (num_b_runs /. float(bcount)))
	 in
	   Hashtbl.add ht stmt (failure_p,context_p,increase_p,importance_p)
    ) bpath_ht; ht

let main () = begin
  let usageMsg = "Path analyzer\n" in
  let ht_file = ref "" in
  let goodpath_files = ref [] in
  let badpath_files = ref [] in 
  let cbi_info = ref "" in 
  let calc_imp = ref false in
  let comp_imp = ref false in 
  let argDescr = [
    "-ht", Arg.Set_string ht_file, " file with location hashtable information";
    "-gp", Arg.String (fun s -> goodpath_files := s :: !goodpath_files), "file with good path";
    "-bp", Arg.String (fun s -> badpath_files := s :: !badpath_files), "file with bad path";
    "-mi", Arg.Set_string cbi_info, "file with cbi info";
    "-calc", Arg.Set calc_imp, " calculate importance of each statement";
	"-mod", Arg.Set_int mod_input, " output suitable to pass as a weighted path file to modify \\
                                     integer denotes which weight you want. 1 is coverage, 2 is
                                     cbi, 3 is importance.";
  ] in
(* note to self: consider default print out without calc; kind of stupid unless
 * cbi printout is also not default *)
  let handleArg str = () in
    Arg.parse (Arg.align argDescr) handleArg usageMsg ;
    let ht_fin = open_in_bin !ht_file in
    let loc_ht = Marshal.from_channel ht_fin in
      close_in ht_fin;
      let imp_ht = 
		if not (!cbi_info = "") then begin
		  build_importance_table !cbi_info loc_ht 
		end else Hashtbl.create 10 in
      let gpath_ht = build_count_ht !goodpath_files in 
      let bpath_ht = build_count_ht !badpath_files in
      let stmt_cov_ht = 
		if !calc_imp then
		  calculate_importance gpath_ht bpath_ht 
			(float(List.length !goodpath_files))
			(float(List.length !badpath_files))
		else Hashtbl.create 10 in 
	  let stmt_head = if !calc_imp then ",failureP,contextP,increaseP,importanceP," else "" in
	  let header = 
		if !mod_input == 0 then 
			Printf.sprintf "Stmt_num,file,line,not_cbi_prob,cbi_prob,failure%s\n" stmt_head
		else "" in
	  let print_info = 
		(fun (stmt,loc,cov,cbi,fp,cp,ip1,ip2) ->
		   Printf.printf "%d,%s\n" stmt 
			 (match !mod_input with 
				 0 -> 
				   if !calc_imp then
					 Printf.sprintf "%s,%d,%g,%g,%g,%g,%g,%g" loc.file loc.line cov cbi fp cp ip1 ip2
				   else 
					 Printf.sprintf "%s,%d,%g,%g" loc.file loc.line cov cbi 
			   | 1 -> Printf.sprintf "%g" cov
			   | 2 -> Printf.sprintf "%g" cbi
			   | 3 -> Printf.sprintf "%g" ip2)) in
		compare_paths bpath_ht gpath_ht imp_ht loc_ht stmt_cov_ht print_info
end ;;

main () ;;
