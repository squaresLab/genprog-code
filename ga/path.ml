open Printf
open Str
open Cil

let good_path_factor = ref 0.01

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

let compare_paths bpath_ht gpath_ht imp_ht loc_ht stmt_cov_ht =
  let badpath = ref [] in
  let stmt_head = ref "" in
    Hashtbl.iter 
      (fun i ->
	 fun _ -> 
	   let (cbi_prob, not_cbi_prob,stmt_prob) = 
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
	     let three = 
	       if Hashtbl.mem stmt_cov_ht i then
		 let (fp,cp,ip1,ip2) = Hashtbl.find stmt_cov_ht i in
		     (Printf.sprintf "%g,%g,%g,%g" fp cp ip1 ip2)
	       else "" in
	     let one' = Printf.sprintf "%g" one in 
	     let two' = Printf.sprintf "%g" two in
	       stmt_head := "failureP,contextP,increaseP,importanceP,";
	       (one', two', three) 
	   in
	   let loc = Hashtbl.find loc_ht i in 
	     badpath := (loc, i, not_cbi_prob, cbi_prob,stmt_prob) :: !badpath)
      bpath_ht;
    let badpath = uniq (List.rev !badpath) in 
    let badpath = 
      List.sort
	(fun (l1,n1,ncp1,cp1,imp1) ->
	   (fun (l2,n2,ncp2,cp2,imp2) ->
	      n1 - n2)) badpath in
      Printf.printf "Stmt_num,file,line,not_cbi_prob,cbi_prob,%s\n" !stmt_head;
      List.iter
	(fun (l,n,ncp,cp,imp) ->
	   Printf.printf "%d,%s,%d,%s,%s,%s\n"
	     n l.file l.line ncp cp imp)
	badpath;
      flush stdout

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
  let modify_input = ref "" in 
  let calc_imp = ref false in
  let comp_imp = ref false in 
  let argDescr = [
    "-ht", Arg.Set_string ht_file, " file with location hashtable information";
    "-gp", Arg.String (fun s -> goodpath_files := s :: !goodpath_files), "file with good path";
    "-bp", Arg.String (fun s -> badpath_files := s :: !badpath_files), "file with bad path";
    "-mi", Arg.Set_string modify_input, "file with cbi info for modify";
    "-calc", Arg.Set calc_imp, "calculate importance of each statement";
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
      let gpath_ht = build_count_ht !goodpath_files in 
      let bpath_ht = build_count_ht !badpath_files in
      let stmt_cov_ht = 
	if !calc_imp then
	  calculate_importance gpath_ht bpath_ht 
	    (float(List.length !goodpath_files))
	    (float(List.length !badpath_files))
	else Hashtbl.create 10 in
	  compare_paths bpath_ht gpath_ht imp_ht loc_ht stmt_cov_ht
end ;;

main () ;;
