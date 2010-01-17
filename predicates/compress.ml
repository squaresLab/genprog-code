let comma_regexp = Str.regexp_string ","
let main () = begin
  let argDescr = [ ] in
  let pred_files = ref [] in
  let handleArg a = pred_files := a :: !pred_files in
	Arg.parse argDescr handleArg "" ;

	List.iter
	  (fun file_name ->
		 let fin = open_in file_name in
		 let site_to_res = Hashtbl.create 10 in
		   try
			 while true do
			   let line = input_line fin in
			   let [site_num; value] = 
				 List.map int_of_string (Str.split comma_regexp line)
			   in
			   let (num_true, num_false) = 
				 if Hashtbl.mem site_to_res site_num then
				   Hashtbl.find site_to_res site_num else
					 (0,0) in
			   let res' =
				 if value == 0 then (num_true, num_false + 1) else 
				   (num_true + 1, num_false)
			   in
				 Hashtbl.replace site_to_res site_num res'
			 done
		   with _ -> close_in fin;
			 Hashtbl.iter 
			   (fun site_num ->
				  fun (num_true, num_false) ->
					Printf.printf "%d,%d,%d\n" site_num num_true num_false;
			   ) site_to_res; flush stdout;
	  ) !pred_files

end;;

main ();;
