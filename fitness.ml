(* 
 * Program Repair Prototype (v2) 
 *
 * Fitness Strategies include:
 *  -> test until first failure
 *  -> standard "test all"
 *  -> test a subset of all available
 *  -> test by calling an outside script and reading in a resulting file
 *)
open Printf
open Global
open Rep
open Pervasives

let negative_test_weight = ref 2.0
let pred_base = ref ""
let pred_loc = ref "~/genprog-code/branches/cbi-branch/predicates/predicates"
let site_loc = ref ""
let csv_name = ref "preds.csv"
let cdiff_loc = ref "~/genprog-code/trunk/ga/cdiff" 
let min_list = ref "min_list.txt"
let fit_weights = ref "predicate_fitness_weights.txt"
let weight_table = ref "minimal_list.txt"
let single_fitness = ref false
let _ = 
  options := !options @ [
  "--negative_test_weight", Arg.Set_float negative_test_weight, "X negative tests fitness factor";
	"--pred-baseline", Arg.Set_string pred_base, "Location of baseline file for comparing predicates";
	"--pred-location", Arg.Set_string pred_loc, "X location of predicate processing program";
	"--site-location", Arg.Set_string site_loc, "X location of sites file";
	"--csv-name", Arg.Set_string csv_name, "X name of output .csv file w/ cdiff and predicate information";
	"--minimal-list", Arg.Set_string min_list, "X name of file that lists location of minimized repairs"; 
	"--cdiff-location", Arg.Set_string cdiff_loc, "X location of cdiff program";
	"--fitness-weights", Arg.Set_string fit_weights, "X file listing predicate fitness weights.";
	"--diff-table", Arg.Set_string weight_table, "X table for weighted cdiff";
  "--single-fitness", Arg.Set single_fitness, " use a single fitness value"
] 

(* What should we do if we encounter a true repair? *)
let note_success (rep : 'a Rep.representation) =
  let name = rep#name () in 
  debug "\nRepair Found: %s\n" name ;
  rep#output_source ("repair." ^ !Global.extension) ;
  exit 1 

exception Test_Failed

(* As an optimization, brute force gives up on a variant as soon
 * as that variant fails a test case. *) 
let test_to_first_failure (rep : 'a Rep.representation) = 
  let count = ref 0.0 in 
  try
    if !single_fitness then begin
      let res, real_value = rep#test_case (Single_Fitness) in 
      count := real_value.(0) ;
      if not res then raise Test_Failed
      else note_success rep 

    end else begin 
      for i = 1 to !neg_tests do
        let res, v = rep#test_case (Negative i) in 
        if not res then raise Test_Failed
        else begin 
         assert(Array.length v > 0); 
         count := !count +. v.(0)
        end 
      done ;
      for i = 1 to !pos_tests do
        let res, v = rep#test_case (Positive i) in 
        if not res then raise Test_Failed
        else begin 
         assert(Array.length v > 0); 
         count := !count +. v.(0)
        end 
      done ;
      note_success rep 
    end 

  with Test_Failed -> 
    debug "\t%3g %s\n" !count  (rep#name ()) 

(* Our default fitness evaluation involves testing a variant on
 * all available test cases. *) 
let test_all_fitness (rep : 'a representation ) = 
  let fitness = ref 0.0 in 
  let failed = ref false in

  if !single_fitness then begin
    (* call just a single test that will return a real value *) 
    let res, real_value = rep#test_case (Single_Fitness) in 
    fitness := real_value.(0) ;
    failed := not res ; 

  end else begin 
    (* Find the relative weight of positive and negative tests *)
    let fac = (float !pos_tests) *. !negative_test_weight /. 
              (float !neg_tests) in 
    for i = 1 to !pos_tests do
      let res, _ = rep#test_case (Positive i) in 
      if res then fitness := !fitness +. 1.0 
      else failed := true 
    done ;
    for i = 1 to !neg_tests do
      let res, _ = rep#test_case (Negative i) in 
      if res then fitness := !fitness +. fac
      else failed := true 
    done ;
  end ;

  (* debugging information, etc. *) 
  debug "\t%3g %s\n" !fitness (rep#name ()) ;
  if not !failed then begin
    note_success rep 
  end ; 
  !fitness 

let test_all_fitness_nonstop (rep : 'a Rep.representation ) = 
  (* Find the relative weight of positive and negative tests *)
  let fac = (float !pos_tests) *. !negative_test_weight /. 
            (float !neg_tests) in 
  let fitness = ref 0.0 in 
  let failed = ref false in
  for i = 1 to !pos_tests do
    if (fst (rep#test_case (Positive i))) then fitness := !fitness +. 1.0 
    else failed := true 
  done ;
  for i = 1 to !neg_tests do
    if (fst (rep#test_case (Negative i))) then fitness := !fitness +. fac
    else failed := true 
  done ;
  debug "\t%3g %s\n" !fitness (rep#name ()) ;
  !fitness 

(*let collect_pred_fitness (rep : 'a Rep.representation ) = 
  (* Find the relative weight of positive and negative tests *)
	let fac = (float !pos_tests) *. !negative_test_weight /. 
            (float !neg_tests) in 
  let fitness = ref 0.0 in 
	let posfit = ref 0.0 in
	let negfit = ref 0.0 in
  let failed = ref false in
	(* Make sure that baseline information exists -- if not, then create it *)
	if not (Sys.file_exists !pred_base) then begin
  	if (Sys.file_exists "00000.txt") then begin
			let bcmd = Printf.sprintf "%s -rs 00000.txt -cbi-hin %s -gen-baseline %s" !pred_loc !site_loc !pred_base in
			let bbsuc = match Unix.system bcmd with
				| Unix.WEXITED(0) -> true
				| _ -> false in
			debug "baseline created\n" ;
			end
			else debug "should generate initial file..." ;
		end
	else () ; 
	(* Altered to do additional predicate processing when evaluating test cases *)
	for i = 1 to !pos_tests do
    if (rep#pred_collect (Positive i) i) then fitness := !fitness +. 1.0 
    else failed := true ;
  done ;
	posfit := !fitness ;
  for i = 1 to !neg_tests do
    if (rep#pred_collect (Negative i) (i + !pos_tests)) then begin
			fitness := !fitness +. fac 
		end
    else failed := true;
  done ;
	negfit := !fitness -. !posfit ;
	debug "\t%3g %s\n" !fitness (rep#name ()) ;
	(* Generate the predicates we are interested in *)
	if (not ((rep#identifier ()) = 0)) then begin
		let proc_preds = Printf.sprintf "%05d.proc_preds" (rep#identifier ()) in
		let apc = Printf.sprintf "%s -rs %05d.txt -cbi-hin %s -bin %s | tail -1 > %s && cat %s >> %s" 
			!pred_loc (rep#identifier ()) !site_loc !pred_base proc_preds proc_preds !csv_name in 
		if (Sys.file_exists (Printf.sprintf "%05d.txt" (rep#identifier ()))) then  (match Unix.system apc with
		| Unix.WEXITED(0) -> begin
			append_file_noline !csv_name (Printf.sprintf "%g,%g,%g" !fitness !posfit !negfit) ;
			(* Evaluate optimal fitness for correlation using diffx *)
			let getdiffx head =
				let w_table = file_to_list !weight_table in
				let diffname = Printf.sprintf "%05d-diff" (rep#identifier ()) in
				let catname = String.concat "-" [diffname;head] in
				let source_out = Printf.sprintf "%05d.c" (rep#identifier ()) in
				(rep#output_source source_out) ;
				let diffx = Printf.sprintf "%s %s %s --generate %s >& /dev/null" !cdiff_loc source_out head catname in
				(match Unix.system diffx with
				| Unix.WEXITED(0) -> ()
				| _ -> begin
				end );
				let diffxname = Printf.sprintf "%s.diff" catname in
				let szdiffx = count_lines_in_file diffxname in
				let full_c_diff = file_to_list diffxname in
				let f_by (w : int) = 
					let p1 = List.map (fun x ->
											if not (List.exists (fun y -> y = x) full_c_diff) then w else 0) w_table in
					let p2 = List.map (fun x ->
											if not (List.exists (fun y -> y = x) w_table) then 1 else 0) full_c_diff in
					let s1 = (List.fold_left (fun x y -> x + y) 0 p1) in 
					let s2 = (List.fold_left (fun x y -> x + y) 0 p2) in
						s1 + s2 in
				let cdiff10 = f_by 10 in
				debug "\t %s\n" diffx ;
				debug "\t\t cdiff10: %d\n" cdiff10 ;
				cdiff10 in
			let file_list = file_to_list !min_list in
			debug "length of file_list is %d" (List.length file_list) ;
			let cdiff_measures = List.map (fun x -> getdiffx x) file_list in
			List.map (fun x -> append_file_noline !csv_name (Printf.sprintf ",%d" x)) cdiff_measures ; 
			append_file_noline !csv_name "\n" ; ()
		end
		| _ -> debug "falied to parse predicates\n") else debug "skipping predicates, did not compile\n" ;
	end
	else () ;
  if not !failed then begin
    (*note_success rep*) () 
  end ; 
  !fitness 
*)
(*let pred_fitness (rep : 'a Rep.representation ) = 
  (* Find the relative weight of positive and negative tests *)
  let fac = (float !pos_tests) *. !negative_test_weight /. 
            (float !neg_tests) in 
	let feature_weights = file_to_list !fit_weights in
	let intercept::fit::pos::restlst = feature_weights in
	(* Add weights to hash table *)
	let w_pairs = 
		List.map (fun s -> Str.split (Str.regexp_string ",") s) restlst in
  let fitness = ref 0.0 in 
	let pos_fit = ref 0.0 in
	let neg_fit = ref 0.0 in
  let failed = ref false in
  for i = 1 to !pos_tests do
    if (rep#pred_collect (Positive i) i) then fitness := !fitness +. 1.0 
    else failed := true 
  done ;
	pos_fit := !fitness ;
  for i = 1 to !neg_tests do
    if (rep#pred_collect (Negative i) i) then fitness := !fitness +. fac
    else failed := true 
  done ;
	neg_fit := !fitness -. !pos_fit ;
	(* Evaluate fitness according to predicate metric *)
	debug "entering predicates for %05d" (rep#identifier ()) ;
	let proc_preds = Printf.sprintf "%05d.proc_preds" (rep#identifier ()) in
	let apc = Printf.sprintf "%s -rs %05d.txt -cbi-hin %s -bin %s | tail -1 > %s" 
			!pred_loc (rep#identifier ()) !site_loc !pred_base proc_preds in 
	let overwrite = if (Sys.file_exists (Printf.sprintf "%05d.txt" (rep#identifier ()))) 
		then (match Unix.system apc with
		| Unix.WEXITED(0) -> begin
			let result = List.hd (file_to_list proc_preds) in
			let res_to_list = List.map float_of_string (Str.split (Str.regexp_string ",") result) in
			let dot_product = 
				List.map (fun pair ->
										let left = int_of_string (List.nth pair 0) in
										let right = float_of_string (List.nth pair 1) in
										(List.nth res_to_list left) *. right) w_pairs in
			let sum = List.fold_left (fun x y -> x +. y) 0.0 dot_product in
			let sum = sum +. (float_of_string intercept) +. ((float_of_string pos) *. !pos_fit) 
				+. ((float_of_string fit) *. !fitness) in
			(abs_float sum) *. -1.0
			end
		| _ -> !fitness ) else begin 
				fitness := neg_infinity ; 
				!fitness end in
	fitness := overwrite ;
  debug "\t%3g %s\n" !fitness (rep#name ()) ;
  if not !failed then begin
    note_success rep 
  end ; 
  !fitness 
*)
