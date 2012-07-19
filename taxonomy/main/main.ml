(* step 1: given a project, a URL, and a start and end revision,
 * collect all changes referencing bugs, bug numbers, or "fix."
 * 1a: diff option 1: tree-based diffs
 * 1b: diff option 2: syntactic (w/alpha-renaming)
 * step 2: process each change
 * step 3: cluster changes (distance metric=what is Ray doing/Hamming
 * distance from Gabel&Su, FSE 10?)
 *)

open Batteries
open List
open Unix
open Utils
open Globals
open Cil
open Difftypes
open Diffs
(*open Distance
open Tprint
open User*)
open Datapoint
open Cluster

let diff_files = ref []
let test_templatize = ref false 
let test_pdg = ref false
let test_cluster = ref ""

let num_temps = ref 10
let load_cluster = ref ""
let save_cluster = ref""
let explore_buckets = ref "" 

let fullload = ref ""
let user_feedback_file = ref ""

let ray = ref ""
let htf = ref ""
let tigen_test = ref ""

let _ =
  options := !options @
    [
      "--test-cluster", Arg.Set_string test_cluster, "\tX test k-medoids on an input set of points in csv file X";
	  "--buckets", Arg.Set_string explore_buckets, "\t print out template info for buckets, taken from lsh output";
	  "--test-delta-doc", Arg.Rest (fun s ->  diff_files := s :: !diff_files), "\t Test delta doc\n"; 
      "--test-templatize", Arg.Rest (fun s -> test_templatize := true;  diff_files := s :: !diff_files), "\t test templatizing\n";
      "--user-distance", Arg.Set_string user_feedback_file, "\t Get user input on change distances, save to X.txt and X.bin";
      "--fullload", Arg.Set_string fullload, "\t load big_diff_ht and big_change_ht from file, skip calls to svn collecton.";
      "--combine", Arg.Set_string htf, "\t Combine diff files from many benchmarks, listed in X file\n"; 
      "--ray", Arg.String (fun file -> ray := file), "\t  Ray mode.  X is config file; if you're Ray you probably want \"default\"";
      "--set-size", Arg.Set_int num_temps, "\t number of random templates to cluster. Default: 10";
      "--cluster",Arg.Int (fun ck -> cluster := true; k := ck), "\t perform clustering";
      "--loadc", Arg.Set_string load_cluster, "\t load saved cluster cache from X\n";
      "--savec", Arg.Set_string save_cluster, "\t save cluster cache to X\n"; 
      "--test-pdg", Arg.Rest (fun s -> test_pdg := true; diff_files := s :: !diff_files), "\ttest pdg, cfg, and vector generation";
	  "--sep", Arg.Set separate_vecs, "\t print context and change vectors separately.";
      "--test-tigen", Arg.Set_string tigen_test, "\tX test symbolic execution on X"
    ]

let ray_logfile = ref ""
let ray_htfile = ref ""
let ray_bigdiff = ref ("/home/claire/taxonomy/main/test_data/ray_full_ht.bin")
let ray_reload = ref true

let ray_options =
  [
    "--logfile", Arg.Set_string ray_logfile, "Write to X.txt.  If .ht file is unspecified, write to X.ht.";
    "--htfile", Arg.Set_string ray_htfile, "Write response ht to X.ht.";
    "--bigdiff", Arg.Set_string ray_bigdiff, "Get diff information from bigdiff; if bigdiff doesn't exist, compose existing default hts and write to X.";
    "--no-reload", Arg.Clear ray_reload, "Don't read in response ht if it already exists/add to it; default=false"
  ]

class everyVisitor = object
  inherit nopCilVisitor
  method vblock b = 
    ChangeDoChildrenPost(b,(fun b ->
      let stmts = List.map (fun stmt ->
        match stmt.skind with
        | Instr([]) -> [stmt] 
        | Instr(first :: rest) -> 
          ({stmt with skind = Instr([first])}) ::
            List.map (fun instr -> mkStmtOneInstr instr ) rest 
        | other -> [ stmt ] 
      ) b.bstmts in
      let stmts = List.flatten stmts in
        { b with bstmts = stmts } 
    ))
end 

let main () = begin  
  let starttime = Unix.localtime (Unix.time ()) in
  let _ = 
	pprintf "start: %02d/%02d %02d:%02d:%02d\n" (starttime.tm_mon + 1) starttime.tm_mday starttime.tm_hour starttime.tm_min starttime.tm_sec;
    Cil.initCIL () ;
    Random.self_init ()
  in
  let config_files = ref [] in
  let handleArg1 str = config_files := str :: !config_files in 
  let handleArg str = configs := str :: !configs in
  let aligned = Arg.align !options in
  let _ =
	Arg.parse aligned handleArg1 usageMsg ; 
	liter (parse_options_in_file ~handleArg:handleArg aligned usageMsg) !config_files;
  in
    debug "one\n";
    if !test_cluster <> "" then 
      ignore(Cluster.test_cluster !test_cluster);
    let changes = 
      debug "two\n";
      if !tigen_test <> "" then begin
        let f1 = Frontc.parse !tigen_test () in
        let my_every = new everyVisitor in
          visitCilFileSameGlobals my_every f1 ; 
        let f1ht = hcreate 255 in
        let fnames = ref [] in
          Cfg.computeFileCFG f1 ;
          let _ = 
            Cil.iterGlobals f1
              (fun g1 ->
                match g1 with
                | Cil.GFun(fd,l) -> hadd f1ht fd.Cil.svar.Cil.vname fd;
                  fnames := fd.Cil.svar.Cil.vname :: !fnames
                | _ -> ()) 
          in
            ignore(Tigen.path_generation f1 f1ht ["scgi_create_env"]);
        exit 0
      end;
      if !diff_files <> [] then 
        Diffs.test_delta_doc (lrev !diff_files)
      else 
        Diffs.get_many_diffs !configs 
    in
      if !cluster then begin
        debug "%d changes to cluster\n" (llen changes);
        let nums = 
          lmap (fun change -> store_change change; change.change_id) changes
        in
          (*          let shuffled = List.take 20 (List.of_enum (Array.enum (Random.shuffle (List.enum nums)))) in*)
        let shuffled = List.take 100 nums in
          ignore(ChangeCluster.kmedoid !k (Set.of_enum (List.enum shuffled)))
      end  (*else begin (* OK I want to still use this...need to integrate changes and template generation again somehow *)
		  if (!user_feedback_file <> "") || (!ray <> "") then begin
			pprintf "Hi, Ray!\n";
			pprintf "%s" ("I'm going to parse the arguments in the specified config file, try to load a big hashtable of all the diffs I've collected so far, and then enter the user feedback loop.\n"^
							 "Type 'h' at the prompt when you get there if you want more help.\n");
			let handleArg _ = 
			  failwith "unexpected argument in RayMode config file\n"
			in
			let aligned = Arg.align ray_options in
			let config_file  =
			  if !ray = "default" 
			  then "/home/claire/taxonomy/main/ray_default.config"
			  else !ray
			in
			  parse_options_in_file ~handleArg:handleArg aligned "" config_file;
			  let big_diff_ht,big_diff_id = hcreate 10, 0 in (* fixme? *)
			  let ht_file = 
				if !ray <> "" then
				  if !ray_htfile <> "" then !ray_htfile else !ray_logfile ^".ht"
				else
				  !user_feedback_file^".ht"
			  in
			  let logfile = 
				if !ray <> "" then
				  let localtime = Unix.localtime (Unix.time ()) in
					Printf.sprintf "%s.h%d.m%d.d%d.y%d.txt" !ray_logfile localtime.tm_hour (localtime.tm_mon + 1) localtime.tm_mday (localtime.tm_year + 1900)
				else
				  !user_feedback_file^".txt"
			  in
			  let reload = if !ray <> "" then !ray_reload else false in
				get_user_feedback logfile ht_file big_diff_ht reload
		  end
		end *);
	  let endtime = Unix.localtime (Unix.time ()) in
		pprintf "end: %02d/%02d %02d:%02d:%02d\n" (endtime.tm_mon + 1) endtime.tm_mday endtime.tm_hour endtime.tm_min endtime.tm_sec
end ;;

main () ;;
