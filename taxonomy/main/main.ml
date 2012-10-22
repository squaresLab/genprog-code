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
open Datapoint
open Cluster

let diff_files = ref []

let tigen_test = ref ""
let test_func = ref ""
let save_medoids = ref ""
let output_templates = ref ""
let test_cluster = ref ""
let num_temps = ref 250
let test_cdiff = ref false

let _ =
  options := !options @
    [
      "--test-cdiff", Arg.Rest (fun s -> test_cdiff := true; diff_files := s :: !diff_files),
      "\t test cdiff\n"; 
      "--test-cluster", Arg.Set_string test_cluster, "\tX test k-medoids on an input set of points in csv file X";
	  "--test-delta-doc", Arg.Rest (fun s ->  diff_files := s :: !diff_files), "\t Test delta doc\n"; 
      "--set-size", Arg.Set_int num_temps, "\t number of random templates to cluster. Default: 250";
      "--cluster",Arg.Int (fun ck -> cluster := true; k := ck), "\t perform clustering";
      "--test-tigen", Arg.Set_string tigen_test, "\tX test symbolic execution on X";
      "--test-fun", Arg.Set_string test_func, "\tX function to test in symex";
      "--medoids", Arg.Set_string save_medoids, "X serialize medoids to X\n";
      "--templates", Arg.Set_string output_templates, "X convert medoids to templates and output to X\n";
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

(* LOOK AT --noInsertImplicitCasts from CIL! *)
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
  List.iter (fun (name,arg,_) ->
    if name = "-help" or name = "--help" then () 
    else
    debug "%s %s\n" name 
    (match arg with
    | Arg.Set br 
    | Arg.Clear br 
    -> Printf.sprintf "%b" !br 
    | Arg.Set_string sr
    -> Printf.sprintf "%S" !sr
    | Arg.Set_int ir
    -> Printf.sprintf "%d" !ir
    | Arg.Set_float fr
    -> Printf.sprintf "%g" !fr
    | _ -> "?") 
  ) (List.sort ~cmp:(fun (a,_,_) (a',_,_) -> compare a a') (!options)) ; 

  let _ = 
    if !test_cluster <> "" then 
      ignore(Cluster.test_cluster !test_cluster)
  in
  let _ = 
    if !test_cdiff then
      ignore(Cdiff.tree_diff_cil (List.hd !diff_files) (List.hd (List.tl !diff_files)))
  in
  let _ = 
    if !tigen_test <> "" then begin
      debug "one\n";
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
                | Cil.GFun(fd,l) when fd.Cil.svar.Cil.vname = !test_func -> debug "foo\n"; hadd f1ht fd.Cil.svar.Cil.vname fd;
                  fnames := fd.Cil.svar.Cil.vname :: !fnames
                | _ -> ()) 
          in
            debug "hashtbl size: %d\n" (hlen f1ht);
            ignore(Tigen.path_generation (List.of_enum (Hashtbl.enum f1ht)))
    end
  in
  let changes = 
    if !diff_files <> [] then 
      Diffs.test_delta_doc (lrev !diff_files)
    else 
      Diffs.get_many_diffs !configs 
  in
    if !cluster then begin
      let _ = 
        debug "%d changes to cluster\n" (llen changes);
      in
      let nums = 
        lmap (fun (a,b,change) -> store_change (a,b,change); change.change_id) changes
      in
      let shuffled = List.take !num_temps (List.of_enum (Array.enum (Random.shuffle (List.enum nums )))) in
        ChangeCluster.init ();
        let medoids = ChangeCluster.kmedoid !k (Set.of_enum (List.enum shuffled)) in
          if !save_medoids <> "" then begin
            let fout = open_out_bin !save_medoids in 
            let num_medoids = Set.cardinal medoids in 
              Marshal.output fout num_medoids;
              Set.iter 
                (fun changeid ->
                  let _,_,change = get_change changeid in
                    debug "changeId: %d, fname: %s, str:{%s}\n" changeid change.file_name1 (change_node_str change);
                    Marshal.output fout change) medoids;
              close_out fout
          end;
          if !output_templates <> "" then begin
            let medoids = 
              lmap (fun changeid ->
                let _,_,change = get_change changeid in
                  change) (List.of_enum (Set.enum medoids))
            in
            Convert.convert medoids !output_templates ;
          end;
          ChangeCluster.save ()
    end	 ;
    let endtime = Unix.localtime (Unix.time ()) in
	  pprintf "end: %02d/%02d %02d:%02d:%02d\n" (endtime.tm_mon + 1) endtime.tm_mday endtime.tm_hour endtime.tm_min endtime.tm_sec
end ;;

main () ;;
