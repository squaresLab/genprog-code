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
let precalc = ref false
let user_input = ref ""
let read_user = ref ""
let user_exclude = ref ""
let combine_from = ref ""
let convert_from = ref ""
let diff_format = ref false

let _ =
  options := !options @
    [
      "--precalc", Arg.Set precalc, "\t Precalculate differences.  Default: false";
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
      "--user", Arg.Set_string user_input, "X get user input.\n";
      "--readu", Arg.Set_string read_user, "read previously filtered table from X\n";
      "--exclude", Arg.Set_string user_exclude, "skip these processed change ids\n";
      "--convert", Arg.Set_string convert_from, "Convert from old to new\n";
      "--combine", Arg.Set_string combine_from, "combine changes in this ht\n";
      "--difft", Arg.Set diff_format, "input ht to be converted is in diff_ht format.  Default: false\n";
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

exception Quit 

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
                | Cil.GFun(fd,l) when fd.Cil.svar.Cil.vname = !test_func -> hadd f1ht fd.Cil.svar.Cil.vname fd;
                  fnames := fd.Cil.svar.Cil.vname :: !fnames
                | _ -> ()) 
          in
            ignore(Tigen.path_generation (List.of_enum (Hashtbl.enum f1ht)))
    end
  in
  let _ = 
    if !convert_from <> "" then begin
      let fin = open_in_bin !convert_from in
      let bench = Marshal.input fin in
      let change_ht = 
        if !diff_format then begin
          let diff_ht = Marshal.input fin in
          let change_ht = hcreate 10 in
          let all_diffs = hfold (fun diffid diff diffs -> diff :: diffs) diff_ht [] in
          let just_changes = lmap (fun d -> lmap (fun c -> (d.rev_num, d.msg, c)) d.changes) all_diffs in
          let changes : (string * string * old_change_node) list = lfoldl (fun changes accum -> changes @ accum) [] just_changes in
            liter (fun (rev_num,msg,c) -> hadd change_ht c.change_id (rev_num,msg,c)) changes;
            change_ht
        end else Marshal.input fin in
        close_in fin;
        let change_ht = convert_ht change_ht in
      let fout = open_out_bin !convert_from in 
        Marshal.output fout bench;
        Marshal.output fout change_ht;
        close_out fout;
    end
  in
  let _ = 
    if !combine_from <> "" then begin
      debug "combining?\n";
      let fin = open_in_bin !combine_from in
      let bench = Marshal.input fin in
      let change_ht = Marshal.input fin in
        close_in fin;
        debug "hlen: %d\n" (hlen change_ht);
        let new_changes = combine_changes change_ht in
        let fout = open_out_bin !combine_from in
          debug "new changes length: %d\n" (hlen new_changes);
          Marshal.output fout bench;
          Marshal.output fout new_changes;
          close_out fout;
          exit 0
    end
  in
  let changes : (string * string * change_node) list = 
    if !diff_files <> [] then 
      Diffs.test_delta_doc (lrev !diff_files)
    else 
      Diffs.get_many_diffs !configs 
  in
    if !user_input <> "" then begin
      let excluded = 
      if !user_exclude <> "" then begin
        lmap int_of_string (List.of_enum (File.lines_of !user_exclude))
      end else [] 
      in
      let bench,new_hash = 
        if !read_user <> "" then begin
          let fin = open_in_bin !read_user in
          let bench = Marshal.input fin in 
            debug "bench: %s\n" bench;
          let h = Marshal.input fin in
            close_in fin; bench,h
        end
        else begin
          let h = hcreate 10 in
            liter (fun (revnum,msg,c) -> hadd h c.nchange_id (revnum,msg,c)) changes;
            "foo",h
        end 
      in
      let changes = 
        let res = ref [] in 
        hiter (fun k (revnum,msg,c) -> res := (revnum,msg,c) :: !res) new_hash;
          !res
      in
      let changes = lfilt (fun (revnum,msg,c) -> not (lmem c.nchange_id excluded)) changes in
      let _ = debug "%d changes to inspect\n" (llen changes) in
      let processed = ref [] in
      let _ =
        try
          liter (fun (rev_num,msg,n1) ->
            if not (lmem n1.nchange_id excluded) then begin
            debug "%d:\n \trev: %s, log: {%s}\n \t{%s}\n"  n1.nchange_id rev_num msg (change_node_str n1);
            debug "Keep? (y/n)\n";
            let user_input = Str.split space_regexp (lowercase (read_line ())) in
            let hdc = if (llen user_input) > 0 then List.hd user_input else "y" in
              processed := n1.nchange_id :: !processed;
		      match hdc  with
		        "n" | "N" ->  hrem new_hash n1.nchange_id
              | "q" ->  raise (Quit)
		      | _ -> ()
          end) changes
        with Quit _ -> ()
      in
      let fout = open_out "numbers.txt" in 
        liter (fun num -> BatIO.write_line fout (Printf.sprintf "%d" num)) !processed;
        close_out fout;
      let fout = open_out_bin !user_input in
        Marshal.output fout bench;
        Marshal.output fout new_hash;
          close_out fout
    end;
    if !cluster || !precalc then begin
      let _ = 
        debug "%d changes to cluster\n" (llen changes);
      in
      let nums = 
        lmap (fun (a,b,change) -> store_change (a,b,change); change.nchange_id) changes
      in
        if !precalc then begin
          debug "precalc?\n";
          let processed = hcreate 10 in 
          let nums = if !num_temps > 0 then List.take !num_temps nums else nums in
          let rec iter1 list1 list2 =
            let rec iter2 ele list2 = 
              match list2 with
                hd :: rest -> 
(*                  if not (hmem processed (ele,hd)) &&
                    not (hmem processed (hd,ele)) then *)begin
                      let distance = ChangePoint.distance ele hd in
                        debug "%d x %d: %g\n" ele hd distance
                    end;
                  iter2 ele rest
              | [] -> ()
            in
              (match list1 with
                hd :: rest -> iter2 hd list2; ChangePoint.save (); iter1 rest (List.tl list2)
              | [] -> ())
          in
            iter1 nums (List.tl nums);
            exit 0
        end;
      let shuffled = List.take !num_temps (List.of_enum (Array.enum (Random.shuffle (List.enum nums )))) in
        debug "foo1\n";
        ChangeCluster.init ();
        debug "foo2\n";
        let medoids = ChangeCluster.kmedoid !k (Set.of_enum (List.enum shuffled)) in
          debug "foo3\n";
          if !save_medoids <> "" then begin
            let fout = open_out_bin !save_medoids in 
            let num_medoids = Set.cardinal medoids in 
              Marshal.output fout num_medoids;
              Set.iter 
                (fun changeid ->
                  let _,_,change = get_change changeid in
                    debug "changeId: %d, fname: %s, str:{%s}\n" changeid change.nfile_name1 (change_node_str change);
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
