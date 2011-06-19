open Printf
open Rep
open Global
open Jast

(*to do
  - the repair currently overwrites the original in multi-file repair
  - cobertura counts lines containing only braces as having been visited,
      make an exception for this to make debugging easier.
  - massive code clones in instrument_fault_localization
  - !program_name cheat in instrument fault localization. since we are using
      subfolders, just name things with the program name instead of sanity.java
      or coverage.java.
  - Leaving the subdirectories around will cause repair failure
  - handle enum's
  - many FIXME's
  *)
let javaRep_version = "3" 

(*this will be added to the top of java repairs (can be empty)*)
let master_trunk_text = "" 
let str integer = (string_of_int integer) (*casting shortcut*)
let cobertura_path = ref ""
let coverage_script = ref "./coverage-test.sh"
let allow_coverage_fail = ref false
let use_build_file = ref false
let global_var = ref false

(*FIXME - workaround for file renaming problem (see todo list)*)
let program_name = ref ""
let code_bank = ref Jast.dummyfile

let _ = 
  options := !options @
  [
    "--cobertura-path", Arg.Set_string cobertura_path, "X use X as path to cobertura";
    "--coverage-script", Arg.Set_string coverage_script, "X use X as instrumentation script name";
    "--use-build-file", Arg.Set use_build_file, " compile with Ant"
  ] 
  
class javaRep = object (self : 'self_type)
    inherit [Jast.ast_node] faultlocRepresentation as super 

    val base = ref Jast.dummyfile

  method atom_to_str ast_node =
    failwith "javaRep#atom_to_str" 

  method structural_signature =
    failwith "javaRep#structural_signature not implemented" 

  (* make a fresh copy of this variant *) 
  method copy () : 'self_type = 
    let super_copy : 'self_type = super#copy () in 
    super_copy#internal_copy () 

  (* being sure to update our local instance variables *) 
  method internal_copy () : 'self_type = 
    {< base = ref (Global.copy !base) ; >} 

  method save_binary ?out_channel (filename : string) = begin
    let fout = 
      match out_channel with
      | Some(v) -> v
      | None -> open_out_bin filename 
    in 
    Marshal.to_channel fout (javaRep_version) [] ; 
    Marshal.to_channel fout (!base) [] ;
    super#save_binary ~out_channel:fout filename ;
    debug "javaRep: %s: saved\n" filename ; 
    if out_channel = None then close_out fout 
  end 


  (* load in serialized state *) 
  method load_binary ?in_channel (filename : string) = begin
    let fin = 
      match in_channel with
      | Some(v) -> v
      | None -> open_in_bin filename 
    in 
    let version = Marshal.from_channel fin in
    if version <> javaRep_version then begin
      debug "javaRep: %s has old version\n" filename ;
      failwith "version mismatch" 
    end ;
    base := Marshal.from_channel fin ; 
    super#load_binary ~in_channel:fin filename ; 
    debug "javaRep: %s: loaded\n" filename ; 
    if in_channel = None then close_in fin 
  end 
  
  method compile ?(keep_source=false) source_name exe_name = begin 
    let dirname = Filename.dirname source_name in
    match !multi_file with 
    | false -> super#compile ~keep_source:true source_name exe_name
    | true -> 
      match !use_build_file with 
      | true -> 
          (let cmd = Printf.sprintf "./compile.sh %s" dirname in
          match Stats2.time "compile" Unix.system cmd with
          | Unix.WEXITED (0) -> true 
          | _ -> false)
          
      | false ->
        begin
        let success = ref true in 
        List.iter (fun source -> 
          let source = Printf.sprintf "%s/%s" dirname source in
          let result = super#compile ~keep_source:true source exe_name in
          if result = false then success := false 
          ) (List.rev (Jast.get_files ()));
        !success
        end
  
    end
    
  method from_source (filename:string) =
    let file = Jast.build_ast filename in
    (*FIXME - workaround for file renaming problem, see todo at top*)
    program_name := filename;
    code_bank := Jast.copy file;
    base := file
    
  method output_source source_name =
    Jast.write !base source_name

  method get_compiler_command () = 
    assert(!use_subdirs = true); 
    (* only works if you compile each variant in a sub-directory *) 
    "--compiler-command __COMPILER_NAME__ __SOURCE_NAME__ __COMPILER_OPTIONS__ >& /dev/null"
  

  method debug_info () = begin
    debug "javaRep: nothing to debug?\n" 
  end 
  
  method instrument_fault_localization coverage_sourcename 
                                       coverage_exename 
                                       coverage_outname  = begin

    (*FIXME - workaround for file renaming problem, see todo at top*)                                  
    let coverage_sourcename = Filename.concat (Filename.dirname coverage_sourcename ) !program_name in
    match !multi_file with 
      |true 
      |false -> begin
      
    debug "javaRep: Fault localization begins\n";
    self#output_source coverage_sourcename;
    ignore (self#compile ~keep_source:true coverage_sourcename coverage_exename);
    
    (*put all the positive tests in one file and all the negative in another*)
    let instrument source dataname report = 
      let instr_dir = 
        (Filename.concat !cobertura_path "cobertura-instrument.sh") in
      let dest_opt = "--destination coverage/instrumented" in
      let data_opt = Printf.sprintf "--datafile %s" dataname in
      let cmd = Printf.sprintf "%s %s %s %s" 
                               instr_dir 
                               dest_opt 
                               data_opt 
                               (Filename.dirname coverage_sourcename) in
      (*print_endline (Printf.sprintf "Instrumentation command: %s" cmd);*)
      match Stats2.time "coverage" Unix.system cmd with
      | Unix.WEXITED(0) -> 
          debug "javaRep: Coverage instrumentation successful\n"
      | _ -> failwith "failure in coverage instrumentation" in
      
    
    
    let coverage_testcase test data = 
      let jar_path = (Filename.concat !cobertura_path "cobertura.jar") in
      let cmd = 
        Printf.sprintf "%s %s %s %s %s %s" !coverage_script 
                                           jar_path
                                           "coverage/instrumented"
                                           "coverage"
                                           data
                                           (test_name test) in
      (match Stats2.time "coverage_test" Unix.system cmd with
      | Unix.WEXITED(0) -> true
      | _ -> false) in 
      
    debug "javaRep: Coverage tests begin\n";
    
    let make_report dataname destination format = begin
      let rep_cmd = Filename.concat !cobertura_path "cobertura-report.sh" in
      let f_opt = Printf.sprintf "--format %s" format in
      let data_opt = Printf.sprintf "--datafile %s" dataname in
      let dest_opt = Printf.sprintf "--destination %s" destination in
      let class_loc = "coverage" in
      let cmd = Printf.sprintf "%s %s %s %s %s" rep_cmd
                                                f_opt
                                                data_opt
                                                dest_opt
                                                class_loc in
      (*print_endline cmd;*)
      match Stats2.time "gen'ing reports" Unix.system cmd with
      | Unix.WEXITED(0) -> debug "javaRep: Coverage reporting successful.\n"
      | _ -> failwith "javaRep: Coverage report generation failed"
    
    end in
    
    (*get the line number out of the coverage.xml line *)
    let extract_match regexp token =
      let possible_match = ref false in 
        (try 
          ignore (Str.search_forward regexp token 0);
          possible_match := true
        with Not_found -> possible_match := false);
      if !possible_match == true
        then Str.matched_string token
        else failwith ("Attempted to extract regexp match where 
                        there was none") in
        
        
    let extract_number token = 
      extract_match (Str.regexp "[0-9]+") token in
    
    let extract_filename token = 
      let file_regexp = Str.regexp "filename=\"[^\n\\><:\"?|*\n\t]+\"" in
      let file_token = extract_match file_regexp token in
      let length = String.length file_token in
      let filename = String.sub file_token 10 (length - 11) in
      filename in
      
    let get_line_nums report_name out_name = 
      let file = open_in report_name in
      let lines = ref [] in
      begin try
        while true do 
        let line = input_line file in 
        lines := line::!lines
        done
      with End_of_file -> () end;
      close_in file;
      let coverage_lines = ref [] in
      let visited_regexp = Str.regexp "number=\"[0-9]+\" hits=\"[1-9][0-9]*\"" in
      let filename_regexp = Str.regexp "filename=\"[^\n\\><:\"?|*\n\t]+\"" in
      List.iter (fun token -> 
        let visited_match = ref false in
        let filename_match = ref false in 
        (try 
          ignore (Str.search_forward visited_regexp token 0);
          visited_match := true
        with Not_found -> visited_match := false);
        
        match !visited_match with 
        | true -> 
            let result = extract_number token in
            coverage_lines := result::!coverage_lines
        | false -> ();
        
        (try 
          ignore (Str.search_forward filename_regexp token 0);
          filename_match := true
        with Not_found -> filename_match := false);
        
        match !filename_match with
        | true -> begin
            let result = extract_filename token in
            coverage_lines := result::!coverage_lines end
        | false -> ()
        ) !lines;
            
      let out_file = (open_out out_name) in
        List.iter (fun num -> (output_string out_file (num ^ "\n"))) !coverage_lines; 
        close_out out_file in 
        
    instrument coverage_sourcename "coverage/positive.data" coverage_outname;
    for i = 1 to !pos_tests do
      let r = coverage_testcase (Positive i) "coverage/positive.data" in
      debug "\tp%d: %b\n" i r ;
      if !allow_coverage_fail
        then assert(r) ; 
    done ;
    make_report "coverage/positive.data" "coverage/positive" "xml";
    
    instrument coverage_sourcename "coverage/negative.data" coverage_outname;
    for i = 1 to !neg_tests do
      let r = coverage_testcase (Negative i) "coverage/negative.data" in
      debug "\tn%d: %b\n" i r ;
      if !allow_coverage_fail
        then assert(not r) ; 
    done ;
    
    make_report "coverage/negative.data" "coverage/negative" "xml";
    
    debug "javaRep: Done running coverage tests\n";
    debug "javaRep: Begin making .pos/.neg path files\n";
    get_line_nums "coverage/negative/coverage.xml" "coverage/coverage.path.neg";
    get_line_nums "coverage/positive/coverage.xml" "coverage/coverage.path.pos"

    
    
      end
    end
    
  method updated () = super#updated ()
    
  (* NOTE: CLG nuked compute_localization here b/c I think it was identical to the
	 one that was in the super class before I changed it.  If I'm wrong, I'm sorry *)

  method atom_id_of_source_line source_file source_line = 
    let source_file = Filename.basename source_file in 
	(*FIXME - do not build the ht every time, just store it in a variable and 
      have a flag check if it's been made before*)
    let id_ht = Jast.atom_id_of_lineno_ht !base in
    if !global_var == false
      then begin (*Hashtbl.iter (fun (x,y) z -> Printf.printf "(%s, %d) %d\n" x y z) id_ht;*)
        global_var := true
        end
      else ();
    let result = ref 0 in
    result := Hashtbl.find id_ht (source_file, source_line);
    assert (!result != 0);
    !result


  method max_atom () = 
    let result = Jast.get_max_id () in
    result
    
  method delete stmt_id = 
    super#delete stmt_id;
    base := Jast.delete !base stmt_id
    
  method append (append_after:atom_id) (what_to_append:atom_id) =
    super#append append_after what_to_append;
    let what_atom_to_append = Jast.get_node !code_bank what_to_append in
    base := Jast.append !base append_after what_atom_to_append
    
  method swap stmt_id1 stmt_id2 = 
    super#swap stmt_id1 stmt_id2;
    base := Jast.swap !base stmt_id1 stmt_id2
    
  method put stmt_id stmt =
    (*print_endline (Jast.string_value stmt);*)
    super#put stmt_id stmt;
    base := Jast.replace !base stmt_id stmt
    
  method get stmt_id = 
    Jast.get_node !base stmt_id
    
end
    
  
    
      
