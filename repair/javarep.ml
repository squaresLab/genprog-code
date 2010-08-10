open Printf
open Rep
open Global
open Jast

(*to do
  repair currently overwrites the original in multi-file repair
  *)
let javaRep_version = "2" 

(*this will be added to the top of java repairs (can be empty)*)
let master_trunk_text = "" 
let str integer = (string_of_int integer) (*casting shortcut*)
let cobertura_path = ref ""
let coverage_script = ref "./coverage-test.sh"
let multi_file = ref false
let _ = 
  options := !options @
  [
    "--cobertura-path", Arg.Set_string cobertura_path, "X use X as path to cobertura";
    "--coverage-script", Arg.Set_string coverage_script, "X use X as instrumentation script name";
    "--multi-file", Arg.Set multi_file, "Program is made up of multiple files"
  ] 
class javaRep = object (self : 'self_type)
    inherit [Jast.ast_node] faultlocRepresentation as super 

    val base = ref Jast.dummyfile

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
    match !multi_file with 
    | false -> super#compile ~keep_source:true source_name exe_name
    | true -> 
      begin
      let dirname = Filename.dirname source_name in
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
    base := file
    
  method output_source source_name =
    Jast.write !base source_name
  
  method get_compiler_command () = 
    assert(!use_subdirs = true); 
    (* only works if you compile each variant in a sub-directory *) 
    "--compiler-command __COMPILER_NAME__ __SOURCE_NAME__ __COMPILER_OPTIONS__ >& /dev/null"
  

  method debug_info () = begin
    (*what would go here? *)
    debug "javaRep: nothing to debug?\n" 
  end 
  
  method instrument_fault_localization coverage_sourcename 
                                       coverage_exename 
                                       coverage_outname  = begin

    failwith "javaRep: no fault localization"
    end
    
  (*FIXME can i delete this?*)
  method updated () = super#updated ()
  
  method atom_id_of_source_line source_file source_line = 
    failwith "javaRep: no mapping from source lines to atom ids yet!" 

  (*FIXME get this from !current_id*)
  method max_atom () = 
    Jast.get_max_id ()
    
  method delete stmt_id = 
    super#delete stmt_id;
    base := Jast.delete !base stmt_id
    
  method append (append_after:atom_id) (what_to_append:atom_id) =
    super#append append_after what_to_append;
    base := Jast.append !base append_after what_to_append
    
  method swap stmt_id1 stmt_id2 = 
    super#swap stmt_id1 stmt_id2;
    base := Jast.swap !base stmt_id1 stmt_id2
    
  method put stmt_id stmt =
    super#put stmt_id stmt;
    base := Jast.replace !base stmt_id stmt
    
  method get stmt_id = 
    Jast.get_node !base stmt_id
    
end
    
  (*
  method instrument_fault_localization coverage_sourcename 
                                       coverage_exename 
                                       coverage_outname  = begin

    match !multi_file with 
      |true -> 
          failwith "javaRep: No fault localization for multifile repairs yet"
      |false -> 
      
    (*steps
    -------------------what-to-do------------------------------single-----multi
    1. write the file to coverage/coverage.java or coverage/*  yes        no
    2. compile the source to the coverage folder.              yes        no
    3. instrument fault localization on the compiled classes.  yes        no
    4. run positive test cases                                 partial    no
    5. run negative test cases                                 partial    no
    6. make separate reports for each                          no         no
    6. parse the two reports and make the .pos and .neg.       partial    no 
    7. orchestrate relative filepaths.                         nope       no                  
    *) 
    (
    debug "javaRep: Fault localization begins\n";
    self#output_source coverage_sourcename;
    print_endline (Printf.sprintf "%b" (self#compile ~keep_source:true coverage_sourcename coverage_exename));
    
    (*put all the positive tests in one file and all the negative in another*)
    let instrument source dataname report = 
      print_endline source;
      print_endline report;
      let instr_dir = 
        (Filename.concat !cobertura_path "cobertura-instrument.sh") in
      let dest_cmd = "--destination coverage/instrumented" in
      let data_cmd = Printf.sprintf "--datafile %s" dataname in
      let cmd = Printf.sprintf "%s %s %s %s" 
                               instr_dir 
                               dest_cmd 
                               data_cmd 
                               (Filename.dirname coverage_sourcename) in
      print_endline (Printf.sprintf "Instrumentation command: %s" cmd);
      match Stats2.time "coverage" Unix.system cmd with
      | Unix.WEXITED(0) -> 
          debug "javaRep: Coverage instrumentation successful\n"
      | _ -> failwith "failure in coverage instrumentation" in
      
    instrument coverage_sourcename coverage_exename coverage_outname;
      
    let coverage_testcase test = 
      let jar_path = (Filename.concat !cobertura_path "cobertura.jar") in
      let cmd = 
        Printf.sprintf "%s %s %s %s %s %s" !coverage_script 
                                           jar_path
                                           "coverage/instrumented"
                                           "coverage"
                                           coverage_exename
                                           (test_name test) in
      (match Stats2.time "coverage_test" Unix.system cmd with
      | Unix.WEXITED(0) -> true
      | _ -> false) in 
      
    debug "javaRep: Coverage tests begin\n";
    
    for i = 1 to !pos_tests do
      let r = coverage_testcase (Positive i) in
      debug "\tp%d: %b\n" i r ;
      assert(r) ; 
    done ;
    for i = 1 to !neg_tests do
      let r = coverage_testcase  (Negative i) in
      debug "\tn%d: %b\n" i r ;
      assert(not r) ; 
    done ;
    
    debug "javaRep: Done running coverage tests\n";
    (*get the line number out of the coverage.xml line *)
    let extract_number token = 
      let number = Str.regexp "[0-9]+" in
      let possible_match = ref false in
        (try 
          ignore (Str.search_forward number token 0);
          possible_match := true
        with Not_found -> possible_match := false);
      match !possible_match with
      | true -> Str.matched_string token
      | false -> failwith "Attempted to extract_number in a string without a number" in
    
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
      
      List.iter (fun token -> 
        let possible_match = ref false in
       (try 
          ignore (Str.search_forward visited_regexp token 0);
          possible_match := true
        with Not_found -> possible_match := false);
        match !possible_match with 
        | true -> 
            let result = extract_number token in
            coverage_lines := result::!coverage_lines
        | false -> ()
        ) !lines;
      let out_file = (open_out out_name) in
        List.iter (fun num -> (output_string out_file (num ^ "\n"))) !coverage_lines;
        close_out out_file in
          
      
      ())
    end
    *)
    
      
