open Printf
open Rep
open Global
open Jast

let javaRep_version = "1" 
let master_trunk_text = "" (*this will be added to the top of java repairs (can be empty)*)
let str integer = (string_of_int integer) (*casting shortcut*)

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
    match Jast.file_list with 
    |[] -> super#compile ~keep_source:true source_name exe_name
    | file_list -> 
      let dirname = Filename.dirname source_name in
      let success = ref true in 
      List.iter (fun source -> 
        let source = Printf.sprintf "%s/%s" dirname source in
        let result = super#compile ~keep_source:true source exe_name (*what exename*) in
        if result = false then success := false 
        ) (List.rev file_list);
      !success
  
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
  
  method instrument_fault_localization _ _ _ =
    failwith "javaRep: no fault localization"
    
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
  (*FIXME comment this out and move to bottom*)
  method instrument_fault_localization (*coverage_sourcename*) _ _ _ (*coverage_outname*) = begin
    (*let ast = !base in 
    assert(ast != Jast.dummyfile);
    let make_print (id:int) (stmt:string) = Printf.sprintf "GenProgLineWriter.Write(\"%s\",\"%d\"); %s" coverage_outname id stmt in
    let rec add_writes ast = 
      match ast with 
      |Trunk_node (file, text, ast_list) ->  Trunk_node (file, text, List.map (fun x-> add_writes x) ast_list)
      |Stem_node (file, text, ast_node) -> Stem_node (file, text, add_writes ast_node)
      |Branch_node (file, id, ast_list) -> Branch_node (file, id, List.map (fun x -> add_writes x) ast_list)
      |Leaf_node (file, id, text) -> let new_text = make_print id text in
                                      Leaf_node (file, id, new_text) 
      |Empty -> Empty in
    let coverage_ast = add_writes !base in
    (*compile the coverage file*)
    let _ = Unix.system "mkdir -p coverage/coverage" in
    Jast.write coverage_ast (sprintf "coverage/coverage/%s" !javaname);
    let cmd = Printf.sprintf "javac coverage/coverage/%s" !javaname in
    let _ = Unix.system cmd in
    
    
    let num_atoms = self#max_atom () in
    print_endline "doing fault localization";
    let pos_atoms = Hashtbl.create num_atoms in
    let neg_atoms = Hashtbl.create num_atoms in
    let coverage_file = open_out coverage_outname in
    output_string coverage_file "p\n";
    close_out coverage_file; (*close it so java can open/close it freely*)
    dirname := "coverage/coverage";
    for i = 1 to !pos_tests do
      let r = self#internal_test_case coverage_outname (Positive i) in
      debug "\tp%d: %b\n" i r ;
      assert(r) ; 
    done ;
     
    let coverage_file = open_out coverage_outname in
    output_string coverage_file "n\n";
    close_out coverage_file;
    
    for i = 1 to !neg_tests do
      let r = self#internal_test_case coverage_outname (Negative i) in
      debug "\tn%d: %b\n" i r ;
      assert(not r) ;
    done;
    
    let pos_done = ref false in
    let coverage_file = open_in coverage_outname in 
    begin try
      while true do
        let line = input_line coverage_file in 
        if line = "n" then pos_done := true 
        else if !pos_done == false 
          then Hashtbl.replace pos_atoms line 1
          else Hashtbl.replace neg_atoms line 1
      done 
    with End_of_file -> () end;
    
    for i = 1 to num_atoms do
      let weight = ref zero_coverage_weight in (* this is not changed if the atom "i" is never visited in either a pos nor neg test case *)
      if Hashtbl.mem neg_atoms (str i)
        then if Hashtbl.mem pos_atoms (str i)
          then weight := pos_and_neg_weight (* atom "i" was visited during both a pos and neg test case only*)
          else weight := neg_only_weight (* atom "i" was visited during a neg test case only *)
        else weight := pos_only_weight; (* atom "i" was visited during a pos test case only *)
      weighted_path := (i, !weight)::!weighted_path
    done;
    debug "javarep: printing weighted path \n";
    List.iter (fun x -> match x with 
                        | (i, weight) -> Printf.printf "(%d %02f)\t" i weight)!weighted_path;
    debug "\njavarep: finished printing weighted path\n"*)()
  end*)  
    
    
    
    
      
