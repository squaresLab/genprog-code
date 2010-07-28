open Printf
open Rep
open Global
open Jast

let javaRep_version = "1" 
let master_trunk_text = "" (*this will be added to the top of java repairs (can be empty)*)
let str integer = (string_of_int integer) (*casting shortcut*)

(*adjustable weights for fault localization*)
let pos_only_weight = 1.0 (* weight if the atom is only visited in a positive case *)
let neg_only_weight = 1.0 (* weight if the atom is only visited in a negative case *)
let pos_and_neg_weight = 1.0 (* weight if the atom is visited in both positive and negative cases *)
let zero_coverage_weight = 1.0 (* weight if the atom is never visited *)

let test_script = ref "sh test.sh" 
let _ = print_endline !program_to_repair
(*take the file /path/to/my/file.java*)

    (*name*)                  (*what it turns into*)
let program_name = ref ""     (*file*)
let dirname = ref ""          (*changes as we move around*)
let javaname = ref ""         (*file.java*)
let program_dirname = ref ""  (*/path/to/my/file*)
let path_list = ref (Str.split (Str.regexp "/") "gcd-test-java/gcd.java")
let rec get_names () = begin
  path_list := List.rev !path_list;
  javaname := List.hd !path_list;
  path_list := List.tl !path_list;
  path_list := List.rev !path_list;
  List.iter (fun str -> print_endline str; dirname := !dirname ^ str ^ "/") !path_list;
  program_name := List.hd (Str.split (Str.regexp "\\.") !javaname);
  program_dirname := !dirname
end
let _ = get_names ()


class javaRep = object (self : 'self_type)
    inherit [Jast.ast_node] faultlocRepresentation as super 

    val base = ref Jast.dummyfile
    (*val imports = ref []*)

  (* make a fresh copy of this variant *) 
  method copy () : 'self_type = 
    let super_copy : 'self_type = super#copy () in 
    super_copy#internal_copy () 

  (* being sure to update our local instance variables *) 
  method internal_copy () : 'self_type = 
    {< base = ref (Global.copy !base) ; 
       (*imports = ref !imports ;*) >} 

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


  method test_case test = try begin

(* debug "\ttest_case %s %s (digest=%S)\n" 
      (self#name ()) (test_name test) 
      (match !already_sourced with | None -> "" | Some(x) -> x) ; *)

    let try_cache () = 
      (* first, maybe we'll get lucky with the persistent cache *) 
      (match !already_sourced with
      | None -> ()
      | Some(digest) -> begin 
        match test_cache_query digest test with
        | Some(x) -> raise (Test_Result x)
        | _ -> ()
        end  
      )  
    in 
    try_cache () ; 

    (* second, maybe we've already compiled it *) 
    let exe_name, worked = match !already_compiled with
    | None -> (* never compiled before, so compile it now *) 
    let cmd = Printf.sprintf "mkdir -p tests/%05d" !test_counter in
    let _ = Unix.system cmd in
      (*set the current dirname for cd'ing*)
      dirname := (Printf.sprintf "tests/%05d" !test_counter);
      let source_name = 
        (sprintf "tests/%05d/%s.%s" !test_counter !program_name !Global.extension) in  
      let exe_name = 
        (sprintf "-cp test(does this do anything? nope)s/%05d %s" !test_counter !program_name) in  
      incr test_counter ; 
      self#output_source source_name ; 
      try_cache () ; 
      if not (self#compile ~keep_source:true source_name exe_name) then 
        exe_name,false
      else
        exe_name,true

    | Some("") -> "", false (* it failed to compile before *) 
    | Some(exe) -> exe, true (* it compiled successfully before *) 
    in
    let result = 
      if worked then begin 
        (* actually run the program on the test input *) 
        self#internal_test_case exe_name test 
      end else false 
    in 
    (* record result for posterity in the cache *) 
    (match !already_sourced with
    | None -> ()
    | Some(digest) -> test_cache_add digest test result
    ) ; 
    raise (Test_Result(result))

  end with Test_Result(x) -> (* additional bookkeeping information *) 
    (match !already_sourced with
    | None -> ()
    | Some(digest) -> Hashtbl.replace tested (digest,test) () 
    ) ;
    x
  

  method internal_test_case exe_name test  = begin
    let cmd = Printf.sprintf "sh %stest.sh %s/%s %s %d" !program_dirname !dirname !program_name (test_name test) 100  in
    match Stats2.time "test" Unix.system cmd with 
    | Unix.WEXITED(0) -> true
    | _ -> false  
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
    
  method from_source (filename:string) =
    let file = Jast.build_ast filename in
    base := file
    (*super#compute_fault_localization ()*)
    
  method output_source source_name =
    Jast.write !base source_name
  
  method compile ?(keep_source=false) source_name exe_name = begin
    let cmd = Printf.sprintf "javac %s" source_name in
    let result = (match Stats2.time "compile" Unix.system cmd with
    | Unix.WEXITED(0) ->
      already_compiled := Some(exe_name);
      true
    | _ ->
      already_compiled := Some("");
      false
    ) in
    if not keep_source then begin
      Unix.unlink source_name;
    end;
    result
  end

  method sanity_check () = begin
    print_endline !program_dirname;
    print_endline !program_name;
    print_endline !javaname;
    print_endline !dirname;
    debug "javarep: sanity checking begins\n" ; 
    let _ = Unix.system "mkdir -p sanity/sanity" in
    self#output_source (Printf.sprintf "sanity/sanity/%s" !javaname); 
    let c = self#compile ~keep_source:true (Printf.sprintf "sanity/sanity/%s" !javaname) "sanity/gcd" in 
    if not c then begin
      debug "javaRep: %s: does not compile\n" sanity_filename ;
      exit 1 
    end ; 
    (*so we can cd to the sanity folder*)
    dirname := "sanity/sanity";
    for i = 1 to !pos_tests do
      let r = self#internal_test_case !program_name (Positive i) in
      debug "\tp%d: %b\n" i r ;
      assert(r) ; 
    done ;
    for i = 1 to !neg_tests do
      let r = self#internal_test_case sanity_exename (Negative i) in
      debug "\tn%d: %b\n" i r ;
      assert(not r) ; 
    done ;
    debug "cachingRepresentation: sanity checking passed\n" ;
  end 
  
  method get_compiler_command () = 
    assert(!use_subdirs = true); 
    (* only works if you compile each variant in a sub-directory *) 
    "--compiler-command __COMPILER_NAME__ __SOURCE_NAME__ __COMPILER_OPTIONS__ >& /dev/null"

  method debug_info () = begin
    (*what would go here? *)
    debug "javaRep: nothing to debug?" 
  end 

  method updated () = super#updated ()
  
  method atom_id_of_source_line source_file source_line = 
    failwith "javaRep: no mapping from source lines to atom ids yet!" 

  method instrument_fault_localization coverage_sourcename _ coverage_outname = begin
    let ast = !base in 
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
    Jast.write coverage_ast (sprintf "coverage/coverage/%s" !program_name);
    let cmd = Printf.sprintf "javac coverage/coverage/%s" !javaname in
    let _ = Unix.system cmd in
    let num_atoms = self#max_atom () in
    let pos_atoms = Hashtbl.create num_atoms in
    let neg_atoms = Hashtbl.create num_atoms in
    
    (*let coverage_file = open_out coverage_outname in
    output_string file "p\n";
    close_out coverage_file; (*close it so java can open/close it freely*)*)
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
    debug "\njavarep: finished printing weighted path\n"

  end

  method max_atom () = 
    Jast.get_max_id !base
    
  method delete stmt_id = 
    self#updated ();
    history := (sprintf "d(%d)" stmt_id) :: !history;
    base := Jast.delete !base stmt_id
    
  method append (append_after:atom_id) (what_to_append:atom_id) =
    self#updated ();
    history := (sprintf "a(%d,%d)" append_after what_to_append) :: !history;
    base := Jast.append !base append_after what_to_append
    
  method swap stmt_id1 stmt_id2 = 
    self#updated ();
    history := (sprintf "s(%d,%d)" stmt_id1 stmt_id2) :: !history;
    base := Jast.swap !base stmt_id1 stmt_id2
    
  method put stmt_id stmt =
    self#updated ();
    history := (sprintf "p(%d)" (stmt_id)) :: !history ;
    base := Jast.replace !base stmt_id stmt
    
  method get stmt_id = 
    Jast.get_node !base stmt_id
    
end
    
    
    
    
    
    
    
    
      
