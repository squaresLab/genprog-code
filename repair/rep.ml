(* 
 * Program Repair Prototype (v2) 
 *
 * The "representation" interface handles:
 *   -> the program representation (e.g., CIL AST, ASM)
 *   -> gathering and storing fault localization (e.g., weighted path, 
 *      predicates)
 *   -> simple mutation operator building blocks (e.g., delete, 
 *      append, swap) 
 *
 * TODO:
 *  -> "Well-Typed" insert/delete/replace 
 *     (also, no moving "break" out of a loop) 
 *  -> repair templates 
 *  -> predicates
 *  -> asm 
 *
 *)
open Printf
open Global
open Pervasives

(*
 * An atom is the smallest unit of our representation: a stmt in CIL,
 * a line of an ASM program, etc.  
 *
 * Sadly, this wording choice was imperfect: we quickly added "subatom"
 * processing (e.g., expressions in C). 
 *)
type atom_id = int 
type subatom_id = int 
type stmt = Cil.stmtkind
type test = 
  | Positive of int 
  | Negative of int 
  | Single_Fitness  (* a single test case that returns a real number *) 

(* Sometimes we want to compute the structural difference between two
 * variants to get a fine-grained diff between them. Currently this is only
 * really supported by CIL using the CDIFF/DIFFX code. *) 
type structural_signature = Cdiff.node_id StringMap.t  

(* The "Code Bank" abstraction: 
 * 
 * When we read in the original program, we copy all of its statements to
 * a "code bank" that we use later as a source for insertions and swaps.
 * For example, even if a particular variant deletes statement #5, it will
 * still be in the code bank. 
 *
 * The code bank is thus a set of atom_ids. 
 *)
module AtomSet = IntSet

(* Convert a weighted path (a list of <atom,weight> pairs) into
 * a set of atoms by dropping the weights. *) 
let wp_to_atom_set lst = 
  lfoldl
	(fun set ->
	  fun (i,w) -> AtomSet.add i set)
	(AtomSet.empty) lst 

(*************************************************************************
 *************************************************************************

                       virtual class REPRESENTATION

   This is the main virtual interface for a program representation (e.g.,
   CIL-AST, Assembly, etc.). 
  
   Most actual representations inherit from "cachingRepresentation", 
   or (better yet) "faultlocRepresentation" below. 

 *************************************************************************
 *************************************************************************)
class virtual (* virtual here means that some methods won't have
               * definitions here, and that they'll have to be filled
               * in when defining a subclass *) 
    ['atom]   (* "atom" is the raw type of the smallest manipulable
               * element. For CIL, this is "Cil.stmtkind"; for 
               * generic assembly code, it could be "string". *)
    representation  (* "representation" is the name of this class/type,
                     * but you'll often see " 'a representation ", where
                     * the 'a means "I don't care what the atom type is".
                     *)
    = object (self : 'self_type)

  method virtual copy : unit -> 'self_type
  method virtual internal_copy : unit -> 'self_type
  method virtual save_binary : ?out_channel:out_channel -> string -> unit (* serialize to a disk file *)
  method virtual load_binary : ?in_channel:in_channel -> string -> unit (* deserialize *) 
  method virtual from_source : string -> unit (* load from a .C or .ASM file, etc. *)
  method virtual output_source : string -> unit (* save to a .C or .ASM file, etc. *)
  method virtual source_name : string list (* is it already saved on the disk as a (set of) .C or .ASM files? *) 
  method virtual cleanup : unit -> unit (* if not keeping source, delete by-products of fitness testing for this rep. *)
  method virtual sanity_check : unit -> unit 
  method virtual compute_localization : unit ->  unit 
  method virtual compile : string -> string -> bool 
  method virtual test_case : test -> (* run a single test case *)
      bool  (* did it pass? *)
    * (float array) 
            (* what was the fitness value? typically 1.0 or 0.0,  but
             * may be arbitrary when single_fitness is used *) 
  method virtual debug_info : unit ->  unit (* print debugging information *) 

  method virtual max_atom : unit -> atom_id  (* 1 to N -- INCLUSIVE *)
  method virtual get_fault_localization : unit -> (atom_id * float) list 
  method virtual get_fix_localization : unit -> (atom_id * float) list 

  method virtual get_history : unit -> string list

  (* atomic mutation operators *) 
  method virtual delete : atom_id -> unit 

  (* append and swap find 'what to append' by looking in the code
   * bank (aka stmt_map) -- *not* in the current variant *)
  method virtual append : 
    (* after what *) atom_id -> 
    (* what to append *) atom_id -> unit 

  method virtual append_sources : 
    (* after what *) atom_id -> 
    (* possible append sources *) WeightSet.t 

  method virtual swap : atom_id -> atom_id -> unit 

  method virtual swap_sources : 
    (* swap with what *) atom_id -> 
    (* possible swap sources *) WeightSet.t 

  (* get obtains an atom from the current variant, *not* from the code
     bank *) 
  method virtual get : atom_id -> 'atom

  (* put places an atom into the current variant; the code bank is not
     involved *) 
  method virtual put : atom_id -> 'atom -> unit

  method virtual add_name_note : string -> unit 
  (* add a "history" note to the variant's descriptive name *)

  method virtual name : unit -> string (* a "descriptive" name for this variant *) 

  (* Subatoms.
   * Some representations support a finer-grain than the atom, but still
   * want to perform crossover and mutation at the atom level. For example,
   * C ASTs might have atoms (stmts) and subatoms (expressions). One
   * might want to change expressions, but that complicates crossover
   * (because the number of subatoms may change between variants). So
   * instead we still perform crossover on atoms, but allow sub-atom
   * changes. *) 
  method virtual subatoms : bool (* are they supported? *) 
  method virtual get_subatoms : atom_id -> ('atom list)
  method virtual replace_subatom : atom_id -> subatom_id -> 'atom -> unit 
  method virtual replace_subatom_with_constant : atom_id -> subatom_id -> unit 
  method virtual note_replaced_subatom : atom_id -> subatom_id -> 'atom -> unit

  (* For debugging. *) 
  method virtual atom_to_str : 'atom -> string 

  method virtual hash : unit -> int 
  (* Hashcode. Equal variants must have equal hash codes, but equivalent
     variants need not. By default, this is a hash of the name. *) 

  (* Tree-Structured Comparisons
   *   Mostly for CIL ASTs using the DiffX algorithm. 
   *   Use the "structural_difference" methods to compute the
   *   actual difference. 
   *) 
  method virtual structural_signature : structural_signature

end 


(* 
 * Tree-Structured Differencing. Use the "structural_signature" method of a
 * rep to get the structural signature. You can either inspet the Cdiff
 * edit script directly (it lists tree-structured edits needed to transform
 * rep1 into rep2) or just take the length of that script as the
 * "distance". 
 *)
let structural_difference_edit_script
      (rep1 : structural_signature)
      (rep2 : structural_signature)
      : (Cdiff.edit_action list)
      = 
  let result = ref [] in 
  StringMap.iter (fun name node1 ->
    try
      let node2 = StringMap.find name rep2 in 
      let m = Cdiff.mapping node1 node2 in 
      let s = Cdiff.generate_script 
        (Cdiff.node_of_nid node1) (Cdiff.node_of_nid node2) m in 
      result := s @ !result
    with Not_found -> () 
  ) rep1 ; 
  !result

let structural_difference
      (rep1 : structural_signature)
      (rep2 : structural_signature)
      : int 
      =
  List.length (structural_difference_edit_script rep1 rep2) 

let structural_difference_to_string
      (rep1 : structural_signature)
      (rep2 : structural_signature)
      : string 
      =
  let b = Buffer.create 255 in
  List.iter (fun elt ->
    Printf.bprintf b "%s " (Cdiff.edit_action_to_str elt)
  ) (structural_difference_edit_script rep1 rep2) ;
  Buffer.contents b 


(*
 * This is a list of variables representing global options related to
 * representations. 
 *)
let coverage_sourcename = "coverage" 
let coverage_exename = "coverage" 
let coverage_outname = ref "coverage.path" 
let sanity_filename = "repair.sanity" 
let sanity_exename = "./repair.sanity" 
let always_keep_source = ref false 
let compiler_command = ref ""
let test_command = ref ""
let flatten_path = ref ""
let compiler_name = ref "gcc" 
let compiler_options = ref "" 
let test_script = ref "./test.sh" 
let label_repair = ref false 
let use_subdirs = ref false 
let delete_existing_subdirs = ref false
let use_full_paths = ref false 
let debug_put = ref false 
let port = ref 808
let allow_sanity_fail = ref false 
let no_test_cache = ref false
let no_rep_cache = ref false 
let print_func_lines = ref false 
let use_subatoms = ref false 
let allow_coverage_fail = ref false 

let regen_paths = ref false
 
let fault_scheme = ref "path"
let fault_path = ref "coverage.path.neg"
let fault_file = ref ""

let fix_scheme = ref "default"
let fix_path = ref "coverage.path.pos"
let fix_file = ref ""
let fix_oracle_file = ref ""

let pick_positive_path = ref false

let prefix = ref "./"
let multi_file = ref false

let nht_server = ref "" 
let nht_port = ref 51000
let nht_id = ref "global" 

let _ =
  options := !options @
  [
	"--prefix", Arg.Set_string prefix, " path to original parent source dir";
    "--keep-source", Arg.Set always_keep_source, " keep all source files";
    "--compiler-command", Arg.Set_string compiler_command, "X use X as compiler command";
    "--test-command", Arg.Set_string test_command, "X use X as test command";
    "--test-script", Arg.Set_string test_script, "X use X as test script name";
    "--compiler", Arg.Set_string compiler_name, "X use X as compiler";
    "--compiler-opts", Arg.Set_string compiler_options, "X use X as options";
    "--label-repair", Arg.Set label_repair, " indicate repair locations";
    "--use-subdirs", Arg.Set use_subdirs, " use one subdirectory per variant.";
	"--delete-subdirs", Arg.Set delete_existing_subdirs, " recreate subdirectories if they already exist. Default: false";
    "--use-full-paths", Arg.Set use_full_paths, " use full pathnames";
    "--flatten-path", Arg.Set_string flatten_path, "X flatten weighted path (sum/min/max)";
    "--debug-put", Arg.Set debug_put, " note each #put in a variant's name" ;
    "--allow-sanity-fail", Arg.Set allow_sanity_fail, " allow sanity checks to fail";
    "--print-func-lines", Arg.Set print_func_lines, " print start/end line numbers of all functions" ;
    "--use-subatoms", Arg.Set use_subatoms, " use subatoms (expression-level mutation)" ;
    "--allow-coverage-fail", Arg.Set allow_coverage_fail, " allow coverage to fail its test cases" ;

	"--regen-paths", Arg.Set regen_paths, " regenerate path files";

	"--fault-scheme", Arg.Set_string fault_scheme, " How to do fault localization.  Options: path, uniform, line, weight. Default: path";
	"--fault-path", Arg.Set_string fault_path, "Negative path file, for path-based fault or fix localization.  Default: coverage.path.neg";
	"--fault-file", Arg.Set_string fault_file, " Fault localization file.  e.g., Lines/weights if scheme is lines/weights.";

	"--fix-scheme", Arg.Set_string fix_scheme, " How to do fix localization.  Options: path, uniform, line, weight, oracle, default (whatever Wes was doing before). Default: default";
	"--fix-path", Arg.Set_string fix_path, "Positive path file, for path-based fault or fix localization. Default: coverage.path.pos";
	"--fix-file", Arg.Set_string fix_file, " Fix localization information file, e.g., Lines/weights.";
	"--fix-oracle", Arg.Set_string fix_oracle_file, " List of source files for the oracle fix information.  Does not consider --prefix!";

	"--pick-pos", Arg.Set pick_positive_path, " Pick positive path by running all positive test cases and picking the path with the most overlap with the negative path.";
	"--coverage-out", Arg.Set_string coverage_outname, " where to put the path info when instrumenting source code for coverage.  Default: ./coverage.path";

(* deprecated *)

	"--use-line-file", 
	Arg.Unit (fun () -> 
	  raise (Arg.Bad " Deprecated.  For the same functionality, do \n \
                         \t\"--fault-scheme line\", \"--fault-file file_with_line_info.ext\"\n")), "--use-line-file is deprecated";
	"--use-path-file", Arg.Unit (fun () -> 
	  raise (Arg.Bad " Deprecated; the behavior is default.  You can be explicit \
                     with \"--fault-scheme path\".  --regen-paths forces path regeneration. Overried the default path files with \
                      \"--fault-path/--fix-path path_files.ext\"")),
	"--use-path-file is deprecated."
  ] 

(*
 * Utility functions for test cases. 
 *)

let test_name t = match t with
  | Positive x -> sprintf "p%d" x
  | Negative x -> sprintf "n%d" x
  | Single_Fitness -> "s" 

let change_port () = (* network tests need a fresh port each time *)
  port := (!port + 1) ;
  if !port > 1600 then 
    port := !port - 800 

(*
 * Networked caching for test case evaluations. 
 *)
let nht_sockaddr = ref None 

let nht_connection () = 
  (* debug "nht_connection: %s %d\n" !nht_server !nht_port ;  *)
  try begin match !nht_server with 
  | "" -> None 
  | x -> 
    let sockaddr = match !nht_sockaddr with
      | Some(sa) -> sa
      | None -> (* build it the first time *) 
          let host_entry = Unix.gethostbyname !nht_server in 
          let inet_addr = host_entry.Unix.h_addr_list.(0) in 
          let addr = Unix.ADDR_INET(inet_addr,!nht_port) in 
          nht_sockaddr := Some(addr) ;
          addr 
    in 
    let inchan, outchan = Unix.open_connection sockaddr in 
    Some(inchan, outchan)
  end with e -> begin  
          debug "ERROR: nht: %s %d: %s\n" 
            !nht_server
            !nht_port
            (Printexc.to_string e) ;
          nht_server := "" ; (* don't try again *) 
          None 
  end 

let add_nht_name_key_string qbuf digest test = 
    Printf.bprintf qbuf "%s\n" !nht_id ; 
    List.iter (fun d -> 
      Printf.bprintf qbuf "%s," (Digest.to_hex d) 
    ) digest ; 
    Printf.bprintf qbuf "%s\n" (test_name test) ;
    () 

let parse_result_from_string str = 
  let parts = Str.split comma_regexp str in 
  match parts with
  | b :: rest -> 
    let b = b = "true" in 
    let rest = lmap my_float_of_string rest in 
    Some(b, (Array.of_list rest))
  | _ -> None 

let nht_cache_query digest test = 
  match nht_connection () with
  | None -> None 
  | Some(inchan, outchan) -> 
    Stats2.time "nht_cache_query" (fun () -> 
      let res = 
        try 
          let qbuf = Buffer.create 2048 in 
          Printf.bprintf qbuf "g\n" ; (* GET *) 
          add_nht_name_key_string qbuf digest test ; 
          Printf.bprintf qbuf "\n\n" ; (* end-of-request *) 
          Buffer.output_buffer outchan qbuf ;
          (* debug "nht_cache_query: %S\n" (Buffer.contents qbuf) ;  *)
          flush outchan ;
          (* debug "nht_cache_query: awaiting response\n" ;  *)
          let header = input_line inchan in
          (* debug "nht_cache_query: response: %S\n" header;  *)
          if header = "1" then begin (* FOUND *) 
            Stats2.time "nht_cache_hit" (fun () -> 
              let result_string = input_line inchan in 
              parse_result_from_string result_string 
            ) () 
          end else None 
        with _ -> None 
      in
      (try close_out outchan with _ -> ()); 
      (try close_in inchan with _ -> ()); 
      res  
    ) () 

let add_nht_result_to_buffer qbuf (result : (bool * (float array))) = 
  let b,fa = result in 
  Printf.bprintf qbuf "%b" b ; 
  Array.iter (fun f ->
    Printf.bprintf qbuf ",%g" f
  ) fa ; 
  Printf.bprintf qbuf "\n" 

let nht_cache_add digest test result = 
  match nht_connection () with
  | None -> () 
  | Some(inchan, outchan) -> 
    Stats2.time "nht_cache_add" (fun () -> 
      let res = 
        try 
          let qbuf = Buffer.create 2048 in 
          Printf.bprintf qbuf "p\n" ; (* PUT *) 
          add_nht_name_key_string qbuf digest test ; 
          add_nht_result_to_buffer qbuf result ; 
          Printf.bprintf qbuf "\n\n" ; (* end-of-request *) 
          Buffer.output_buffer outchan qbuf ;
          flush outchan ;
          () 
        with _ -> () 
      in
      (try close_out outchan with _ -> ()); 
      (try close_in inchan with _ -> ()); 
      res  
    ) () 

(*
 * Persistent caching for test case evaluations. 
 *)
let test_cache = ref 
  ((Hashtbl.create 255) : (Digest.t list, (test,(bool*(float array))) Hashtbl.t) Hashtbl.t)
let test_cache_query digest test = 
  if Hashtbl.mem !test_cache digest then begin
    let second_ht = Hashtbl.find !test_cache digest in
    try
      let res = Hashtbl.find second_ht test in
      Stats2.time "test_cache hit" (fun () -> Some(res)) () 
    with _ -> nht_cache_query digest test  
  end else nht_cache_query digest test  
let test_cache_add digest test result =
  let second_ht = 
    try
      Hashtbl.find !test_cache digest 
    with _ -> Hashtbl.create 7 
  in
  Hashtbl.replace second_ht test result ;
  Hashtbl.replace !test_cache digest second_ht ;
  nht_cache_add digest test result 
let test_cache_version = 3
let test_cache_save () = 
  let fout = open_out_bin "repair.cache" in 
  Marshal.to_channel fout test_cache_version [] ; 
  Marshal.to_channel fout (!test_cache) [] ; 
  close_out fout 
let test_cache_load () = 
  try 
    let fout = open_in_bin "repair.cache" in 
    let v = Marshal.from_channel fout in  
    if v <> test_cache_version then begin
      debug "repair.cache: file format %d expected, %d found (skipping)" 
        test_cache_version v ; 
      close_in fout ; 
      raise Not_found 
    end ;
    test_cache := Marshal.from_channel fout ; 
    close_in fout 
  with _ -> () 

(* 
 * We track the number of unique test evaluations we've had to
 * do on this run, ignoring of the persistent cache.
 *)
let tested = (Hashtbl.create 4095 : ((Digest.t list * test), unit) Hashtbl.t)
let num_test_evals_ignore_cache () = 
  let result = ref 0 in
  Hashtbl.iter (fun _ _ -> incr result) tested ;
  !result

let compile_failures = ref 0 
let test_counter = ref 0 
exception Test_Result of (bool * (float array))

let add_subdir str = 
  let result = 
    if not !use_subdirs then
      "." 
    else begin
      let dirname = match str with
      | None -> sprintf "%06d" !test_counter
      | Some(specified) -> specified 
      in
		if Sys.file_exists dirname && !delete_existing_subdirs then begin
		  let cmd = "rm -rf ./"^dirname in
			try ignore(Unix.system cmd) with e -> ()
		end;
      (try Unix.mkdir dirname 0o755 with e -> ()) ;
      dirname 
    end 
  in
  if !use_full_paths then
    Filename.concat (Unix.getcwd ()) result
  else
    result 

(*************************************************************************
 *************************************************************************

                    virtual class CACHINGREPRESENTATION

   This interface for a program representaton handles the caching
   of compilations and test case results for you, as well as 
   dealing with some sanity checks. 
  
 *************************************************************************
 *************************************************************************)
class virtual ['atom, 'codeBank] cachingRepresentation = object (self) 
  inherit ['atom] representation 

 
   (***********************************
   * State variables
   ***********************************)

  val virtual weighted_path : 'codeBank ref
  val virtual codeBank : 'codeBank ref


  (***********************************
   * Methods that must be provided
   * by a subclass. 
   ***********************************)

  method virtual internal_test_case : 
    string -> (* exename *) 
    string -> (* source name *) 
    test -> (* test case *) 
    (bool * (* passed? *) 
     float array) (* real-valued fitness, or 1.0/0.0 *) 

  method virtual get_compiler_command : unit -> string 

  method virtual instrument_fault_localization : 
    string -> (* coverage source name *) 
    string -> (* coverage exe name *) 
    string -> (* coverage data out name *) 
    unit 

  method virtual internal_compute_source_buffers : 
    unit -> 
    (((string option) * string) list)  (* (filename, contents) list *) 
  (* 
   * For single-file representations, the filename string option should
   * be "None" and the list should be one element long. For multi-file
   * representations, each string option should be Some(Individual_Name).
   *)

  (***********************************
   * State Variables
   ***********************************)
  val already_source_buffers = ref None (* cached file contents from
                                  * internal_compute_source_buffers *) 
  val already_sourced = ref None (* list of filenames on disk containing
                                  * the source code *) 
  val already_digest = ref None  (* list of Digest.t. Use #compute_digest 
                                  * to access. *)  
  val already_compiled = ref None (* ".exe" filename on disk *) 
  val source_file = ref "" 
  val history = ref [] 

  (***********************************
   * Methods
   ***********************************)
  method source_name = begin
    match !already_sourced with
    | Some(source_names) -> source_names
    | None -> [] 
  end 


  method compute_source_buffers () = 
    match !already_source_buffers with
    | Some(sbl) -> sbl
    | None -> begin 
      let result = self#internal_compute_source_buffers () in
      already_source_buffers := Some(result) ;
      result 
    end 

  method output_source source_name = 
    let sbl = self#compute_source_buffers () in 
    match sbl with
	(* CLG: compute_source_buffers used to return None for the filename for single
	   CilRep instances; now that we don't distinguish b/w single and multi for
	   CilRep, everything returns a filename, but in the single case we can ignore
	   it *)
    | [(_,source_string)] -> 
      let fout = open_out source_name in
		output_string fout source_string ;
		close_out fout ;
		already_sourced := Some([source_name]) 

    | many_files -> begin
      let source_dir,_,_ = split_base_subdirs_ext source_name in 
      let many_files = List.map (fun (source_name,source_string) -> 
        let source_name = match source_name with
          | Some(source_name) -> source_name
          | None -> debug "ERROR: rep: output_source: multiple files, one of which does not have a name\n" ; exit 1
        in 
          source_name, source_string
      ) many_files in 
		List.iter (fun (source_name,source_string) -> 
          let full_output_name = Filename.concat source_dir source_name in
			ensure_directories_exist full_output_name ; 
			let fout = open_out full_output_name in
			  output_string fout source_string ;
			  close_out fout ;
		) many_files;
		already_sourced := Some(lmap (fun (sname,_) -> sname) many_files)
    end 

  (* We compute a Digest (Hash) of a variant by
   * running MD5 on what its source would look
   * like in memory. *) 
  method compute_digest () = 
    match !already_digest with
    | Some(digest_list) -> digest_list 
    | None -> begin
      let source_buffers = self#compute_source_buffers () in 
      let digest_list = List.map (fun (fname,str) -> 
        Digest.string str) source_buffers in
      already_digest := Some(digest_list) ;
      digest_list 
    end 

  method cleanup () = begin
    if not !always_keep_source then begin
	  (match !already_sourced with
		Some(source_names) ->
		  liter 
			(fun sourcename -> try Unix.unlink sourcename with _ -> ())
			source_names
	  | None -> ());
	  (match !already_compiled with
		Some(exe_name, _) -> (try Unix.unlink exe_name with _ -> ())
		| None -> ());
	  if !use_subdirs then begin
		let subdir_name = sprintf "./%06d" (!test_counter - 1) in
		  ignore(Unix.system ("rm -rf "^subdir_name));
	  end
	end
  end

  method get_test_command () = 
    "__TEST_SCRIPT__ __EXE_NAME__ __TEST_NAME__ __PORT__ __SOURCE_NAME__ __FITNESS_FILE__ 1>/dev/null 2>/dev/null" 

  method copy () = 
    ({< history = ref !history ; 
        already_source_buffers = ref !already_source_buffers ; 
        already_sourced = ref !already_sourced ; 
        already_digest = ref !already_digest ;
        already_compiled = ref !already_compiled ; 
      >})

  method get_history () =
    !history

  (* indicate that cached information based on our AST structure
   * is no longer valid *) 
  method updated () = 
    already_compiled := None ;
    already_source_buffers := None ; 
    already_digest := None ; 
    already_sourced := None ; 
    () 

  (* Compile this variant to an executable on disk. *)
 method compile source_name exe_name = begin
(*	debug "FaultLocRep compile, source_name: %s, exe_name: %s\n" source_name exe_name;*)
    let base_command = 
      match !compiler_command with 
      | "" -> self#get_compiler_command () 
      |  x -> x
    in
    let cmd = Global.replace_in_string base_command 
      [ 
        "__COMPILER_NAME__", !compiler_name ;
        "__EXE_NAME__", exe_name ;
        "__SOURCE_NAME__", source_name ;
        "__COMPILER_OPTIONS__", !compiler_options ;
      ] 
    in 
    let result = (match Stats2.time "compile" Unix.system cmd with
    | Unix.WEXITED(0) -> 
        already_compiled := Some(exe_name,source_name) ; 
        true
    | _ -> 
        already_compiled := Some("",source_name) ; 
        debug "\t%s %s fails to compile\n" source_name (self#name ()) ; 
        incr compile_failures ;
        false 
    ) in
      result
  end 

  (* An internal method for the raw running of a test case.
   * This does the bare bones work: execute the program
   * on the test case. No caching at this level. *)
  method internal_test_case exe_name source_name test = begin
    let port_arg = Printf.sprintf "%d" !port in
    change_port () ; 
    let base_command = 
      match !test_command with 
      | "" -> self#get_test_command () 
      |  x -> x
    in
    let fitness_file = exe_name ^ ".fitness" in 
    let cmd = Global.replace_in_string base_command 
      [ 
        "__TEST_SCRIPT__", !test_script ;
        "__EXE_NAME__", exe_name ;
        "__TEST_NAME__", (test_name test) ;
        "__SOURCE_NAME__", (source_name) ;
        "__FITNESS_FILE__", (fitness_file) ;
        "__PORT__", port_arg ;
      ] 
    in 
    let real_valued = ref [| 0. |] in 
    let result = 
      match Stats2.time "test" Unix.system cmd with
      | Unix.WEXITED(0) -> (real_valued := [| 1.0 |]) ; true 
      | _ -> (real_valued := [| 0.0 |]) ; false
    in 
    (try
      let str = file_to_string fitness_file in 
      let parts = Str.split (Str.regexp "[, \t\r\n]+") str in 
      let values = List.map (fun v ->
        try 
          float_of_string v 
        with _ -> begin 
          debug "%s: invalid\n%S\nin\n%S" 
            fitness_file v str ;
          0.0
        end
      ) parts in
      (*
      debug "internal_test_case: %s" (self#name ()) ; 
      List.iter (fun x ->
        debug " %g" x
      ) values ;
      debug "\n" ; 
      *) 
      if values <> [] then 
        real_valued := Array.of_list values 
    with _ -> ()) ;
    (if not !always_keep_source then 
	  (* I'd rather this goes in cleanup() but it's not super-obvious how *)
		try Unix.unlink fitness_file with _ -> ());
    (* return the results *) 
    result, !real_valued
  end 

  (* Perform various sanity checks. Currently we check to
   * ensure that that original program passes all positive
   * tests and fails all negative tests. *) 
  method sanity_check () = begin
    debug "cachingRepresentation: sanity checking begins\n" ; 
    let subdir = add_subdir (Some("sanity")) in 
    let sanity_filename = Filename.concat subdir (sanity_filename
      ^ "." ^ !Global.extension ^ !Global.suffix_extension) in 
    let sanity_exename = Filename.concat subdir sanity_exename in 
      self#output_source sanity_filename ; 
    let c = self#compile sanity_filename sanity_exename in
    if not c then begin
      debug "cachingRepresentation: %s: does not compile\n" sanity_filename ;
      if not !allow_sanity_fail then 
        exit 1 
    end ; 
    for i = 1 to !pos_tests do
      let r, g = self#internal_test_case sanity_exename sanity_filename 
        (Positive i) in
      debug "\tp%d: %b (%s)\n" i r (float_array_to_str g) ;
      assert(!allow_sanity_fail || r) ; (* Yam, if you need this to be
      commented out, do it on your local copy and/or add a new flag *) 
    done ;
    for i = 1 to !neg_tests do
      let r, g = self#internal_test_case sanity_exename sanity_filename 
        (Negative i) in
      debug "\tn%d: %b (%s)\n" i r (float_array_to_str g) ;
	assert(!allow_sanity_fail || (not r)) ; 
    done ;
    debug "cachingRepresentation: sanity checking passed\n" ; 
  end 

  (* This is our public interface for running a single test case.
   * It checks in the cache, compiles this to an EXE if  
   * needed, and runs the EXE on the test case. *) 
  method test_case test = 
    let digest_list = self#compute_digest () in 
    try begin
    let try_cache () = 
      (* first, maybe we'll get lucky with the persistent cache *) 
      match test_cache_query digest_list test with
      | Some(x,f) -> raise (Test_Result (x,f))
      | _ -> ()
    in 
    try_cache () ; 
    (* second, maybe we've already compiled it *) 
    let exe_name, source_name, worked = match !already_compiled with
    | None -> (* never compiled before, so compile it now *) 
      let subdir = add_subdir None in 
      let source_name = Filename.concat subdir
        (sprintf "%06d.%s%s" !test_counter !Global.extension 
          !Global.suffix_extension) in  
      let exe_name = Filename.concat subdir
        (sprintf "%06d" !test_counter) in  
      incr test_counter ; 
      if !test_counter mod 10 = 0 && not !no_test_cache then begin
        test_cache_save () ;
      end ; 
      self#output_source source_name ; 
      try_cache () ; 
      if not (self#compile source_name exe_name) then 
        exe_name,source_name,false
      else
        exe_name,source_name,true

    | Some("",source) -> "", source, false (* it failed to compile before *) 
    | Some(exe,source) -> exe, source, true (* compiled successfully before *) 
    in
    let result = 
      if worked then begin 
        (* actually run the program on the test input *) 
        self#internal_test_case exe_name source_name test 
      end else false, [| 0.0 |] 
    in 
    (* record result for posterity in the cache *) 
    test_cache_add digest_list test result ;
    raise (Test_Result(result))

  end with
    | Test_Result(x) -> (* additional bookkeeping information *) 
      Hashtbl.replace tested (digest_list,test) () ;
      x

  (* give a "descriptive" name for this variant. For most, the name is
   * based on the atomic mutations applied in order. Those are stored
   * in the "history" list. *) 
  method name () = 
    if !history = [] then "original"
    else begin 
      let b = Buffer.create 40 in
      ignore (List.rev_map (fun s ->
        Buffer.add_string b s ; () 
      ) !history) ;
      Buffer.contents b 
    end 

  method hash () = Hashtbl.hash self#name 

  method add_name_note str = history := str :: !history 

  method delete stmt_id = 
    self#updated () ; 
    history := (sprintf "d(%d)" stmt_id) :: !history 

  method append x y = 
    self#updated () ; 
    history := (sprintf "a(%d,%d)" x y) :: !history 

  method append_sources x = 
	lfoldl
	  (fun weightset ->
		fun (i,w) ->
		  WeightSet.add (i,w) weightset) (WeightSet.empty) !codeBank

  method swap_sources x = 
	lfoldl
	  (fun weightset ->
		fun (i,w) ->
		  WeightSet.add (i,w) weightset)
	  (WeightSet.empty) !codeBank

  method swap x y =
    self#updated () ; 
    history := (sprintf "s(%d,%d)" x y) :: !history 

  method put x y = 
    self#updated () ;
    (if !debug_put then 
      history := (sprintf "p(%d)" (x)) :: !history ;
    ) 

  method note_replaced_subatom x y atom =  
    self#updated () ;
    history := (sprintf "e(%d,%d,%s)" x y (self#atom_to_str atom)) :: !history 

end 


(*
 * We may want to turn
 *  1, 5
 *  2, 3
 *  2, 3
 *  3, 10
 *
 * into
 *  1, 5
 *  2, 6
 *  3, 10 
 *)
let flatten_weighted_path wp = 
  let seen = Hashtbl.create 255 in
  let id_list = List.fold_left (fun acc (sid,v) ->
    try
      let v_so_far = Hashtbl.find seen sid in
      let v_new = match !flatten_path with
      | "min" -> min v_so_far v  
      | "max" -> max v_so_far v
      | "sum" | _ -> v_so_far +. v
      in 
      Hashtbl.replace seen sid v_new ;
      acc 
    with Not_found ->
      sid :: acc) [] wp in  
  let id_list = List.rev id_list in 
  List.map (fun sid ->
    sid, Hashtbl.find seen sid
  ) id_list 

let faultlocRep_version = "4" 

(*************************************************************************
 *************************************************************************

                   virtual class FAULTLOCREPRESENTATION

   This interface for a program representaton handles various
   simple fault localization (i.e., "weighted path") approaches
   for you. 
   
   This is currently a good class to inherit your representation
   from. 
  
 *************************************************************************
 *************************************************************************)
class virtual ['atom] faultlocRepresentation = object (self) 
  inherit ['atom, (atom_id * float) list] cachingRepresentation as super 

  (***********************************
   * State Variables
   ***********************************)

  val weighted_path = ref []
  val codeBank = ref []

  (***********************************
   * No Subatoms 
   * (subclasses can override)
   ***********************************)
  method subatoms = false
  method get_subatoms = failwith "get_subatoms" 
  method replace_subatom = failwith "replace_subatom" 
  method replace_subatom_with_constant = failwith "replace_subatom_with_constant" 

  (***********************************
   * Methods
   ***********************************)
	(* POST CONDITION: adds atom_ids to code_bank *)
  method virtual load_oracle : string -> unit

  method virtual atom_id_of_source_line : string -> int -> atom_id 

  method save_binary ?out_channel (filename : string) = begin
    let fout = 
      match out_channel with
      | Some(v) -> v
      | None -> assert(false); 
    in 
    Marshal.to_channel fout (faultlocRep_version) [] ; 
    Marshal.to_channel fout (!weighted_path) [] ;
    Marshal.to_channel fout (!codeBank) [] ;
    debug "faultlocRep: %s: saved\n" filename ; 
  end 

  method load_binary ?in_channel (filename : string) = begin
    let fin = 
      match in_channel with
      | Some(v) -> v
      | None -> assert(false); 
    in 
    let version = Marshal.from_channel fin in
    if version <> faultlocRep_version then begin
      debug "faultlocRep: %s has old version\n" filename ;
      failwith "version mismatch" 
    end ;
    weighted_path := Marshal.from_channel fin ; 
    codeBank := Marshal.from_channel fin ; 
    debug "faultlocRep: %s: loaded\n" filename ; 
  end 

  (* Compute the fault localization information. *)

  (* get_coverage helps compute_localization by running the instrumented code *)

  method get_coverage coverage_sourcename coverage_exename coverage_outname = 
    (* 
     * Traditional "weighted path" or "set difference" or
     * Reiss-Renieris fault localization involves finding all of the
     * statements visited while executing the negative test case(s)
     * and removing all statements visited while executing the positive
     * test case(s). 
     *)
    let fix_path = if !use_full_paths then 
        Filename.concat (Unix.getcwd()) !fix_path else !fix_path 
    in
    let fault_path = if !use_full_paths then 
        Filename.concat (Unix.getcwd()) !fault_path else !fault_path 
    in
      
    (* We run the negative test case first to find the statements covered. *) 
      
    let res, _ = 
      self#internal_test_case coverage_exename coverage_sourcename 
	(Negative 1) 
    in 
      if res then begin 
	debug "ERROR: coverage PASSES test Negative 1\n" ;
	if not !allow_coverage_fail then exit 1 
      end ;
      Unix.rename coverage_outname fault_path;
      
    (* For simplicity, we sometimes only run one positive test case to
     * compute coverage. Running them all is more precise, but also takes
     * longer. *) 
      let max_positive = if !pick_positive_path then !pos_tests else 1 in
	debug "faultLocRep: get_coverage: max_positive: %d\n" max_positive;
	
	let pos_files = ref [] in
	  for pos_test = 1 to max_positive do
	    let res, _ = 
              self#internal_test_case coverage_exename coverage_sourcename 
		(Positive pos_test) 
	    in
	      if not res then begin 
		debug "ERROR: coverage FAILS test Positive 1 (coverage_exename=%s)\n" 
		  coverage_exename ;
		if not !allow_coverage_fail then exit 1 
	      end ;
	      
	      let path_name = Printf.sprintf "%s%d" fix_path pos_test in
		try 
		  Unix.rename coverage_outname path_name;
		  pos_files := path_name :: !pos_files
		with _ -> ()
	  done; (* end of: for all desired positive tests *) 
	  
    (* this is slightly inefficient because it requires reading in the
     * ultimate path files twice, but CLG [FIXME: Who?] thinks it's OK since
     * we don't intend to do this step more than once/on the cloud *)

    (* Pair each positive test coverage filename with the set of
     * atoms its covers. *) 
	  let best_file = 
	    if !pick_positive_path then begin
	      let all_pos_paths = lmap
		(fun path_file ->
		  path_file,
		  lfoldl 
		    (fun stmt_set ->
		      fun item ->
			IntSet.add (my_int_of_string item) stmt_set) 
		    (IntSet.empty)
		    (get_lines path_file)
		) !pos_files
	      in
	      let neg_path = 
		lfoldl 
		  (fun stmt_set ->
		    fun item ->
	(* CLG: bizarrely, if you do an lmap my_int_of_string on (get_lines fault_path), 
	 * repair segfaults.  If you do it in here, no problems *)
		      IntSet.add (my_int_of_string item) stmt_set)
		  (IntSet.empty)
		  (get_lines fault_path)
	      in
    (* 
     * Currently (Sun Jul 10 22:34:15 EDT 2011), we appear to find
     * the positive test case that visits the greatest number of
     * statements and simply use that. Weimer notes that this is _not_
     * what we did in ICSE'09 -- instead, we "should" _union_ together
     * everything visited on all of the positive tests, paying careful
     * attention to the weighting. 
     *) 
		fst
		(lfoldl
		  (fun (best_file,best_size) ->
		    fun (file,set) ->
		      let this_size = IntSet.cardinal (IntSet.inter set neg_path) in
			if this_size > best_size then file,this_size
			else best_file,best_size) ("",(-1)) all_pos_paths)
	    end else Printf.sprintf "%s%d" fix_path max_positive
    in
    try
	  if not (Sys.file_exists best_file) then 
		ignore(Unix.system (Printf.sprintf "touch %s\n" fix_path))
	  else begin
		let cmd = Printf.sprintf "cp %s %s" best_file fix_path in
		  ignore(Unix.system cmd)
	  end
    with _ -> begin
      debug "WARNING: faultLocRep: no positive path generated by the test case, positive path will be empty (probably not a win).\n";
      (* in all likelihood, the positive test case(s) doesn't/don't touch the 
       file in question, which is probably bad. *)
      let cmd = Printf.sprintf "touch %s\n" fix_path in
      ignore(Unix.system cmd)
    end
	(* now we have a positive path and a negative path *) 

  (*
   * compute_localization should product fault and fix localization sets
   * for use by later mutation operators. This is typically done by running
   * the program to find the atom coverage on the positive and negative
   * test cases, but there are other schemes: 
   *
   *  path      --- default 'weight path' localization
   *  uniform   --- all atoms in the program have uniform 1.0 weight
   *  line      --- an external file specifies a list of source-code
   *                line numbers; the corresponding atoms are used
   *  weight    --- an external file specifies a weighted list of
   *                atoms
   *  oracle    --- for fix localization, an external file specifies
   *                source code (e.g., repair templates, human-written
   *                repairs) that is used as a source of possible fixes
   *)
  method compute_localization () =
	debug "faultLocRep: compute_localization: fault_scheme: %s, fix_scheme: %s\n" 
	  !fault_scheme !fix_scheme;
	
	(* check legality *)
	(match !fault_scheme with 
	  "path" | "uniform" | "line" | "weight" -> ()
	| "default" -> fault_scheme := "path" 
	| _ -> 	failwith (Printf.sprintf "faultLocRep: Unrecognized fault localization scheme: %s\n" !fault_scheme));
	
	if !fix_oracle_file <> "" then fix_scheme := "oracle";
	(match !fix_scheme with
	  "path" | "uniform" | "line" | "weight" | "default" -> ()
	| "oracle" -> assert(!fix_oracle_file <> "" && !fix_file <> "")
	| _ -> failwith (Printf.sprintf "faultLocRep: Unrecognized fix localization scheme: %s\n" !fix_scheme));
	
	let fix_weights_to_lst ht = 
      let res = ref [] in 
		hiter (fun stmt_id weight  ->
		  res := (stmt_id,weight) :: !res 
		) ht; !res
	in
	let uniform lst = 
	  let lst = uniq lst in
		lmap (fun (i,_) -> (i, 1.0)) lst 
	in
	  
	(* 
	 * Default "ICSE'09"-style fault and fix localization from path files. 
	 * The weighted path fault localization is a list of <atom,weight>
	 * pairs. The fix weights are a hash table mapping atom_ids to
	 * weights. 
	 *)
	let compute_weighted_path_and_fix_weights_from_path_files () = 
	  let fw = hcreate 10 in
		if (llen !codeBank) == 0 then begin
		  debug "WARNING: faultLocRep: codeBank is empty\n" 
		end ; (* CLAIRE SAYS FIX ME *)
		liter (fun (i,_) -> Hashtbl.replace fw i 0.1) !weighted_path;
		let neg_ht = Hashtbl.create 255 in 
		let pos_ht = Hashtbl.create 255 in 
		  iter_lines !fix_path
			(fun line ->
			  Hashtbl.replace pos_ht line () ;
			  Hashtbl.replace fw (int_of_string line) 0.5);
		  lfoldl
			(fun (wp,fw) ->
			  fun line ->
				if not (Hashtbl.mem neg_ht line) then
				  begin 
					  (* a statement only on the negative path gets weight 1.0 ;
					   * if it is also on the positive path, its weight is 0.1 *) 
					let weight = if Hashtbl.mem pos_ht line then 0.1 else 1.0 in 
					  Hashtbl.replace neg_ht line () ; 
					  Hashtbl.replace fw (int_of_string line) 0.5 ; 
					  (int_of_string line,weight) :: wp, fw
					end
				  else wp,fw) ([],fw) (get_lines !fault_path)
	in
	(* 
	 * Process a special user-provided file to obtain a list of <atom,weight>
	 * pairs. The input format is a list of "file,stmtid,weight" tuples. You
	 * can separate with commas and/or whitespace. If you leave off the
	 * weight, we assume 1.0. You can leave off the file as well. 
	 *) 
	let process_line_or_weight_file fname scheme =
	  let regexp = Str.regexp "[ ,\t]" in 
	  let fix_weights = hcreate 10 in 
		liter (fun (i,_) -> Hashtbl.replace fix_weights i 0.1) !codeBank;
		let weighted_path = ref [] in 
		  liter (fun line -> 
			let s, w, file = 
			  match (Str.split regexp line) with
			  | [stmt] -> (int_of_string stmt), 1.0, ""
			  | [stmt ; weight] -> 
				(try
				   (my_int_of_string stmt), (float_of_string weight), ""
				 with _ -> my_int_of_string weight,1.0,stmt)
			  | [file ; stmt ; weight] -> (my_int_of_string stmt), 
                (float_of_string weight), file
			  | _ -> debug "ERROR: faultLocRep: compute_localization: %s: malformed line:\n%s\n" !fault_file line;
				failwith "malformed input"
			in 
      (* In the "line" scheme, the file uses source code line numbers 
       * (rather than atom-ids). In such a case, we must convert them to
       * atom-ids. *) 
			let s = 
			  if scheme = "line" then self#atom_id_of_source_line file s else s
			in
			  if s >= 1 then begin 
				Hashtbl.replace fix_weights s 0.5; 
				weighted_path := (s,w) :: !weighted_path
			  end 
		  ) (get_lines fname);
		  lrev !weighted_path, fix_weights
	in
	  
	(* set_fault and set_fix set the codebank and change location atomsets to
	 * contain the atom_ids of the actual code in the weighted path or in the
	 * fix localization set.  Set_fault is currently unecessary but in case the
	 * correctness of weighted_path becomes relevant I'm [Claire] going to
	 * implement it now to save the hassle of forgetting that it needs to be.
	 *)
	let set_fault wp = weighted_path := wp in
	let set_fix lst = codeBank := lst in

	  if !fault_scheme = "path" || 
		!fix_scheme = "path" || 
		!fix_scheme = "default" then begin
		(* If the fault path file is missing, or if the fix path file
		 * is missing, or if we've been asked to regenerate this information,
		 * we'll go compute it. *) 
		  if (not ((Sys.file_exists !fault_path) && 
					  (Sys.file_exists !fix_path))) || !regen_paths then begin
		  (* instrument for coverage if necessary *)
 			let subdir = add_subdir (Some("coverage")) in 
			let coverage_sourcename = Filename.concat subdir 
			  (coverage_sourcename ^ "." ^ !Global.extension ^
				 !Global.suffix_extension) in 
			let coverage_exename = Filename.concat subdir coverage_exename in 
			let coverage_outname = Filename.concat subdir !coverage_outname in
			let coverage_outname = if !use_full_paths then 
				Filename.concat (Unix.getcwd()) coverage_outname 
			  else coverage_outname in
			  self#instrument_fault_localization 
				coverage_sourcename coverage_exename coverage_outname ;
			  if not (self#compile coverage_sourcename coverage_exename) then 
				begin
				  debug "ERROR: faultLocRep: compute_localization: cannot compile %s\n" 
					coverage_sourcename ;
				  exit 1 
				end ;
			  self#get_coverage coverage_sourcename coverage_exename coverage_outname
		  end;
		  
		(* At this point the relevant path files definitely exist -- 
		 * because we're reusing them, or because we recomputed them above. *) 
		  let wp, fw = compute_weighted_path_and_fix_weights_from_path_files () in
			if !fault_scheme = "path" then 
			  set_fault (lrev wp);
			if !fix_scheme = "path" || !fix_scheme = "default" then 
			  set_fix (fix_weights_to_lst fw)
		end; (* end of: "path" fault or fix *) 
	  
	  (* Handle "uniform" fault or fix localization *) 
	  weighted_path :=
		if !fault_scheme = "uniform" then uniform !weighted_path
		else !weighted_path;
	  codeBank :=
		if !fix_scheme = "uniform" then uniform !codeBank 
		else !codeBank;
	  
	  (* Handle "line" or "weight" fault localization *) 
	  if !fault_scheme = "line" || !fault_scheme = "weight" then begin
		let wp,fw = process_line_or_weight_file !fault_file !fault_scheme in 
		  set_fault wp;
		  if !fix_scheme = "default" then 
			set_fix (fix_weights_to_lst fw)
	  end;
	  
  (* Handle "line" or "weight" fix localization *) 
	  if !fix_scheme = "line" || !fix_scheme = "weight" then 
		set_fix (fst (process_line_or_weight_file !fix_file !fix_scheme))
		  
  (* Handle "oracle" fix localization *) 
	  else if !fix_scheme = "oracle" then begin
		self#load_oracle !fix_oracle_file;
		set_fix (fst (process_line_or_weight_file !fix_file "line"));
	  end;

  (* CLG: if I did this properly, weighted_path should already be
   * reversed *)
	  if !flatten_path <> "" then 
		weighted_path := flatten_weighted_path !weighted_path
  (*	  debug "weighted_path:\n";
		  liter (fun (s,w) -> debug "%d: %g\n" s w) !weighted_path;
		  debug "fix localization:\n";
		  liter (fun (s,w) -> debug "%d: %g\n" s w) !fix_weights*)

  method get_fault_localization () = !weighted_path

  method get_fix_localization () = !codeBank

end 

let global_filetypes = ref ([] : (string * (unit -> unit)) list)

