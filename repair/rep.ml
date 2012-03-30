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
 *  -> predicates
 *  -> asm 
 *
 *)
open Printf
open Global
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
module AtomMap = IntMap

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

(* Ref: referenced in the hole referenced by the integer *)
type hole_type = Stmt_hole | Exp_hole | Lval_hole
type constraints =  Fault_path | Fix_path | Ref of string | InScope of string

module OrderedConstraint = 
struct
  type t = constraints
  let compare c1 c2 = 
	if c1 = c2 then 0 else 
	  match c1,c2 with
	  | Fault_path,_ -> -1
	  | _,Fault_path -> 1
	  | Fix_path,_ -> -1
	  | _,Fix_path -> 1
	  | Ref(i1),Ref(i2)
	  | Ref(i1),InScope(i2)
	  | InScope(i1),Ref(i2)
	  | InScope(i1),InScope(i2) -> compare i1 i2
end

module ConstraintSet = Set.Make(OrderedConstraint)

type hole = hole_type * ConstraintSet.t

module OrderedHole =
struct 
  type t = hole
  let compare h1 h2 =
	match h1, h2 with
	  (ht1,cons1),(ht2,cons2) ->
		  match ht1,ht2 with
			Stmt_hole, Stmt_hole
		  | Exp_hole, Exp_hole
		  | Lval_hole, Lval_hole -> compare cons1 cons2
		  | Stmt_hole, Exp_hole
		  | Stmt_hole, Lval_hole
		  | Exp_hole, Lval_hole -> 1
		  | Exp_hole, Stmt_hole
		  | Lval_hole,Stmt_hole
		  | Lval_hole, Exp_hole -> -1
end

module HoleSet = Set.Make(OrderedHole)

type filled = hole_type * atom_id * atom_id option

type hole_info =
	{
	  hole_id : string;
	  htyp : hole_type;
	  constraints : ConstraintSet.t
	}

type 'atom template = 
	{
	  template_name : string;
	  hole_constraints : hole_info StringMap.t;
	  hole_code_ht : (string, 'atom) Hashtbl.t
	}

type mutation_id = Delete_mut | Append_mut | Swap_mut | Replace_mut | Template_mut of string

type 'atom edit_history = 
  | Template of string * filled StringMap.t
  | Delete of atom_id 
  | Append of atom_id * atom_id
  | Swap of atom_id * atom_id 
  | Replace of atom_id * atom_id
  | Replace_Subatom of atom_id * subatom_id * 'atom 
  | Crossover of (atom_id option) * (atom_id option) 

type mutation = mutation_id * float

(** virtual abstract representation class type.  virtual means that there are
	methods without definitions, which will be defined in concrete subclasses.
	An instantiation of representation corresponds to an individual e.g., in a
	population.  Individuals at their most basic consist of a genome, or a list
	of genes.  Individual representations may mutate themselves using the
	builtin mutation operators/functions or using user-defined templates.
	Representations may be serialized or deserialized and constructed by setting
	a genome.  Representations may be of variable length or not (which
	influences crossover).  *)
class virtual 
	(** 'gene is the type of the individual genes that comprise an
		individual's genome, such as an edit operator or a C AST node.
		'code is the type of the manipulable source, such as a
		cilRep_atom *)
	['gene,'code] representation = object (self : 'self_type)

	  (** variable_length denotes whether individuals of this representation
		  have genomes of varying length.  This influences one-point
		  crossover *)
	  method virtual variable_length : bool

	  (** get_genome returns this individual's genome, which is a list of genes.
		  It may or may not be of fixed length *)
	  method virtual get_genome : unit -> 'gene list
	  method virtual load_genome_from_string : string -> unit

	  (** set_genome gs sets the genome of this individual to gs,
		  typically overwriting the previous genome. *)
	  method virtual set_genome : 'gene list -> unit

	  (** genome_length returns the length of the entire genome, which is not
		  necessarily just the length of the list (see stringrep) *)
	  method virtual genome_length : unit -> int

	  (** note_success is called when an individual is identified as a
		  successful variant, i.e., with maximum fitness.  For example,
		  note_success might call minimization code to minimize this
		  individual's difference from the original, or might output this
		  individual's source to a special file on disk *)
	  method virtual note_success : unit -> unit

	  (** returns a copy of this individual *)
	  method virtual copy : unit -> 'self_type

		
	  method virtual serialize : ?out_channel:out_channel -> ?global_info:bool -> string -> unit (* serialize to a disk file *)
	  method virtual deserialize : ?in_channel:in_channel -> ?global_info:bool -> string -> unit (* deserialize *)
	  method virtual debug_info : unit ->  unit (* print debugging information *) 
	  method virtual max_atom : unit -> atom_id  (* 1 to N -- INCLUSIVE *)
	  method virtual get_faulty_atoms : unit -> (atom_id * float) list 


	  (* maybe "load" should be called "initialize" and also deal with registering
		 mutations?  I hate separating the intialization into separate pieces like
		 this...*)
	  method virtual load : string -> unit
	  method virtual sanity_check : unit -> unit 
	  method virtual compute_localization : unit ->  unit 

	  method virtual from_source : string -> unit (* load from a .C or .ASM file, etc. *)
	  method virtual output_source : string -> unit (* save to a .C or .ASM file, etc. *)
	  method virtual source_name : string list (* is it already saved on the disk as a (set of) .C or .ASM files? *) 
	  method virtual cleanup : unit -> unit (* if not keeping source, delete by-products of fitness testing for this rep. *)

	  method virtual set_fitness : float -> unit (* record the fitness, particularly if it's from another source *)
	  method virtual fitness : unit -> float option (* get recorded fitness, if it exists *)

	  method virtual compile : string -> string -> bool 

	  (** test_case t returns a boolean value corresponding to whether
		  this variant passes the test case t and a floating point
		  number denoting the fitness.  fitness is usually 1.0 or 0.0
		  but may be arbitrary when single_fitness is used *)
	  method virtual test_case : test -> bool * (float array) 

	  method virtual test_cases : test list (* run many tests --
											   only relevant if "--fitness-in-parallel" exceeds 1 *) 
		  -> ((bool * float array) list) (* as "test_case", but many answers *) 


	  method virtual name : unit -> string (* a "descriptive" name for this variant *) 
	  method virtual get_history : unit -> ('code edit_history) list

	  (* add a "history" note to the variant's descriptive name *)
	  method virtual add_history : ('code edit_history) -> unit 
	  method virtual history_element_to_str : ('code edit_history) -> string  

  (* reduce search space modifies the fault localization based on an optional
	 space splitting function (probably based on the number of computers since
	 it's most likely to be called from the distributed algorithm as of now) and
	 whether we're doing proportional mutation (that's the second bool) *)
	  method virtual reduce_search_space : ((atom_id * float) -> bool) -> bool -> unit
	  (* used to tell the representation the default mutations that are allowed
		 according to search parameters *)
	  method virtual register_mutations : mutation list -> unit 
	  method virtual available_mutations : atom_id -> mutation list
	  method virtual available_crossover_points : unit -> IntSet.t * (IntSet.t -> IntSet.t -> int list)

  (* For arbitrary templates, just give name and the atom ids in order (for
	 history/patch rep) *)

	  method virtual get_template : string -> 'code template
	  method virtual load_templates : string -> unit
	  method virtual apply_template : 'code template -> filled StringMap.t -> unit
	  (* FIXME: I'm not liking the return value for template available mutations, it's unecessarily complicated... *)
	  method virtual template_available_mutations : atom_id -> ('code template * float * filled StringMap.t) list
	  (* atomic mutation operators *) 

	  method virtual delete : atom_id -> unit 
	  (* append, swap, and replace find 'what to append' by looking in the code
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

	  method virtual replace : atom_id -> atom_id -> unit 

	  method virtual replace_sources : 
    (* replace with what *) atom_id -> 
    (* possible replace sources *) WeightSet.t 


	  (* Subatoms.
	   * Some representations support a finer-grain than the atom, but still
	   * want to perform crossover and mutation at the atom level. For example,
	   * C ASTs might have atoms (stmts) and subatoms (expressions). One
	   * might want to change expressions, but that complicates crossover
	   * (because the number of subatoms may change between variants). So
	   * instead we still perform crossover on atoms, but allow sub-atom
	   * changes. *) 
	  method virtual subatoms : bool (* are they supported? *) 
	  (* replace the atom here with "subatom_id list" *)
	  method virtual get_subatoms : atom_id -> ('code list)
	  method virtual replace_subatom : atom_id -> subatom_id -> 'code -> unit 
	  method virtual replace_subatom_with_constant : atom_id -> subatom_id -> unit 
	  method virtual note_replaced_subatom : atom_id -> subatom_id -> 'code -> unit

	  (* For debugging. *) 
	  method virtual atom_to_str : 'code -> string 

	  method virtual hash : unit -> int 
(* Hashcode. Equal variants must have equal hash codes, but equivalent
   variants need not. By default, this is a hash of the history. 
   Claire wonders: Why the history and not the genome? *) 
end 


(*
 * This is a list of variables representing global options related to
 * representations. 
 *)

let coverage_sourcename = "coverage" 
let coverage_exename = "coverage" 
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
let port = ref 808
let no_test_cache = ref false
let no_rep_cache = ref false 
let allow_coverage_fail = ref false 

let regen_paths = ref false
let recompute_path_weights = ref false
 
let fault_scheme = ref "path"
let fault_path = ref "coverage.path.neg"
let fault_file = ref ""

let fix_scheme = ref "default"
let fix_path = ref "coverage.path.pos"
let fix_file = ref ""
let fix_oracle_file = ref ""
let coverage_info = ref ""

let nht_server = ref "" 
let nht_port = ref 51000
let nht_id = ref "global" 
let fitness_in_parallel = ref 1 

let negative_path_weight = ref 1.0
let positive_path_weight = ref 0.1

let rep_cache_file = ref ""

let templates = ref ""

let sanity = ref "xDefault"
let _ =
  options := !options @
  [
	"--sanity", Arg.Set_string sanity, "Do sanity. Options: \"yes\" or \"no\".  Default: yes if no preexisting rep cache, no otherwise.";
    "--no-rep-cache", Arg.Set no_rep_cache, " do not load representation (parsing) .cache file" ;
	"--templates", Arg.Set_string templates, " Use repair templates; read from file X.  Default: none";
	"--neg-weight", Arg.Set_float negative_path_weight, " weight to give statements only on the negative path. Default: 1.0";
	"--pos-weight", Arg.Set_float positive_path_weight, " weight to give statements on both the positive and the negative paths. Default: 0.1";
    "--fitness-in-parallel", Arg.Set_int fitness_in_parallel, "X allow X fitness evals for 1 variant in parallel";
    "--keep-source", Arg.Set always_keep_source, " keep all source files";
    "--test-command", Arg.Set_string test_command, "X use X as test command";
    "--test-script", Arg.Set_string test_script, "X use X as test script name";
    "--compiler", Arg.Set_string compiler_name, "X use X as compiler";
    "--compiler-command", Arg.Set_string compiler_command, "X use X as compiler command";
    "--compiler-opts", Arg.Set_string compiler_options, "X use X as options";
    "--label-repair", Arg.Set label_repair, " indicate repair locations";
    "--flatten-path", Arg.Set_string flatten_path, "X flatten weighted path (sum/min/max)";
    "--allow-coverage-fail", Arg.Set allow_coverage_fail, " allow coverage to fail its test cases" ;
    "--regen-paths", Arg.Set regen_paths, " regenerate path files";
	"--recompute-weights", Arg.Set recompute_path_weights, " recompute the path weighting scheme; for use with neg-weight and pos-weight";
	
    "--fault-scheme", Arg.Set_string fault_scheme, " How to do fault localization.  Options: path, uniform, line, weight. Default: path";
    "--fault-path", Arg.Set_string fault_path, "Negative path file, for path-based fault or fix localization.  Default: coverage.path.neg";
    "--fault-file", Arg.Set_string fault_file, " Fault localization file.  e.g., Lines/weights if scheme is lines/weights.";

    "--fix-scheme", Arg.Set_string fix_scheme, " How to do fix localization.  Options: path, uniform, line, weight, oracle, default (whatever Wes was doing before). Default: default";
    "--fix-path", Arg.Set_string fix_path, "Positive path file, for path-based fault or fix localization. Default: coverage.path.pos";
    "--fix-file", Arg.Set_string fix_file, " Fix localization information file, e.g., Lines/weights.";
    "--fix-oracle", Arg.Set_string fix_oracle_file, " List of source files for the oracle fix information.";
    "--coverage-info", Arg.Set_string coverage_info, " Collect and print out suite coverage info to file X";
    "--rep-cache", Arg.Set_string rep_cache_file, " X rep cache file.  Default: base_name.cache.";
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
type test_case_preparation_result =
  | Must_Run_Test of (Digest.t list) * string * string * test 
  | Have_Test_Result of (Digest.t list) * (bool * float array) 

let add_subdir str = 
  let result = 
    if not !use_subdirs then
      "." 
    else begin
      let dirname = match str with
      | None -> sprintf "%06d" !test_counter
      | Some(specified) -> specified 
      in
		if Sys.file_exists dirname then begin
		  let cmd = "rm -rf ./"^dirname in
			try ignore(Unix.system cmd) with e -> ()
		end;
      (try Unix.mkdir dirname 0o755 with e -> ()) ;
      dirname 
    end 
  in
    Filename.concat (Unix.getcwd ()) result

let dev_null = Unix.openfile "/dev/null" [Unix.O_RDWR] 0o640 
let cachingRep_version = 1 

(*************************************************************************
 *************************************************************************

                    virtual class CACHINGREPRESENTATION

   This interface for a program representaton handles the caching
   of compilations and test case results for you, as well as 
   dealing with some sanity checks. 
  
 *************************************************************************
 *************************************************************************)
class virtual ['gene,'code] cachingRepresentation = object (self) 
  inherit ['gene, 'code] representation 

   (***********************************
   * Methods that must be provided
   * by a subclass. 
   ***********************************)
  method variable_length = true
  method load_genome_from_string str = failwith "load genome from string is not implemented"

  method available_crossover_points () =
	lfoldl
	  (fun accset ele ->
		IntSet.add ele accset) IntSet.empty 
	  (1 -- (self#genome_length())), (fun a b -> IntSet.elements a)

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
  val fitness = ref None (* if I already know it, don't bother checking the cache! *)

  val already_source_buffers = ref None (* cached file contents from
										 * internal_compute_source_buffers *) 
  val already_sourced = ref None (* list of filenames on disk containing
                                  * the source code *) 
  val already_digest = ref None  (* list of Digest.t. Use #compute_digest 
                                  * to access. *)  
  val already_compiled = ref None (* ".exe" filename on disk *) 

  val history = ref [] 

  (***********************************
   * Methods - Binary Serialization
   ***********************************)
  method note_success orig = ()

  method load base = begin
	let cache_file = if !rep_cache_file = "" then (base^".cache") else !rep_cache_file in
	let success = 
	  try 
		if !no_rep_cache then begin
		  if !sanity = "xDefault" then sanity := "yes";
		  false 
		end else begin
		  self#deserialize ?in_channel:None ~global_info:true cache_file; 
		  if !sanity = "xDefault" then sanity := "no";
		  true
		end
	  with _ -> false 
	in
	  if not success then 
		self#from_source !program_to_repair;
	  if !sanity = "yes" then 
        self#sanity_check () ; 
(*	  if (not success) || !regen_paths || !recompute_path_weights then
		self#compute_localization () ;*)
	  self#serialize ~global_info:true cache_file
  end

  (* serialize the state *) 
  method serialize ?out_channel ?global_info (filename : string) = begin
    let fout = 
      match out_channel with
      | Some(v) -> v
      | None -> open_out_bin filename 
    in 
      Marshal.to_channel fout (cachingRep_version) [] ; 
      debug "cachingRep: %s: saved\n" filename ; 
      if out_channel = None then close_out fout 
  end 

  (* load in serialized state *) 
  method deserialize ?in_channel ?global_info (filename : string) = begin
    let fin = 
      match in_channel with
      | Some(v) -> v
      | None -> open_in_bin filename 
    in 
    let version = Marshal.from_channel fin in
      if version <> cachingRep_version then begin
        debug "cachingRep: %s has old version\n" filename ;
        failwith "version mismatch" 
      end ;
      debug "cachingRep: %s: loaded\n" filename ; 
      if in_channel = None then close_in fin ;
  end 

  (***********************************
   * Methods
   ***********************************)

  method source_name = begin
    match !already_sourced with
    | Some(source_names) -> source_names
    | None -> [] 
  end 

  method set_fitness f = fitness := Some(f)
  method fitness () = !fitness

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
    (match sbl with
    | [(None,source_string)] ->
      let fout = open_out source_name in
		output_string fout source_string ;
		close_out fout ;
		already_sourced := Some([source_name]) 

    | many_files -> begin
      let source_dir,_,_ = split_base_subdirs_ext source_name in 
      let many_files = List.map (fun (source_name,source_string) -> 
        let source_name = match source_name with
          | Some(source_name) -> source_name
          | None -> abort "ERROR: rep: output_source: multiple files, one of which does not have a name\n" 
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
		already_sourced := Some(lmap (fun (sname,_) -> sname) many_files);
    end ) ; 
    () 

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
          source_names ;
        already_sourced := None ; 
      | None -> ()
      );
      (match !already_compiled with
        Some(exe_name, _) -> 
          (try Unix.unlink exe_name with _ -> ()) ;
          already_compiled := None ;
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
		fitness = ref !fitness ;
        already_source_buffers = ref !already_source_buffers ; 
        already_sourced = ref !already_sourced ; 
        already_digest = ref !already_digest ;
        already_compiled = ref !already_compiled ; 
      >})

  method get_history () = !history

  (* indicate that cached information based on our AST structure
   * is no longer valid *) 
  method updated () = 
	fitness := None ;
    already_compiled := None ;
    already_source_buffers := None ; 
    already_digest := None ; 
    already_sourced := None ; 
    () 

  (* Compile this variant to an executable on disk. *)
 method compile source_name exe_name = begin
(*	debug "FaultLocRep compile, source_name: %s, exe_name: %s\n" source_name exe_name; *)
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

  method private internal_test_case_command exe_name source_name test = begin
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
    cmd, fitness_file 
  end 

  (* This method is called after a test has been run and 
   * interprets its process exit status and any fitness files left on disk
   * to determine if that test passed or not. *) 
  method internal_test_case_postprocess status fitness_file = begin
    let real_valued = ref [| 0. |] in 
    let result = match status with 
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
      if values <> [] then 
        real_valued := Array.of_list values 
    with _ -> ()) ;
    (if not !always_keep_source then 
	  (* I'd rather this goes in cleanup() but it's not super-obvious how *)
		try Unix.unlink fitness_file with _ -> ());
    (* return the results *) 
    result, !real_valued
  end 

  (* An internal method for the raw running of a test case.
   * This does the bare bones work: execute the program
   * on the test case. No caching at this level. *)
  method internal_test_case exe_name source_name test = begin
    let cmd, fitness_file = 
      self#internal_test_case_command exe_name source_name test in 
    (* Run our single test. *) 
    let status = Stats2.time "test" Unix.system cmd in
    self#internal_test_case_postprocess status (fitness_file: string) 
  end 

  (* Perform various sanity checks. Currently we check to
   * ensure that that original program passes all positive
   * tests and fails all negative tests. *) 
  method sanity_check () = begin
    debug "cachingRepresentation: sanity checking begins\n" ; 
    let subdir = add_subdir (Some("sanity")) in 
    let sanity_filename = Filename.concat subdir
      sanity_filename ^ if (!Global.extension <> "")
      then "." ^ !Global.extension ^ !Global.suffix_extension
      else "" in 
    let sanity_exename = Filename.concat subdir sanity_exename in 
      self#output_source sanity_filename ; 
    let c = self#compile sanity_filename sanity_exename in
    if not c then begin
      debug "cachingRepresentation: %s: does not compile\n" sanity_filename ;
      abort "cachingRepresentation: sanity check failed (compilation)\n" 
    end ; 
    for i = 1 to !pos_tests do
      let r, g = self#internal_test_case sanity_exename sanity_filename 
        (Positive i) in
      debug "\tp%d: %b (%s)\n" i r (float_array_to_str g) ;
      if not r then
        abort "cachingRepresentation: sanity check failed (%s)\n"
          (test_name (Positive i)) 
    done ;
    for i = 1 to !neg_tests do
      let r, g = self#internal_test_case sanity_exename sanity_filename 
        (Negative i) in
      debug "\tn%d: %b (%s)\n" i r (float_array_to_str g) ;
      if not r then 
        abort "cachingRepresentation: sanity check failed (%s)\n"
          (test_name (Negative i)) 
    done ;
    debug "cachingRepresentation: sanity checking passed\n" ; 
  end 

  (* This helper method is associated with "test_case" -- 
   * It checks in the cache, compiles this to an EXE if  
   * needed, and indicates whether the EXE must be run on the test case. *) 
  method private prepare_for_test_case test : test_case_preparation_result = 
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
        (sprintf "%06d" !test_counter) ^ if (!Global.extension <> "")
        then "." ^ !Global.extension ^ !Global.suffix_extension
        else "" in  
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
    if worked then begin 
      (* actually run the program on the test input *) 
      Must_Run_Test(digest_list,exe_name,source_name,test) 
    end else ((Have_Test_Result(digest_list, (false, [| 0.0 |] ))))

  end with
    | Test_Result(x) -> (* additional bookkeeping information *) 
      Have_Test_Result(digest_list,x) 

  (* This is our public interface for running a single test case. *) 
  method test_case test = 
    let tpr = self#prepare_for_test_case test in
    let digest_list, result = 
      match tpr with
      | Must_Run_Test(digest_list,exe_name,source_name,test) -> 
        let result = self#internal_test_case exe_name source_name test in
        digest_list, result 
      | Have_Test_Result(digest_list,result) -> 
        digest_list, result 
    in 
    test_cache_add digest_list test result ;
    Hashtbl.replace tested (digest_list,test) () ;
    result 

  method test_cases tests =
    if !fitness_in_parallel <= 1 || (List.length tests) < 2 then 
      (* If we're not going to run them in parallel, then just run them
       * sequentially in turn. *) 
      List.map (self#test_case) tests
    else if (List.length tests) > !fitness_in_parallel then begin
      let first, rest = split_nth tests !fitness_in_parallel in
      (self#test_cases first) @ (self#test_cases rest)  
    end else begin
      let preps = List.map self#prepare_for_test_case tests in 
      let todo = List.combine tests preps in  
      let wait_for_count = ref 0 in 
      let result_ht = Hashtbl.create 255 in 
      let pid_to_test_ht = Hashtbl.create 255 in 
      List.iter (fun (test,prep) -> 
        match prep with
        | Must_Run_Test(digest,exe_name,source_name,_) -> 
          incr wait_for_count ; 
          let cmd, fitness_file = 
            self#internal_test_case_command exe_name source_name test in 
            (*
          (match Unix.fork () with
          | 0 -> begin
            match Unix.system cmd with
            | Unix.WEXITED(i) -> exit i
            | _ -> exit 1 
          end 
          | pid -> 
            debug "rep: forked %S as %d\n" cmd pid ;  
            Hashtbl.replace 
                pid_to_test_ht pid (test,fitness_file,digest) 
          ) 
          *) 
          let cmd_parts = Str.split space_regexp cmd in 
          let cmd_1 = "/bin/bash" in 
          let cmd_2 = Array.of_list ("/bin/bash" :: cmd_parts) in 
          let pid = Stats2.time "test" (fun () -> 
            Unix.create_process cmd_1 cmd_2 
            dev_null dev_null dev_null) ()  in 
          Hashtbl.replace 
                pid_to_test_ht pid (test,fitness_file,digest) 

        | Have_Test_Result(digest,result) -> 
          Hashtbl.replace result_ht test (digest,result)
      ) todo ; 
      Stats2.time "wait (for parallel tests)" (fun () -> 
        while !wait_for_count > 0 do 
          try 
            match Unix.wait () with
            | pid, status -> 
              let test, fitness_file, digest_list = 
                Hashtbl.find pid_to_test_ht pid in 
              let result = 
                self#internal_test_case_postprocess status fitness_file in 
              decr wait_for_count ; 
              Hashtbl.replace result_ht test (digest_list,result) 
          with e -> 
            wait_for_count := 0 ;
            debug "cachingRep: test_cases: wait: %s\n" (Printexc.to_string e) 
        done 
      ) () ; 
      Hashtbl.iter (fun test (digest_list,result) -> 
        test_cache_add digest_list test result ;
        Hashtbl.replace tested (digest_list,test) () ;
      ) result_ht ; 
      List.map (fun test -> 
        try 
          let _, result = Hashtbl.find result_ht test in
          result 
        with _ -> 
          debug "cachingRep: test_cases: %s assumed failed\n" (test_name test) ;
          (false, [| 0. |]) 
      ) tests 
    end 

  (* give a "descriptive" name for this variant. For most, the name is
   * based on the atomic mutations applied in order. Those are stored
   * in the "history" list. *) 
  method history_element_to_str h = 
    match h with 
	| Template(name, fillins) -> 
	  let ints = StringMap.fold (fun k -> fun (_,v,_) -> fun lst -> v :: lst) fillins [] in
	  let str = lfoldl (fun str -> fun int -> Printf.sprintf "%s,%d" str int) "(" ints
	  in
		Printf.sprintf "%s%s)" name str
    | Delete(id) -> Printf.sprintf "d(%d)" id 
    | Append(dst,src) -> Printf.sprintf "a(%d,%d)" dst src 
    | Swap(id1,id2) -> Printf.sprintf "s(%d,%d)" id1 id2 
    | Replace(id1,id2) -> Printf.sprintf "r(%d,%d)" id1 id2 
    | Crossover(None,None) -> Printf.sprintf "x(:)" (* ??? *) 
    | Crossover(Some(id),None) -> Printf.sprintf "x(:%d)" id 
    | Crossover(None,Some(id)) -> Printf.sprintf "x(%d:)" id 
    | Crossover(Some(id1),Some(id2)) -> Printf.sprintf "x(%d:%d)" id1 id2
    | Replace_Subatom(aid,sid,atom) -> 
      Printf.sprintf "e(%d,%d,%s)" aid sid
        (self#atom_to_str atom) 

  method name () = 
    let history = self#get_history () in 
    if history = [] 
      then "original"
    else begin 
      let b = Buffer.create 40 in
      List.iter (fun h -> 
        let str = self#history_element_to_str h in
        if str <> "" then 
          Printf.bprintf b "%s " str 
      ) history ; 
      Buffer.contents b 
    end 

  method hash () = Hashtbl.hash (self#get_history ()) 

  method add_history edit = 
    history := !history @ [edit] 

  method apply_template template fillins =
	self#updated () ; 
	self#add_history (Template(template.template_name, fillins));
	()

  method delete stmt_id = 
    self#updated () ; 
    self#add_history (Delete(stmt_id)) ;
    () 

  method append x y = 
    self#updated () ; 
    self#add_history (Append(x,y)) ;
    () 

  method swap x y =
    self#updated () ; 
    self#add_history (Swap(x,y)) ;
    () 

  method replace x y =
    self#updated () ; 
    self#add_history (Replace(x,y)) ;
    () 

  method note_replaced_subatom x y atom =  
    self#updated () ;
    self#add_history (Replace_Subatom(x,y,atom)) ;
    () 

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
let flatten_fault_localization wp = 
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

let faultlocRep_version = "5" 

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
class virtual ['gene,'code] faultlocRepresentation = object (self) 
  inherit ['gene,'code] cachingRepresentation as super 

  (***********************************
   * State Variables
   ***********************************)

  val fault_localization = ref []
  val fix_localization = ref []

  method debug_info () =
	let fix_local = self#get_fix_localization() in
	let fault_local = self#get_faulty_atoms() in
	debug "fault path length: %d, fix path length: %d\n" (llen fault_local) (llen fix_local);
	debug "fault weight: %g\n" (lfoldl (fun accum -> fun (_,g) -> accum +. g) 0.0 fault_local);
	debug "fix weight: %g\n"  (lfoldl (fun accum -> fun (_,g) -> accum +. g) 0.0 fix_local);
	let fout = open_out "fault_path.weights" in
	liter (fun (id,w) -> output_string fout (Printf.sprintf "%d,%g\n" id w)) fault_local;
	close_out fout; 
	let fout = open_out "fix_path.weights" in
	liter (fun (id,w) -> output_string fout (Printf.sprintf "%d,%g\n" id w)) fix_local;
	close_out fout; 

  method replace_sources x =
	lfoldl
	  (fun weightset ->
		fun (i,w) ->
		  WeightSet.add (i,w) weightset)
	  (WeightSet.empty) (lfilt (fun (i,w) -> i <> x) !fix_localization)


  method append_sources x = 
	lfoldl
	  (fun weightset ->
		fun (i,w) ->
		  WeightSet.add (i,w) weightset) (WeightSet.empty) !fix_localization

  method swap_sources x = 
	lfoldl
	  (fun weightset ->
		fun (i,w) ->
		  WeightSet.add (i,w) weightset)
	  (WeightSet.empty) (lfilt (fun (i,w) -> i <> x) !fault_localization)

  (***********************************
   * No Subatoms 
   * (subclasses can override)
   ***********************************)
  method subatoms = false
  method get_subatoms = failwith "get_subatoms" 
  method replace_subatom = failwith "replace_subatom" 
  method replace_subatom_with_constant = failwith "replace_subatom_with_constant" 

	
  (***********************************
   *  no templates
   * (subclasses can override)
   ***********************************)
  val templates = ref false
  val template_cache = hcreate 10

  method get_template = failwith "get template"
  method load_templates template_file = templates := true
  method apply_template foo bar = super#apply_template foo bar
  method template_available_mutations location_id = failwith "available mutations"

  (***********************************
   * Methods
   ***********************************)

  (* fixme: load_templates must add to the available mutations *)
  val mutations = ref []
  val mutation_cache = hcreate 10 

  method register_mutations muts =
	liter
	  (fun (mutation,prob) ->
		if prob > 0.0 then
		  mutations := (mutation,prob) :: !mutations
	  ) muts 

  method available_mutations mut_id = 
	ht_find mutation_cache mut_id
	  (fun _ ->
		lfilt
		  (fun (mutation,prob) ->
			match mutation with
			  Delete_mut -> true
			| Append_mut -> 
				 (* thought: cache the sources list? *)
			  (WeightSet.cardinal (self#append_sources mut_id)) > 0
			| Swap_mut ->
			  (WeightSet.cardinal (self#swap_sources mut_id)) > 0
			| Replace_mut ->
			  (WeightSet.cardinal (self#replace_sources mut_id)) > 0
			| Template_mut(s) -> failwith "not handled here"
		  ) !mutations
	  )

  method virtual atom_id_of_source_line : string -> int -> atom_id 
  method source_line_of_atom_id id = id

  method serialize ?out_channel ?global_info (filename : string) = begin
    let fout = 
      match out_channel with
      | Some(v) -> v
      | None -> assert(false); 
    in 
    Marshal.to_channel fout (faultlocRep_version) [] ; 
    Marshal.to_channel fout (!fault_localization) [] ;
    Marshal.to_channel fout (!fix_localization) [] ;
	Marshal.to_channel fout !fault_scheme [] ; 
	Marshal.to_channel fout !fix_scheme [] ; 
	Marshal.to_channel fout !negative_path_weight [] ; 
	Marshal.to_channel fout !positive_path_weight [] ; 
    super#serialize ~out_channel:fout filename ;
    debug "faultlocRep: %s: saved\n" filename ; 
    if out_channel = None then close_out fout 
  end 

  method deserialize ?in_channel ?global_info (filename : string) = begin
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
	  let gval = match global_info with Some(true) -> true | _ -> false in
		if gval then begin
		(* CLG isn't sure if this is quite right *)
		fault_localization := Marshal.from_channel fin ; 
		fix_localization := Marshal.from_channel fin ; 
		let fault_scheme' = Marshal.from_channel fin in
		let fix_scheme' = Marshal.from_channel fin in
		let negative_path_weight' = Marshal.from_channel fin in
		let positive_path_weight' = Marshal.from_channel fin in
		  if fault_scheme' <> !fault_scheme ||
			fix_scheme' <> !fix_scheme ||
			negative_path_weight' <> !negative_path_weight ||
			positive_path_weight' <> !positive_path_weight ||
			!regen_paths then
			self#compute_localization()
	  end;
	  super#deserialize ?in_channel:(Some(fin)) ?global_info:global_info filename ; 
      debug "faultlocRep: %s: loaded\n" filename ; 
      if in_channel = None then close_in fin ;
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
    let fix_path = 
        Filename.concat (Unix.getcwd()) !fix_path 
    in
    let fault_path = 
        Filename.concat (Unix.getcwd()) !fault_path 
    in
    let run_tests ?debug_str:(debug_str = "") test_maker max_test out_path expected =
	  let stmts = ref [] in
	  for test = 1 to max_test do
(*		if debug_str <> "" then
		  debug "%s: %d\n" debug_str test;*)
		(try Unix.unlink coverage_outname with _ -> ());
		let cmd = Printf.sprintf "touch %s\n" coverage_outname in
		  ignore(Unix.system cmd);
		  let actual_test = test_maker test in 
		  let res, _ = 
			self#internal_test_case coverage_exename coverage_sourcename 
			  actual_test
		  in 
			if res <> expected then begin 
			  debug "ERROR: coverage either PASSES negative test or FAILS positive test\n" ;
			  if not !allow_coverage_fail then 
				abort "Rep: unexpected coverage result on %s\n" coverage_exename (test_name actual_test)
			end ;
			liter
			  (fun stmt_id ->
				stmts :=  (my_int_of_string stmt_id) :: !stmts 
			  ) (get_lines coverage_outname);
			stmts := uniq !stmts;
	  done;
		stmts := lrev !stmts;
        let fout = open_out out_path in
          liter
            (fun stmt ->
              let str = Printf.sprintf "%d\n" stmt in
				output_string fout str) !stmts;
          close_out fout; !stmts
	in
    (* We run the negative test case first to find the statements covered. *) 
	  ignore(run_tests ~debug_str:"neg_test:" (fun t -> Negative t) !neg_tests fault_path false);
(*      try*)
    (* For simplicity, we sometimes only run one positive test case to
     * compute coverage. Running them all is more precise, but also takes
     * longer. *) 
      let max_positive = !pos_tests in (*if not !one_positive_path || (!coverage_info <> "") then !pos_tests else 1 in
Claire is confused by the inclusion of "coverage_info <> """ in that test *)
	  let pos_stmts = run_tests ~debug_str:"pos_test:" (fun t -> Positive t) max_positive fix_path true in
        if !coverage_info <> "" then begin
          let uniq_stmts = uniq pos_stmts in
          let perc = (float_of_int (llen uniq_stmts)) /. (float_of_int (self#max_atom())) in
            debug "COVERAGE: %d unique stmts visited by pos test suite (%d/%d: %g%%)\n" 
              (llen uniq_stmts) (llen uniq_stmts) (self#max_atom()) perc;
            let fout = open_out !coverage_info in 
            liter
              (fun stmt ->
                let str = Printf.sprintf "%d\n" stmt in
                  output_string fout str) uniq_stmts;
              close_out fout;
        end;
		debug "done\n";
(*    with _ -> begin
      debug "WARNING: faultLocRep: no positive path generated by the test case, positive path will be empty (probably not a win).\n";
      (* in all likelihood, the positive test case(s) doesn't/don't touch the 
       file in question, which is probably bad. *)
      let cmd = Printf.sprintf "touch %s\n" fix_path in
      ignore(Unix.system cmd)
    end*)
	(* now we have a positive path and a negative path *) 

  (*
   * compute_localization should produce fault and fix localization sets
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
  method load_oracle (fname : string) : unit = failwith "load_oracle unimplemented"

  method reduce_search_space split_fun do_uniq =
	(* there's no reason this can't do something to fix localization as well but
	   for now I'm only implementing the stuff we currently need *)
	let fault_localization' = 
	  if do_uniq then uniq !fault_localization
	  else !fault_localization 
	in
	  fault_localization := (lfilt split_fun fault_localization')

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
      let atoms = ref [] in
        for i = 1 to self#max_atom () do
          atoms := ((self#source_line_of_atom_id i),1.0) :: !atoms ;
        done ;
        List.rev !atoms
	in
	  
	(* 
	 * Default "ICSE'09"-style fault and fix localization from path files. 
	 * The weighted path fault localization is a list of <atom,weight>
	 * pairs. The fix weights are a hash table mapping atom_ids to
	 * weights. 
	 *)
	let compute_fault_localization_and_fix_weights_from_path_files () = 
	  let fw = Hashtbl.create 10 in
		liter (fun (i,_) -> Hashtbl.replace fw i 0.1) !fault_localization;
		let neg_ht = Hashtbl.create 255 in 
		let pos_ht = Hashtbl.create 255 in 
		  iter_lines !fix_path
			(fun line ->
			  Hashtbl.replace pos_ht line () ;
			  Hashtbl.replace fw (my_int_of_string line) 0.5);
		  lfoldl
			(fun (wp,fw) ->
			  fun line ->
				if not (Hashtbl.mem neg_ht line) then
				  begin 
					  (* a statement only on the negative path gets weight 1.0 ;
					   * if it is also on the positive path, its weight is 0.1 *) 
					let weight = if Hashtbl.mem pos_ht line then !positive_path_weight else !negative_path_weight in 
					  Hashtbl.replace neg_ht line () ; 
					  Hashtbl.replace fw (my_int_of_string line) 0.5 ; 
					  (my_int_of_string line,weight) :: wp, fw
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
	  let fix_weights = Hashtbl.create 10 in 
		liter (fun (i,_) -> Hashtbl.replace fix_weights i 0.1) !fix_localization;
		let fault_localization = ref [] in 
		  liter (fun line -> 
			let s, w, file = 
			  match (Str.split regexp line) with
			  | [stmt] -> (my_int_of_string stmt), 1.0, ""
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
				fault_localization := (s,w) :: !fault_localization
			  end 
		  ) (get_lines fname);
		  lrev !fault_localization, fix_weights
	in
	  
	(* set_fault and set_fix set the codebank and change location atomsets to
	 * contain the atom_ids of the actual code in the weighted path or in the
	 * fix localization set.  Set_fault is currently unecessary but in case the
	 * correctness of fault_localization becomes relevant I'm [Claire] going to
	 * implement it now to save the hassle of forgetting that it needs to be.
	 *)
	let set_fault wp = fault_localization := wp in
	let set_fix lst = fix_localization := lst in

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
			  (coverage_sourcename ^ if (!Global.extension <> "")
                           then "." ^ !Global.extension ^ !Global.suffix_extension
                           else "") in 
			let coverage_exename = Filename.concat subdir coverage_exename in 
			let coverage_outname = Filename.concat subdir !coverage_outname in
(*			let coverage_outname = if !use_full_paths then 
				Filename.concat (Unix.getcwd()) coverage_outname 
			  else coverage_outname in*)
			  self#instrument_fault_localization 
				coverage_sourcename coverage_exename coverage_outname ;
			  if not (self#compile coverage_sourcename coverage_exename) then 
				begin
				  abort "ERROR: faultLocRep: compute_localization: cannot compile %s\n" 
					coverage_sourcename 
				end ;
			  self#get_coverage coverage_sourcename coverage_exename coverage_outname
		  end;
		  
		(* At this point the relevant path files definitely exist -- 
		 * because we're reusing them, or because we recomputed them above. *) 
		  let wp, fw = compute_fault_localization_and_fix_weights_from_path_files () in
			if !fault_scheme = "path" then 
			  set_fault (lrev wp);
			if !fix_scheme = "path" || !fix_scheme = "default" then 
			  set_fix (fix_weights_to_lst fw)
		end; (* end of: "path" fault or fix *) 
	  
	  (* Handle "uniform" fault or fix localization *) 
	  fault_localization :=
		if !fault_scheme = "uniform" then uniform ()
		else !fault_localization;
	  fix_localization :=
		if !fix_scheme = "uniform" then uniform ()
		else !fix_localization;
	  
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
  (* CLG: if I did this properly, fault_localization should already be
   * reversed *)
	  if !flatten_path <> "" then 
		fault_localization := flatten_fault_localization !fault_localization;
	  if !coverage_info <> "" then begin
		let fout = open_out !coverage_info in
(* FIXME so we don't overwrite  elsewhere*)
		  liter
			(fun (stmt,_) ->
			  (* CLG: FIXME: we probably don't need both this and coverage_info but whatever *)
			  let str = Printf.sprintf "%d\n" stmt in
			  output_string fout str)
			!fix_localization;
		  close_out fout;
		  exit 1
	  end
  method get_faulty_atoms () = !fault_localization

  method get_fix_localization () = !fix_localization

end 

let global_filetypes = ref ([] : (string * (unit -> unit)) list)

