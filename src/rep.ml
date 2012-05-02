(**  The  [representation] interface defines an individual in a population, and
     handles such pressing issues as: representation (e.g., CIL AST,
     ASM), gatheringing and storing fault localization info (e.g., weighted path, 
     predicates), simple mutation operator building blocks (e.g., delete,
     append, swap), etc *)
open Printf
open Global
open Template

(** the {b atom} is the basic node of a representation's underlying code
    structure, such as a line in an ASM program or a Cil statement.  They are
    IDd by integers.  {b subatom}s are smaller nodes than atoms and are
    referenced by tuples *)
type atom_id = int 
type subatom_id = int 
module AtomSet = IntSet
module AtomMap = IntMap

(** represents single test cases *)
type test = 
  | Positive of int 
  | Negative of int 
  | Single_Fitness  (** a single test case that returns a real number *) 

(* CLG is hating _mut but whatever, for now *)

(** represents an edit to an individual *)
type 'atom edit_history = 
  | Template of string * filled StringMap.t
  | Delete of atom_id 
  | Append of atom_id * atom_id
  | Swap of atom_id * atom_id 
  | Replace of atom_id * atom_id
  | Replace_Subatom of atom_id * subatom_id * 'atom 
  | Crossover of (atom_id option) * (atom_id option) 

(** [mutation] and [mutation_id] are used to describe what atom-level
    mutations are permitted in a given representation type *) 


type mutation_id = | Delete_mut | Append_mut 
                   | Swap_mut | Replace_mut | Template_mut of string
type mutation = mutation_id * float

(** abstract {b representation} class type; primary interface for a program
    representation.  An instantiation of a concrete representation corresponds
    to an individual e.g., in a population.  Individuals at their most basic
    consist of a genome, or a list of genes.  Individual representations may
    mutate themselves using the builtin mutation operators/functions or using
    user-defined templates.  Representations may be serialized or deserialized
    and constructed by setting a genome.  Representations may be of variable
    length or not (which influences crossover).  *)
class type ['gene,'code] representation = object('self_type)
  (** ['gene] (or ['a] in the ocamldoc) is the type of the genes that comprise
      an individual's genome, such as an edit operation or a C AST node.
      ['code] (or ['b] in the ocamldoc) is the type of the manipulable source
      nodes, such as a [cilRep_atom] *)

  (** whether individuals of this representation have genomes of varying length.
      This influences one-point crossover *)
  method variable_length : bool

  (** @return genome for this individual genome, or a list of genes.  It may or
      may not be of fixed length (see [variable_length]) *)
  method get_genome : unit -> 'gene list


  (** converts a string (a filename or a string-representation of the
      genome; the choice is left to the subclass) to a genome and mutates the
      given individual accordingly.  Primarily used for oracle-based search

      @param filename string representing the genome
  *)
  method load_genome_from_string : string -> unit

  (** @param gs 

      sets the genome of this individual to gs, typically overwriting the
      previous genome *)
  method set_genome : 'gene list -> unit

  (** @return length of the entire genome, which is not necessarily just the
      length of the list (see [Stringrep.stringRep]) *)
  method genome_length : unit -> int

  (** called when an individual is identified as a successful variant, i.e.,
      with maximum fitness.  For example, note_success might call minimization
      code to minimize this individual's difference from the original *)
  method note_success : unit -> unit

  (** @return new_individual, a copy of this individual *)
  method copy : unit -> 'self_type

  (** tries to load the representation (global and original individual info)
      from base_filename.cache.  If that info cannot be loaded or the cache does
      not exist, load initializes this individual by loading the program to
      repair from source.  If applicable, performs the sanity check.
      Fault-localization-based subclasses may also call [compute_localization]
      at this stage.

      @param base_filename a string (potentially) corresponding to the store
      cache.  *)
  method load : string -> unit

  (** serializes the individual to disk, either to the descripter passed in
      optional [out_channel] or to the file described by [filename].  If the
      optional [global_info] is true, the representation also serializes global
      data associated with representation (most important for [Cilrep.cilRep]).
      Otherwise, serialize only saves data specific to this particular
      individual. Outputs the version of the representation to help
      deserialization.

      @param out_channel optional out_channel
      @param global_info optional boolean describing whether to save global
      state 
      @param filename string describing where to serialize. 
  *)
  method serialize : 
    ?out_channel:out_channel -> ?global_info:bool   -> string -> unit

  (** Loads the representation from disk, either from the descripter passed
      in optional [in_channel] or from the file described by filename.  If the
      optional [global_info] is true, the representation also deserializes
      global data associated with the representation (most important for
      [Cilrep.cilRep].  Otherwise, assumes the existence of the global data and
      only loads data specific for one individual.  Checks the version number of
      the serialized data against the current version.

      @param in_channel optional in_channel
      @param global_info optional boolean describing whether to load global
      state 
      @param filename string potentially corresponding to an on-disk cache. 
  *)
  method deserialize : 
    ?in_channel:in_channel -> ?global_info:bool -> string -> unit 

  (** prints debugging information for this individual*)
  method debug_info : unit ->  unit

  (** @return max_atom, or the largest atom_id, inclusive.  CLG wants to get rid
      of this method but hasn't yet figured out how *)
  method max_atom : unit -> atom_id

  (** @return atom_ids, a list of potentially-faulty atoms and their associated
      weights, or the "weighted path" if you prefer the old nomenclature *)
  method get_faulty_atoms : unit -> (atom_id * float) list 
  method get_fix_source_atoms : unit -> (atom_id * float) list 

  (** performs sanity checking on the given individual, typically at
      load/initialization type.  Sanity check typically makes sure that the base
      variant can be compiled, that it passes all positive test cases, and that
      it fails all negative test cases *)
  method sanity_check : unit -> unit 

  (** computes localization information for this (usually the original)
      variant.*)
  method compute_localization : unit ->  unit 

  (** loads the variant from a source code file (such
      as a .c or .asm) or a list of source code files (.txt)

      @param filename either the source code file or a list of source code
      files. *)
  method from_source : string -> unit

  (** outputs the variant to disk 

      @param filename where to put the source code *)
  method output_source : string -> unit

  (** if this variant exists on disk as a set of files, is the list of filenames
      corresponding to those files; otherwise, is the empty list *)
  method source_name : string list 

  (** delete by-products of fitness testing for this representation, unless
      --keep-source is specified (in which case source file byproducts are
      retained) *)
  method cleanup : unit -> unit

  (** records the fitness; particularly useful if fitness is obtained from
      elsewhere (such as a distributed GA).

      @param fitness float, this variant's fitness
  *)
  method set_fitness : float -> unit

  (** @return fitness option, [Some(fitness)] of this variant if it knows it,
      [None] otherwise *)
  method fitness : unit -> float option

  (** compiles this variant on disk.  

      @param source_name output the variant to this source name
      @param exe_name compile to this exe name
      @return boolean denoting whether the compilation succeeded
  *)
  method compile : string -> string -> bool 

  (** public interface for running a single test case.

      @param t test case to run

      @return boolean corresponding to whether the variant passes test case [t]
      and an array of floating point numbers denoting the fitness; this array
      may have more than one entry if the search is multiobjective. fitness
      values are usually 1.0 or 0.0 but may be arbitrary when [single_fitness]
      is used. *)
  method test_case : test -> bool * (float array) 

  (** as [test_case], but for several test cases, run in parallel (specified by
      --fitness-in-parallel exceeding 1)

      @param tests list of tests to run in parallel
      @return as [test_case], but many answers, one for each test run *)
  method test_cases : test list -> ((bool * float array) list)

  (** @return a "descriptive" name for this variant, such as its edit history as
      a string *)
  method name : unit -> string

  (** @return the list of edits performed on the original variant to produce this
      one *)
  method get_history : unit -> ('code edit_history) list

  (** @param edit history note to add to this variant's descriptive name *)
  method add_history : ('code edit_history) -> unit 

  (** @param edit to serialize
      @return edit_as_string serialized [edit] *)
  method history_element_to_str : ('code edit_history) -> string  


  (** modifies the fault localization space by filtering it (as we might do for
      a distributed GA) and removing duplicates if uniqify_atom_list is true (as
      when proportional mutation is in use)

      @param filter_function function to use to choose atoms to retain
      @param uniquify_atom_list whether to uniquify the space 
  *)
  method reduce_search_space : ((atom_id * float) -> bool) -> bool -> unit
    

  (** specifies mutations that are allowed and their relative weightings,
      typically according to the search parameters

      @param mut_lst [(mutation * float)] list of mutations to register. 
  *)
  method register_mutations : mutation list -> unit 

  (** @param atom id of atom for which mutations are desired
      @return mutations legal at [atom],  assumed to be one of the
      mutatable "potentially-faulty" locations *)
  method available_mutations : atom_id -> mutation list

  (** @return (crossover points * combining function), a set of valid crossover
      points (indices into the genome) and a function that may combine two
      sets of valid crossover points (taken from two different individuals) *)
  (*  CLG thinks this is sort of a bad hack but hasn't yet come up with a
      better way to combine info from two individuals for crossover when
      it's necessary. *)
  method available_crossover_points : 
    unit -> IntSet.t * (IntSet.t -> IntSet.t -> int list)

  (** loads user-defined templates and registers them as available mutations
      for this representation. Postcondition: must add loaded to the
      available mutations

      @param filename file containing template specification. *)
  method load_templates : string -> unit

  (** mutates the individual according to the specified template name, with the
      mappings between template holes and node IDs specified by the
      instantiation

      @param template_name template to be used to mutate
      @param instantiation mapping between template holes and node IDs to use
      with the template. *)
  method apply_template : string -> filled StringMap.t -> unit

  (** @param atom_id location to query for available template-based mutations.
      @return (template, weight, instantiation) list, legal templates, their
      weights, and instantiations for location [atom_id]. CLG doesn't like the
      return type here and will probably change it *)
  method template_available_mutations : 
    string -> atom_id -> (string * float * filled StringMap.t) list

  (** {6 {L {b delete}, {b append}, {b swap}, and {b replace} are the default atomic
      mutation operators that most any individual probably should support.
      append, swap, and replace find 'what to append' by looking in the code
      bank (aka stmt_map) -- *not* in the current variant. }} *)

  (** Does the obvious thing
      @param atom_id to delete.  *)
  method delete : atom_id -> unit 

  (** modifies this variant by appending [what_to_append] after [after_what] 

      @param after_what where to append
      @param what_to_append what to append there*)
  method append : atom_id -> atom_id -> unit 

  (** @param faulty_atom query atom
      @return sources the set of valid atoms that may be appended after
      [faulty_atom], with weights *)
  method append_sources : atom_id -> WeightSet.t 

  (** replaces [swap_one] with [swap_two] and [swap_two] with [swap_one]

      @param swap_one first atom involved in the swap
      @param swap_two second atom involved in the swap *)
  method swap : atom_id -> atom_id -> unit 

  (** @param faulty_atom query atom
      @return sources the set of valid atoms that may be swapped with
      [faulty_atom] with weights *)
  method swap_sources : atom_id -> WeightSet.t 

  (** replaces [replace_what] with [replace_with]

      @param replace_what atom to be replaced
      @param replace_with atom to do the replacing *)
  method replace : atom_id -> atom_id -> unit 

  (** @param faulty_atom query atom
      @return sources a set of valid atoms that may replace [faulty_atom], with
      weights *)
  method replace_sources : atom_id -> WeightSet.t 


  (** {6 {L Subatoms: Some representations support a finer-grain than the atom,
      but still want to perform crossover and mutation at the atom
      level. For example, C ASTs might have atoms (stmts) and subatoms
      (expressions). One might want to change expressions, but that
      complicates crossover (because the number of subatoms may change
      between variants). So instead we still perform crossover on atoms, but
      allow sub-atom changes.}} *)

  (** whether subatoms are supported by this representation *)
  method subatoms : bool

  (** @param atom queried for subatoms
      @return subatoms mutable subatoms, as code, associated with [atom] *)
  method get_subatoms : atom_id -> 'code list

  (** replaces the subatom denoted by the [(base_atom,subatom)] id pair with
      [replacement_subatom]

      @param base_atom id of the atom in which the replaced subatom resides
      @param subatom id of the replaced subatom, relative to [base_atom]
      @param replacement_subatom code to use in the replacement. *)
  method replace_subatom : atom_id -> subatom_id -> 'code -> unit

  (** replaces the subatom denoted by the [(base_atom,subatom)] id pair with a
      constant

      @param base_atom id of the atom in which the replaced subatom resides
      @param subatom id of the replaced subatom, relative to [base_atom] *)
  method replace_subatom_with_constant : atom_id -> subatom_id -> unit 

  (** @param node code fragment
      @return node_as_string a string representation of node; useful for
      debugging. *) 
  method atom_to_str : 'code -> string 

  (** Equal variants must have equal hash codes, but equivalent variants need
      not. By default, this is a hash of the history.

      @return hashvalue for this variant.*)
  method hash : unit -> int 
end 


(*
 * This is a list of variables representing global options related to
 * representations. 
 *)
(**/**)
let prefix = ref "./"
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
  
let fault_scheme = ref "path"
let fault_path = ref "coverage.path.neg"
let fault_file = ref ""

let fix_scheme = ref "default"
let fix_path = ref "coverage.path.pos"
let fix_file = ref ""
let fix_oracle_file = ref ""
let coverage_info = ref ""

let previx = ref "./"

let nht_server = ref "" 
let nht_port = ref 51000
let nht_id = ref "global" 
let fitness_in_parallel = ref 1 

let negative_path_weight = ref 1.0
let positive_path_weight = ref 0.1

let rep_cache_file = ref ""

let sanity = ref "default"
let _ =
  options := !options @
    [
      "--prefix", Arg.Set_string prefix, 
      "X append X on file names to access original source.  Default: ./";

      "--sanity", Arg.Set_string sanity, 
      "X Sanity strategy. Options: \"yes\", \"no\".  Default: yes if no preexisting rep cache, no otherwise.";

      "--no-rep-cache", Arg.Set no_rep_cache, 
      " do not load representation (parsing) .cache file" ;

      "--neg-weight", Arg.Set_float negative_path_weight, 
      "X weight to give statements only on the negative path. Default: 1.0";

      "--pos-weight", Arg.Set_float positive_path_weight, 
      "X weight to give statements on both the positive and the negative paths. Default: 0.1";

      "--fitness-in-parallel", Arg.Set_int fitness_in_parallel, 
      "X allow X fitness evals for 1 variant in parallel";

      "--keep-source", Arg.Set always_keep_source, 
      " keep all source files";

      "--test-command", Arg.Set_string test_command, "X use X as test command";

      "--test-script", Arg.Set_string test_script, "X use X as test script name";

      "--compiler", Arg.Set_string compiler_name, "X use X as compiler";

      "--compiler-command", Arg.Set_string compiler_command, 
      "X use X as compiler command";

      "--compiler-opts", Arg.Set_string compiler_options, "X use X as options";

      "--label-repair", Arg.Set label_repair, " indicate repair locations";

      "--flatten-path", Arg.Set_string flatten_path, 
      "X flatten weighted path (sum/min/max)";

      "--allow-coverage-fail", Arg.Set allow_coverage_fail, 
      " allow coverage to fail its test cases" ;

      "--regen-paths", Arg.Set regen_paths, " regenerate path files";
      
      "--fault-scheme", Arg.Set_string fault_scheme, 
      "X fault localization scheme X.  Options: path, uniform, line, weight. Default: path";

      "--fault-path", Arg.Set_string fault_path, 
      "X Negative path file, for path-based localization.  Default: coverage.path.neg";

      "--fault-file", Arg.Set_string fault_file, 
      "X Fault localization file.  e.g., Lines/weights if scheme is lines/weights.";

      "--fix-scheme", Arg.Set_string fix_scheme, 
      "X Fix localization scheme X.  Options: path, uniform, line, weight, oracle, default (whatever Wes was doing before). Default: default";

      "--fix-path", Arg.Set_string fix_path, 
      "X Positive path file, for path-based localization. Default: coverage.path.pos";

      "--fix-file", Arg.Set_string fix_file, 
      "X Fix localization information file, e.g., Lines/weights.";

      "--fix-oracle", Arg.Set_string fix_oracle_file, 
      "X List of source files for the oracle fix information.";

      "--coverage-info", Arg.Set_string coverage_info, 
      "X Collect and print out suite coverage info to file X";

      "--rep-cache", Arg.Set_string rep_cache_file, 
      " X rep cache file.  Default: base_name.cache.";
    ] 

let dev_null = Unix.openfile "/dev/null" [Unix.O_RDWR] 0o640 

(**/**)
(*
 * Utility functions for test cases. 
 *)

(** Test name to string *)
let test_name t = match t with
  | Positive x -> sprintf "p%d" x
  | Negative x -> sprintf "n%d" x
  | Single_Fitness -> "s" 

(** generate fresh port for network-based test suites (e.g., for webserver
    bugs) *)
let change_port () = 
  port := (!port + 1) ;
  if !port > 1600 then 
    port := !port - 800 

(** [nht_FOO] methods and variables implement distributed networked caching for test case
    evaluations. *)

let nht_sockaddr = ref None 

let nht_connection () = 
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
            flush outchan ;
            let header = input_line inchan in
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

(** test_cache, test_cache_query, etc, implement persistent caching for test
    case evaluations.  If the networked hash table is running, uses it as well *)
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

let tested = (Hashtbl.create 4095 : ((Digest.t list * test), unit) Hashtbl.t)

(** num_test_evals_ignore_cache () provides the number of unique test
    evaluations we've had to do on this run, ignoring of the persistent
    cache.  *)
let num_test_evals_ignore_cache () = 
  let result = ref 0 in
    Hashtbl.iter (fun _ _ -> incr result) tested ;
    !result

(**/**)
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
(**/**)
let cachingRep_version = "1"

(** virtual class cachingRepresentation.  virtual means that there
    are methods without definitions, which will be defined in concrete
    subclasses.  The cachingRepresentation handles the caching of compilations
    and test cases; also handles some sanity checks and very basic
    representation functionality *)
class virtual ['gene,'code] cachingRepresentation = object (self : ('gene,'code) #representation) 
  (* the ('gene,'code) #representation syntax denotes that this
     class definition implements the #representatioin interface *)


  (*** State Variables ***)

  (** the fitness testing code in the Fitness module can/should always tell to
      tell an individual what its fitness is.  If I already know it, don't bother
      checking the cache or recomputing it.  MUST BE RESET (using
      [self#updated()]) after an edit to the individual.  *)
  val fitness = ref None

  (** cached file contents from [internal_compute_source_buffers]; avoid
      recomputing/reserializing *)
  val already_source_buffers = ref None

  (** list of filenames on disk containing the source code *) 
  val already_sourced = ref None

  (** list of Digest.t. Use [#compute_digest] to access. *)  
  val already_digest = ref None 

  (** ".exe" filename on disk *) 
  val already_compiled = ref None

  (** history is a list of edit operations performed to acheive this variant *)
  val history = ref [] 


  (***********************************)
  (* Methods that must be provided by a subclass.  *)
  (***********************************)

  (**@return (filename option,content) list For single-file representations, the
     filename string option should be "None" and the list should be one element
     long. For multi-file representations, each string option should be
     Some(Individual_Name).  This should only be called once per individual per
     update; the answer is cached.  *)
  method virtual internal_compute_source_buffers : 
      unit -> (((string option) * string) list)


  (***********************************)
  (* Concrete methods implementing the interface *)
  (***********************************)

  (** by default, we assume genomes are of variant length (since as of when CLG
     wrote this, the majority of them are). *)
  method variable_length = true

  (** @raise Fail("load genome from string is not implemented") Not all concrete
      instantiations of representation implement this, and it will raise an
      exception if called on such individuals.  CLG has tried to get away from
      this idiom (e.g., implementing functionality in only one representation
      type and forcing the rest to implement it as a failwith), but in some
      cases she hasn't come up with a better way *)
  method load_genome_from_string str = 
    failwith "load genome from string is not implemented"

  (** the default representation does nothing special when it's told it's the
      successful variant, but subclasses can override (e.g., to provide
      minimization capabilities *)
  method note_success orig = ()
    
  (**/**)
  method copy () = 
    ({< history = ref !history ; 
        fitness = ref !fitness ;
        already_source_buffers = ref !already_source_buffers ; 
        already_sourced = ref !already_sourced ; 
        already_digest = ref !already_digest ;
        already_compiled = ref !already_compiled ; 
     >})
  (***********************************)
  (* Methods - Serialization/Deserialization *)
  (***********************************)

  (* CLG notes during the March 2012 refactor that she pulled any reference to
     localization out of cachingRepresentation and put them into
     faultlocRepresentation, because it seemed to make more sense in terms of
     modularity.  Thus, all load does is try to load, and calls sanity if
     applicable. *)
  method load base = begin
    let cache_file = if !rep_cache_file = "" then (base^".cache") else !rep_cache_file in
    let success = 
      try 
        if !no_rep_cache then begin
          if !sanity = "default" then sanity := "yes";
          false 
        end else begin
          self#deserialize ?in_channel:None ~global_info:true cache_file; 
          if !sanity = "default" then sanity := "no";
          true
        end
      with _ -> (if !sanity = "default" then sanity := "yes"); false 
    in
      if not success then begin
        self#from_source !program_to_repair;
      end;
      if !sanity = "yes" then 
        self#sanity_check () ; 
      if not success then begin
        self#compute_localization ();
      end;
      self#serialize ~global_info:true cache_file
  end

  method serialize ?out_channel ?global_info (filename : string) = 
    let fout = 
      match out_channel with
      | Some(v) -> v
      | None -> open_out_bin filename 
    in 
      Marshal.to_channel fout (cachingRep_version) [] ; 
      debug "cachingRep: %s: saved\n" filename ; 
      if out_channel = None then close_out fout 
  (**/**)

  (** @raise Fail("version mismatch") if the version of the binary being read
      does not match the current [cachingRep_version]  *)
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

  (** Perform various sanity checks. Currently we check to ensure that that
     original program passes all positive tests and fails all negative tests.
     You can change the sanity check scheme with the [--sanity] command line
     flag to skip it

     @raise Fail("abort") if the program does not compile, if it passes a
     negative test case, or if it fails a positive test case.   *)
  method sanity_check () = begin
    debug "cachingRepresentation: sanity checking begins\n" ; 
    let subdir = add_subdir (Some("sanity")) in 
    let sanity_filename = Filename.concat subdir
      sanity_filename ^ if (!Global.extension <> "")
        then !Global.extension
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
            if r then 
              abort "cachingRepresentation: sanity check failed (%s)\n"
                (test_name (Negative i)) 
        done ;
        debug "cachingRepresentation: sanity checking passed\n" ; 
  end 

  (** @raise Fail("multipile files, one of which does not have a name") if
      [compute_source_buffers] fails to name one of multiple files. *)
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
            | None -> 
              abort "ERROR: rep: output_source: multiple files,"^
                " one of which does not have a name\n" 
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

  (**/**)
  method source_name =
    match !already_sourced with
    | Some(source_names) -> source_names
    | None -> [] 
  (**/**)

  (** ignores the return values of the unix system calls it uses, namely [system]
      and [unlink], and thus will fail silently if they do *)
  method cleanup () =
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

  (**/**)
  method set_fitness f = fitness := Some(f)
  method fitness () = !fitness

  method compile source_name exe_name = 
    let base_command = self#get_compiler_command () in
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
  (**/**)

  (** @raise Fail("internal test case fail") may fail if test_case or
      internal_test_case does, such as by running into a system call error
      (e.g., [Unix.fork]), or if any of its own Unix system calls (such as
      [create_process] or [wait]) fail *)
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

  (**/**)
  method get_history () = !history

  method add_history edit = history := !history @ [edit] 

  (* give a "descriptive" name for this variant. For most, the name is based on
   * the atomic mutations applied in order. Those are stored in the "history"
   * list. *)
  method history_element_to_str h = 
    match h with 
    | Template(name, fillins) -> 
      let ints = 
        StringMap.fold (fun k -> fun (_,v,_) -> fun lst -> v :: lst) fillins [] 
      in
      let str = 
        lfoldl (fun str -> fun int -> Printf.sprintf "%s,%d" str int) "(" ints
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
      Printf.sprintf "e(%d,%d,%s)" aid sid (self#atom_to_str atom) 
        

  method name () = 
    let history = self#get_history () in 
      if history = [] then "original"
      else begin 
        let b = Buffer.create 40 in
          List.iter (fun h -> 
            let str = self#history_element_to_str h in
              if str <> "" then 
                Printf.bprintf b "%s " str 
          ) history ; 
          Buffer.contents b 
      end 

  (* by default, we can crossover at any point along the genome, and given
     individuals a and b (where this current object is a, we don't do any funny
     combination of crossover points *)
  method available_crossover_points () =
    if (self#genome_length()) > 0 then
    lfoldl
      (fun accset ele ->
        IntSet.add ele accset) IntSet.empty 
      (1 -- (self#genome_length())), (fun a b -> IntSet.elements a)
    else (IntSet.singleton 0),(fun a b -> IntSet.elements a)

  method apply_template template_name fillins =
    self#updated () ; 
    self#add_history (Template(template_name, fillins))

  method delete stmt_id = 
    self#updated () ; 
    self#add_history (Delete(stmt_id)) 

  method append x y = 
    self#updated () ; 
    self#add_history (Append(x,y)) 

  method swap x y =
    self#updated () ; 
    self#add_history (Swap(x,y)) 

  method replace x y =
    self#updated () ; 
    self#add_history (Replace(x,y)) 

  method hash () = Hashtbl.hash (self#get_history ()) 

  (***********************************)
  (* Internal methods that may not be called from outside the class.  Much of
   * these serve to do the work of either compilation or caching. *)
  (***********************************)

  method private get_compiler_command () =     
    match !compiler_command with 
    | "" -> 
      "__COMPILER_NAME__ -o __EXE_NAME__ __SOURCE_NAME__ __COMPILER_OPTIONS__ "^
        "2>/dev/null >/dev/null"
    |  x -> x

  method private get_test_command () = 
    match !test_command with 
    | "" -> 
      "__TEST_SCRIPT__ __EXE_NAME__ __TEST_NAME__"^
        " __PORT__ __SOURCE_NAME__ __FITNESS_FILE__ 1>/dev/null 2>/dev/null" 
    |  x -> x
  (**/**)

  (** @return source buffers either from the cache if available or generated
      fresh if not *)
  method private compute_source_buffers () = 
    match !already_source_buffers with
    | Some(sbl) -> sbl
    | None -> begin 
      let result = self#internal_compute_source_buffers () in
        already_source_buffers := Some(result) ;
        result 
    end 

  (** @return digest (Hash) of a variant by running MD5 on what its source looks
      like in memory. *)
  method private compute_digest () = 
    match !already_digest with
    | Some(digest_list) -> digest_list 
    | None -> begin
      let source_buffers = self#compute_source_buffers () in 
      let digest_list = List.map (fun (fname,str) -> 
        Digest.string str) source_buffers in
        already_digest := Some(digest_list) ;
        digest_list 
    end 

  (** indicates that cached information based on our AST structure is no longer
      valid *)
  method private updated () = 
    fitness := None ;
    already_compiled := None ;
    already_source_buffers := None ; 
    already_digest := None ; 
    already_sourced := None ; 
    () 

(** @param exe_name compiled executable for this variant
    @param source_name source code corresponding to this variant
    @param test to run
    @return [(cmd, fitness_file)] command to run for a test case and the file to
    which the result will be written by the test script *)
  method private internal_test_case_command exe_name source_name test =
    let port_arg = Printf.sprintf "%d" !port in
      change_port () ; 
      let base_command = self#get_test_command () in
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

  (** called after a test has been run, interprets the test's process exit
      status and any fitness files on disk to determine if the test passed.  It
      ignores failures from its own Unix system calls.  If the data in
      [fitness_file] is unreadable, returns 0.0 for fitness and merely print a
      warning

      @param status process status for the executed test case
      @param fitness_file on-disk file to which the test script may have written
      @return fitness array of fitnesses, floating point numbers
  *)
  method private internal_test_case_postprocess status fitness_file =
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
      result, !real_valued

  (** an internal method for the raw running of a test case that does the bare
      bones work: execute the program on the test case. No caching at this
      level.

      @param exe_name name of the compiled executable
      @param source_name on-disk source code for this variant.
      @return (bool,float array) tuple corresponding to
      (passed?,real_valued_fitness).  real_valued_fitness is usually 1.0/0.0, except
      in the case of single_valued_fitness.

      @raise Fail("Unix.fork") if the call to [Unix.system] fails. *)
  method private internal_test_case exe_name source_name test =
    let cmd, fitness_file = 
      self#internal_test_case_command exe_name source_name test in 
    (* Run our single test. *) 
    let status = Stats2.time "test" Unix.system cmd in
      self#internal_test_case_postprocess status (fitness_file: string) 

  (** associated with [test_case] -- checks in the
      cache, compiles the variant to an EXE if needed, 
      
      @param test case to be run
      @return preparation_result indicating whether the EXE must be run on the
      test case or if we have the result in the cache.  *)
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
                  then !Global.extension
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
            | Some("",source) -> 
              "", source, false (* it failed to compile before *) 
            | Some(exe,source) -> 
              exe, source, true (* compiled successfully before *) 
          in
            if worked then 
              (* we need to actually run the program on the test input *) 
              Must_Run_Test(digest_list,exe_name,source_name,test) 
            else ((Have_Test_Result(digest_list, (false, [| 0.0 |] ))))
      end with
      | Test_Result(x) -> (* additional bookkeeping information *) 
        Have_Test_Result(digest_list,x) 
end 


let faultlocRep_version = "5" 

(** This virtual representation interface handles various simple localization
    (i.e., "weighted path") approaches. This is typically a good class to
    inherit your representation from.  *)
class virtual ['gene,'code] faultlocRepresentation = object (self) 
  inherit ['gene,'code] cachingRepresentation as super 


  (***********************************)
  (* State Variables *)
  (***********************************)
  val fault_localization = ref []
  val fix_localization = ref []

  (***********************************)
  (* Methods that must be provided by a subclass.  *)
  (***********************************)

  (** instruments this variant for fault localization and writes it to disk.
      Does not compile or run the instrumented variant.

      @param coverage_source_name filename for the instrumented source code
      @param coverage_exe_name executable name for when it's ocmpiled
      @param coverage_data_out_name path file to which to write the coverage
      information.*)
  method virtual private instrument_fault_localization : 
      string -> string -> string -> unit

  (** used for line-based localization: 

      @param filename name of source file
      @param lineno line number on which we are looking for an atom
      @return atom id associated with/closest lineno in filename *)
  method virtual private atom_id_of_source_line : string -> int -> atom_id 

  method internal_copy () : 'self_type = 
    ({< fault_localization = ref !fault_localization ; 
        fix_localization = ref !fix_localization ;
     >})


  (** [deserialize] can fail if the version saved in the binary file does not
      match the current [faultLocRep_version].  As it can call
      [compute_localization], it may also abort there *)
  method deserialize ?in_channel ?global_info (filename : string) = 
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
      fault_localization := Marshal.from_channel fin ; 
      fix_localization := Marshal.from_channel fin ; 
      let gval = match global_info with Some(n) -> n | _ -> false in
        if gval then begin
          (* CLG isn't sure if this is quite right *)
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
        if in_channel = None then close_in fin 

  (***********************************)
  (* Concrete methods implementing the interface *)
  (***********************************)
  (**/**)
  method serialize ?out_channel ?global_info (filename : string) =
    let fout = 
      match out_channel with
      | Some(v) -> v
      | None -> assert(false); 
    in 
      Marshal.to_channel fout (faultlocRep_version) [] ; 
      Marshal.to_channel fout (!fault_localization) [] ;
      Marshal.to_channel fout (!fix_localization) [] ;
      let gval = match global_info with Some(n) -> n | _ -> false in 
        if gval then begin
          Marshal.to_channel fout !fault_scheme [] ; 
          Marshal.to_channel fout !fix_scheme [] ; 
          Marshal.to_channel fout !negative_path_weight [] ; 
          Marshal.to_channel fout !positive_path_weight [] ; 
        end;
        super#serialize ~out_channel:fout filename ;
        debug "faultlocRep: %s: saved\n" filename ; 
        if out_channel = None then close_out fout 

  method debug_info () =
    let fix_local = self#get_fix_source_atoms() in
    let fault_local = self#get_faulty_atoms() in
      debug "fault path length: %d, fix path length: %d\n" 
        (llen fault_local) (llen fix_local);
      debug "fault weight: %g\n" 
        (lfoldl (fun accum -> fun (_,g) -> accum +. g) 0.0 fault_local);
      debug "fix weight: %g\n"  
        (lfoldl (fun accum -> fun (_,g) -> accum +. g) 0.0 fix_local);
      let fout = open_out "fault_path.weights" in
        liter 
          (fun (id,w) -> output_string fout (Printf.sprintf "%d,%g\n" id w)) 
          fault_local;
        close_out fout; 
        let fout = open_out "fix_path.weights" in
          liter (fun (id,w) -> output_string fout (Printf.sprintf "%d,%g\n" id w)) 
            fix_local;
          close_out fout

  method get_faulty_atoms () = !fault_localization

  method get_fix_source_atoms () = !fix_localization

  method reduce_search_space split_fun do_uniq =
    (* there's no reason this can't do something to fix localization as well but
       for now I'm only implementing the stuff we currently need *)
    let fault_localization' = 
      if do_uniq then uniq !fault_localization
      else !fault_localization 
    in
      fault_localization := (lfilt split_fun fault_localization')


  val mutations = ref []
  val mutation_cache = hcreate 10 

  method register_mutations muts =
    liter
      (fun (mutation,prob) ->
        if prob > 0.0 then
          mutations := (mutation,prob) :: !mutations
      ) muts 


  (* available_mutations can fail if template_mutations are enabled because
     Claire has not finished implementing that yet *)
  method available_mutations mut_id = 
    ht_find mutation_cache mut_id
      (fun _ ->
        lfilt
          (fun (mutation,prob) ->
            match mutation with
              Delete_mut -> true
            | Append_mut -> 
              (* CLG FIXME/thought: cache the sources list? *)
              (WeightSet.cardinal (self#append_sources mut_id)) > 0
            | Swap_mut ->
              (WeightSet.cardinal (self#swap_sources mut_id)) > 0
            | Replace_mut ->
              (WeightSet.cardinal (self#replace_sources mut_id)) > 0
            | Template_mut(s) -> (llen (self#template_available_mutations s mut_id)) > 0
          ) !mutations
      )

  (***********************************)
  (* no templates (subclasses can override) *)
  (***********************************)
  val templates = ref false
  val template_cache = hcreate 10

  method load_templates template_file = templates := true
  method template_available_mutations str location_id =  [] 

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

  method replace_sources x =
    lfoldl
      (fun weightset ->
        fun (i,w) ->
          WeightSet.add (i,w) weightset)
      (WeightSet.empty) (lfilt (fun (i,w) -> i <> x) !fix_localization)
  (**/**)      

  (***********************************)
  (* No Subatoms (subclasses can override) *)
  (***********************************)

  (** the subatoms functions fail by default, unless a subclass implements
      them *)

  method subatoms = false

  (** @raise Fail("get_subatoms") not supported by default *)
  method get_subatoms = failwith "get_subatoms" 
  (** @raise Fail("replace_subatom") not supported by default *)
  method replace_subatom = failwith "replace_subatom" 
  (** @raise Fail("replace_subatom_with_constant") not supported by default *)
  method replace_subatom_with_constant = failwith "replace_subatom_with_constant" 

      
  (***********************************)
  (* Compute the fault localization information. *)
  (***********************************)

  (** helper function for localization; you probably want to override this. 
      @param atom_id id we're looking for
      @return line number on which the id is found *)
  method private source_line_of_atom_id id = id

  (** run the instrumented code to attain coverage information.  Writes the
      generated paths to disk (the fault and fix path files respectively) but
      does not otherwise return.

      If the calls to [Unix.unlink] fail, they will do so silently.

      @param coverage_sourcename instrumented source code on disk
      @param coverage_exename compiled executable
      @param coverage_outname on disk path file name 
      @raise Fail("abort") if variant produces produces unexpected behavior on
      either positive or negative test cases and [--allow-coverage-fail] is not on.
     get_coverage will abort if allow_coverage_fail is not toggled and the variant
  *)
  method private get_coverage coverage_sourcename coverage_exename coverage_outname = 
    let fix_path = Filename.concat (Unix.getcwd()) !fix_path in
    let fault_path = Filename.concat (Unix.getcwd()) !fault_path in
    (* Traditional "weighted path" or "set difference" or Reiss-Renieris fault
     * localization involves finding all of the statements visited while
     * executing the negative test case(s) and removing/down-weighting
     * statements visited while executing the positive test case(s).  *)
    let run_tests test_maker max_test out_path expected =
      let stmts = 
        lfoldl
          (fun stmts test ->
            let _ = 
              try Unix.unlink coverage_outname with _ -> ()
            in
            let cmd = Printf.sprintf "touch %s\n" coverage_outname in
            let _ = ignore(Unix.system cmd) in
            let actual_test = test_maker test in 
            let res, _ = 
              self#internal_test_case coverage_exename coverage_sourcename 
                actual_test
            in 
              if res <> expected then begin 
                debug "ERROR: Rep: unexpected coverage result on %s\n" 
                  (test_name actual_test);
                if not !allow_coverage_fail then 
                  abort "Rep: unexpected coverage result on %s\n" 
                    (test_name actual_test)
              end ;
              let stmts' = ref [] in
              let fin = Pervasives.open_in coverage_outname in 
                (try
                   while true do
                     let line = input_line fin in
                     let num = my_int_of_string line in 
                       if not (List.mem num !stmts') then
                         stmts' := num :: !stmts'
                   done
                 with End_of_file -> close_in fin);
                uniq (!stmts'@stmts)
          )  [] (1 -- max_test) 
      in
      let fout = open_out out_path in
        liter
          (fun stmt ->
            let str = Printf.sprintf "%d\n" stmt in
              output_string fout str) stmts;
        close_out fout; stmts
    in
      ignore(run_tests (fun t -> Negative t) !neg_tests fault_path false);
      ignore(run_tests (fun t -> Positive t) !pos_tests fix_path true)
  (* now we have a positive path and a negative path *) 


  (** @raise Fail("load_oracle not supported on this implementation") not
      supported by default; subclasses can override. *)
  method private load_oracle (fname : string) : unit = 
    failwith "load_oracle not supported on this implementation"
  (* there are a number of ways compute_localization can fail.  Will abort
  *)
 
  (** produces fault and fix localization sets for use by later mutation
      operators. This is typically done by running the program to find the atom
      coverage on the positive and negative test cases, but there are other
      schemes:

      {ul
      {- path:     default 'weight path' localization}
      {- uniform:  all atoms in the program have uniform 1.0 weight}
      {- line: an external file specifies a list of source-code line numbers;
      the corresponding atoms are used}
      {- weight: an external file specifies a weighted list of atoms}
      {- oracle: for fix localization, an external file specifies source code
      (e.g., repair templates, human-written repairs) that is used as a source
      of possible fixes}}

      There are a number of ways this function can fail.  

      @raise Fail("general confusion") this function will fail if either the fault or
      the fix scheme is unrecognized, if the oracle scheme is specified without
      an [oracle_file], if coverage info must be generated but the result does
      not compile, or if the scheme is [line] or [weight] and the input file is
      malformed
  *)
  method compute_localization () =
    debug "faultLocRep: compute_localization: fault_scheme: %s, fix_scheme: %s\n" 
      !fault_scheme !fix_scheme;
    
    (*********************************)
    (* localization utilities *)
    (*********************************)
    
    let fix_weights_to_lst ht = hfold (fun k v acc -> (k,v) :: acc) ht [] in
    let uniform lst = 
      lfoldl 
        (fun acc atom -> ((self#source_line_of_atom_id atom),1.0) :: acc)
        [] (1 -- self#max_atom())
    in
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
    in  
      
    (* Default "ICSE'09"-style fault and fix localization from path files.  The
     * weighted path fault localization is a list of <atom,weight> pairs. The fix
     * weights are a hash table mapping atom_ids to weights.  *)
    let compute_localization_from_path_files () = 
      let fw = Hashtbl.create 10 in
        liter 
          (fun (i,_) -> Hashtbl.replace fw i !positive_path_weight) 
          !fault_localization;
        let neg_ht = Hashtbl.create 255 in 
        let pos_ht = Hashtbl.create 255 in 
          iter_lines !fix_path
            (fun line ->
              Hashtbl.replace pos_ht line () ;
              Hashtbl.replace fw (my_int_of_string line) 0.5);
          lfoldl
            (fun (wp,fw) line ->
              if not (Hashtbl.mem neg_ht line) then
                begin 
                  let neg_weight = if Hashtbl.mem pos_ht line 
                    then !positive_path_weight 
                    else !negative_path_weight 
                  in 
                    Hashtbl.replace neg_ht line () ; 
                    Hashtbl.replace fw (my_int_of_string line) 0.5 ; 
                    (my_int_of_string line,neg_weight) :: wp, fw
                end
              else wp,fw) ([],fw)
            (get_lines !fault_path)
    in

    (* Process a special user-provided file to obtain a list of <atom,weight>
     * pairs. The input format is a list of "file,stmtid,weight" tuples. You can
     * separate with commas and/or whitespace. If you leave off the weight, we
     * assume 1.0. You can leave off the file as well.  *)
    let process_line_or_weight_file fname scheme =
      let regexp = Str.regexp "[ ,\t]" in 
      let fix_weights = Hashtbl.create 10 in 
        liter 
          (fun (i,_) -> Hashtbl.replace fix_weights i !positive_path_weight) 
          !fix_localization;
        let fault_localization = ref [] in 
          liter 
            (fun line -> 
              let stmt, weight, file = 
                match Str.split regexp line with
                | [stmt] -> my_int_of_string stmt, !negative_path_weight, ""
                | [stmt ; weight] -> begin
                  try
                    my_int_of_string stmt, float_of_string weight, ""
                  with _ -> my_int_of_string weight,!negative_path_weight,stmt
                end
                | [file ; stmt ; weight] -> 
                  my_int_of_string stmt, float_of_string weight, file
                | _ -> 
                  abort ("ERROR: faultLocRep: compute_localization: %s: malformed line:\n%s\n"
                  ) !fault_file line
              in 
              (* In the "line" scheme, the file uses source code line numbers
               * (rather than atom-ids). In such a case, we must convert them to
               * atom-ids. *)
              let stmt = if scheme = "line" then 
                  self#atom_id_of_source_line file stmt 
                else stmt
              in
                if stmt >= 1 then begin 
                  Hashtbl.replace fix_weights stmt 0.5; 
                  fault_localization := (stmt,weight) :: !fault_localization
                end 
            ) (get_lines fname);
          lrev !fault_localization, fix_weights
    in
    let set_fault wp = fault_localization := wp in
    let set_fix lst = fix_localization := lst in

    let _ = 
      (* sanity/legality checking on the command line options *)
      (match !fault_scheme with 
        "path" | "uniform" | "line" | "weight" -> ()
      | "default" -> fault_scheme := "path" 
      | _ -> 
        abort "faultLocRep: Unrecognized fault localization scheme: %s\n" 
          !fault_scheme);
      if !fix_oracle_file <> "" then fix_scheme := "oracle";
      match !fix_scheme with
        "path" | "uniform" | "line" | "weight" | "default" -> ()
      | "oracle" -> assert(!fix_oracle_file <> "" && !fix_file <> "")
      | _ -> 
        abort  "faultLocRep: Unrecognized fix localization scheme: %s\n" 
          !fix_scheme
    in
    let _ =
      (* if we need the path files and they are either missing or we've been
       * asked to regenerate them, generate them *)
      match !fault_scheme,!fix_scheme with
        "path",_  | _,"path"| _,"default" 
          when !regen_paths ||
            (not ((Sys.file_exists !fault_path) && (Sys.file_exists !fix_path))) ->
              let subdir = add_subdir (Some("coverage")) in 
              let coverage_sourcename = Filename.concat subdir 
                (coverage_sourcename ^ if (!Global.extension <> "")
                  then !Global.extension
                  else "") 
              in 
              let coverage_exename = Filename.concat subdir coverage_exename in 
              let coverage_outname = Filename.concat subdir "coverage.path" in
                debug "coverage_sourcename: %s\n" coverage_sourcename;
                self#instrument_fault_localization 
                  coverage_sourcename coverage_exename coverage_outname ;
                if not (self#compile coverage_sourcename coverage_exename) then 
                  abort "ERROR: faultLocRep: compute_localization: cannot compile %s\n" 
                    coverage_sourcename ;
                self#get_coverage coverage_sourcename coverage_exename coverage_outname
      | _,_ -> ()
    in
      (* that setup all aside, actually compute the localization *)
      if !fault_scheme = "path" || !fix_scheme = "path" || !fix_scheme =
        "default" then begin
          let wp, fw = compute_localization_from_path_files () in
            if !fault_scheme = "path" then set_fault (lrev wp);
            if !fix_scheme = "path" || !fix_scheme = "default" then 
              set_fix (fix_weights_to_lst fw)
        end; (* end of: "path" fault or fix *) 
      
      (* Handle "uniform" fault or fix localization *) 
      if !fault_scheme = "uniform" then set_fault (uniform ());
      if !fix_scheme = "uniform" then set_fix (uniform ());

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

      (* finally, flatten the fault path if specified *)
      if !flatten_path <> "" then 
        fault_localization := flatten_fault_localization !fault_localization;

      (* print debug/converage info if specified *)
      if !coverage_info <> "" then begin
        let pos_stmts = lmap fst !fix_localization in 
        let perc = 
          (float_of_int (llen pos_stmts)) /. (float_of_int (self#max_atom())) 
        in
          debug "COVERAGE: %d unique stmts visited by pos test suite (%d/%d: %g%%)\n"
            (llen pos_stmts) (llen pos_stmts) (self#max_atom()) perc;
          let fout = open_out !coverage_info in 
            liter
              (fun stmt ->
                let str = Printf.sprintf "%d\n" stmt in
                  output_string fout str) pos_stmts;
            liter
              (fun stmt ->
                let str = Printf.sprintf "%d\n" stmt in
                  output_string fout str) pos_stmts;
            close_out fout
      end
end 

(** a hideous hack to overcome OCaml's fairly particular typesystem and it's
    impact on conditionally-compiled modules (e.g., the graphics reps) *)
let global_filetypes = ref ([] : (string * (unit -> unit)) list)

