(*
 *
 * Copyright (c) 2012-2018,
 *  Wes Weimer          <weimerw@umich.edu>
 *  Stephanie Forrest   <steph@asu.edu>
 *  Claire Le Goues     <clegoues@cs.cmu.edu>
 *  Eric Schulte        <eschulte@cs.unm.edu>
 *  Jeremy Lacomis      <jlacomis@cmu.edu>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. The names of the contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)
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
type partition_id = int
type subatom_id = int
module AtomSet = IntSet
module AtomMap = IntMap

(** represents single test cases *)
type test =
  | Positive of int
  | Negative of int
  | Single_Fitness  (** a single test case that returns a real number *)

type test_metrics = {
  pass_count : float ;
  fail_count : float ;
  cost : float ; (* "cost" of test, e.g., runtime in seconds *)
}

module OrderedTest =
struct
  type t = test
  let compare = compare
end
module TestMap = Map.Make(OrderedTest)
module TestSet = Set.Make(OrderedTest)

(** Set of all positive and negative tests. Initialized the first time
    {!Rep.representation.tests_visiting_atoms} is called when
    {!Rep.representation.per_atom_covering_tests} is empty *)
let set_of_all_tests = ref TestSet.empty

let test_metrics_table = Hashtbl.create 255

(* CLG is hating _mut but whatever, for now *)

(** represents an edit to an individual *)
type 'atom edit_history =
  | LaseTemplate of string
  | Template of string * filled StringMap.t
  | Delete of atom_id
  | Append of atom_id * atom_id
  | Swap of atom_id * atom_id
  | Replace of atom_id * atom_id
  | Replace_Subatom of atom_id * subatom_id * 'atom

(* --coverage-per-test needs to known which atoms have been touched
 * by a particular set of genprog edits. *)
let atoms_visited_by_edit_history eh =
  List.fold_left (fun acc elt ->
      AtomSet.union acc (
        match elt with
        | LaseTemplate _
        | Template _ -> failwith "atoms_visited_by_edit_history: template"
        | Delete(x) -> AtomSet.singleton x
        | Append(where,what) -> AtomSet.singleton where
        | Swap(x,y) -> AtomSet.add y (AtomSet.singleton x)
        | Replace(where,what) -> AtomSet.singleton where
        | Replace_Subatom(where,_,_) -> AtomSet.singleton where
      )
    ) (AtomSet.empty) eh

(** [mutation] and [mutation_id] are used to describe what atom-level
    mutations are permitted in a given representation type *)


type mutation_id = | Delete_mut | Append_mut
                   | Swap_mut | Replace_mut | Template_mut of string
                   | Lase_Template_mut

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

  (** @return the set of atom_ids that may be targets or sources of mutations *)
  method get_atoms : unit -> AtomSet.t

  (** @return atom_ids, a list of potentially-faulty atoms and their associated
      weights, or the "weighted path" if you prefer the old nomenclature *)
  method get_faulty_atoms : unit -> (atom_id * float) list
  method get_fix_source_atoms : unit -> (atom_id * float) list

  (** methods for clone- or partition-based fault localization queries *)
  method get_all_fault_partitions : unit -> (partition_id * WeightSet.t * float) list

  (** given a partition id, return atoms in that partition
      @param id of a partition
      @return WeightSet of atoms in that partition*)
  method get_fault_partition : partition_id -> WeightSet.t * float

  (** @param atom_id statement id of interest
      @param partition_id id of a partition
      @return boolean indicating whether given statement is in given partition *)
  method is_in_partition : atom_id -> partition_id -> bool

  (** read in a clone file and create two mappings:
        statement id -> partition
        partition -> list of statement id's in that group
      @param string indicating path to a formatted code clone file*)
  method read_clone_file : unit -> unit

  (** performs sanity checking on the given individual, typically at
      load/initialization type.  Sanity check typically makes sure that the base
      variant can be compiled, that it passes all positive test cases, and that
      it fails all negative test cases *)
  method sanity_check : unit -> unit

  (** computes localization information for this (usually the original)
      variant.*)
  method compute_localization : unit ->  unit


  (** if --coverage-per-test information is available, indicate which
      tests must be re-run if the given atoms have been changed *)
  method tests_visiting_atoms : AtomSet.t -> TestSet.t

  (** if --coverage-per-test information is available, indicate which
      tests must be re-run given the atoms touched in this variant *)
  method tests_visiting_edited_atoms : unit -> TestSet.t

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
      elsewhere (such as a distributed GA).  Fitness can be along several
      dimensions, and thus may be accessed by a key.  If unspecified, the key
      defaults to "tests."

      @param key optional string key
      @param fitness float, this variant's fitness
  *)
  method set_fitness : ?key:string -> float -> unit

  (** @param key optional key, desired fitness dimension. Defaults to "tests."
      @return fitness option, [Some(fitness)] of this variant if it knows it,
      [None] otherwise *)
  method fitness : ?key:string -> unit -> float option

  (** compiles this variant on disk.

      @param source_name output the variant to this source name
      @param exe_name compile to this exe name
      @return boolean denoting whether the compilation succeeded
  *)
  method compile : string -> string -> bool

  (** returns the number of tests run on this variant, including duplicates *)
  method num_evals : unit -> int

  (** public interface for running a single test case.

      @param t test case to run

      @return boolean corresponding to whether the variant passes test case [t]
      and an array of floating point numbers denoting the fitness; this array
      may have more than one entry if the search is multiobjective. fitness
      values are usually 1.0 or 0.0 but may be arbitrary when [single_fitness]
      is used. *)
  method test_case : test -> bool * (float array list)

  (** as [test_case], but for several test cases, run in parallel (specified by
      --fitness-in-parallel exceeding 1)

      @param tests list of tests to run in parallel
      @return as [test_case], but many answers, one for each test run *)
  method test_cases : test list -> ((bool * float array list) list)

  (** @return the metrics collected from running [test] *)
  method test_metrics : test -> test_metrics

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


  (** modifies the fix localization space by quotienting it with respect
      to program equivalence (among other possible optimizations)
  *)
  method reduce_fix_space : unit -> unit

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
      available mutations. Note: this may be called with an empty string,
      indicating there is no file to load.

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
    string -> atom_id -> (filled StringMap.t * float) list

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

  (** @param name name of template to apply *)
  method lase_template : string -> unit

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
  method get_subatoms : fault_src:bool -> atom_id -> 'code list

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

(** Test name to string *)
let test_name t = match t with
  | Positive x -> sprintf "p%d" x
  | Negative x -> sprintf "n%d" x
  | Single_Fitness -> "s"

let test_of_string s =
  let scan_test_name b =
    Scanf.bscanf b "%c" (function
        | 'p' -> Scanf.bscanf b "%d" (fun i -> Positive i)
        | 'n' -> Scanf.bscanf b "%d" (fun i -> Negative i)
        | 's' -> Single_Fitness
        | _   -> failwith ("invalid test name '"^s^"'")
      )
  in
  Scanf.sscanf s "%r" scan_test_name (fun t -> t)

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
let preprocess_command = ref ""
let test_command = ref ""
let flatten_path = ref "last"
let compiler_name = ref "gcc"
let compiler_options = ref ""
let test_script = ref "./test.sh"
let label_repair = ref false
let use_subdirs = ref false
let port = ref 808
let no_test_cache = ref false
let name_in_test_cache = ref false
let no_rep_cache = ref false
let num_fitness_samples = ref 1
let allow_coverage_fail = ref false
let coverage_per_test = ref false
let coverage_per_test_warning_printed = ref false
let skipped_tests = ref ""
let skip_failed_sanity_tests = ref false
let use_global_source_cache = ref false

let do_nested = ref false

let regen_paths = ref false

let fault_scheme = ref "path"
let fault_path = ref "coverage.path.neg"
let fault_file = ref ""

let fix_scheme = ref "default"
let fix_path = ref "coverage.path.pos"
let fix_file = ref ""
let fix_oracle_file = ref ""
let coverage_info = ref ""
let is_valgrind = ref false
let partition   = ref (-1)

let nht_server = ref ""
let nht_port = ref 51000
let nht_id = ref "global"
let fitness_in_parallel = ref 1

let negative_path_weight = ref 1.0
let positive_path_weight = ref 0.1

let rep_cache_file = ref ""
let atom_test_coverage = Hashtbl.create 255

let sanity = ref "default"

let ccfile = ref ""  (* path to code clone file *)

let _ =
  options := !options @
             [
               "--prefix", Arg.Set_string prefix,
               "X append X on file names to access original source.  Default: ./";

               "--sanity", Arg.Set_string sanity,
               "X Sanity strategy. Options: \"yes\", \"no\".  Default: yes if no preexisting rep cache, no otherwise.";

               "--no-rep-cache", Arg.Set no_rep_cache,
               " do not load representation (parsing) .cache file" ;

               "--name-in-test-cache", Arg.Set name_in_test_cache,
               " cache variant names with test results. Default: unset to same memory";

               "--neg-weight", Arg.Set_float negative_path_weight,
               "X weight to give statements only on the negative path. Default: 1.0";

               "--pos-weight", Arg.Set_float positive_path_weight,
               "X weight to give statements on both the positive and the negative paths. Default: 0.1";

               "--fitness-in-parallel", Arg.Set_int fitness_in_parallel,
               "X allow X fitness evals for 1 variant in parallel";

               "--keep-source", Arg.Set always_keep_source,
               " keep all source files";

               "--nested", Arg.Set(do_nested),
               " allow mutating the results of a previous mutation" ;

               "--test-command", Arg.Set_string test_command, "X use X as test command";

               "--test-script", Arg.Set_string test_script, "X use X as test script name";

               "--compiler", Arg.Set_string compiler_name, "X use X as compiler";

               "--compiler-command", Arg.Set_string compiler_command,
               "X use X as compiler command";

               "--compiler-opts", Arg.Set_string compiler_options, "X use X as options";

               "--preprocessor", Arg.Set_string preprocess_command,
               " preprocessor command. Default: __COMPILER_NAME__ -E __COMPILER_OPTIONS__" ;

               "--label-repair", Arg.Set label_repair, " indicate repair locations";

               "--flatten-path", Arg.Set_string flatten_path,
               "X flatten weighted path (sum/min/max/first/last)";

               "--allow-coverage-fail", Arg.Set allow_coverage_fail,
               " allow coverage to fail its test cases" ;

               "--regen-paths", Arg.Set regen_paths, " regenerate path files";

               "--fault-scheme", Arg.Set_string fault_scheme,
               "X fault localization scheme X.  Options: path, uniform, line, weight, tarantula, jaccard, ochiai, clone. Default: path";

               (* CLG potential TODO: get rid of ccfile, use fault_file instead? *)
               "--clone-file", Arg.Set_string ccfile,
               "X code clone file used to modify weights";

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

               "--coverage-per-test", Arg.Set coverage_per_test,
               " create and use 'per test case' coverage information" ;

               "--use-partition", Arg.Set_int partition,
               "N restrict mutations to partition N";

               "--valgrind", Arg.Set is_valgrind, " the program under repair is valgrind; lots of hackiness/special processing.";

               "--rep-cache", Arg.Set_string rep_cache_file,
               "X rep cache file.  Default: base_name.cache.";

               "--num-fitness-samples", Arg.Set_int num_fitness_samples,
               "X max number of times to resample a variant's fitness. Default: 1";

               "--skip-tests", Arg.Set_string skipped_tests,
               "X assume test cases X (concat all names) pass" ;

               "--skip-failed-sanity-tests", Arg.Set skip_failed_sanity_tests,
               " skip those tests that the sanity check fails" ;

               "--use-global-source-cache", Arg.Set use_global_source_cache,
               " Use the global source cache  Default: false";
             ]

let dev_null = Unix.openfile "/dev/null" [Unix.O_RDWR] 0o640

(**/**)
(*
 * Utility functions for test cases.
 *)

let should_skip_test t =
  lmem (test_name t) (Str.split comma_regexp !skipped_tests)

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
  let parts = Str.split (Str.regexp ";;") str in
  match parts with
  | b :: rest ->
    let b = b = "true" in
    let rest =
      lmap (fun s ->
          lmap (fun s ->
              let fs = Str.split comma_regexp s in
              Array.of_list (lmap my_float_of_string fs)
            ) (Str.split (Str.regexp ";") s)
        ) rest
    in
    Some(b, rest)
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

let add_nht_result_to_buffer qbuf (result : (bool * (float array list list))) =
  let b,fass = result in
  Printf.bprintf qbuf "%b" b ;
  liter (fun fas ->
      Printf.bprintf qbuf ";" ;
      liter (fun fa ->
          Printf.bprintf qbuf ";" ;
          Array.iteri (fun i f ->
              if i > 0 then
                Printf.bprintf qbuf "," ;
              Printf.bprintf qbuf "%g" f
            ) fa ;
        ) fas ;
    ) fass ;
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
(*
 * The test cache is a two-level hashtable. The first level maps from a list of
 * digests (representing the source for this variant) to a pair containing the
 * "canonical" name for the variant (i.e., the first one added to the cache) and
 * the second level table. The second level table maps from the test to a pair
 * containing a boolean indicating success and a list of fitness results. The
 * list of fitness results is stored as a list of lists of arrays of floats as
 * follows: the outer list contains one entry for each time the test was run,
 * which may be more than once, depending on !num_fitness_samples. The inner
 * list contains one array for each line of the fitness file, while each array
 * contains a float for each value on one line of the fitness file.
 *
 * FIXME: If any line consists of only the special value zero, all fitness
 * values are reset to zero. This is intended to capture non-deterministic
 * failures of the single fitness function. This would probably be better
 * handled using the boolean flag instead of throwing away all the other fitness
 * values.
 *)
let test_cache = ref
    ((Hashtbl.create 255) : ((Digest.t list), (string * (test,(bool*float array list list)) Hashtbl.t)) Hashtbl.t)
let test_cache_query digest test =
  try
    let second_ht = snd (Hashtbl.find !test_cache digest) in
    let res = Hashtbl.find second_ht test in
    Stats2.time "test_cache hit" (fun () -> Some(res)) ()
  with Not_found ->
    nht_cache_query digest test
let test_cache_add digest name test result =
  let name, second_ht =
    try Hashtbl.find !test_cache digest with _ -> name, Hashtbl.create 7
  in
  let success0, fitness0 = try Hashtbl.find second_ht test with _ -> true, [] in
  let success1, fitness1 = result in
  let fitness = fitness1 :: fitness0 in
  let fitness =
    let all_zeros = Array.fold_left (fun b x -> b && x = 0.0) true in
    let any pred = List.fold_left (fun b x -> b || pred x) false in
    if any (any all_zeros) fitness then begin
      List.map (List.map (fun xs -> Array.make (Array.length xs) 0.0)) fitness
    end else
      fitness
  in
  let value = (success0 && success1), fitness in
  Hashtbl.replace second_ht test value ;
  if !name_in_test_cache then
    Hashtbl.replace !test_cache digest (name, second_ht)
  else
    Hashtbl.replace !test_cache digest ("", second_ht);
  nht_cache_add digest test value
let test_cache_version = 9
let test_cache_save () =
  let fout = open_out_bin "repair.cache" in
  Marshal.to_channel fout test_cache_version [] ;
  Marshal.to_channel fout !name_in_test_cache [] ;
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
    let nitc = Marshal.from_channel fout in
    if nitc <> !name_in_test_cache then begin
      close_in fout ;
      abort "repair.cache: --name-in-test-cache (%b) does not match cache (%b)\n"
        !name_in_test_cache nitc ;
    end ;
    test_cache := Marshal.from_channel fout ;
    hiter (fun _ (_, second_ht) ->
        hiter (fun t (passed,_) ->
            let old = ht_find test_metrics_table t (fun () ->
                {pass_count = 0.; fail_count = 0.; cost = 0.}) in
            if passed then
              hrep test_metrics_table t
                {old with pass_count = old.pass_count +. 1.}
            else
              hrep test_metrics_table t
                {old with fail_count = old.fail_count +. 1.}) second_ht)
      !test_cache ;
    close_in fout
  with _ -> ()

(* Jon Dorn has made the argument that this function (human_readable_cache_save) should exist in its
   own module. However, there does not exist a module currently that has a function similar to this
   and I would prefer not to introduce yet another one; further, the presence of test_cache_save and
   similar functions here suggests that this is an acceptable place to write to the disk. Therefore,
   this will remain here until someone decides that all the test cache things do not belong in rep
   - Martin Kellogg
*)

(* save a human readable version of the test cache to <filename> *)
let human_readable_cache_save filename =
  let fout = open_out filename in
  Printf.fprintf fout "test cache version: %d\n\n" test_cache_version;
  Hashtbl.iter (fun digest_list (name, second_ht) ->
      Printf.fprintf fout "name: %s\n" name;
      Printf.fprintf fout "\tdigest: ";
      List.iter (fun digest -> Digest.output fout digest) digest_list;
      Printf.fprintf fout "\n\ttest data:\n";
      Hashtbl.iter (fun test (pass,data) ->
          Printf.fprintf fout "\ttest: %s\n" (test_name test) ;
          Printf.fprintf fout "\t\tpass: %s\n" (if pass then "TRUE" else "FALSE");
          List.iter (fun data' ->
              Printf.fprintf fout "\t\tlength of data: %d\n" (Array.length data');
              Array.iter (fun datum -> Printf.fprintf fout "\t\t\tdatum: %g\n" datum) data'
            ) (List.flatten data)
        ) second_ht;
      Printf.fprintf fout "\tend of test data\n"
    ) !test_cache;
  close_out fout

let tested = ref 0

(** num_test_evals_ignore_cache () provides the number of test
    evaluations we've had to do on this run, whether they were cached
    or not.  *)
let num_test_evals_ignore_cache () =  !tested

(**/**)
let compile_failures = ref 0
let test_counter = ref 0
exception Test_Result of (bool * (float array list))
type test_case_preparation_result =
  | Must_Run_Test of (Digest.t list) * string * string * test
  | Have_Test_Result of (Digest.t list) * (bool * float array list)

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
        try ignore(system cmd) with e -> ()
      end;
      (try Unix.mkdir dirname 0o755 with e -> ()) ;
      dirname
    end
  in
  Filename.concat (Unix.getcwd ()) result
(**/**)
let cachingRep_version = "3"

let global_source_cache = ref None

(** virtual class cachingRepresentation.  virtual means that there
    are methods without definitions, which will be defined in concrete
    subclasses.  The cachingRepresentation handles the caching of compilations
    and test cases; also handles some sanity checks and very basic
    representation functionality *)
class virtual ['gene,'code] cachingRepresentation = object (self : ('gene,'code) #representation)
  (* the ('gene,'code) #representation syntax denotes that this
     class definition implements the #representatioin interface *)


  (*** State Variables ***)
  (* JD: These values must be mutable to allow self#copy() to work. I don't know
     that they also need to be ref cells, but I just left them as-is. *)

  (** the fitness testing code in the Fitness module can/should always tell to
      tell an individual what its fitness is.  If I already know it, don't bother
      checking the cache or recomputing it.  MUST BE RESET (using
      [self#updated()]) after an edit to the individual.  *)
  val mutable fitness = hcreate 10

  (** the number of times each test has been run on this variant (including
      duplicates *)
  val mutable eval_count = (TestMap.empty, 0)

  (** cached file contents from [internal_compute_source_buffers]; avoid
      recomputing/reserializing *)
  val mutable already_source_buffers = ref None

  (** list of filenames on disk containing the source code *)
  val mutable already_sourced = ref None

  (** list of Digest.t. Use [#compute_digest] to access. *)
  val mutable already_digest = ref None

  (** ".exe" filename on disk *)
  val mutable already_compiled = ref None

  (** history is a list of edit operations performed to acheive this variant *)
  val mutable history = ref []

  (** failed_sanity_tests tracks tests that failed during the sanity check when
      --skip-failed-sanity-tests is set. This is distinct from [skipped_tests]
      since the user may change that from run to run on the command line, but
      the tests that failed sanity should not change until the next time sanity
      is run. *)
  val mutable failed_sanity_tests = TestSet.empty

  (***********************************)
  (* Methods that must be provided by a subclass.  *)
  (***********************************)

  (**@return (filename option,content) list For single-file representations, the
     filename string option should be "None" and the list should be one element
     long. For multi-file representations, each string option should be
     Some(Individual_Name).  This should only be called once per individual per
     update; the answer is cached.  *)
  method virtual internal_compute_source_buffers :
    unit -> (((string option) * (string option)) list)


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
    let other = Oo.copy self in
    fitness <- Hashtbl.copy fitness ;
    already_source_buffers <- ref !already_source_buffers ;
    already_sourced        <- ref !already_sourced ;
    already_digest         <- ref !already_digest ;
    already_compiled       <- ref !already_compiled ;
    history                <- ref !history ;
    other

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
      with e ->
        begin
          debug "Exception in loading: %s\n" (Printexc.to_string e);
          (if !sanity = "default" then sanity := "yes"); false
        end
    in
    if not success then begin
      self#from_source !program_to_repair;
    end;
    if !sanity = "yes" then
      Stats2.time "sanity_check" self#sanity_check () ;
    if (not success) || !regen_paths then begin
      self#compute_localization ();
    end;
    self#serialize ~global_info:true cache_file;

    self#load_templates !Template.templates_file
  end

  method serialize ?out_channel ?global_info (filename : string) =
    let fout =
      match out_channel with
      | Some(v) -> v
      | None -> open_out_bin filename
    in
    Marshal.to_channel fout (cachingRep_version) [] ;
    Marshal.to_channel fout (failed_sanity_tests) [] ;
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
    failed_sanity_tests <- Marshal.from_channel fin;
    skipped_tests := String.concat ","
        (!skipped_tests::(lmap test_name (TestSet.elements failed_sanity_tests)));
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
    failed_sanity_tests <- TestSet.empty ;
    let time_start = Unix.gettimeofday () in
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
    let tests =
      (lmap (fun i -> Negative i, (fun b -> b)) (1 -- !neg_tests))
      @ (lmap (fun i -> Positive i, (fun b -> not b)) (1 -- !pos_tests))
    in
    liter (fun (t, failed) ->
        let name = test_name t in
        debug "\t%s: " name;
        if should_skip_test t then
          debug "skipped\n"
        else begin
          let r, g = self#internal_test_case sanity_exename sanity_filename
              t in
          debug "%b (%s)\n" r (float_array_to_str (col_means g)) ;
          if failed r then begin
            if !skip_failed_sanity_tests then begin
              debug "\t\t--skip-failed-sanity-tests\n" ;
              failed_sanity_tests <- TestSet.add t failed_sanity_tests ;
              skipped_tests := (!skipped_tests)^","^name
            end else
              abort "cachingRepresentation: sanity check failed (%s)\n" name
          end
        end
      ) tests ;
    let time_now = Unix.gettimeofday () in
    self#cleanup();
    self#updated();
    debug "cachingRepresentation: sanity checking passed (time_taken = %g)\n" (time_now -. time_start) ;
  end

  (** @raise Fail("multipile files, one of which does not have a name") if
      [compute_source_buffers] fails to name one of multiple files. *)
  method output_source source_name =
    let sbl = self#compute_source_buffers () in
    (match sbl with
     | [(None,None)] ->
       ignore (abort "ERROR: rep: output_source: single file with default "^
               " contents not supported yet\n")
     | [(None,Some(source_string))] ->
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
         let sources =
           lfoldl (fun sources (source_name,source_string) ->
               let full_output_name = Filename.concat source_dir source_name in
               let full_source_name = Filename.concat !prefix source_name in
               ensure_directories_exist full_output_name ;
               (match source_string with
                | Some(s) ->
                  let fout = open_out full_output_name in
                  output_string fout s ;
                  close_out fout
                | None -> Unix.link full_source_name full_output_name) ;
               full_output_name :: sources
             ) [] many_files
         in
         already_sourced := Some(lrev sources)
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
         let extensions = "" :: ".i" :: ".s" :: ".o" :: ".ll" :: [] in
         liter
           (fun ext -> try Unix.unlink (exe_name ^ ext) with _ -> ())
           extensions ;
         already_compiled := None ;
       | None -> ());
      if !use_subdirs then begin
        let subdir_name = sprintf "./%06d" (!test_counter - 1) in
        ignore(system ("rm -rf "^subdir_name));
      end
    end

  (**/**)
  method set_fitness ?(key="tests") (f : float) = Hashtbl.add fitness key f
  method fitness ?(key="tests") () : float option =
    try
      Some(hfind fitness key)
    with Not_found -> None

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
    let result = (match Stats2.time "compile" system cmd with
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

  method preprocess source_name out_name =
    let base_command = self#get_preprocess_command () in
    let cmd = Global.replace_in_string base_command
        [
          "__COMPILER_NAME__", !compiler_name ;
          "__COMPILER_OPTIONS__", !compiler_options ;
          "__OUT_NAME__", out_name ;
          "__SOURCE_NAME__", source_name ;
        ]
    in
    let result = match system cmd with
      | Unix.WEXITED(0) -> true
      | _ ->
        debug "\t%s %s fails to preprocess\n" source_name (self#name ()) ;
        false
    in
    result

  method num_evals () = snd eval_count

  (** Checks the test cache for this variant and the given test. If next is
      false, attempts to return the first N cached fitness values where N is the
      number of evaluations of this test. If next is true, attempts to retrieve
      the first N+1 cached fitness value. This second behavior allows the cache
      to be replayed properly: the first call retrieves only the first value,
      the second call gets the first two value, etc.
  *)
  method private internal_check_test_cache ?(next=false) test =
    let extend = if next then (fun x y -> x <= y) else (fun x y -> x < y) in
    let numtests = try TestMap.find test (fst eval_count) with _ -> 0 in
    match test_cache_query (self#compute_digest()) test with
    | Some(x, fss) ->
      let n, fss =
        lfoldl (fun (n,fs) fs' ->
            if extend n numtests then
              n + (llen fs'), List.rev_append fs' fs
            else
              n, fs
          ) (0,[]) (lrev fss)
      in
      if numtests < n then begin
        let map, total = eval_count in
        eval_count <- TestMap.add test n map, total + n - numtests
      end ;
      Some(x, fss)
    | None -> None

  method test_case test =
    if should_skip_test test then
      (true, [ [|1.0|] ])
    else begin
      let tpr = self#prepare_for_test_case test in
      let digest_list, result =
        match tpr with
        | Must_Run_Test(digest_list,exe_name,source_name,test) ->
          let result = self#internal_test_case exe_name source_name test in
          test_cache_add digest_list (self#name()) test result ;
          digest_list, (get_opt (self#internal_check_test_cache test))
        | Have_Test_Result(digest_list,result) ->
          digest_list, result
      in
      incr tested ;
      result
    end
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
            let p = Stats2.time "test" (fun () ->
                popen ~stdout:(UseDescr(dev_null)) ~stderr:(UseDescr(dev_null))
                  "/bin/bash" ["-c"; cmd]) () in
            Hashtbl.replace
              pid_to_test_ht p.pid (test,(Unix.gettimeofday()),fitness_file,digest)

          | Have_Test_Result(digest,result) ->
            Hashtbl.replace result_ht test (digest,result)
        ) todo ;
      Stats2.time "wait (for parallel tests)" (fun () ->
          while !wait_for_count > 0 do
            try
              match Unix.wait () with
              | pid, status ->
                let test, start, fitness_file, digest_list =
                  Hashtbl.find pid_to_test_ht pid in
                let runtime = (Unix.gettimeofday()) -. start in
                let result =
                  self#internal_test_case_postprocess test runtime status fitness_file in
                decr wait_for_count ;
                test_cache_add digest_list (self#name()) test result ;
                Hashtbl.replace result_ht test
                  (digest_list, get_opt (self#internal_check_test_cache test))
            with e ->
              wait_for_count := 0 ;
              debug "cachingRep: test_cases: wait: %s\n" (Printexc.to_string e)
          done
        ) () ;
      Hashtbl.iter (fun test (digest_list,result) ->
          incr tested ;
        ) result_ht ;
      List.map (fun test ->
          try
            let _, result = Hashtbl.find result_ht test in
            result
          with _ ->
            debug "cachingRep: test_cases: %s assumed failed\n" (test_name test) ;
            (false, [ [| 0. |] ])
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
    | LaseTemplate(name) -> Printf.sprintf "l(%s)" name
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

  method lase_template name =
    self#updated () ;
    self#add_history (LaseTemplate(name))

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

  method private get_preprocess_command () =
    match !preprocess_command with
    | "" ->
      "__COMPILER_NAME__ -E __SOURCE_NAME__ __COMPILER_OPTIONS__ > __OUT_NAME__"
    | x -> x

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
    let cache =
      if !use_global_source_cache then
        global_source_cache
      else
        already_source_buffers
    in
    match !cache with
    | Some(sbl, id) when id == Oo.id self -> sbl
    | _ -> begin
        let result = self#internal_compute_source_buffers () in
        cache := Some(result, Oo.id self) ;
        result
      end

  (** @return digest (Hash) of a variant by running MD5 on what its source looks
      like in memory. *)
  method private compute_digest () =
    match !already_digest with
    | Some(digest_list) -> digest_list
    | None -> begin
        let source_buffers = self#compute_source_buffers () in
        let digest_list =
          List.map (function
              | (_,Some(str)) -> Digest.string str
              | (Some(fname),_) -> Digest.string fname
              | (None,None) -> Digest.string ""
            ) source_buffers
        in
        already_digest := Some(digest_list) ;
        digest_list
      end

  (** indicates that cached information based on our AST structure is no longer
      valid *)
  method private updated () =
    hclear fitness ;
    eval_count <- TestMap.empty, 0 ;
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

      @param test the test that produced these results
      @param runtime the duration of the test
      @param status process status for the executed test case
      @param fitness_file on-disk file to which the test script may have written
      @return fitness array of fitnesses, floating point numbers
  *)
  method private internal_test_case_postprocess test runtime status fitness_file =
    let result = match status with
      | Unix.WEXITED(0) -> true
      | _ -> false
    in
    let real_valued =
      if Sys.file_exists fitness_file then
        lfoldl (fun real_valued str ->
            let parts = Str.split (Str.regexp "[, \t]+") str in
            let values = List.map (fun v ->
                try
                  float_of_string v
                with _ -> begin
                    debug "%s: invalid\n%S\nin\n%S" fitness_file v str ;
                    0.0
                  end
              ) parts in
            if values <> [] then
              (Array.of_list values) :: real_valued
            else
              real_valued
          ) [] (get_lines fitness_file)
      else []
    in
    let real_valued =
      if (List.length real_valued) > 0 then
        real_valued
      else if result then
        [ [| 1.0 |] ]
      else
        [ [| 0.0 |] ]
    in
    (if not !always_keep_source then
       (* I'd rather this goes in cleanup() but it's not super-obvious how *)
       try Unix.unlink fitness_file with _ -> ());

    (* update counts of passing/failing tests for test prioritization *)
    let old = self#test_metrics test in
    let count = old.pass_count +. old.fail_count in
    let cost = old.cost +. (runtime -. old.cost) /. (count +. 1.) in
    if result then
      Hashtbl.replace test_metrics_table test
        {old with pass_count = old.pass_count +. 1.; cost = cost}
    else
      Hashtbl.replace test_metrics_table test
        {old with fail_count = old.fail_count +. 1.; cost = cost} ;

    (* update eval_count to reflect new results *)
    let map, total = eval_count in
    let old_count = try TestMap.find test map with _ -> 0 in
    let delta = llen real_valued in
    eval_count <- TestMap.add test (old_count + delta) map, total + delta ;

    result, real_valued

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
    let start = Unix.gettimeofday () in
    let status = Stats2.time "test" system cmd in
    let runtime = (Unix.gettimeofday ()) -. start in
    self#internal_test_case_postprocess test runtime status fitness_file

  method test_metrics test =
    ht_find test_metrics_table test (fun () ->
        {pass_count = 0.; fail_count = 0.; cost = 0.})

  (** associated with [test_case] -- checks in the
      cache, compiles the variant to an EXE if needed,

      @param test case to be run
      @return preparation_result indicating whether the EXE must be run on the
      test case or if we have the result in the cache.  *)
  method private prepare_for_test_case test : test_case_preparation_result =
    let digest_list = self#compute_digest () in
    try begin
      let try_cache next =
        let count = try TestMap.find test (fst eval_count) with _ -> 0 in
        (* first, maybe we've reached the max number of evals *)
        if !num_fitness_samples <= count then
          let result = self#internal_check_test_cache test in
          match result with
          | Some(r) -> raise (Test_Result r)
          | None -> ()
        else
          (* second, maybe we'll get lucky with the persistent cache *)
          match self#internal_check_test_cache ~next test with
          | Some(x,f) when count < (TestMap.find test (fst eval_count)) ->
            raise (Test_Result (x,f))
          | _ -> ()
      in
      try_cache true ;
      (* third, maybe we've already compiled it *)
      let exe_name, source_name, worked = match !already_compiled with
        | None -> (* never compiled before, so compile it now *)
          let subdir = add_subdir None in
          let source_name = Filename.concat subdir
              (sprintf "%06d" !test_counter) ^ if (!Global.extension <> "")
                            then !Global.extension
                            else "" in
          if !always_keep_source then
            debug "\t\t%s is stored at %s\n" (self#name()) source_name;
          let exe_name = Filename.concat subdir
              (sprintf "%06d" !test_counter) in
          incr test_counter ;
          if !test_counter mod 10 = 0 && not !no_test_cache then begin
            test_cache_save () ;
          end ;
          self#output_source source_name ;
          try_cache false ;
          if not (self#compile source_name exe_name) then begin
            test_cache_add digest_list (self#name()) test (false, [ [| 0.0 |] ]) ;
            exe_name,source_name,false
          end else
            exe_name,source_name,true
        | Some("",source) ->
          "", source, false (* it failed to compile before *)
        | Some(exe,source) ->
          exe, source, true (* compiled successfully before *)
      in
      if worked then
        (* we need to actually run the program on the test input *)
        Must_Run_Test(digest_list,exe_name,source_name,test)
      else ((Have_Test_Result(digest_list, (false, [ [| 0.0 |] ]))))
    end with
    | Test_Result(x) -> (* additional bookkeeping information *)
      Have_Test_Result(digest_list,x)
end


let faultlocRep_version = "6"

(** This virtual representation interface handles various simple localization
    (i.e., "weighted path") approaches. This is typically a good class to
    inherit your representation from.  *)
class virtual ['gene,'code] faultlocRepresentation = object (self)
  inherit ['gene,'code] cachingRepresentation as super


  (***********************************)
  (* State Variables *)
  (***********************************)
  (* JD: These values must be mutable to allow self#copy() to work. I don't know
     that they also need to be ref cells, but I just left them as-is. *)
  (* CLG: if they're mutable, they don't need to be refs, but I'm too lazy to
     change them all now *)
  val mutable fault_localization = ref []
  val mutable fix_localization = ref []

  (* state related to clone- or partition-based fault localization *)
  val mutable partitions : (partition_id, WeightSet.t * float) Hashtbl.t = Hashtbl.create 10


  (* state related to --coverage-per-test *)
  val per_test_localization = ref (TestMap.empty : AtomSet.t TestMap.t)
  val per_atom_covering_tests = ref (AtomMap.empty : TestSet.t AtomMap.t)

  (***********************************)
  (* Methods that must be provided by a subclass.  *)
  (***********************************)

  (** instruments this variant for fault localization and writes it to disk.
      Does not compile or run the instrumented variant.

      @param coverage_source_name filename for the instrumented source code
      @param coverage_exe_name executable name for when it's compiled
      @param coverage_data_out_name path file to which to write the coverage
      information.*)
  method virtual private instrument_fault_localization :
    string -> string -> string -> unit

  (** used for line-based localization:

      @param filename name of source file
      @param lineno line number on which we are looking for an atom
      @return atom id associated with/closest lineno in filename *)
  method virtual private atom_id_of_source_line : string -> int -> atom_id list

  method copy () : 'self_type =
    let super_copy : 'self_type = super#copy () in
    fault_localization <- ref !fault_localization ;
    fix_localization   <- ref !fix_localization ;
    super_copy

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
    per_test_localization := Marshal.from_channel fin ;
    per_atom_covering_tests := Marshal.from_channel fin ;

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
    Marshal.to_channel fout (!per_test_localization) [] ;
    Marshal.to_channel fout (!per_atom_covering_tests) [] ;
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

  method get_all_fault_partitions () =
    Hashtbl.fold (fun k (s,w) acc -> (k,s,w) :: acc) partitions []

  (** @raise Not_found if partition id is invalid *)
  method get_fault_partition id = Hashtbl.find partitions id

  (** @raise Not_found if partition id is invalid *)
  method is_in_partition atom_id partition_id =
    try
      let set,w = self#get_fault_partition partition_id in
      WeightSet.exists (fun (id,w) -> id = atom_id) set
    with Not_found -> false

  method read_clone_file () =
    let inchan = open_in !ccfile in
    let count = ref 0 in
    let set = ref WeightSet.empty in
    let split = Str.regexp_string "," in
    let split_line_fn = Str.split_delim split in
    try
      while(true) do
        let line = input_line inchan in
        let split_line = split_line_fn line in
        (*	      let () = List.iter (debug "%s\n") split_line in*)
        match split_line with
        | [] ->  Hashtbl.add partitions !count (!set,1.0); count := !count + 1; set := WeightSet.empty
        | _ ->
          let start_line = int_of_string (List.nth split_line 0) in
          let num_line = int_of_string (List.nth split_line 1) in
          let fname = List.nth split_line 2 in
          for i = start_line to (start_line + num_line) do
            let stmts = lmap (fun i -> i, 1.0) (self#atom_id_of_source_line fname i) in
            liter
              (fun p -> set := WeightSet.add p !set) stmts
          done
      done
    with End_of_file -> close_in inchan


  (* particular representations, such as Cilrep, can override this
   * method to reduce the fix space *)
  method reduce_fix_space () = ()

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

  (* Returns the set of tests that visit (cover) the given atoms. To take
   * advantage of this, use --coverage-per-test. *)
  method tests_visiting_atoms (atomset : AtomSet.t) : TestSet.t =
    if AtomMap.is_empty !per_atom_covering_tests then begin
      if not !coverage_per_test_warning_printed then begin
        debug "rep: WARNING: test_visiting_atoms: no data available\n\ttry using --coverage-per-test and/or --regen-paths\n\tdefaulting to 'all tests'\n" ;
        coverage_per_test_warning_printed := true ;
      end ;
      if TestSet.is_empty !set_of_all_tests then begin
        let answer = ref TestSet.empty in
        for i = 1 to !pos_tests do
          answer := TestSet.add (Positive i) !answer ;
        done ;
        for i = 1 to !neg_tests do
          answer := TestSet.add (Negative i) !answer ;
        done ;
        set_of_all_tests := !answer ;
        !answer
      end else begin
        !set_of_all_tests
      end

    end else begin
      AtomSet.fold (fun atom acc ->
          TestSet.union acc
            (try (AtomMap.find atom !per_atom_covering_tests)
             with _ -> TestSet.empty )
        ) atomset (TestSet.empty)
    end

  (* Using the information associated with --coverage-per-test, we can
   * compute this special common case of impact analysis:
   * which tests must I run given my edits? *)
  method tests_visiting_edited_atoms () : TestSet.t =
    let atoms = atoms_visited_by_edit_history (self#get_history ()) in
    self#tests_visiting_atoms atoms

  (* available_mutations can fail if template_mutations are enabled because
     Claire has not finished implementing that yet *)
  method available_mutations mut_id =
    let compute_available _ =
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
           | Lase_Template_mut -> true
           | Template_mut(s) -> (llen (self#template_available_mutations s mut_id)) > 0
        ) !mutations
    in
    (* Cannot cache available mutations if nested mutations are enabled; the
       set of applicable sources may change based on previous mutations. *)
    if !do_nested then compute_available ()
    else ht_find mutation_cache mut_id compute_available

  (***********************************)
  (* no templates (subclasses can override) *)
  (***********************************)
  val templates = ref false
  val template_cache = hcreate 10

  method load_templates template_file =
    if template_file <> "" then
      templates := true

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

  (** @raise Fail("replace_subatom") if subatoms not supported *)
  method replace_subatom stmt_id subatom_id atom = begin
    if not self#subatoms then
      failwith "replace_subatom"
    else begin
      self#updated () ;
      self#add_history (Replace_Subatom(stmt_id,subatom_id,atom));
    end
  end

  (** @raise Fail("replace_subatom_with_constant") not supported by default *)
  method replace_subatom_with_constant = failwith "replace_subatom_with_constant"


  (***********************************)
  (* Compute the fault localization information. *)
  (***********************************)

  (** helper function for localization; you probably want to override this.
      @param atom_id id we're looking for
      @return pair of filename and line number on which the id is found *)
  method private source_line_of_atom_id id = "",id

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
             debug "\ttest: %d\n" test;
             let _ =
               try Unix.unlink coverage_outname with _ -> ()
             in
             let cmd = Printf.sprintf "touch %s\n" coverage_outname in
             let _ = ignore(system cmd) in
             let actual_test = test_maker test in
             let res, _ =
               self#internal_test_case coverage_exename coverage_sourcename
                 actual_test
             in
             if res <> expected then begin
               if not !allow_coverage_fail then
                 abort "Rep: unexpected coverage result on %s\n"
                   (test_name actual_test)
             end ;
             let stmts' = ref [] in
             let fin = Pervasives.open_in coverage_outname in
             (try
                while true do
                  let num = my_int_of_string (input_line fin) in
                  if not (List.mem num !stmts') then begin
                    (* CLG note for RAMSEY: you used to have a hashtable
                       called "found" here that I think was keeping track
                       of whether a statement was seen on this test before.
                       I've gotten rid of it and wrapped the code in with
                       the check on stmts'.  If that's *not* what found was
                       doing, please let me know and I can revert the
                       change *)
                    stmts' := num :: !stmts';
                    let pos,neg = ht_find atom_test_coverage num (fun _ -> 0.0, 0.0) in
                    let pos',neg' =
                      if expected then pos +. 1.0, neg
                      else pos, neg +. 1.0
                    in
                    Hashtbl.replace atom_test_coverage num (pos', neg')
                  end
                done
              with End_of_file -> close_in fin);
             (* If you specify --coverage-per-test, we retain this
              * information and remember that this particular test
              * visited this set of atoms.
              *
              * Otherwise, we just union up all of the atoms visited
              * by all of the tests. *)
             if !coverage_per_test then begin
               let visited_atom_set = List.fold_left (fun acc elt ->
                   AtomSet.add elt acc
                 ) (AtomSet.empty) !stmts' in
               debug "\t\tcovers %d atoms\n"
                 (AtomSet.cardinal visited_atom_set) ;
               AtomSet.iter (fun atom ->
                   let other_tests_visiting_this_atom =
                     try
                       AtomMap.find atom !per_atom_covering_tests
                     with _ -> TestSet.empty
                   in
                   per_atom_covering_tests := AtomMap.add atom
                       (TestSet.add actual_test other_tests_visiting_this_atom)
                       !per_atom_covering_tests ;
                 ) visited_atom_set ;
               per_test_localization := TestMap.add
                   actual_test visited_atom_set !per_test_localization ;
             end ;
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
    debug "coverage negative:\n";
    ignore(run_tests (fun t -> Negative t) !neg_tests fault_path false);
    debug "coverage positive:\n";
    ignore(run_tests (fun t -> Positive t) !pos_tests fix_path true) ;

    if !coverage_per_test then begin
      let total_tests = ref 0 in
      let total_seen = ref 0 in
      AtomMap.iter (fun a ts ->
          incr total_seen ;
          total_tests := (TestSet.cardinal ts) + !total_tests ;
        ) !per_atom_covering_tests ;
      debug "coverage: average tests per atom: %g / %d\n"
        (float_of_int !total_tests /. float_of_int !total_seen)
        (!neg_tests + !pos_tests)
      ;
    end ;
    ()

  (* now we have a positive path and a negative path *)


  (** @raise Fail("load_oracle not supported on this implementation") not
      supported by default; subclasses can override. *)
  method private load_oracle (fname : string) : unit =
    failwith "load_oracle not supported on this implementation"

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
          let v_new, acc =
            try
              let v_so_far = Hashtbl.find seen sid in
              match !flatten_path with
              | "min"   -> min v_so_far v, acc
              | "max"   -> max v_so_far v, acc
              | "sum"   -> v_so_far +. v, acc
              | "first" -> v_so_far, acc
              | "last"  -> v, acc
              | _ -> failwith "unrecognized algorithm"
            with Not_found ->
              v, sid::acc
          in
          Hashtbl.replace seen sid v_new ;
          acc) [] wp in
      List.rev_map (fun sid -> sid, Hashtbl.find seen sid) id_list
    in

    (* Default "ICSE'09"-style fault and fix localization from path files.  The
     * weighted path fault localization is a list of <atom,weight> pairs. The fix
     * weights are a hash table mapping atom_ids to weights.  *)
    (* refactored to allow parameterizing by various weighting schemes *)
    (* ht : hash table mapping atom id's to <neg test executions, pos test executions>
       fn : function operating on the hash table to compute fix or fault localization *)
    let compute_localization_from_path_files ht fn = Hashtbl.fold fn ht [] in

    (* Process a special user-provided file to obtain a list of <atom,weight>
     * pairs. The input format is a list of "file,stmtid,weight" tuples. You can
     * separate with commas and/or whitespace. If you leave off the weight, we
     * assume 1.0. You can leave off the file as well.  *)
    let process_line_or_weight_file fname scheme =
      let regexp = Str.regexp "[ ,\t]+" in
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
           let stmts = if scheme = "line" then
               self#atom_id_of_source_line file stmt
             else [stmt]
           in
           let stmts = lmap (fun stmt -> stmt,weight) stmts in
           fault_localization := stmts @ !fault_localization
        ) (get_lines fname);
      lrev !fault_localization
    in
    let set_fault wp = fault_localization := wp in
    let set_fix lst = fix_localization := lst in
    let printht ht = Hashtbl.iter (fun atom (pos,neg) -> debug "%d <%f,%f>\n" atom pos neg) ht in

    (* sanity/legality checking on the command line options *)
    let _ =
      match !fault_scheme with
      | "line" | "weight" when !fault_file = "" ->
        abort "faultLocRep: fault scheme %s requires --fault-file\n"
          !fault_scheme
      | "clone" when !ccfile = "" ->
        abort "faultLocRep: fault scheme %s requires --clone-file\n"
          !fault_scheme
      | "path" | "uniform" | "line" | "weight" | "tarantula" | "jaccard" | "ochiai" | "clone" -> ()
      | "default" -> fault_scheme := "path"
      | _ ->
        abort "faultLocRep: Unrecognized fault localization scheme: %s\n"
          !fault_scheme
    in
    let _ =
      match !fix_scheme with
      | "default" when !fix_oracle_file =  "" -> fix_scheme := "path"
      (* CLG may have slightly modified JD's modification to oracle sanity checking in
         particular, but since no one uses it and the sanity check is obscure, doesn't
         think it matters *)
      | "default" when !fix_oracle_file <> "" -> fix_scheme := "line"
      | "line" | "weight" when !fix_file = "" ->
        abort "faultLocRep: fix scheme %s requires --fix-file\n" !fix_scheme;
      | "path" when !fix_oracle_file <> "" ->
        abort "faultLocRep: path fix localization unavailable with --fix-oracle\n";
      | "oracle" when !fix_oracle_file = "" || !fix_file = "" ->
        abort "faultLocRep: fix scheme oracle requires --fix-oracle and --fix-file\n";
      | "oracle" -> fix_scheme := "line"
      | "line" | "weight" | "path" | "uniform" -> ()
      | _ ->
        abort  "faultLocRep: Unrecognized fix localization scheme: %s\n"
          !fix_scheme
    in
    let _ =
      (* if we need the path files and they are either missing or we've been
       * asked to regenerate them, generate them *)
      match !fault_scheme,!fix_scheme with
        "path",_  | "clone",_ | _,"path"| _,"default" | "tarantula",_ | "jaccard",_ | "ochiai",_ ->
        if
          !regen_paths ||
          (not ((Sys.file_exists !fault_path) && (Sys.file_exists !fix_path))) then begin
          let subdir = add_subdir (Some("coverage")) in
          let coverage_sourcename = Filename.concat subdir
              (coverage_sourcename ^ if (!Global.extension <> "")
               then !Global.extension
               else "")
          in
          let coverage_exename = Filename.concat subdir coverage_exename in
          let coverage_outname = Filename.concat subdir "coverage.path" in
          debug "Rep: coverage_sourcename: %s\n" coverage_sourcename;
          self#instrument_fault_localization
            coverage_sourcename coverage_exename coverage_outname ;
          if not (self#compile coverage_sourcename coverage_exename) then
            abort "ERROR: faultLocRep: compute_localization: cannot compile %s\n"
              coverage_sourcename ;
          self#get_coverage coverage_sourcename coverage_exename coverage_outname
        end else begin
          (* TODO: CLG: read in SFL weights if desired. Right now this only
             works if you want ICSE-09 style localization.  *)
          debug "WARNING: grabbing coverage from existing path files, so SFL will not work.\n";
          let read_path_file filename =
            let path = ref [] in
            let fin = Pervasives.open_in filename in
            let _ =
              try
                while true do
                  let num = my_int_of_string (input_line fin) in
                  path := num :: !path
                done
              with End_of_file -> ()
            in
            close_in fin;
            !path
          in
          let fix_path_file = Filename.concat (Unix.getcwd()) !fix_path in
          let fix_path = read_path_file fix_path_file in
          let fault_path_file = Filename.concat (Unix.getcwd()) !fault_path in
          let fault_path = read_path_file fault_path_file in
          List.iter (fun num ->
              let pos,neg = ht_find atom_test_coverage num (fun _ -> 0.0,0.0) in
              Hashtbl.replace atom_test_coverage num (pos, neg +. 1.0)) fault_path;
          List.iter (fun num ->
              let pos,neg = ht_find atom_test_coverage num (fun _ -> 0.0,0.0) in
              Hashtbl.replace atom_test_coverage num (pos +. 1.0, neg)) fix_path
        end
      | _,_ -> ()
    in

    (* that setup aside, actually compute the localization *)

    (* generate fault localization function, used for paths *)
    let fault_fn id (pos,neg) lst =
      match !fault_scheme with
        "path" | "default" | "clone" -> begin
          if (pos > 0.0) && (neg > 0.0) then (id,!positive_path_weight) :: lst
          else  if neg > 0.0 then (id,!negative_path_weight) :: lst
          else (id,0.0) :: lst
        end
      | "uniform" -> (id, 1.0) :: lst
      | "tarantula" -> (id,((neg /. (float_of_int !neg_tests)) /. (pos /. (float_of_int !pos_tests) +. neg /. (float_of_int !neg_tests)))) :: lst
      | "jaccard" -> (id,(neg /. ((float_of_int !neg_tests) +. pos))) :: lst
      | "ochiai" -> (id, (neg /. (sqrt ((float_of_int !neg_tests) *. (neg +. pos))))) :: lst
      | _ -> abort "faultLocRepresentation: unexpected fault scheme in path-based localization (compute localization)\n"
    in
    (* generate fix localization function *)
    let fix_fn id (pos,neg) lst =
      match !fix_scheme with
        "path" | "default" -> (id,0.5) :: lst
      | "uniform" -> (id, 1.0) :: lst
      | _ -> abort "faultLocRepresentation: unexpected fix scheme in path-based localization (compute localization)\n"
    in
    (*call appropriate function to compute localization*)
    let _ = (* fault localization *)
      match !fault_scheme with
      | "line" | "weight" ->
        set_fault (process_line_or_weight_file !fault_file !fault_scheme)
      | "uniform" ->
        set_fault (AtomSet.fold (fun id lst ->
            fault_fn id (1.0,1.0) lst
          ) (self#get_atoms ()) [])
      | _ -> (* "path" | "tarantula" | "default" | "jaccard" | "ochiai" | "clone" *)
        set_fault (compute_localization_from_path_files atom_test_coverage fault_fn);

        if !fault_scheme = "clone" && (!ccfile <> "") then begin
          (*given a weighted path, read in a CC file (if one is specified) and recompute weights *)
          self#read_clone_file ();
          let ochan = open_out "partition.ht" in (* CLG assumes this is for debugging? *)
          Hashtbl.iter (fun id (set,weight) -> fprintf ochan "%d %f %s" id weight (WeightSet.fold (fun (elem, w) accum -> (accum ^ (string_of_int elem) ^ " ")) set "")) partitions
        end
    in
    let _ = (* fix localization *)
      match !fix_scheme with
        "path" | "default" ->
        set_fix (compute_localization_from_path_files atom_test_coverage fix_fn)
      | "uniform" ->
        (* If the --fix-oracle was specified, use those statements instead of
           the ones in the fault space. This allows the --fix-oracle to serve as
           a 'code bank' for mutations. *)
        let atoms =
          if !fix_oracle_file <> "" then begin
            let baseline = self#get_atoms () in
            self#load_oracle !fix_oracle_file;
            AtomSet.diff (self#get_atoms ()) baseline
          end else
            self#get_atoms ()
        in
        set_fix (AtomSet.fold (fun id lst ->
            fix_fn id (1.0,1.0) lst
          ) atoms [])
      | "line" | "weight" ->
        (* If the --fix-oracle was specified, use those statments instead of
           the ones in the fault space. This allows the --fix-oracle to serve as
           a 'code bank' for mutations. *)
        if !fix_oracle_file <> "" then
          self#load_oracle !fix_oracle_file;
        set_fix (process_line_or_weight_file !fix_file !fix_scheme)
      | "oracle" ->
        self#load_oracle !fix_oracle_file;
        set_fix (process_line_or_weight_file !fix_file "line")
      | _ -> abort "faultLocRepresentation: unexpected fix localization scheme; should have been caught by the sanity check (compute localization)\n"
    in

    fault_localization := flatten_fault_localization !fault_localization;
    fix_localization   := flatten_fault_localization !fix_localization;

    (* print debug/converage info if specified *)
    if !coverage_info <> "" then begin
      let pos_stmts = lmap fst !fix_localization in
      let num_stmts = AtomSet.cardinal (self#get_atoms ()) in
      let perc =
        (float_of_int (llen pos_stmts)) /. (float_of_int num_stmts)
      in
      debug "COVERAGE: %d unique stmts visited by pos test suite (%d/%d: %g%%)\n"
        (llen pos_stmts) (llen pos_stmts) num_stmts perc;
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
    end;
    match !fault_scheme with
    (* additional debug/coverage info needed for various special fault
       localization schemes, for ramsey *)
      "tarantula" | "jaccard" | "ochiai" | "clone" ->
      let cacheout = open_out ("results_"^(!fault_scheme)^".txt") in
      let ochan = open_out "cachetable.txt" in
      let printer = fun id (pos,neg) -> (fprintf ochan "%d %f %f\n" id pos neg) in
      let printer2 = fun (id,weight) -> (fprintf cacheout "%d %d %f %s\n" (snd(self#source_line_of_atom_id id)) id weight (fst(self#source_line_of_atom_id id))) in
      Hashtbl.iter printer atom_test_coverage;
      close_out ochan;
      List.iter printer2 !fault_localization;
      close_out cacheout;
    | _ -> ();

end

(** a hideous hack to overcome OCaml's fairly particular typesystem and it's
    impact on conditionally-compiled modules (e.g., the graphics reps) *)
let global_filetypes = ref ([] : (string * (unit -> unit)) list)
