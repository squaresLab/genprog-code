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

(*
 * An atom is the smallest unit of our representation: a stmt in CIL,
 * a line of an ASM program, etc.  
 *)
type atom_id = int 
(*type atom_id_two = int*)
type stmt = Cil.stmtkind
type exp = Cil.exp
type test = Positive of int | Negative of int 

(*
 * This is the main interface for a program representation (e.g., CIL-AST,
 * Assembly, etc.). 
 *)
class type representation = object
  method copy : unit -> representation 
  method save_binary : string -> unit (* serialize to a disk file *)
  method load_binary : string -> unit (* desreialize *) 
  method from_source : string -> unit (* load from a .C or .ASM file, etc. *)
  method output_source : string -> unit (* pretty-print a .C file, etc. *)
  method sanity_check : unit -> unit 
  method compute_fault_localization : unit ->  unit 
  method compile : ?keep_source:bool -> string -> string -> bool 
  method test_case : test -> bool (* run a single test case *) 
  method debug_info : unit ->  unit (* print debugging information *) 
  method max_atom : unit -> atom_id (* 1 to N -- INCLUSIVE *)
  method filter_quark_lst: atom_id -> float ref -> float ref  -> (atom_id) list
  method filter_quark_in_atom: atom_id -> float ref -> (atom_id) list
  method max_quark_src : unit -> atom_id (* 1 to N -- INCLUSIVE *) 
  method get_quark_index : atom_id -> atom_id
  method max_quark_in_atom : atom_id -> atom_id 
  method quark_from_atom : atom_id -> atom_id -> atom_id 
  method get_fault_localization : unit -> (atom_id * float) list 
  method get_fix_localization : unit -> (atom_id * float) list 

  (* atomic mutation operators *) 
  method delete : atom_id -> unit 
  method append : atom_id -> atom_id -> unit 
  method swap : atom_id -> atom_id -> unit 
  method swap_exp : atom_id -> atom_id -> unit
  method get : atom_id -> stmt
  method put : atom_id -> stmt -> unit

  method add_name_note : string -> unit 
  (* add a "history" note to the variant's descriptive name *)

  method name : unit -> string (* a "descriptive" name for this variant *) 

end 

(*
 * A new representation can "inherit nullRep" and fill in features
 * as time goes by. 
 *)
class nullRep : representation = object
  method copy = failwith "copy" 
  method save_binary = failwith "save_binary" 
  method load_binary = failwith "load_binary" 
  method from_source = failwith "from_source" 
  method output_source = failwith "output_source" 
  method sanity_check = failwith "sanity_check" 
  method compute_fault_localization = failwith "fault_localization" 
  method compile = failwith "compile" 
  method test_case = failwith "test_case" 
  method debug_info = failwith "debug_info" 
  method max_atom = failwith "max_atom" 
  method filter_quark_lst = failwith "filter_quark_lst"
  method filter_quark_in_atom = failwith "filter_quark_in_atom"
  method max_quark_src = failwith "max_quark" 
  method get_quark_index = failwith "get_quark_index"
  method max_quark_in_atom = failwith "max_quark_in_atom"
  method quark_from_atom = failwith "quark_from_atom"
  method get_fault_localization = failwith "get_fault_localization" 
  method get_fix_localization = failwith "get_fix_localization" 
  method delete = failwith "delete" 
  method append = failwith "append" 
  method swap = failwith "swap" 
  method swap_exp = failwith "swap_exp"
  method put = failwith "put"
  method get = failwith "get"
  method name = failwith "name" 
  method add_name_note = failwith "add_name_note"
end 

let compiler_name = ref "gcc" 
let compiler_options = ref "" 
let test_command = ref "./test.sh" 
let label_repair = ref false 
let port = ref 808
let change_port () =
  port := (!port + 1) ;
  if !port > 1600 then 
    port := !port - 800 

let test_name t = match t with
  | Positive x -> sprintf "p%d" x
  | Negative x -> sprintf "n%d" x

let _ =
  options := !options @
  [
    "--compiler", Arg.Set_string compiler_name, "X use X as compiler";
    "--compiler-opts", Arg.Set_string compiler_options, "X use X as options";
    "--test-command", Arg.Set_string test_command, "X use X to run tests";
    "--label-repair", Arg.Set label_repair, " indicate repair locations";
  ] 

(*
 * Persistent caching for test case evaluations. 
 *)
let test_cache = ref 
  ((Hashtbl.create 255) : (Digest.t, (test,bool) Hashtbl.t) Hashtbl.t)
let test_cache_query digest test = 
  if Hashtbl.mem !test_cache digest then begin
    let second_ht = Hashtbl.find !test_cache digest in
    try
      let res = Hashtbl.find second_ht test in
      Stats2.time "test_cache hit" (fun () -> Some(res)) () 
    with _ -> None 
  end else None 
let test_cache_add digest test result =
  let second_ht = 
    try
      Hashtbl.find !test_cache digest 
    with _ -> Hashtbl.create 7 
  in
  Hashtbl.replace second_ht test result ;
  Hashtbl.replace !test_cache digest second_ht 
let test_cache_save () = 
  let fout = open_out_bin "repair.cache" in 
  Marshal.to_channel fout (!test_cache) [] ; 
  close_out fout 
let test_cache_load () = 
  try 
    let fout = open_in_bin "repair.cache" in 
    test_cache := Marshal.from_channel fout ; 
    close_in fout 
  with _ -> () 

(* 
 * We track the number of unique test evaluations we've had to
 * do on this run, ignoring of the persistent cache.
 *)
let tested = (Hashtbl.create 4095 : ((Digest.t * test), unit) Hashtbl.t)
let num_test_evals_ignore_cache () = 
  let result = ref 0 in
  Hashtbl.iter (fun _ _ -> incr result) tested ;
  !result

let compile_failures = ref 0 

exception Test_Result of bool 

