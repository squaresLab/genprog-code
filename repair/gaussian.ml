(* 
 * Program Repair Prototype (v2) 
 *
 * This file implements a simple Gaussian blur function for the
 * smoothing of sampled memory addresses and assembly file offsets.
 *
 *)
open Global
open Rep
open Stringrep

let sample_runs = ref 100
let _ = 
  options := !options @
	["--sample-runs", Arg.Set_int sample_runs, "X Execute X runs of the test suite while sampling with oprofile.";]

module Gaussian = struct
  let version = "1"

  let kernel =
    [(-3, 0.006); (-2, 0.061); (-1, 0.242);
     (0, 0.383);
     (1, 0.242);  (2, 0.061); (3, 0.006);]

  let blur (* blur a list of (address, count) pairs by the given kernel *)
      (kernel : (int * float) list)
      (list : (int * int) list) =
    let map = ((Hashtbl.create (List.length list)) : (int, float) Hashtbl.t) in
    let result = ref ([] : (int * float) list) in
      List.iter
        (fun (addr, count) ->
           List.iter
             (fun (offset, mult) ->
                let index = (offset + addr) in
                let current =
                  try Hashtbl.find map index
                  with Not_found -> (float_of_int 0)
                in
                  Hashtbl.replace map index (current +. ((float_of_int count) *. mult)))
             kernel)
        list ;
      Hashtbl.iter (fun a b -> result := (a,b) :: !result) map ;
      List.sort (fun (a,_) (b,_) -> a - b) !result

end

(** binRep is a virtual superclass for asmrep and elfrep to reduce the amount
	of duplicated code between them, particularly in coverage info
	generation *)
class virtual binRep = object (self : 'self_type)
  (** binRep inherits explicitly from both faultlocRep and stringRep to give us
	  access to particular superclass implementations as necessary *)
  (* note that the order here matters because OCaml inheritence is syntactic,
	 noto semantic, relationship *)
  inherit [string list,string list] faultlocRepresentation as faultlocSuper
  inherit stringRep as super 

  method private virtual combine_coverage :  (int * float) list -> (int, int)	Hashtbl.t  -> (int * float) list 
  method private virtual mem_mapping :  string -> string -> (int, int) Hashtbl.t

  method get_compiler_command () = faultlocSuper#get_compiler_command ()

  (** get_coverage for asmRep (and elfRep) calls out to oprofile to produce
	  samples of visited instructions on the fault and fix paths.  This version
	  of get_coverage does not care if the coverage version of the program
	  displays unexpected behavior on the positive/negative test cases *)
  method get_coverage coverage_sourcename coverage_exename coverage_outname =
    (* the use of two executable allows oprofile to sample the pos
     * and neg test executions separately.  *)
    let pos_exe = coverage_exename^".pos" in
    let neg_exe = coverage_exename^".neg" in
	  ignore(Unix.system ("cp "^coverage_exename^" "^coverage_exename^".pos"));
	  ignore(Unix.system ("cp "^coverage_exename^" "^coverage_exename^".neg"));
      for i = 1 to !sample_runs do (* run the positive tests *)
        for i = 1 to !pos_tests do
          ignore(self#internal_test_case pos_exe
                   coverage_sourcename (Positive i))
        done ;
        for i = 1 to !neg_tests do
          ignore(self#internal_test_case neg_exe coverage_sourcename (Negative i)) 
        done ;
      done ;
      (* collect the sampled results *)
      let from_opannotate sample_path =
        let regex = Str.regexp "^[ \t]*\\([0-9]\\).*:[ \t]*\\([0-9a-zA-Z]*\\):.*" in
		let lst = get_lines sample_path in
        let res = 
		  lfoldl
            (fun acc line ->
              if (Str.string_match regex line 0) then
                let count = int_of_string (Str.matched_group 1 line) in
                let addr = int_of_string ("0x"^(Str.matched_group 2 line)) in
                  (addr, count) :: acc 
			  else acc) [] lst 
		in
          List.sort (fun (a,_) (b,_) -> a - b) res in
      let drop_ids_only_to counts file path =
        let fout = open_out path in
          List.iter (fun (line,_) -> Printf.fprintf fout "%d\n" line) counts ;
          close_out fout in
      let pos_samp = pos_exe^".samp" in
      let neg_samp = neg_exe^".samp" in
      let mapping  = self#mem_mapping coverage_sourcename coverage_exename in
        (* collect the samples *)
        if not (Sys.file_exists pos_samp) then
          ignore (Unix.system ("opannotate -a "^pos_exe^">"^pos_samp)) ;
        if not (Sys.file_exists neg_samp) then
          ignore (Unix.system ("opannotate -a "^neg_exe^">"^neg_samp)) ;
        (* do a Guassian blur on the samples and convert to LOC *)
		let combine_pos = Gaussian.blur Gaussian.kernel (from_opannotate pos_samp) in
		let combine_neg = Gaussian.blur Gaussian.kernel (from_opannotate neg_samp) in
		  drop_ids_only_to
			(self#combine_coverage combine_pos mapping) 
		  pos_exe !fix_path ;
          drop_ids_only_to (self#combine_coverage combine_neg mapping)
          neg_exe !fault_path

  (* the stringRep compute_localization throws a fail, so we explicitly dispatch
	 to faultLocSuper here *)
  method compute_localization () = faultlocSuper#compute_localization ()

  (* because fault localization uses oprofile, instrumenting asmRep for fault
	 localization requires only that we output the program to disk *)
  method instrument_fault_localization 
	coverage_sourcename 
	coverage_exename 
    coverage_outname =
    debug "binRep: computing fault localization information\n" ;
    debug "binRep: ensure oprofile is running\n" ;
    self#output_source coverage_sourcename ;

end
