(*
 * Program Repair Prototype (v2)
 *
 * Program Representation -- text .s assembly file
 *
 * compiled .s assembly files, as produced with gcc -S
 *
 *)

open Printf
open Global
open Gaussian
open Rep

(*************************************************************************
 *************************************************************************
               ASM Representation - Compiled Assembly Programs
 *************************************************************************
 *************************************************************************)

let asm_sample_runs = ref 100
let asm_code_only = ref false
let _ =
  options := !options @
  [
    "--asm-sample-runs",
    Arg.Set_int asm_sample_runs,
    "X Execute X runs of the test suite while sampling with oprofile.";
    "--asm-code-only",
    Arg.Set asm_code_only,
    " Limit mutation operators to code sections of assembly files";
  ]

let asmRep_version = "2"

class asmRep = object (self : 'self_type)

  inherit [string list] faultlocRepresentation as super
  (* TODO: implement faultlocRepresentation to apply lines of memory addresses *)

  val base = ref [| (* array of string lists *) |]

  val range = ref [ (* beginning and ends of code sections *) ]

  method atom_to_str slist =
    let b = Buffer.create 255 in
    List.iter (fun s -> Printf.bprintf b "%S" s) slist ;
    Buffer.contents b

  (* make a fresh copy of this variant *)
  method copy () : 'self_type =
    let super_copy : 'self_type = super#copy () in
    super_copy#internal_copy ()

  (* being sure to update our local instance variables *)
  method internal_copy () : 'self_type =
    {<
      base  = ref (Global.copy !base)  ;
      range = ref (Global.copy !range) ;
    >}

  method from_source (filename : string) = begin
    let lst = ref [] in
    let fin = open_in filename in
    (try while true do
      let line = input_line fin in
      lst := [line] :: !lst
    done with _ -> close_in fin) ;
    base := Array.of_list ([] :: (List.rev !lst)) ;
    if !asm_code_only then begin
      let beg_points = ref [] in
      let end_points = ref [] in
        (* beg/end start and stop code sections respectively *)
      let beg_regx = Str.regexp "^[0-9a-zA-Z_]+:$" in
      let end_regx = Str.regexp "^[ \t]+\\.size.*" in
      let in_code_p = ref false in
        Array.iteri (fun i line ->
                       if ( i > 0 ) then begin
                         if !in_code_p then begin
                           if (Str.string_match end_regx (List.hd line) 0) then begin
                             in_code_p := false ;
                             end_points := i :: !end_points ;
                           end
                         end else begin
                           if (Str.string_match beg_regx (List.hd line) 0) then begin
                             in_code_p := true ;
                             beg_points := i :: !beg_points ;
                           end
                         end
                       end
                    ) !base ;
        if !in_code_p then
          end_points := (Array.length !base) :: !end_points ;
        range := List.rev (List.combine !beg_points !end_points) ;
    end
  end

  method internal_compute_source_buffers () = 
    let buffer = Buffer.create 10240 in 
    Array.iteri (fun i line_list ->
      if i > 0 then begin 
        List.iter (fun line -> 
          Printf.bprintf buffer "%s\n" line 
        ) line_list 
      end 
    ) !base ;
    [ None, (Buffer.contents buffer) ]

  method save_binary ?out_channel (filename : string) = begin
    let fout =
      match out_channel with
      | Some(v) -> v
      | None -> open_out_bin filename
    in
    Marshal.to_channel fout (asmRep_version) [] ;
    Marshal.to_channel fout (!range) [] ;
    Marshal.to_channel fout (!base) [] ;
    super#save_binary ~out_channel:fout filename ;
    debug "asm: %s: saved\n" filename ;
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
    if version <> asmRep_version then begin
      debug "asm: %s has old version\n" filename ;
      failwith "version mismatch"
    end ;
    range := Marshal.from_channel fin ;
    base := Marshal.from_channel fin ;
    super#load_binary ~in_channel:fin filename ;
    debug "asm: %s: loaded\n" filename ;
    if in_channel = None then close_in fin
  end

  method max_atom () =
    if !asm_code_only then
      List.fold_left (+) 0 (List.map (fun (a,b) -> (b - a)) !range)
    else
      Array.length !base

  method atom_id_of_source_line source_file source_line =
    (* return the in-code offset from the global offset *)
    if !asm_code_only then
      List.fold_left (+) 0 (List.map (fun (a,b) ->
                                        if (a > source_line) then
                                          if (b > source_line) then
                                            (b - a)
                                          else
                                            (source_line - a)
                                        else
                                          0) !range)
    else
      source_line

  method source_line_of_atom_id atom_id = begin
    (* return global offset from in-code offset *)
    if !asm_code_only then begin
      let j = ref 0 in
      let i = ref atom_id in
        List.iter (fun (b,e) ->
                     if (!j == 0) then begin
                       let chunk_size = (e - b) in
                         if (!i > chunk_size) then
                           i := !i - chunk_size
                         else
                           j := b + !i
                     end
                  ) !range ;
        !j
    end else
      atom_id
  end

  method load_oracle oracle_file = 
	failwith "asm: no oracle fix localization"

  method structural_signature =
    failwith "asm: no structural differencing"

  method get_compiler_command () =
    "__COMPILER_NAME__ -o __EXE_NAME__ __SOURCE_NAME__ __COMPILER_OPTIONS__ "^
      "2>/dev/null >/dev/null"

  method mem_mapping asm_name bin_name =
    let keep_by_regex reg_str lst =
      let it = ref [] in
      let regexp = Str.regexp reg_str in
        List.iter (fun line ->
                     if (Str.string_match regexp line 0) then
                       it := Str.matched_string line :: !it) lst ;
        List.rev !it in
    let read_file filename =
      let lst = ref [] in
      let fin = open_in filename in
        (try while true do
           let line = input_line fin in
             lst := line :: !lst
         done with _ -> close_in fin) ;
        List.rev !lst in
    let asm_lines = read_file asm_name in
    let lose_by_regexp_ind reg_str indexes =
      let lst = List.map (fun i -> (i, List.nth asm_lines i)) indexes in
      let it = ref [] in
      let regexp = Str.regexp reg_str in
        List.iter (fun (i, line) ->
                     if not (Str.string_match regexp line 0) then
                       it := i :: !it) lst ;
        (List.rev !it) in
    let gdb_disassemble func =
      let tmp = Filename.temp_file func ".gdb-output" in
        ignore (Unix.system
                  ("gdb --batch --eval-command=\"disassemble "^func^"\" "^bin_name^">"^tmp)) ;
        read_file tmp in
    let addrs func =
      let regex = Str.regexp "[ \t]*0x\\([a-zA-Z0-9]+\\)[ \t]*<\\([^ \t]\\)*>:.*" in
      let it = ref [] in
        List.iter (fun line ->
                     if (Str.string_match regex line 0) then
                       it := (Str.matched_group 1 line) :: !it)
          (gdb_disassemble func) ;
        List.rev !it in
    let lines func =
      let on = ref false in
      let collector = ref [] in
      let regex = Str.regexp "^\\([^\\.][^ \t]+\\):" in
        Array.iteri (fun i line ->
                       if !on then
                         collector := i :: !collector;
                       if (Str.string_match regex line 0) then
                         if ((String.compare func (Str.matched_group 1 line)) == 0) then
                           on := true
                         else
                           on := false)
          (Array.of_list asm_lines) ;
        List.rev !collector in
    let map = 
      List.sort (fun (adr_a, ln_a) (adr_b, ln_b) -> adr_a - adr_b)
        (List.flatten
           (List.map
              (fun func ->
                 let f_lines = (lose_by_regexp_ind "^[ \t]*\\." (lines func)) in
                 let f_addrs = (List.map (fun str -> int_of_string ("0x"^str)) (addrs func)) in
                 let min x y = if (x < y) then x else y in
                 let len = min (List.length f_lines) (List.length f_addrs) in
                 let sub lst n = Array.to_list (Array.sub (Array.of_list lst) 0 n) in
                   List.combine (sub f_addrs len) (sub f_lines len))
              (List.map (fun line -> String.sub line 0 (String.length line - 1))
                 (keep_by_regex "^[^\\.][a-zA-Z0-9]*:" asm_lines)))) in
    let hash = Hashtbl.create (List.length map) in
      List.iter (fun (addr, count) -> Hashtbl.add hash addr count) map ;
      hash

  method get_coverage coverage_sourcename coverage_exename coverage_outname =
    (* the use of two executable allows oprofile to sample the pos
     * and neg test executions separately.  *)
    let pos_exe = coverage_exename^".pos" in
      let neg_exe = coverage_exename^".neg" in
		ignore(Unix.system ("cp "^coverage_exename^" "^coverage_exename^".pos"));
		ignore(Unix.system ("cp "^coverage_exename^" "^coverage_exename^".neg"));
        for i = 1 to !asm_sample_runs do (* run the positive tests *)
          for i = 1 to !pos_tests do
            let res, _ = (self#internal_test_case pos_exe
                            coverage_sourcename (Positive i)) in
              if not res then begin
                debug "ERROR: coverage FAILS test Positive %d\n" i ;
              end ;
          done ;
          for i = 1 to !neg_tests do
            let res, _ = (self#internal_test_case neg_exe 
                            coverage_sourcename (Negative i)) in 
              if res then begin 
                (* debug "ERROR: coverage PASSES test Negative %d\n" i ; *)
              end ;
          done ;
        done ;
        (* collect the sampled results *)
        let from_opannotate sample_path =
          let regex = Str.regexp "^[ \t]*\\([0-9]\\).*:[ \t]*\\([0-9a-zA-Z]*\\):.*" in
          let res = ref [] in
          let lst = ref [] in
          let fin = open_in sample_path in
            (try while true do
               let line = input_line fin in
                 lst := line :: !lst
             done with _ -> close_in fin) ;
            List.iter
              (fun line ->
                 if (Str.string_match regex line 0) then
                   let count = int_of_string (Str.matched_group 1 line) in
                   let addr = int_of_string ("0x"^(Str.matched_group 2 line)) in
                     res := (addr, count) :: !res) !lst ;
            List.sort (fun (a,_) (b,_) -> a - b) !res in
        let combine
            (samples : (int * float) list)
            (map     : (int, int)    Hashtbl.t)
            =
          let results = Hashtbl.create (List.length samples) in
            List.iter
              (fun (addr, count) ->
                 if Hashtbl.mem map addr then begin
                   let line_num = Hashtbl.find map addr in
                   let current =
                     try Hashtbl.find results line_num
                     with Not_found -> (float_of_int 0)
                   in
                     Hashtbl.replace results line_num (current +. count)
                 end) samples ;
            List.sort (fun (a,_) (b,_) -> a-b)
              (Hashtbl.fold (fun a b accum -> (a,b) :: accum) results []) in
        (* let drop_to counts file path = *)
        (*   let fout = open_out path in *)
        (*     List.iter (fun (line,weight) -> *)
        (*                  Printf.fprintf fout "%s,%d,%f\n" file line weight) *)
        (*       counts ; *)
        (*     close_out fout in *)
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
          (* convert samples to LOC *)
          drop_ids_only_to (combine
                              (Gaussian.blur
                                 Gaussian.kernel (from_opannotate pos_samp)) mapping)
            pos_exe !fix_path ;
          drop_ids_only_to (combine
                              (Gaussian.blur
                                 Gaussian.kernel (from_opannotate neg_samp)) mapping)
            neg_exe !fault_path

  method instrument_fault_localization
    coverage_sourcename
    coverage_exename
    coverage_outname
    = begin
      debug "asmRep: computing fault localization information\n" ;
      debug "asmRep: ensure oprofile is running\n" ;
      debug "asmRep: this may take some time...\n" ;
      (* save the source to coverage_sourcename *)
      self#output_source coverage_sourcename ;
    end

  method debug_info () = begin
    debug "asm: lines = %d\n" (self#max_atom ());
    let sortedBank = List.sort (fun a b -> a-b)
      (List.map (fun (a,_) -> a) !codeBank) in
    let size = List.length !codeBank in
    debug "asm: code bank size:%d from:%d to:%d\n"
      size (List.nth sortedBank 0) (List.nth sortedBank (size - 1)) ;
  end

  method get ind =
    !base.(self#source_line_of_atom_id ind)
  method put ind newv =
    let idx = self#source_line_of_atom_id ind in
    super#put idx newv ;
    !base.(idx) <- newv

  method swap i_off j_off =
    let i = self#source_line_of_atom_id i_off in
    let j = self#source_line_of_atom_id j_off in
    super#swap i j ;
    let temp = !base.(i) in
    !base.(i) <- !base.(j) ;
    !base.(j) <- temp

  method delete i_off =
    let i = self#source_line_of_atom_id i_off in
    super#delete i ;
    !base.(i) <- []

  method append i_off j_off =
    let i = self#source_line_of_atom_id i_off in
    let j = self#source_line_of_atom_id j_off in
    super#append i j ;
    !base.(i) <- !base.(i) @ !base.(j)

end
