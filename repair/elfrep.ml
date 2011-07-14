(*
 * Program Repair Prototype (v2)
 *
 * Program Representation -- binary elf executables
 *
 *)

open Elf
open Printf
open Global
open Gaussian
open Rep

(*************************************************************************
 *************************************************************************
         ELF Executables - Compiled and Linked Executable Programs
 *************************************************************************
 *************************************************************************)

exception Schulte of int;;

let elf_sample_runs = ref 100
let _ =
  options := !options @
  [
    "--elf-sample-runs",
    Arg.Set_int elf_sample_runs,
    "X Execute X runs of the test suite while sampling with oprofile."
  ]

let elfRep_version = "1"

class elfRep = object (self : 'self_type)

  inherit [string list] faultlocRepresentation as super

  val path = ref ""
  val bytes = ref [| (* array of integer bytes *) |]
  val elf = ref "" (* String to hold binary elf lisp object *)
  val offset = ref 0

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
      path = ref (Global.copy !path) ;
      offset = ref (Global.copy !offset) ;
      bytes  = begin
        let temp = Array.make (Array.length !bytes) (Array.get !bytes 0) in
          Array.iteri (fun i el -> Array.set temp i el) !bytes;
          ref temp
      end;
      elf = elf;
    >}

  (* use objdump to find the instruction borders in this elf file *)
  method disasm (filename : string ) = begin
    let path = filename^".instr-sizes" in
    let cmd = "objdump-parse "^filename^">"^path in
    let sizes = ref [] in
      ignore (Unix.system (cmd)) ;
      let fin = open_in path in
        (try while true do (* read in instruction sizes *)
           let line = input_line fin in
             sizes := int_of_string(line) :: !sizes ;
         done with _ -> close_in fin) ;
        List.rev !sizes
  end

  method bytes_of filename = begin
    let raw_bytes = ref (Array.to_list (get_text !elf)) in
    let ins_sizes = ref (self#disasm filename) in
      Array.of_list
        (List.map
           (fun size ->
              let tmp = ref [] in
                for i = 1 to size do
                  tmp := (List.hd !raw_bytes) :: !tmp;
                  raw_bytes := List.tl !raw_bytes
                done;
                List.rev !tmp)
           !ins_sizes)
  end

  method from_source (filename : string) = begin
    path := filename;
    elf := read_elf filename;
    offset := get_text_offset !elf;
    bytes := self#bytes_of filename;
  end

  method output_source source_name = begin
    (* debug "bytes:%d\n" (Array.length !bytes); *)
    (* Array.iter (fun i -> debug "%d " i) *)
    (*   (Array.of_list (List.flatten (Array.to_list !bytes))); *)
    (* debug "\n"; *)
    write_w_text !elf source_name
      (Array.of_list (List.flatten (Array.to_list !bytes)));
  end

  method internal_compute_source_buffers () =
    failwith "elf: internal compute source buffers"

  method save_binary ?out_channel (filename : string) = begin
    let fout =
      match out_channel with
      | Some(v) -> v
      | None -> open_out_bin filename
    in
    Marshal.to_channel fout (elfRep_version) [] ;
      Marshal.to_channel fout path [] ;
    super#save_binary ~out_channel:fout filename ;
    debug "elf: %s: saved\n" filename ;
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
    if version <> elfRep_version then begin
      debug "elf: %s has old version\n" filename ;
      failwith "version mismatch"
    end ;
    debug "ERROR: can not load serialize lisp object\n" ;
    elf := exit 1;
    path := Marshal.from_channel fin ;
    elf := read_elf !path;
    offset := get_text_offset !elf;
    bytes := self#bytes_of !path;
    super#load_binary ~in_channel:fin filename ;
    debug "elf: %s: loaded\n" filename ;
    if in_channel = None then close_in fin
  end

  method max_atom () = (Array.length !bytes) - 1

  (* convert a memory address into a genome index *)
  method atom_id_of_source_line source_file source_line =
    let line = source_line - !offset in
      if line < 0 || line > self#max_atom () then begin
        debug "elfrep: bad line access %d:%d\n" source_line line;
        0
      end
      else
        line

  (* convert a genome index into a memory address *)
  method source_line_of_atom_id (atom_id : int) = atom_id + !offset

  method load_oracle oracle_file = 
	failwith "elf: no oracle fix localization"

  method structural_signature =
    failwith "elf: no structural differencing"

  method get_compiler_command () =
    "__COMPILER_NAME__ __SOURCE_NAME__ __EXE_NAME__ 2>/dev/null >/dev/null"


  method get_coverage coverage_sourcename coverage_exename coverage_outname =
    (* the use of two executable allows oprofile to sample the pos
     * and neg test executions separately.  *)
    let pos_exe = coverage_exename^".pos" in
      let neg_exe = coverage_exename^".neg" in
		ignore(Unix.system ("cp "^coverage_exename^" "^coverage_exename^".pos"));
		ignore(Unix.system ("cp "^coverage_exename^" "^coverage_exename^".neg"));
        for i = 1 to !elf_sample_runs do (* run the positive tests *)
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
        let filter_by_bounds (samples : (int * float) list) =
          let results = ref [] in
          let size = Array.length !bytes in
            List.iter
              (fun (addr, count) ->
                 let index = addr - !offset in
                   if (0 <= index) && (index <= size) then
                     results := (index,count) :: !results ;) samples ;
            List.sort (fun (a,_) (b,_) -> a - b) !results in
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
          (* collect the samples *)
          ignore (Unix.system ("opannotate -a "^pos_exe^">"^pos_samp)) ;
          ignore (Unix.system ("opannotate -a "^neg_exe^">"^neg_samp)) ;
          (* get the addresses and samples, filter down to addresses in
           * the text section, and write them to the output file as
           * integers
           *)
          drop_ids_only_to (filter_by_bounds
                              (Gaussian.blur Gaussian.kernel (from_opannotate pos_samp)))
            pos_exe !fix_path ;
          drop_ids_only_to (filter_by_bounds
                              (Gaussian.blur Gaussian.kernel (from_opannotate neg_samp)))
            neg_exe !fault_path

  method instrument_fault_localization
    coverage_sourcename
    coverage_exename
    coverage_outname
    = begin
      debug "elfRep: computing fault localization information\n" ;
      debug "elfRep: ensure oprofile is running\n" ;
      debug "elfRep: this may take some time...\n" ;
      (* save the source to coverage_sourcename *)
      self#output_source coverage_sourcename ;
    end

  method debug_info () = begin
    debug "elf: lines = %d\n" (self#max_atom ());
  end

  method atom_to_byte atom = List.map int_of_string atom

  method byte_to_atom byte = List.map string_of_int byte

  method get ind =
    if (ind <= self#max_atom ()) then
      self#byte_to_atom (Array.get !bytes ind)
    else
      []
      
  method put ind newv =
    if (ind <= self#max_atom ()) then
      Array.set !bytes ind (self#atom_to_byte newv)

  method swap i j =
    super#swap i j;
    try
    let temp = Array.get !bytes i in
      Array.set !bytes i (Array.get !bytes j) ;
      Array.set !bytes j temp
    with 
      | Invalid_argument  "index out of bounds" ->
          debug "iob: swap(%d %d) length:%d\n" i j (Array.length !bytes)
      | Invalid_argument  "Array.sub" ->
          debug "a.sub: swap(%d %d) length:%d\n" i j (Array.length !bytes)

  method delete i =
    super#delete i ;
    try 
    let num = List.length (Array.get !bytes i) in
    let len = Array.length !bytes in
    let rep = Array.make num [144] in
      if (i == 0) then
        bytes := Array.append rep (Array.sub !bytes 1 (len - 1))
      else if (i == (len - 1)) then
        bytes := Array.append (Array.sub !bytes 0 (len - 1)) rep
      else
        bytes := Array.append
          (Array.append (Array.sub !bytes 0 i) rep)
          (Array.sub !bytes (i + 1) ((len - i) - 1))
    with
      | Invalid_argument  "index out of bounds" ->
          debug "iob: delete(%d) length:%d\n" i (Array.length !bytes)
      | Invalid_argument  "Array.sub" ->
          debug "a.sub: delete(%d) length:%d\n" i (Array.length !bytes)

  method append i j =
    super#append i j ;
    try
    let inst = ref (Array.get !bytes j) in
    let reps = ref (List.length !inst) in
      (* append new instruction into the array *)
      bytes := Array.concat [
        (Array.sub !bytes 0 (i - 1));
        [|!inst|];
        (Array.sub !bytes i ((Array.length !bytes) - i));
      ] ;
      (* delete an appropriate amount of nop's *)
      let max x y = if x > y then x else y in
        for p = 0 to max i ((Array.length !bytes) - i) do
          if (!reps > 0) then begin
            try
              match Array.get !bytes (i+p) with
                | [144] -> begin
                    reps := !reps - 1 ;
                    Array.set !bytes (i+p) []
                  end
                | _     -> begin
                    match Array.get !bytes (i-p) with
                      | [144] -> begin
                          reps := !reps - 1 ;
                          Array.set !bytes (i-p) []
                        end
                      | _ -> ()
                  end
            with
              | Invalid_argument  "index out of bounds" ->
                  debug "iob: append(%d,%d) length:%d\n" i j (Array.length !bytes)
              | Invalid_argument  "Array.sub" ->
                  debug "a.sub: append(%d,%d) length:%d\n" i j (Array.length !bytes)
          end
        done ;
        (* if still too long, then truncate *)
        if (!reps > 0) then
          bytes := Array.sub !bytes 0 ((Array.length !bytes) - !reps) ;
        (* remove empty instruction strings *)
        bytes := Array.of_list
          (List.rev (Array.fold_left
                       (fun a e -> match e with
                          | [] -> a
                          | _  -> e :: a)
                       [] !bytes))
    with
      | Invalid_argument  "index out of bounds" ->
          debug "iob: append(%d,%d) length:%d\n" i j (Array.length !bytes)
      | Invalid_argument  "Array.sub" ->
          debug "a.sub: append(%d %d) length:%d\n" i j (Array.length !bytes)

end
