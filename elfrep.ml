(*
 * Program Repair Prototype (v2)
 *
 * Program Representation -- binary elf executables
 *
 *)

open Elf
open Printf
open Global
open Rep

(*************************************************************************
 *************************************************************************
         ELF Executables - Compiled and Linked Executable Programs
 *************************************************************************
 *************************************************************************)

let sample_runs = ref 100

let elfRep_version = "0.5"

class elfRep = object (self : 'self_type)

  inherit [string list] faultlocRepresentation as super

  val bytes = ref [| (* array of integer bytes *) |]
  val elf = ref "" (* String to hold binary elf lisp object *)

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
      bytes  = ref (Global.copy !bytes)  ;
      elf = ref (Global.copy !elf) ;
    >}

  method from_source (filename : string) = begin
      elf := read_elf filename;
      bytes := get_text !elf;
  end

  method output_source source_name = write_elf !elf source_name

  method save_binary ?out_channel (filename : string) = begin
    let fout =
      match out_channel with
      | Some(v) -> v
      | None -> open_out_bin filename
    in
    Marshal.to_channel fout (elfRep_version) [] ;
    Marshal.to_channel fout (!elf) [] ;
    Marshal.to_channel fout (!bytes) [] ;
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
    elf := Marshal.from_channel fin ;
    bytes := Marshal.from_channel fin ;
    super#load_binary ~in_channel:fin filename ;
    debug "elf: %s: loaded\n" filename ;
    if in_channel = None then close_in fin
  end

  method max_atom () = Array.length !bytes

  (* this doesn't really make sense for a binary file w/o LOC *)
  method atom_id_of_source_line source_file source_line = source_line

  (* this doesn't really make sense for a binary file w/o LOC *)
  method source_line_of_atom_id (atom_id : int) = atom_id

  method structural_signature =
    failwith "elf: no structural differencing"

  method get_compiler_command () =
    "__COMPILER_NAME__ __EXE_NAME__ __SOURCE_NAME__ 2>/dev/null >/dev/null"

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
      (* compile this representation to both a pos and neg executable
       * the use of two executable allows oprofile to sample the pos
       * and neg test executions separately.  *)
      let pos_exe = coverage_exename^".pos" in
      let neg_exe = coverage_exename^".neg" in
        if not (self#compile ~keep_source:true coverage_sourcename pos_exe) then begin
          debug "ERROR: cannot compile %s to %s\n" coverage_sourcename pos_exe ;
        end ;
        if not (self#compile ~keep_source:true coverage_sourcename neg_exe) then begin
          debug "ERROR: cannot compile %s to %s\n" coverage_sourcename neg_exe ;
        end ;
        for i = 1 to !sample_runs do (* run the positive tests *)
          for i = 1 to !pos_tests do
            let res, _ = (self#internal_test_case coverage_exename 
                            coverage_sourcename (Positive i)) in 
              if res then begin 
                debug "ERROR: coverage FAILS test Positive %d\n" i ;
              end ;
          done ;
          for i = 1 to !neg_tests do
            let res, _ = (self#internal_test_case coverage_exename 
                            coverage_sourcename (Negative i)) in 
              if res then begin 
                debug "ERROR: coverage FAILS test Negative %d\n" i ;
              end ;
          done ;
        done ;
        (* collect the sampled results *)
        let grep = "|grep '^  *[0-9]'|sed 's/://g'|awk '{print $3\" \"$1}'|sort" in
        let pos_path = coverage_outname^".pos" in
        let neg_path = coverage_outname^".neg" in
          (* collect the samples *)
          ignore (Unix.system ("opannotate -a "^pos_exe^grep^">"^pos_path)) ;
          ignore (Unix.system ("opannotate -a "^neg_exe^grep^">"^neg_path)) ;
    end          
    
  method debug_info () = begin
    debug "elf: lines = %d\n" (self#max_atom ());
  end

  method atom_to_byte atom = int_of_string (List.nth atom 0)
  method byte_to_atom byte = [string_of_int byte]

  method get ind = self#byte_to_atom (Array.get !bytes ind)
  method put ind newv = Array.set !bytes ind (self#atom_to_byte newv)

  method swap i j =
    super#swap i j;
    let temp = Array.get !bytes i in
      Array.set !bytes i (Array.get !bytes j) ;
      Array.set !bytes j temp

  method delete i =
    super#delete i ;
    Array.set !bytes i 144 (* 144 is a nop *)

  method append i j =
    super#append i j ;
    Array.set !bytes i (Array.get !bytes j)

end
