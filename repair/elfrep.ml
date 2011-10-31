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

let elf_sample_runs = ref 100
let elf_risc = ref false
let _ =
  options := !options @
  [
    "--elf-sample-runs",
    Arg.Set_int elf_sample_runs,
    "X Execute X runs of the test suite while sampling with oprofile.";
    "--elf-risc",
    Arg.Set elf_risc,
    " Specify that a RISC instruction set is used with fixed-width instructions."
  ]

let elfRep_version = "1"

class elfRep = object (self : 'self_type)

  inherit [string list] faultlocRepresentation as super

  val path = ref ""
  val bytes = ref [| (* array of integer bytes *) |]
  val elf = ref "" (* String to hold binary elf lisp object *)
  val address = ref 0
  val offset = ref 0
  val size = ref 0

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
      address = ref (Global.copy !address) ;
      offset = ref (Global.copy !offset);
      bytes  = begin
        let temp = Array.make (Array.length !bytes) (Array.get !bytes 0) in
          Array.iteri (fun i el -> Array.set temp i el) !bytes;
          ref temp
      end;
      elf = elf;
    >}

  (* use objdump to find the instruction borders in this elf file *)
  method disasm (filename : string ) = begin
    let tmp = Filename.temp_file "disasm" ".objdump-output" in
    let trim str =
      if Str.string_match (Str.regexp "^[ \t]*\\([^ \t].+\\)$") str 0 then
        Str.matched_group 1 str
      else
        str in
    let read_file filename =
      let lst = ref [] in
      let fin = open_in filename in
        (try while true do
           let line = input_line fin in
             lst := line :: !lst
         done with _ -> close_in fin) ;
        List.rev !lst in
    let parse_address line =
      let bytes = ref [] in
        List.iter
          (fun str ->
             try
               bytes := (int_of_string ("0x"^str)) :: !bytes
             with Failure "int_of_string" -> ())
          (Str.split (Str.regexp "[ \t]")
             (String.sub line 10
                (if ((String.length line) > 32) then 21 else (String.length line - 10)))) ;
        ((int_of_string ("0x"^(trim (String.sub line 1 7)))), !bytes) in
    let parse_addresses lines =
      let results = ref [] in
      let header_re = Str.regexp "^\\([0-9a-fA-F]+\\) <\\(.+\\)>:$" in
        List.iter (fun line ->
                     if (not (Str.string_match header_re line 0) &&
                           ((String.length line) > 10) &&
                           (try
                              ignore (int_of_string ("0x"^(trim (String.sub line 1 7)))) ; true
                            with Failure "int_of_string" -> false)) then
                       results := (parse_address line) :: !results) lines ;
        List.sort (fun (a,_) (b,_) -> a-b) !results in

      ignore (Unix.system ("objdump -j .text -d "^filename^">"^tmp)) ;
      let parsed = (parse_addresses (read_file tmp)) in
        (* for debugging: list the memory address of instructions with their sizes *)
        (* List.iter (fun (addr,bytes) -> debug "\t%d:%d\n" addr (List.length bytes)) parsed ; *)
        List.map (fun (_,bytes) -> List.length bytes) parsed
  end

  method bytes_of filename = begin
    let raw_bytes = ref (Array.to_list (text_data filename)) in
      debug "raw_bytes:%d\n" (List.length !raw_bytes) ;
    let tmp_bytes = ref [] in
      if !elf_risc then
        let holder = ref [] in
          List.iter (fun a ->
                       holder := a :: !holder ;
                       if List.length !holder == 4 then begin
                         tmp_bytes := (List.rev !holder) :: !tmp_bytes ;
                           holder := []
                       end) !raw_bytes ;
          Array.of_list (List.rev !tmp_bytes)
      else
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

  method show_bytes () =
    if false then begin
      debug "(";
      Array.iter
        (fun lst ->
           debug "("; List.iter (fun byte -> debug "%d " byte;) lst; debug ")";)
        !bytes;
      debug ")\n";
    end

  method flat_bytes () = (Array.of_list(List.flatten(Array.to_list !bytes)))

  method from_source (filename : string) = begin
    path := filename;
    address := text_address filename;
    offset := text_offset filename;
    bytes := self#bytes_of filename;
    self#show_bytes();
  end

  method output_source source_name = begin
    ignore (Unix.system ("cp " ^ !path ^ " " ^ source_name));
    update_text source_name !offset (self#flat_bytes())
  end

  method internal_compute_source_buffers () =
    let tmp_file =
      Filename.temp_file "internal_compute_source_buffers" ".source-hash" in
    let buffer = Buffer.create 10240 in
      self#output_source tmp_file ;
      List.iter (fun line -> Buffer.add_string buffer line)
        (get_lines tmp_file);
      (try Unix.unlink tmp_file with _ -> ());
      [ None, Buffer.contents buffer ]

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
    debug "ERROR: might not be able to load serialized object, bailing...\n" ;
    path := exit 1;
    address := text_address !path;
    offset := text_offset !path;
    bytes := self#bytes_of !path;
    super#load_binary ~in_channel:fin filename ;
    if in_channel = None then close_in fin
  end

  method max_atom () = (Array.length !bytes)

  method address_offset_to_instruction line =
    let byte_offset = ref(line - !address) in
    let instr_offset = ref 0 in
      Array.iter (fun lst ->
                    if (!byte_offset > 0) then begin
                      byte_offset := (!byte_offset - (List.length lst));
                      instr_offset := (!instr_offset + 1)
                    end) !bytes;
      !instr_offset

  (* convert a memory address into a genome index *)
  method atom_id_of_source_line source_file source_line =
    let instruction_id = self#address_offset_to_instruction(source_line) in
      if instruction_id < 0 || instruction_id > self#max_atom () then begin
        debug "elfrep: bad line access %d\n" instruction_id;
        0
      end
      else
        instruction_id

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
                debug "ERROR: coverage PASSES test Negative %d\n" i ;
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
          let size = Array.length (self#flat_bytes()) in
            List.iter
              (fun (addr, count) ->
                 let index = addr in
                   if (!address <= index) && (index <= (!address + size)) then
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
          if not (Sys.file_exists pos_samp) then
            ignore (Unix.system ("opannotate -a "^pos_exe^">"^pos_samp)) ;
          if not (Sys.file_exists neg_samp) then
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
    debug "elf: lines=%d bytes=%d\n"
      (self#max_atom ()) (Array.length (self#flat_bytes()));
    (* print out information about the code bank *)
    let sortedBank = List.sort (fun a b -> a-b)
      (List.map (fun (a,_) -> a) !fault_localization) in
    let size = List.length !fault_localization in
      debug "elf: code bank size:%d from:%d to:%d\n"
        size (List.nth sortedBank 0) (List.nth sortedBank (size - 1)) ;
  end

  method atom_to_byte atom = List.map int_of_string atom

  method byte_to_atom byte = List.map string_of_int byte

  method atom_length atom = List.length atom

  method get_genome () =
    List.map self#byte_to_atom (Array.to_list !bytes)

  method set_genome new_g =
    bytes := Array.of_list (List.map self#atom_to_byte new_g)

  method get ind =
    let i = self#address_offset_to_instruction ind in
      self#byte_to_atom (Array.get !bytes i)

  method put ind newv =
    let i = self#address_offset_to_instruction ind in
      super#put i newv;
      Array.set !bytes i (self#atom_to_byte newv)

  method swap i j =
    let i_i = self#address_offset_to_instruction i in
    let j_i = self#address_offset_to_instruction j in
      super#swap i_i j_i;
      let temp = Array.get !bytes i_i in
        Array.set !bytes i_i (Array.get !bytes j_i) ;
        Array.set !bytes j_i temp ;
        self#show_bytes() ;

  method delete i =
    let i_i = self#address_offset_to_instruction i in
      super#delete i_i ;
      let num = List.length (Array.get !bytes i_i) in
      let len = Array.length !bytes in
      let rep = Array.make num (if !elf_risc then [0; 0; 160; 225] else [144]) in
        if (i_i == 0) then
          bytes := Array.append rep (Array.sub !bytes 1 (len - 1))
        else if (i_i == (len - 1)) then
          bytes := Array.append (Array.sub !bytes 0 (len - 1)) rep
        else
          bytes := Array.append
            (Array.append (Array.sub !bytes 0 i_i) rep)
            (Array.sub !bytes (i_i + 1) ((len - i_i) - 1)) ;
        self#show_bytes() ;

  method append i j =
    let i_i = self#address_offset_to_instruction i in
    let j_i = self#address_offset_to_instruction j in
      super#append i_i j_i ;
      let inst = ref (Array.get !bytes j_i) in
      let reps = ref (List.length !inst) in
        (* append new instruction into the array *)
        bytes := Array.concat [
          (Array.sub !bytes 0 i_i);
          [|!inst|];
          (Array.sub !bytes i_i ((Array.length !bytes) - i_i));
        ] ;
        (* delete an appropriate amount of nop's *)
        let max x y = if x > y then x else y in
          for p = 0 to max i_i ((Array.length !bytes) - i_i) do
            if (!reps > 0) then begin
              try
                match Array.get !bytes (i_i+p) with
                  | [0; 0; 160; 225] when !elf_risc -> begin
                      reps := !reps - 4 ;
                      Array.set !bytes (i_i+p) []
                    end
                  | [144] when (not !elf_risc) -> begin
                      reps := !reps - 1 ;
                      Array.set !bytes (i_i+p) []
                    end
                  | _     -> begin
                      match Array.get !bytes (i_i-p) with
                        | [0; 0; 160; 225] when !elf_risc -> begin
                            reps := !reps - 4 ;
                            Array.set !bytes (i_i-p) []
                          end
                        | [144] when (not !elf_risc) -> begin
                            reps := !reps - 1 ;
                            Array.set !bytes (i_i-p) []
                          end
                        | _ -> ()
                    end
              with
                | Invalid_argument  "index out of bounds" -> ()
                | Invalid_argument  "Array.sub" -> ()
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
                         [] !bytes)) ;
          self#show_bytes();

end
