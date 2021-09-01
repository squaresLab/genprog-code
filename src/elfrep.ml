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
(** [Elfrep] provides a representation for binary elf (compiled and linked)
    executables.  Like [Asmrep], this mostly extends the [Stringrep.stringRep]
    functionality (since we represent ELF files as lists of strings), with the
    notable exception of the use of oprofile sampling for localization and the
    use of the external libelf for manipulating the binaries to produce testable
    variants. *)
open Elf
open Printf
open Global
open Gaussian
open Rep

(**/**)
exception Not_Supported of string

let elf_risc = ref false
let _ =
  options := !options @
             [
               "--elf-risc", Arg.Set elf_risc,
               " Specify that a RISC instruction set is used with fixed-width instructions."
             ]

let elfRep_version = "1"
(**/**)

(* the majority of the calls out to the system (system, etc) in this module
   do not check the return codes of the call; thus, they may fail silently *)
(** @version 1 *)
class elfRep = object (self : 'self_type)
  (** [Elfrep.elfRep] inherits from [Binrep.binRep] to avoid duplicating
      coverage generation code between [Elfrep.elfRep] and [Asmrep.asmRep] *)
  inherit binRep as super

  (* FIXME, Eric: document these? *)
  (* JD: These values must be mutable to allow self#copy() to work. I don't know
     that they also need to be ref cells, but I just left them as-is. *)
  val mutable path = ref ""
  val mutable bytes = ref [| (* array of integer bytes *) |]
  val mutable elf = ref "" (* String to hold binary elf lisp object *)
  val mutable address = ref 0
  val mutable offset = ref 0
  val mutable size = ref 0

  (**/**)
  method variable_length = true

  method copy () : 'self_type =
    let super_copy = super#copy () in
    path <- ref (Global.copy !path) ;
    address <- ref (Global.copy !address) ;
    offset <- ref (Global.copy !offset);
    bytes  <- begin
      let temp = Array.make (Array.length !bytes) (Array.get !bytes 0) in
      Array.iteri (fun i el -> Array.set temp i el) !bytes;
      ref temp
    end;
    elf <- elf;
    super_copy

  method from_source (filename : string) =
    path := filename;
    address := text_address filename;
    offset := text_offset filename;
    bytes := self#bytes_of filename

  method output_source source_name =
    ignore (system ("cp " ^ !path ^ " " ^ source_name));
    update_text source_name !offset (self#flat_bytes())

  method internal_compute_source_buffers () =
    let tmp_file =
      Filename.temp_file "internal_compute_source_buffers" ".source-hash" in
    self#output_source tmp_file ;
    let str = file_to_string tmp_file in
    (try Unix.unlink tmp_file with _ -> ());
    [ None, Some(str) ]

  method serialize ?out_channel ?global_info (filename : string) =
    let fout =
      match out_channel with
      | Some(v) -> v
      | None -> open_out_bin filename
    in
    Marshal.to_channel fout (elfRep_version) [] ;
    Marshal.to_channel fout !path [] ;
    debug "elf: %s: saved\n" filename ;
    super#serialize ~out_channel:fout filename ;
    if out_channel = None then close_out fout

  method get_atoms () =
    let atoms =
      Array.fold_left (fun (atoms,i) _ ->
          AtomSet.add i atoms, i+1
        ) (AtomSet.empty,1) !bytes
    in
    fst atoms

  method get_compiler_command () =
    (* note the slight difference between this and faultlocSuper#get_compiler_command *)
    "__COMPILER_NAME__ __SOURCE_NAME__ __EXE_NAME__ 2>/dev/null >/dev/null"

  (**/**)

  (** uses objdump to find the instruction borders in a given elf file
      @param filename ELF file to inspect
      @return address_list list of addresses corresponding to instruction
      borders in filename
  *)
  method disasm (filename : string ) = begin
    let tmp = Filename.temp_file "disasm" ".objdump-output" in
    let trim str =
      if Str.string_match (Str.regexp "^[ \t]*\\([^ \t].+\\)$") str 0 then
        Str.matched_group 1 str
      else str
    in
    let parse_address line =
      let line_length =
        if String.length line > 32 then 21 else String.length line - 10
      in
      let line_lst =
        Str.split whitespace_regexp (String.sub line 10 line_length)
      in
      lfoldl
        (fun bytes str ->
           try
             (int_of_string ("0x"^str)) :: bytes
           with Failure "int_of_string" -> bytes)
        [] line_lst
    in
    let parse_addresses lines =
      let header_re =
        Str.regexp "^\\([0-9a-fA-F]+\\) <\\(.+\\)>:$"
      in
      let lines_match =
        List.filter
          (fun line ->
             if not (Str.string_match header_re line 0)
             && (String.length line) > 10 then
               try
                 ignore (int_of_string ("0x"^(trim (String.sub line 1 7)))) ;
                 true
               with Failure "int_of_string" -> false
             else false)
          lines
      in
      let results = lmap parse_address lines_match in
      List.sort compare results
    in
    ignore (system ("objdump -j .text -d "^filename^">"^tmp)) ;
    lmap llen (parse_addresses (get_lines tmp))
  end

  (** @param filename ELF file from which to read bytes
      @return bytes array of lists of bytes of the file *)
  method bytes_of (filename : string) =
    let raw_bytes = ref (Array.to_list (text_data filename)) in
    debug "raw_bytes:%d\n" (List.length !raw_bytes) ;
    let lst =
      if !elf_risc then
        let _,tmp_bytes =
          lfoldl (fun (holder,tmp_bytes) a ->
              let holder = a :: holder in
              if (llen holder) = 4 then
                [], (lrev holder) :: tmp_bytes
              else
                holder,tmp_bytes)
            ([],[]) !raw_bytes
        in
        lrev tmp_bytes
      else
        List.map
          (fun size ->
             let tmp = ref [] in
             for i = 1 to size do
               tmp := (List.hd !raw_bytes) :: !tmp;
               raw_bytes := List.tl !raw_bytes
             done;
             List.rev !tmp) (self#disasm filename)
    in
    Array.of_list lst


  (** @return flat array of bytes of the file (not grouped by instruction
      borders in other words *)
  method flat_bytes () = Array.of_list(List.flatten(Array.to_list !bytes))

  (** @raise Not_Supported deserialization is not supported on the Elf
      representation.  *)
  method deserialize ?in_channel ?global_info (filename : string) =
    raise (Not_Supported "elfrep unable to load serialized object");
    (* FIXME ERIC: is this actually not supported? If so, can we make that
       explicit?
       [EMS] Yes, this is absolutely not working, (I believe because
       of the need to read C objects) I've changed the above to an
       error, is there a way to skip the attempted loading of
       serialized objects for elfrep?
       [CLG] well that's basically what you're doing, already... I mean we
       could move this skip deserialization failure elsewhere but it
       accomplishes what you want here. I got rid of the extraneous stuff. *)


    (** Helper function for [atom_id_of_source_line]
        @param line line in file
        @return instruction offset in that line *)
  method private address_offset_to_instruction line =
    let byte_offset = ref(line - !address) in
    let instr_offset = ref 0 in
    Array.iter (fun lst ->
        if !byte_offset > 0 then begin
          byte_offset := !byte_offset - (List.length lst);
          instr_offset := !instr_offset + 1
        end) !bytes;
    !instr_offset

  (** convert a memory address into a genome index.  Prints a warning but does
      not abort if the pair does not correspond to anything sensible.
      @param source_file file in which the line is found
      @param source_line line in file
      @return list of indices into genome (instruction id) corresponding to [source_line] (in
      source_file).
  *)
  method atom_id_of_source_line source_file source_line =
    let instruction_id = self#address_offset_to_instruction(source_line) in
    if instruction_id < 0 || instruction_id >= (Array.length !bytes) then begin
      debug "elfrep: bad line access %d\n" instruction_id;
      [0]
    end else begin
      (* debug "elf-trans: %d→%d\n" source_line instruction_id ; *)
      [instruction_id]
    end

  (** Elfrep is why we need this function; individuals may only be crossed over
      at reasonable instruction boundaries *)
  method available_crossover_points () =
    let borders lsts =
      List.rev
        (List.tl
           (List.fold_left
              (fun acc el -> ((self#atom_length el) + (List.hd acc)) :: acc)
              [0] lsts))
    in
    let place el lst =
      let out = ref (-1) in
      Array.iteri
        (fun i it ->
           if (!out < 0) && (el = it) then out := i)
        (Array.of_list lst);
      !out
    in
    let g_one = self#get_genome () in    (* raw genomes *)
    let b_one = borders g_one in          (* lengths at atom borders *)
    let combine_function one two =
      let inter = IntSet.elements (IntSet.inter one two) in
      lmap (fun ele -> place ele b_one) inter
    in
    lfoldl
      (fun ele  acc -> IntSet.add acc  ele) IntSet.empty b_one,
    combine_function

  (**/**)
  method private mem_mapping _ _ = hcreate 10

  method private combine_coverage samples optional_mapping =
    let size = Array.length (self#flat_bytes()) in
    let results =
      List.fold_left
        (fun acc (addr, count) ->
           if (!address <= addr) && (addr <= (!address + size)) then
             (addr,count) :: acc
           else acc) [] samples
    in
    List.sort pair_compare results

  method debug_info () =
    debug "elf: lines=%d bytes=%d\n"
      (Array.length !bytes) (Array.length (self#flat_bytes()));
    (* print out information about the code bank *)
    let sortedBank = List.sort compare
        (List.map fst !fault_localization) in
    let size = List.length !fault_localization in
    debug "elf: code bank size:%d from:%d to:%d\n"
      size (List.nth sortedBank 0) (List.nth sortedBank (size - 1)) ;
    (* debug "code bank="; *)
    (* List.map (fun coin -> debug "%d " coin) sortedBank; *)
    (* debug "\n"; *)

  method atom_to_byte atom = List.map int_of_string atom

  method get_genome () : string list list =
    List.map (fun byte -> lmap string_of_int byte) (Array.to_list !bytes)

  method set_genome new_g =
    bytes := Array.of_list (List.map self#atom_to_byte new_g);
    self#updated();
    (**/**)

    (** {6 {L The Elfrep edit functions like {b swap}, {b append} and {b delete}
        must maintain two invariants:
        {ol
        {- the length of bytes must never drop below its initial length
        because the fault localization IDs are never updated.}
        {- the total number of bytes held in the lists in bytes must
        remain constant so we don't change the size of the .text section}}.
        }


        These functions all raise [Invalid_argument] if the arguments are out of
        bounds or otherwise invalid.}
    *)


  method swap i j =
    let starting_length = Array.length !bytes in
    super#swap i j;
    try
      let temp = Array.get !bytes i in
      Array.set !bytes i (Array.get !bytes j) ;
      Array.set !bytes j temp ;
      if starting_length > (Array.length !bytes) then
        debug "ERROR: swap changed the byte length %d->%d\n"
          starting_length (Array.length !bytes);
    with
    | Invalid_argument  "index out of bounds" ->
      debug "ERROR: swap %d %d -> \"index out of bounds\"\n" i j;
      flush stdout ;
      raise (Invalid_argument "index out of bounds");
    | Invalid_argument  "Array.sub" ->
      debug "ERROR: swap %d %d -> \"Array.sub\"\n" i j;
      flush stdout ;
      raise (Invalid_argument "Array.sub");

      (** @raise Abort if the rep is operating on risc *)
  method delete i =
    let starting_length = Array.length !bytes in
    if !elf_risc then
      abort "Error: elfrep#delete is not implemented for risc\n"
    else begin
      super#delete i ;
      try
        let removed = List.length (Array.get !bytes i) in
        let length = Array.length !bytes in
        let replacement =
          if removed > 0 then Array.make removed [144]
          else Array.make 1 [] in
        if i == 0 then
          bytes := Array.append replacement (Array.sub !bytes 1 (length - 1))
        else if i == (length - 1) then
          bytes := Array.append (Array.sub !bytes 0 (length - 1)) replacement
        else
          bytes := Array.append
              (Array.append (Array.sub !bytes 0 i) replacement)
              (Array.sub !bytes (i + 1) ((length - i) - 1)) ;
        if starting_length > (Array.length !bytes) then
          debug "ERROR: delete shortened bytes (%d->%d) length:%d i:%d\n"
            starting_length (Array.length !bytes) length i;
      with
      | Invalid_argument  "index out of bounds" ->
        debug "ERROR: delete %d -> \"index out of bounds\"\n" i;
        flush stdout ;
        raise (Invalid_argument "index out of bounds");
      | Invalid_argument  "Array.sub" ->
        debug "ERROR: delete %d -> \"Array.sub\"\n" i;
        flush stdout ;
        raise (Invalid_argument "Array.sub");
    end

  method append i j =
    let starting_length = Array.length !bytes in
    super#append i j ;
    let inst = ref (Array.get !bytes j) in
    let reps = ref (List.length !inst) in
    (* append new instruction into the array *)
    bytes := Array.concat [
        (Array.sub !bytes 0 i);
        [|!inst|];
        (Array.sub !bytes i ((Array.length !bytes) - i));
      ] ;
    try
      (* delete an appropriate amount of nop's *)
      let max x y = if x > y then x else y in
      for p = 0 to max i ((Array.length !bytes) - i) do
        if !reps > 0 then begin
          if((i+p) < (Array.length !bytes)) then begin
            match Array.get !bytes (i+p) with
            | [0; 0; 160; 225] when !elf_risc -> begin
                reps := !reps - 4 ;
                Array.set !bytes (i+p) []
              end
            | [144] when (not !elf_risc) -> begin
                reps := !reps - 1 ;
                Array.set !bytes (i+p) []
              end
            | _ -> ()
          end ;
          if((i-p) >= 0) then begin
            match Array.get !bytes (i-p) with
            | [0; 0; 160; 225] when !elf_risc -> begin
                reps := !reps - 4 ;
                Array.set !bytes (i-p) []
              end
            | [144] when (not !elf_risc) -> begin
                reps := !reps - 1 ;
                Array.set !bytes (i-p) []
              end
            | _ -> ()
          end ;
        end ;
      done ;
    with
    | Invalid_argument  "index out of bounds" ->
      debug "ERROR: append %d %d -> \"index out of bounds\"\n" i j;
      flush stdout ;
      raise (Invalid_argument "index out of bounds");
    | Invalid_argument  "Array.sub" ->
      debug "ERROR: append %d %d -> \"Array.sub\"\n" i j;
      flush stdout ;
      raise (Invalid_argument "Array.sub");
      (* No longer truncating or removing empty instruction strings *)
      if starting_length > (Array.length !bytes) then
        debug "ERROR: append changed the byte length %d->%d\n"
          starting_length (Array.length !bytes);

end
