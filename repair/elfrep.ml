(** Elfrep provides a representation for binary elf (compiled and linked)
    executables.  Like ASMRep, Elfrep mostly extends the Stringrep functionality
    (since we represent ELF files as lists of strings), with the notable
    exception of the use of oprofile sampling for localization and the use of
    the external libelf for manipulating the binaries to produce testable
    variants.  As with multiopt, CLG did not write this, so cannot comment it as
    well as she can some other modules in GenProg. *)
open Elf
open Printf
open Global
open Gaussian
open Rep
open Stringrep

let elf_risc = ref false
let _ =
  options := !options @
    [
      "--elf-risc", Arg.Set elf_risc,
      " Specify that a RISC instruction set is used with fixed-width instructions."
    ]

let elfRep_version = "1"

(* the majority of the calls out to the system (Unix.system, etc) in this module
   do not check the return codes of the call; thus, they may fail silently *)
class elfRep = object (self : 'self_type)
  (** elfRep inherits from binRep to avoid duplicating coverage generation code
      between elfrep and asmrep *)
  inherit binRep as super 

  val path = ref ""
  val bytes = ref [| (* array of integer bytes *) |]
  val elf = ref "" (* String to hold binary elf lisp object *)
  val address = ref 0
  val offset = ref 0
  val size = ref 0

  method variable_length = true

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

  (* disasm uses objdump to find the instruction borders in this elf file *)
  (* FIXME ERIC: CLG removed code here that *appeared* dead to her.  Please
     double-check to make sure she didn't break anything *)
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
      ignore (Unix.system ("objdump -j .text -d "^filename^">"^tmp)) ;
      lmap llen (parse_addresses (get_lines tmp))
  end

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


  method flat_bytes () = Array.of_list(List.flatten(Array.to_list !bytes))

  method from_source (filename : string) =
    path := filename;
    address := text_address filename;
    offset := text_offset filename;
    bytes := self#bytes_of filename

  method output_source source_name =
    ignore (Unix.system ("cp " ^ !path ^ " " ^ source_name));
    update_text source_name !offset (self#flat_bytes())

  method internal_compute_source_buffers () =
    let tmp_file =
      Filename.temp_file "internal_compute_source_buffers" ".source-hash" in
      self#output_source tmp_file ;
      let str = file_to_string tmp_file in
        (try Unix.unlink tmp_file with _ -> ());
        [ None, str ]
          
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

  (* it is not clear to CLG that this deserialize will ever work; in any case,
     it definitely fails if there's a version mismatch or if the binary file
     does not conform to the expected format. *)
  method deserialize ?in_channel ?global_info (filename : string) =
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
      (* FIXME ERIC: is this actually not supported? If so, can we make that
         explicit? *)
      path := exit 1;
      address := text_address !path;
      offset := text_offset !path;
      bytes := self#bytes_of !path;
      super#deserialize ~in_channel:fin ?global_info:global_info filename ;
      if in_channel = None then close_in fin

  method max_atom () = Array.length !bytes

  method address_offset_to_instruction line =
    let byte_offset = ref(line - !address) in
    let instr_offset = ref 0 in
      Array.iter (fun lst ->
        if !byte_offset > 0 then begin
          byte_offset := !byte_offset - (List.length lst);
          instr_offset := !instr_offset + 1
        end) !bytes;
      !instr_offset

  (* convert a memory address into a genome index *)
  method atom_id_of_source_line source_file source_line =
    let instruction_id = self#address_offset_to_instruction(source_line) in
      if instruction_id < 0 || instruction_id >= self#max_atom () then begin
        debug "elfrep: bad line access %d\n" instruction_id;
        0
      end else instruction_id

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
        
  method get_compiler_command () = 
    (* note the slight difference between this and faultlocSuper#get_compiler_command *)
    "__COMPILER_NAME__ __SOURCE_NAME__ __EXE_NAME__ 2>/dev/null >/dev/null"

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
      (self#max_atom ()) (Array.length (self#flat_bytes()));
    (* print out information about the code bank *)
    let sortedBank = List.sort compare
      (List.map fst !fault_localization) in
    let size = List.length !fault_localization in
      debug "elf: code bank size:%d from:%d to:%d\n"
        size (List.nth sortedBank 0) (List.nth sortedBank (size - 1)) ;

  method atom_to_byte atom = List.map int_of_string atom

  method get_genome () : string list list =
    List.map (fun byte -> lmap string_of_int byte) (Array.to_list !bytes)

  method set_genome new_g =
    bytes := Array.of_list (List.map self#atom_to_byte new_g);
    self#updated();


  method put ind newv =
    super#put ind newv;
    Array.set !bytes ind (self#atom_to_byte newv)
  (*
    The following must maintain two invariants.
    1. The length of bytes must never drop below its initial length
    because the fault localization IDs are never updated
    2. The total number of bytes help in the lists in bytes must
    remain constant so we don't change the size of the .text section
  *)      

  (* the elfrep-modifying functions like swap, etc will output an error to
     stdout if the requested modification is invalid (does not maintain the
     invariants stiuplated above), but will not abort; I don't know what this
     means for the rest of a run containing such a (corrupted) variant *)
  method swap i j =
    let starting_length = Array.length !bytes in
      super#swap i j;
      let temp = Array.get !bytes i in
        Array.set !bytes i (Array.get !bytes j) ;
        Array.set !bytes j temp ;
        if (starting_length > (Array.length !bytes)) then
          debug "ERROR: swap changed the byte length %d->%d\n"
            starting_length (Array.length !bytes);

  (* this will abort if the rep is operating on risc *)
  method delete i =
    let starting_length = Array.length !bytes in
      if !elf_risc then 
        abort "Error: elfrep#delete is not implemented for risc\n"
      else begin
        super#delete i ;
        let removed = List.length (Array.get !bytes i) in
        let length = Array.length !bytes in
        let replacement =
          if (removed > 0) then Array.make removed [144]
          else Array.make 1 [] in
          if (i == 0) then
            bytes := Array.append replacement (Array.sub !bytes 1 (length - 1))
          else if (i == (length - 1)) then
            bytes := Array.append (Array.sub !bytes 0 (length - 1)) replacement
          else
            bytes := Array.append
              (Array.append (Array.sub !bytes 0 i) replacement)
              (Array.sub !bytes (i + 1) ((length - i) - 1)) ;
          if (starting_length > (Array.length !bytes)) then
            debug "ERROR: delete shortened bytes (%d->%d) length:%d i:%d\n"
              starting_length (Array.length !bytes) length i;
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
        (* delete an appropriate amount of nop's *)
        let max x y = if x > y then x else y in
          for p = 0 to max i ((Array.length !bytes) - i) do
            if (!reps > 0) then begin
              try
                match Array.get !bytes (i+p) with
                | [0; 0; 160; 225] when !elf_risc -> begin
                  reps := !reps - 4 ;
                  Array.set !bytes (i+p) []
                end
                | [144] when (not !elf_risc) -> begin
                  reps := !reps - 1 ;
                  Array.set !bytes (i+p) []
                end
                | _     -> begin
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
                end
              with
              | Invalid_argument  "index out of bounds" -> ()
              | Invalid_argument  "Array.sub" -> ()
            end
          done ;
          (* No longer truncating or removing empty instruction strings *)
          if starting_length > (Array.length !bytes) then
            debug "ERROR: append changed the byte length %d->%d\n"
              starting_length (Array.length !bytes);

end
