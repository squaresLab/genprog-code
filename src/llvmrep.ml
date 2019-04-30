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
(** LLVM IR representation *)

open Global
open Rep

let llvmRep_version = "1"

class llvmRep = object (self : 'self_type)

  (** holds the name of the `llvm-mutate' excutable **)
  val llvm_mutate = "llvm-mutate"

  (** no fault localization *)
  inherit [string list, string list] faultlocRepresentation as super

  (** the genome for llvmRep is a single large string *)
  val genome = ref ""

  (** llvmRep variants are of not fixed length *)
  method variable_length = true

  method get_genome () = failwith "llvmRep: black box genome"
  method set_genome g = failwith "llvmRep: black box genome"
  method genome_length () = failwith "llvmRep: black box genome"

  method copy () : 'self_type =
    let super_copy : 'self_type = super#copy () in
    super_copy

  method from_source (filename : string) =
    genome := file_to_string filename

  method to_source (filename : string) =
    string_to_file filename !genome

  method get_compiler_command () =
    match !compiler_command with
    | "" -> "cat __SOURCE_NAME__|"^llvm_mutate^" -l -o __EXE_NAME__"
    |  x -> x

  (* internal_compute_source_buffers can theoretically overflow the buffer if
     the rep is extremely large *)
  method internal_compute_source_buffers () =
    [ None, Some(copy !genome) ]

  method serialize ?out_channel ?global_info (filename : string) =
    let fout =
      match out_channel with
      | Some(v) -> v
      | None -> open_out_bin filename
    in
    Marshal.to_channel fout (llvmRep_version) [] ;
    Marshal.to_channel fout (!genome) [] ;
    debug "llvmRep: %s: saved\n" filename ;
    super#serialize ~out_channel:fout ?global_info:global_info filename ;
    if out_channel = None then close_out fout

  (* load in serialized state.  Deserialize can fail if the file from which the
     rep is being read does not conform to the expected format, or if the
     version written to that file does not match the current rep version. *)
  method deserialize ?in_channel ?global_info (filename : string) =
    let fin =
      match in_channel with
      | Some(v) -> v
      | None -> open_in_bin filename
    in
    let version = Marshal.from_channel fin in
    if version <> llvmRep_version then begin
      debug "llvmRep: %s has old version\n" filename ;
      failwith "version mismatch"
    end ;
    genome := Marshal.from_channel fin ;
    debug "llvmRep: %s: loaded\n" filename ;
    super#deserialize ~in_channel:fin ?global_info:global_info filename ;
    if in_channel = None then close_in fin

  (* Run an llvm-mutate command with a temporary source file *)
  method run (cmd : string) : unit =
    let tmp_from = Filename.temp_file "llvmRepFrom" ".ll" in
    let tmp_to = Filename.temp_file "llvmRepTo" ".ll" in
    let cmd = "cat "^tmp_from^"|"^llvm_mutate^" "^cmd^" >"^tmp_to in
    let cleanup () =
      if Sys.file_exists tmp_from then Sys.remove tmp_from;
      if Sys.file_exists tmp_to then Sys.remove tmp_to in
    (* write to tmp_from *)
    self#to_source tmp_from;
    (* run the command *)
    debug "%s\n" cmd;
    let return =
      match (system cmd) with
      | Unix.WEXITED(0) -> true
      | _ -> false in
    (* read from tmp_from *)
    self#from_source tmp_to;
    cleanup();
    if return then
      ()
    else begin
      failwith "llvmRep: `"^cmd^"' failed";
      ()
    end

  method max_atom () =
    let tmp_from = Filename.temp_file "llvmRepFrom" ".ll" in
    let tmp_to = Filename.temp_file "llvmRepTo" ".ll" in
    let cleanup () =
      if Sys.file_exists tmp_from then Sys.remove tmp_from;
      if Sys.file_exists tmp_to then Sys.remove tmp_to in
    (* write to tmp_from *)
    self#to_source tmp_from;
    (* run the command *)
    let cmd = "cat "^tmp_from^"|"^llvm_mutate^" -I -o /dev/null 2>"^tmp_to in
    let return =
      match (system cmd) with
      | Unix.WEXITED(0) -> true
      | _ -> false in
    let result =
      if return then (my_int_of_string (file_to_string tmp_to))
      else begin failwith "llvmRep: `"^cmd^"' failed"; 0 end in
    cleanup();
    result

  method get_atoms () =
    List.fold_left (fun atoms i -> AtomSet.add i atoms)
      AtomSet.empty (1--(self#max_atom ()))

  method source_line_of_atom_id (id : int) = "",id

  method atom_id_of_source_line file line = [line]

  method atom_to_str _ =
    failwith "llvmRep: does not implement `atom_to_str'"

  (* override this because the default must be caching or something *)
  method output_source source_name = string_to_file source_name !genome

  method instrument_fault_localization
      coverage_sourcename coverage_exename coverage_outname =
    (* Instrument with calls to tracing routine *)
    let tmp_from = Filename.temp_file "llvmRepFrom" ".ll" in
    let cmd = "cat "^tmp_from^"|"^llvm_mutate^" -t >"^coverage_sourcename in
    self#to_source tmp_from;
    match (system cmd) with
    | Unix.WEXITED(0) -> ()
    | _ -> failwith "llvmRep: fault localization instrumentation failed";
      if Sys.file_exists tmp_from then Sys.remove tmp_from;

  method debug_info () = debug "llvmRep: lines = 1--%d\n" (self#max_atom ())

  method swap i j =
    super#swap i j ;
    self#run (Printf.sprintf "-s %d,%d" i j)

  method delete i =
    super#delete i ;
    self#run (Printf.sprintf "-c %d" i)

  method append i j =
    super#append i j ;
    self#run (Printf.sprintf "-i %d,%d" i j)

  method replace i j =
    super#replace i j ;
    self#run (Printf.sprintf "-r %d,%d" i j)

end
