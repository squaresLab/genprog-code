(*
 *
 * Copyright (c) 2012-2013, 
 *  Wes Weimer          <weimer@cs.virginia.edu>
 *  Stephanie Forrest   <forrest@cs.unm.edu>
 *  Claire Le Goues     <legoues@cs.virginia.edu>
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
(** Stringrep represents a program as an array of STRINGS.  
    This a simple/trivial implementation of the "Rep" interface. It shows 
    the base minimum required to implement a representation for program
    repair. 
*)

open Printf
open Global
open Rep

let stringRep_version = "1" 

class stringRep = object (self : 'self_type)
  (** although stringRep inherits from faultlocRep, it does not do fault
      localization by default *)
  inherit [string list, string list] faultlocRepresentation as super

  (** the basic genome for stringRep is an array of string lists *)   
  val genome = ref [| (* array of string lists *) |] 
  (** by default, stringRep variants are of fixed length *)
  method variable_length = false

  method get_genome () = Array.to_list !genome
  method set_genome g = self#updated(); genome := Array.of_list g
  method atom_length atom = llen atom

  method genome_length () = 
    lfoldl (fun acc atom -> acc + (self#atom_length atom)) 0 (self#get_genome())

  method atom_to_str slist = 
    let b = Buffer.create 255 in 
      List.iter (fun s -> Printf.bprintf b "%S" s) slist ;
      Buffer.contents b 

  method copy () : 'self_type = 
    let super_copy : 'self_type = super#copy () in 
      super_copy#internal_copy () 

  method internal_copy () : 'self_type = 
    {< genome = ref (Global.copy !genome) ; >} 

  method from_source (filename : string) = 
    let lst = get_lines filename in
      genome := Array.of_list (lmap (fun i -> [i]) lst)

  (* internal_compute_source_buffers can theoretically overflow the buffer if
     the rep is extremely large *)
  method internal_compute_source_buffers () = 
    let buffer = Buffer.create 10240 in 
      Array.iteri (fun i line_list ->
        List.iter (fun line -> 
          Printf.bprintf buffer "%s\n" line 
        ) line_list 
      ) !genome ;
      [ None, (Buffer.contents buffer) ]

  method serialize ?out_channel ?global_info (filename : string) =
    let fout = 
      match out_channel with
      | Some(v) -> v
      | None -> open_out_bin filename 
    in 
      Marshal.to_channel fout (stringRep_version) [] ; 
      Marshal.to_channel fout (!genome) [] ;
      debug "stringRep: %s: saved\n" filename ; 
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
      if version <> stringRep_version then begin
        debug "stringRep: %s has old version\n" filename ;
        failwith "version mismatch" 
      end ;
      genome := Marshal.from_channel fin ; 
      debug "stringRep: %s: loaded\n" filename ; 
      super#deserialize ~in_channel:fin ?global_info:global_info filename ; 
      if in_channel = None then close_in fin 

  method max_atom () = Array.length !genome 

  method atom_id_of_source_line source_file source_line = 
    if source_line < 0 || source_line > self#max_atom () then 0
    else source_line 

  method instrument_fault_localization _ _ _ = 
    failwith "stringRep: no fault localization" 

  method debug_info () = debug "stringRep: lines = 1--%d\n" (self#max_atom ())

  (* the rep-modifying methods, like get,put, swap, append, etc, do
     not do error checking on their arguments to guarantee that they
     are valid indices into the rep array.  Thus, they may fail *)
     method get idx = !genome.(pred idx)

  method put idx newv = !genome.(pred idx) <- newv 

  method swap i j = 
    super#swap i j ; 
    let temp = !genome.(pred i) in
      !genome.(pred i) <- !genome.(pred j) ;
      !genome.(pred j) <- temp 

  method delete i =
    super#delete i ; 
    !genome.(pred i) <- []  

  method append i j = 
    super#append i j ; 
    !genome.(pred i) <- !genome.(pred i) @ !genome.(pred j) 

  method replace i j = 
    super#replace i j ; 
    !genome.(pred i) <- !genome.(pred j) 

end 
