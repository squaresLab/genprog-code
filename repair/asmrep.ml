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
open Rep

let asmRep_version = "2"

class asmRep = object (self : 'self_type)

  inherit [string list] faultlocRepresentation as super
  (* TODO: implement faultlocRepresentation to apply lines of memory addresses *)

  val base = ref [| (* array of string lists *) |]

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
    {< base = ref (Global.copy !base) ; >}

  method from_source (filename : string) = begin
    let lst = ref [] in
    let fin = open_in filename in
    (try while true do
      let line = input_line fin in
      lst := [line] :: !lst
    done with _ -> close_in fin) ;
    base := Array.of_list ([] :: (List.rev !lst))
  end


  method output_source source_name = begin
    let fout = open_out source_name in
    Array.iteri (fun i line_list ->
      if i > 0 then begin
        List.iter (fun line ->
          Printf.fprintf fout "%s\n" line
        ) line_list
      end
    ) !base ;
    close_out fout ;
    let digest = Digest.file source_name in
    already_sourced := Some([source_name],[digest]) ;
  end

  method save_binary ?out_channel (filename : string) = begin
    let fout =
      match out_channel with
      | Some(v) -> v
      | None -> open_out_bin filename
    in
    Marshal.to_channel fout (asmRep_version) [] ;
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
    base := Marshal.from_channel fin ;
    super#load_binary ~in_channel:fin filename ;
    debug "asm: %s: loaded\n" filename ;
    if in_channel = None then close_in fin
  end

  method max_atom () = (Array.length !base) - 1

  method atom_id_of_source_line source_file source_line =
    if source_line < 0 || source_line > self#max_atom () then
      0
    else
      source_line

  method structural_signature =
    failwith "asm: no structural differencing"

  method get_compiler_command () =
    "__COMPILER_NAME__ -o __EXE_NAME__ __SOURCE_NAME__ __COMPILER_OPTIONS__ 2>/dev/null >/dev/null"

  method instrument_fault_localization _ _ _ =
    failwith "asm: no fault localization"

  method debug_info () = begin
    debug "asm: lines = %d\n" (self#max_atom ());
  end

  method get idx =
    !base.(idx)
  method put idx newv =
    super#put idx newv ;
    !base.(idx) <- newv

  method swap i j =
    super#swap i j ;
    let temp = !base.(i) in
    !base.(i) <- !base.(j) ;
    !base.(j) <- temp
  method delete i =
    super#delete i ;
    !base.(i) <- []
  method append i j =
    super#append i j ;
    !base.(i) <- !base.(i) @ !base.(j)

end
