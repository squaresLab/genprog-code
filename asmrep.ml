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

(*************************************************************************
 *************************************************************************
               ASM Representation - Compiled Assembly Programs
 *************************************************************************
 *************************************************************************)

let sample_runs = ref 100
let _ =
  options := !options @
  [
    "--sample-runs",
    Arg.Set_int sample_runs,
    "X Execute X runs of the test suite while sampling with oprofile."
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
    let beg_points = ref [] in
    let end_points = ref [] in
    let beg_regx = Str.regexp "\\.globl [0-9a-zA-Z]+" in
    let end_regx = Str.regexp "^[ \t]+\\.size.*" in
      Array.iteri (fun i line ->
                     if ( i > 0 ) then begin
                       if (Str.string_match beg_regx (List.hd line) 0) then
                         beg_points := i :: !beg_points ;
                       if (Str.string_match end_regx (List.hd line) 0) then
                         end_points := i :: !end_points ;
                     end
                  ) !base ;
      range := List.rev (List.combine !beg_points !end_points) ;
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

  method max_atom () = List.fold_left (+) 0 (List.map (fun (a,b) -> (b - a)) !range)

  method atom_id_of_source_line source_file source_line =
    if source_line < 0 || source_line > self#max_atom () then
      0
    else
      source_line

  method structural_signature =
    failwith "asm: no structural differencing"

  method get_compiler_command () =
    "__COMPILER_NAME__ -o __EXE_NAME__ __SOURCE_NAME__ __COMPILER_OPTIONS__ "^
      "2>/dev/null >/dev/null"

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
        let join = "|awk '{print $3}'|sort -n" in
        let mapping  = coverage_exename^".mapping" in
        let pos_samp = pos_exe^".samp" in
        let neg_samp = neg_exe^".samp" in
        let pos_path = coverage_outname^".pos" in
        let neg_path = coverage_outname^".neg" in
          (* calculate the mapping from addresses to asm LOC *)
          ignore (Unix.system ("mem-mapping "^coverage_sourcename^" "^pos_exe^">"^mapping)) ;
          (* collect the samples *)
          ignore (Unix.system ("opannotate -a "^pos_exe^grep^">"^pos_samp)) ;
          ignore (Unix.system ("opannotate -a "^neg_exe^grep^">"^neg_samp)) ;
          (* convert samples to LOC *)
          ignore (Unix.system ("join "^pos_samp^" "^mapping^join^">"^pos_path)) ;
          ignore (Unix.system ("join "^neg_samp^" "^mapping^join^">"^neg_path)) ;
    end          
    
  method debug_info () = begin
    debug "asm: lines = %d\n" (self#max_atom ());
  end

  method place_atom (atom_i) = begin
    (* return global offset from in-code offset *)
    let j = ref 0 in
    let i = ref atom_i in
      List.iter (fun (b,e) ->
                   if (!j == 0) then begin
                     i := !i - (e - b) ;
                     if (!i <= 0) then j := (e + !i) ;
                   end
                ) !range ;
      !i
  end

  method get ind =
    !base.(self#place_atom ind)
  method put ind newv =
    let idx = self#place_atom ind in
    super#put idx newv ;
    !base.(idx) <- newv

  method swap i_off j_off =
    let i = self#place_atom i_off in
    let j = self#place_atom j_off in
    super#swap i j ;
    let temp = !base.(i) in
    !base.(i) <- !base.(j) ;
    !base.(j) <- temp

  method delete i_off =
    let i = self#place_atom i_off in
    super#delete i ;
    !base.(i) <- []

  method append i_off j_off =
    let i = self#place_atom i_off in
    let j = self#place_atom j_off in
    super#append i j ;
    !base.(i) <- !base.(i) @ !base.(j)

end
