(* 
 * Program Repair Prototype (v2) 
 *
 * Program Representation -- array of STRINGs 
 *
 * This a simple/trivial implementation of the "Rep" interface. It shows 
 * the base minimum required to implement a representation for program
 * repair. 
 *
 *)

open Printf
open Global
open Rep

let stringRep_version = "1" 

class stringRep = object (self : 'self_type)

  inherit [string list] faultlocRepresentation as super
  (* inheriting from faultlocRep is a bit of a cheat here, since I
   * don't plan to add any fault localization! This is mostly for show. *) 
   
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
    let line_count = ref 0 in 
    (try while true do
      let line = input_line fin in
      incr line_count ;
      lst := [line] :: !lst 
    done with _ -> close_in fin) ; 
    base := Array.of_list (List.rev !lst);
    let atom_set = ref AtomSet.empty in 
    for i = 1 to !line_count do 
      atom_set := AtomSet.add i !atom_set ;  
    done 
  end 

  method load_oracle filelist = failwith "load oracle not implemented for stringrep"

  method internal_compute_source_buffers () = 
    let buffer = Buffer.create 10240 in 
    Array.iteri (fun i line_list ->
        List.iter (fun line -> 
          Printf.bprintf buffer "%s\n" line 
        ) line_list 
    ) !base ;
    [ None, (Buffer.contents buffer) ]

  method serialize ?out_channel (filename : string) = begin
    let fout = 
      match out_channel with
      | Some(v) -> v
      | None -> open_out_bin filename 
    in 
    Marshal.to_channel fout (stringRep_version) [] ; 
    Marshal.to_channel fout (!base) [] ;
    super#serialize ~out_channel:fout filename ;
    debug "stringRep: %s: saved\n" filename ; 
    if out_channel = None then close_out fout 
  end 

  (* load in serialized state *) 
  method deserialize ?in_channel (filename : string) = begin
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
    base := Marshal.from_channel fin ; 
    super#deserialize ~in_channel:fin filename ; 
    debug "stringRep: %s: loaded\n" filename ; 
    if in_channel = None then close_in fin 
  end 

  method max_atom () = (Array.length !base) 

  method atom_id_of_source_line source_file source_line = 
    if source_line < 0 || source_line > self#max_atom () then
      0
    else
      source_line 

  method get_compiler_command () = 
    failwith "stringRep: ERROR: use --compiler-command" 

  method instrument_fault_localization _ _ _ = 
    failwith "stringRep: no fault localization" 

  method debug_info () = begin
    debug "stringRep: lines = 1--%d\n" (self#max_atom ());
  end 

  method get idx = 
    !base.(pred idx) 
  method put idx newv =
    super#put idx newv ; 
    !base.(pred idx) <- newv 

  method swap i j = 
    super#swap i j ; 
    let temp = !base.(pred i) in
    !base.(pred i) <- !base.(pred j) ;
    !base.(pred j) <- temp 
  method delete i =
    super#delete i ; 
    !base.(pred i) <- []  
  method append i j = 
    super#append i j ; 
    !base.(pred i) <- !base.(pred i) @ !base.(pred j) 

end 
