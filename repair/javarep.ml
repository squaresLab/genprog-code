open Printf
open Rep
open Global

let javaRep_version = "1" 

class javaRep = object (self : 'self_type)
    inherit [Jast.ast_node] faultlocRepresentation as super 

    val base = ref Jast.dummyfile
    val imports = ref []

  (* make a fresh copy of this variant *) 
  method copy () : 'self_type = 
    let super_copy : 'self_type = super#copy () in 
    super_copy#internal_copy () 

  (* being sure to update our local instance variables *) 
  method internal_copy () : 'self_type = 
    {< base = ref (Global.copy !base) ; 
       imports = ref !imports ; >} 

  method save_binary ?out_channel (filename : string) = begin
    let fout = 
      match out_channel with
      | Some(v) -> v
      | None -> open_out_bin filename 
    in 
    Marshal.to_channel fout (javaRep_version) [] ; 
    Marshal.to_channel fout (!base) [] ;
    super#save_binary ~out_channel:fout filename ;
    debug "javaRep: %s: saved\n" filename ; 
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
    if version <> javaRep_version then begin
      debug "javaRep: %s has old version\n" filename ;
      failwith "version mismatch" 
    end ;
    base := Marshal.from_channel fin ; 
    super#load_binary ~in_channel:fin filename ; 
    debug "javaRep: %s: loaded\n" filename ; 
    if in_channel = None then close_in fin 
  end 
    
    method from_source (filename:string) =
      let file = Jast.build_ast filename; in
      base := file;
    method output_source source_name =
      Jast.write !base source_name

    method compile ?(keep_source=false) source_name exe_name = begin
      let cmd = Printf.sprintf "javac %s" source_name in
      let result = (match Stats2.time "compile" Unix.system cmd with
      | Unix.WEXITED(0) ->
        already_compiled := Some(exe_name);
        true
      | _ ->
        already_compiled := Some("");
        false
      ) in
      if not keep_source then begin
        Unix.unlink source_name;
      end;
      result
    end

  method get_compiler_command () = 
    assert(!use_subdirs = true); 
    (* only works if you compile each variant in a sub-directory *) 
    "--compiler-command __COMPILER_NAME__ __SOURCE_NAME__ __COMPILER_OPTIONS__ >& /dev/null"

  method debug_info () = begin
    debug "javaRep: nothing to debug?" 
  end 

  method atom_id_of_source_line source_file source_line = 
    failwith "javaRep: no mapping from source lines to atom ids yet!" 

  method instrument_fault_localization _ _ _ = 
    failwith "javaRep: no fault localization" 

    method max_atom () = 
      Jast.get_max_id !base
      
    method delete stmt_id = 
      super#delete stmt_id ; 
      (* FIXME: do you want
          base := Jast.delete ...
        here instead?  *)
      ignore (Jast.delete !base stmt_id)
      
    method append (append_after:atom_id) (what_to_append:atom_id) =
      super#append append_after what_to_append ; 
      (* FIXME: *) 
      failwith "Jast.append !base append_after what_to_append"
      
    method swap stmt_id1 stmt_id2 = 
      super#swap stmt_id1 stmt_id2 ; 
      (* FIXME: do you want
        base := Jast.swap ...
          here instead? *) 
      ignore (Jast.swap !base stmt_id1 stmt_id2)
      
    method put stmt_id stmt =
      super#put stmt_id stmt ; 
      (* FIXME *) 
      failwith "Jast.replace_node !base stmt_id stmt"
      
    method get stmt_id = 
      Jast.get_node !base stmt_id
    
end
    
    
    
    
    
    
    
    
      
