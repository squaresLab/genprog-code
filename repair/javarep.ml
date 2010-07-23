open Printf
open Rep
open Global


class javarep : representation = 
  object (self)
    inherit nullRep
    val base = ref Jast.dummyfile
    val imports = ref []
    
    val stmt_count = ref 1
    val stmt_map = ref (Hashtbl.create 255)
    val stmt_count = ref 1
    val weighted_path = ref ([] : (atom_id*float) list)
    val weights = ref (Hashtbl.create 255)
    val already_sourced = ref None
    val already_compiled = ref None
    val history = ref []
    
    method copy = Jast.copy !base
    method save_binary (filename:string) = begin
      let fout = open_out_bin filename in
      Marshal.to_channel fout (Jast.version) [] ;
      Marshal.to_channel fout (!base) [] ;
      Marshal.to_channel fout (!stmt_map) [] ;
      Marshal.to_channel fout (!stmt_count) [] ;
      Marshal.to_channel fout (!weighted_path) [] ;
      Marshal.to_channel fout (!weights) [] ;
      debug "jast: %s: saved\n" filename ;
      close_out fout
      
    method load_binary (filename:string) = begin
    (*unedited portion from cilrep: *)
    (*if !use_path_files then begin
      debug "cilRep: --use-path-files: not loading %s" filename ;
      failwith "--use-path-files"
    end ;
    if !use_weight_file then begin
      debug "cilRep: --use-weight-files: not loading %s" filename ;
      failwith "--use-weight-files"
    end ;*)
    let fout = open_in_bin filename in
    let version = Marshal.from_channel fout in
    if version != Jast.version then begin
      debug "jast: %s has old version\n" filename ;
      failwith "version mismatch"
    end ;
    base := Marshal.from_channel fout ;
    stmt_map := Marshal.from_channel fout ;
    stmt_count := Marshal.from_channel fout ;
    weighted_path := Marshal.from_channel fout ;
    weights := Marshal.from_channel fout ;
    debug "jast: %s: loaded\n" filename ;
    close_in fout
  end

    method from_source (filename:string) =
      let file = Jast.build_ast filename; in
      base := file;
    method output_source source_name =
      Jast.write !base source_name
    method sanity_check () = ()
    method compute_fault_localization () = ()
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
    
    method private internal_test_case exe_name test = begin
      let port_arg = Printf.sprintf "%d" !port in
      change_port ();
      let cmd = Printf.sprintf "%s %s %s %s >& /dev/null"
        !test_command exe_name (test_name test) port_arg in
      match Stats2.time "test" Unix.system cmd with
      | Unix.WEXITED(0) -> true
      | _ -> false
    end
    
    method test_case test = try begin
      let try_cache () =
      (* first, maybe we'll get lucky with the persistent cache *)
      (match !already_sourced with
      | None -> ()
      | Some(digest) -> begin
        match test_cache_query digest test with
        | Some(x) -> raise (Test_Result x)
        | _ -> ()
        end
      )
      in
      try_cache () ;
      
      let exe_name, worked = match !already_compiled with
      | None -> (* never compiled before, so compile it now *)
        let source_name = sprintf "%05d.c" !test_counter in
        let exe_name = sprintf "./%05d" !test_counter in
        incr test_counter ;
        self#output_source source_name ;
        try_cache () ;
        if not (self#compile source_name exe_name) then
          exe_name,false
        else
          exe_name,true
      | Some("") -> "", false (* it failed to compile before *)
      | Some(exe) -> exe, true (* it compiled successfully before *)
      in
      let result =
        if worked then begin
          (* actually run the program on the test input *)
          self#internal_test_case exe_name test
          end 
        else false
      in
      (* record result for posterity in the cache *)
      (match !already_sourced with
      | None -> ()
      | Some(digest) -> test_cache_add digest test result
      ) ;
      raise (Test_Result(result))

      end with Test_Result(x) -> (* additional bookkeeping information *)
        (match !already_sourced with
        | None -> ()
        | Some(digest) -> Hashtbl.replace tested (digest,test) ()
        ) ;
        x 
    method debug_info = begin
      debug "jast: stmt_count = %d\n" !stmt_count;
      debug "jast: stmts in weighted_path = %d\n" (List.length !weighted_path);
      debug "jast: stmts in weighted_path with weight >= 1.0 = %d\n"
        (List.length (List.filter (fun (a,b) -> b >= 1.0) !weighted_path));
      end
    method max_atom () = 
      Jast.get_max_id !base
      
    method get_fault_localization = !weighted_path
    
    method get_fix_localization = 
      let res = ref [] in
      Hashtbl.iter (fun stmt_id weight ->
        res := (stmt_id,weight)::!res
      ) !weights;
      !res
      
    method delete stmt_id = 
      Jast.delete !base stmt_id
      
    method append append_after what_to_append =
      Jast.append !base append_after what_to_append
      
    method swap stmt_id1 stmt_id2 = 
      Jast.swap !base stmt_id1 stmt_id2
      
    method put stmt_id stmt =
      Jast.replace_node !base stmt_id stmt
      
    method get stmt_id = 
      Jast.get_node !base stmt_id
      
    method name () = 
      if !history = [] then "original"
      else begin
        let b = Buffer.create 40 in
        ignore (List.rev_map (fun s ->
          Buffer.add_string b s; ()
        ) !history) ;
        Buffer.contents b
      end
      
    method add_name_note str =
      history := str::!history
    
    
    
    
    
    
    
    
    
      