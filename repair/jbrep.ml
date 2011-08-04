
(*                           *
 *  Jimple Representation    *
 *                           *)
(*eventually should be a java
bytecode representation*)

open Printf
open Global
open Cil
open Rep
open Jast

let jbRep_version = "1" 

class jbrep = object (self : 'self_type)
  inherit [string list] faultlocRepresentation as super
    
  val base = ref [| (* array of string lists *) |] 
  val oracle_code : (string, Cil.file) Hashtbl.t ref = ref (Hashtbl.create 11)
  val filst_list_path = ref ""
  val use_build_file = ref false
  val filelist = ref []

  method atom_to_str slist = 
    let b = Buffer.create 255 in 
    List.iter (fun s -> Printf.bprintf b "%S" s) slist ;
    Buffer.contents b 
 
  method copy () : 'self_type = 
    let super_copy : 'self_type = super#copy () in 
    super_copy#internal_copy () 
 
  method internal_copy () : 'self_type = 
    {< base = ref (Global.copy !base) ; >} 
   
  (* should eventually change a .class to a .jimple file*)
  method from_source (filename : string) = begin 
    let lst = ref [] in
    let filelist = ref [] in
    let line_count = ref 0 in 
    let _,ext = split_ext filename in
    match ext with
	"jimple" -> begin filelist := [filename] end
      | _ -> begin filelist := get_lines filename end;
    let fin = open_in filename in
    (try while true do
	let line = input_line fin in
	incr line_count;
	lst := [line] :: !lst ;
    done with _ -> close_in fin) ; 
    base := Array.of_list (List.rev !lst);

    let atom_set = ref AtomSet.empty in
    for i = 0 to !line_count do 
      atom_set := AtomSet.add i !atom_set ;  
    done 
    
  end

  method get_files () = begin
    match !file_list_path with 
      | "" -> []
      | otherwise -> begin
	let filepath = !file_list_path in 
	let file = open_in filepath in 
	let filepaths = ref [] in
	begin try 
		while true do
		  let line = input_line file in
		  filepaths := line::!filepaths
		done
	  with End_of_file -> () end;
	close_in file;
	List.rev !filepaths
      end
  end

  method from_source_one_file (filename:string) : unit = failwith "from_source note impl. jbrep"

(*
  method load_oracle (filename: string)= begin
    let base,ext = split_ext filename in
    let filelist =
      match ext with
	  "c" -> [filename]
	| _ -> get_lines filename
    in
    let old_count = self#max_atom() in
    List.iter
      (fun fname ->
	let file = self#from_source_one_file ~pre:false fname in
	Hashtbl.add !oracle_code fname file) filelist;
    (*dosomethinghere: base := :: !base *)
    let atmst =
      lfoldl
	(fun set ->
	  fun ele ->
	    AtomSet.add ele set) (!codeBank) (old_count -- self#max_atom()) in
    codeBank := atmst;
    
  end
*)

  method load_oracle = failwith "load_oracle not implemented for jbrep"
 

(* should change a .jimple file to a .class file?*)
(*
  method output_source source_name = begin
    let fout = open_out source_name in
    Array.iteri (fun i line_list ->
        List.iter (fun line -> 
          Printf.fprintf fout "%s\n" line 
        ) line_list 
    ) !base ;
    close_out fout ; 
    let digest = Digest.file source_name in  
    already_sourced := Some([source_name],[digest]) ; 
  end 
  *)

  method internal_compute_source_buffers () = begin
   let b = Buffer.create 255 in
    Array.iteri (fun i line_list ->
        List.iter (fun line ->
          Printf.bprintf b "%s\n" line
        ) line_list
    ) !base ;
    let contents = Buffer.contents b in
    [(None,contents)]
    end

  method save_binary ?out_channel (filename : string) = begin
    let fout = 
      match out_channel with
      | Some(v) -> v
      | None -> open_out_bin filename 
    in 
    Marshal.to_channel fout (jbRep_version) [] ; 
    Marshal.to_channel fout (!base) [] ;
    super#save_binary ~out_channel:fout filename ;
    debug "jbRep: %s: saved\n" filename ; 
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
    if version <> jbRep_version then begin
      debug "jbRep: %s has old version\n" filename ;
      failwith "version mismatch" 
    end ;
    base := Marshal.from_channel fin ; 
    super#load_binary ~in_channel:fin filename ; 
    debug "jbRep: %s: loaded\n" filename ; 
    if in_channel = None then close_in fin 
  end 

  (*compile a variant to a .class file*)
method compile source_name exe_name = begin
  let dirname = Filename.dirname source_name in
  match !multi_file with
    | false -> super#compile source_name exe_name
    | true -> 
      match !use_build_file with
	| true ->
	  (let cmd = Printf.sprintf "./compile.sh %s" dirname in
	   match Stats2.time "compile" Unix.system cmd with
	     | Unix.WEXITED (0) -> true
	     | _ -> false)
	| false ->
	  begin
	    let success = ref true in
	    List.iter (fun source ->
	      let source = Printf.sprintf "%s/%s" dirname source in
	      let result = super#compile source exe_name in
	      if result = false then success := false
	    ) (List.rev (self#get_files ()));
	    !success
	  end
end
  
 (*
     *command should look like this:             *
     * java -cp $soot soot.Main -src-prec jimple *
     * -f class -cp . -pp source_name            *)

(*
  method compile source_name exe_name = begin
    let base_command = 
      match !compiler_command with 
	| "" -> self#get_compiler_command ()
	| x -> x
    in

    *
    let cmd = Global.replace_in_string base_command
      [
	"__COMPILER_NAME__", !compiler_name ;
	"__EXE_NAME__", exe_name ;
	"__SOURCE_NAME__", source_name ;
	"__COMPILER_OPTIONS__", !compiler_options;
      ]
					  in
    let cmd = "java -cp $soot soot.Main -src-prec jimple -f class -cp . -pp " ^ source_name in

    let result = (match Stats2.time "compile" Unix.system cmd with
      | Unix.WEXITED(0) ->
	already_compiled := Some(exe_name,source_name) ;
	true
      | _ -> 
	already_compiled := Some ("",source_name) ;
	debug "\t%s %s fails to compile\n" source_name (self#name ()) ;
	false
    ) in
    result
 end
*)


  method compute_localization = failwith "compute localization not implemented for jbrep"
	 
  method compute_localization = begin
    
  end


  method test_case test = failwith "test_case not implemented jbrep"

  method max_atom () = (Array.length !base) 

  method atom_id_of_source_line source_file source_line = begin
    if source_line < 0 || source_line > self#max_atom() then
      0
    else
      source_line
  end

(*
  method atom_id_of_source_line source_file source_line = begin
    if Hashtbl.mem !oracle_code source_file then
      let file = Hashtbl.find !oracle_code source_file in
      visitCilFileSameGlobals (my_find_atom source_file
    else
      visitCilFileSameGlobals (my_find_atom source_file source_line) file
    if source_line < 0 || source_line > self#max_atom () then
      0
    else
      source_line
  end
  *)

							 
  method structural_signature =
    failwith "jbRep: no structural differencing" 

  method get_compiler_command () = 
    failwith "jbRep: ERROR: use --compiler-command" 

  method instrument_fault_localization _ _ _ = 
    failwith "jbRep: no fault localization" 

  method debug_info () = begin
    debug "jbRep: lines = 1--%d\n" (self#max_atom ());
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
