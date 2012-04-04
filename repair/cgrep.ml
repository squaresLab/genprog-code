(* 
 * Program Repair Prototype (v2) 
 *
 * Program Representation -- CG -- "shader C" 
 *
 *)

open Printf
open Global
open Pellacini
open Population
open Rep
open Cil
open Cilrep

let cgRep_version = "1" 

class simpleFunVisitor = object
  inherit nopCilVisitor 
  method vfunc f = 
    Hashtbl.add Pellacini.funs f.svar.vname f ;
    DoChildren
end 
let my_simple_fun_visitor = new simpleFunVisitor

class cgRep = object (self : 'self_type)

  inherit astCilRep as super 

  val averages = ref (Hashtbl.create 255)

  method internal_parse (filename : string) = begin
    debug "cgRep: %s: parsing\n" filename ; 
    let result = Pellacini.parse_cg filename in 
      debug "cgRep: %s: parsed\n" filename ; 
      result 
  end 

  method internal_post_source (filename : string) = begin
    debug "cgRep: computing average values for original\n" ; 
	assert((map_cardinal !base) = 1);
	let file = StringMap.find filename !base in 
	  visitCilFileSameGlobals (my_simple_fun_visitor) file;
      if !pellacini_method_name = "" then begin
		debug "use --pellacini-method to set the shader method\n" ;
		exit 1 
      end ; 
      let computed_averages = 
		if !incoming_pop = "" then 
          let x, _ = compute_average_values file !pellacini_method_name in 
			x
		else
          Hashtbl.create 255
      in
		debug "cgRep: done computing average values for original\n" ; 
		averages := computed_averages ;
		Hashtbl.iter (fun (sid,str) v ->
		  debug "cgRep: #%d %s -> %s\n" 
			sid str
			(Pretty.sprint ~width:80 (d_exp () (Const v)))
		) !averages ; 
  end 

  method internal_output_source (source_name : string) = begin
	assert((map_cardinal !base) == 1);
    try
	  StringMap.iter (fun k file ->
		Pellacini.print_cg file source_name ) !base
    with _ ->
      debug "Print fail";
  end 

  (* save_binary, load_binary *) 
  method serialize ?out_channel ?global_info (filename : string) = begin
    let fout = 
      match out_channel with
      | Some(v) -> v
      | None -> open_out_bin filename 
    in 
      Marshal.to_channel fout (cilRep_version) [] ; 
      Marshal.to_channel fout (!averages) [] ; 
      super#serialize ~out_channel:fout ?global_info:global_info filename ;
      debug "cgRep: %s: saved\n" filename ; 
      if out_channel = None then close_out fout 
  end 

  method deserialize ?in_channel ?global_info (filename : string) = 
    let fin = 
      match in_channel with
      | Some(v) -> v
      | None -> open_in_bin filename 
    in 
    let version = Marshal.from_channel fin in
      if version <> cgRep_version then begin
		debug "cgRep: %s has old version\n" filename ;
		failwith "version mismatch" 
      end ;
      averages := Marshal.from_channel fin ; 
      super#deserialize ~in_channel:fin ?global_info:global_info filename ; 
      debug "cgRep: %s: loaded\n" filename ; 
      if in_channel = None then close_in fin 

  method compute_localization () = 
	match !fault_scheme,!fix_scheme with
	  "path",_ | _,"path" | "weight",_ | _,"weight" | "line",_ | _,"line" ->
		super#compute_localization () 
	| _ ->
      debug "cgRep: all %d statements are equally likely for fault and fix\n" 
        (self#max_atom ()) ;
	  let fix_weights = hcreate 10 in
      for i = self#max_atom () downto 1 do
        Hashtbl.replace fix_weights i 1.0 ;
        fault_localization := (i,1.0) :: !fault_localization ; 
      done ;
		fix_localization := hfold (fun k v acc -> (k,v) :: acc) fix_weights [] 

  method replace_subatom_with_constant stmt_id subatom_id =  
    let subs = self#get_subatoms stmt_id in 
      assert(subatom_id >= 0); 
    (*assert(subatom_id < (List.length subs)); *)
      if subatom_id < (List.length subs) then
		let sub_exp = List.nth subs subatom_id in 
		  match sub_exp with
		  | Exp(exp) -> begin 
			let expr_str = Pretty.sprint ~width:80 (d_exp () exp) in 
			  try 
				let avg_const = Hashtbl.find !averages (stmt_id,expr_str) in 
				let avg_exp = Const(avg_const) in 
				  self#replace_subatom stmt_id subatom_id (Exp avg_exp)
			  with e -> begin 
            (*
              debug "cgRep: avg for %s of stmt #%d not found\n" 
              expr_str stmt_id ;
            *)
				() 
			  end 
		  end 
		  | _ -> failwith "cgRep: replace_subatom_with_constant 2" 
      else
		()
end 

(*
 * This horrible hack is because I can't quite figure out how to have a
 * global list of representation types in a way that makes ocaml's type
 * system happy.
 *)
let _ = 
  global_filetypes := !global_filetypes @
    [ ("cg",(fun () -> 
    let rep = ((new cgRep) :> ('a,'b) Rep.representation) in 
    let base, ext = split_ext !program_to_repair in 
	  rep#load base;
	  rep#debug_info ();
	  let population = if !incoming_pop_file <> "" then 
		let fin = open_in_bin !incoming_pop_file in
		  GPPopulation.deserialize ~in_channel:fin !incoming_pop_file rep
		else []
	  in
		ignore (Multiopt.ngsa_ii rep population) ; exit 1
	)) ]
