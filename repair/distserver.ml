(** Distserver -- implements the server for the distributed GA search algorithm.
	The server coordinates the communication of the clients.  As it does not
	actually do any searching, it is a separate utility from the rest of
	repair. *)
open Distglobal
open Global
open Unix

(* this can fail if the unix socket/network calls fail or if a client sends
   messages that do not conform to the expected format. *)
let main ()= begin
  let options = [
    "--num-comps", Arg.Set_int num_comps, 
	"X Distributed: Number of computers to simulate" ;
    "--sport", Arg.Set_int server_port, "X server port"	;
  ] in
  let aligned = Arg.align options in 
  let usage_msg = "Program repair prototype -- Distributed GA server" in 
	Arg.parse aligned (usage_function aligned usage_msg) usage_msg;

	let client_tbl = Hashtbl.create !num_comps in
	let info_tbl = Hashtbl.create !num_comps in
	(* Adds all client computers to client_tbl (and set info_tbl to its
	   default)*)
	let rec getcomps sock =
      for currcomp=0 to (!num_comps-1) do
		let (sock,address) = accept sock  in
		let addr = match address with
		  | ADDR_INET(addr,port) -> (string_of_inet_addr addr)
		  | _ -> failwith("Client did not have an inet_addr")
		in
		let str = (Printf.sprintf "%d" currcomp) in
		  fullsend sock str;
		  Hashtbl.add client_tbl currcomp (sock,addr,0);
		  Hashtbl.add info_tbl currcomp (false,0,0,0);
      done
	in

	(* Takes care of all the stuff we need to do at the end *)
	let server_exit_fun () =
      let total_bytes = ref 0 in
      let repair_found = ref [] in
		hiter 
		  (fun comp ->
			fun (found,bytes_read,total_evals, gens) -> 
			  total_bytes := !total_bytes + bytes_read;
			  debug "Computer %d:\n" comp;
			  if found then begin
				debug "\tRepair found!\n";
				debug "\tGeneration:%d\n" gens;
				repair_found := comp :: !repair_found
			  end
			  else
				debug "\tNo repair found.\n";
			  debug "\tTotal test suite evals:%d\n" total_evals;
			  debug "\tTotal bytes received:%d\n\n" bytes_read;
		  ) info_tbl;
		debug "Total bytes sent as messages: %d\n" !total_bytes;

      (*Kill all other computers *)
		if (llen !repair_found) > 0 then
		  hiter (fun comp (sock,_,_) ->
			if not (List.mem comp !repair_found) then
			  fullsend sock "X"
		  ) client_tbl;
		try
		  Hashtbl.iter (fun _ (sock,_,_) ->
			try
			  close sock
			with _ -> ()) client_tbl;
		  close server_socket;
		with _ -> ()
	in

	  at_exit server_exit_fun;

  (* Connect to all the computers *)
	  setsockopt server_socket (SO_REUSEADDR) true ;
	  bind server_socket  (ADDR_INET (inet_addr_any, !server_port));
	  listen server_socket (!num_comps+5);
	  getcomps server_socket;
	  
	  let socketlist = ref [] in
  (* Send all clients all clients' information (address and port) *)
		Hashtbl.iter (fun key (sock,addr,_) ->
		  socketlist := sock :: !socketlist;
		  let port = my_int_of_string (fullread sock) in
			Hashtbl.replace client_tbl key (sock,addr,port);
			let str = Printf.sprintf "%d %s %d" key addr port in
			  Hashtbl.iter (fun _ (sock,_,_) ->
				fullsend sock str
			  )client_tbl;
		) client_tbl;

    (* Processes all the stats and makes the server do as it should *)
		let bool = ref true in

		let process_stats buffer =
       (* DEBUG *)
		  debug "Buffer = %s\n" buffer;
		  (match String.sub buffer 0 1 with
		  | "X" -> ()
		  | _ -> bool := false);
		  if not !bool then begin
			let split = (Str.split space_regexp buffer) in
			let comp,split = (my_int_of_string (List.hd split)),List.tl split in
			let found_repair = 
			  match (List.hd split) with
			  | "DN" -> false
			  | "DF" ->
				hiter
				  (fun id (sock,_,_) ->
					if id <> comp then 
					  fullsend sock "X";
				  ) client_tbl;
				true
			  | _ -> failwith 
				(Printf.sprintf "Unexpected buffer in process_stats: %s\n" buffer)
			in
			let split = List.tl split in 
			let bytes_read,split = 
			  my_int_of_string (List.hd split), List.tl split in
			let evals_done,split = 
			  my_int_of_string (List.hd split), List.tl split in
			let gens = my_int_of_string (List.hd split) in
			  hrep info_tbl comp (found_repair,bytes_read,evals_done,gens)
		  end
			
		in

		let rec inform_neighbours () =
		  let rec lstmaker curr =
			if curr < !num_comps then
			  curr :: (lstmaker (curr+1))
			else []
		  in
		  let rec sender list last =
			match list with
			| [] -> ()
			| hd :: tl ->
			  let (sock,_,_) = Hashtbl.find client_tbl last in
				fullsend sock (Printf.sprintf "%d" hd);
				sender tl hd
		  in
		  let lst = random_order (lstmaker 0) in
		  let last = List.nth lst (!num_comps-1) in
			sender lst last
			  
		in
		  (* Let's spin until we find a client who's done *)
		  while !bool do
			let msglist = spin !socketlist [] in
			  liter process_stats msglist;
			  if !bool then
				inform_neighbours()
		  done
end ;;

main () ;;
