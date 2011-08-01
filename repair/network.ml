open Global
open Unix
open Search

(* Global variable *)
let server = ref false
let server_hostname = ref "church"
let server_port = ref 65000
let my_port = ref 65000
let _ =
  options := !options @
  [
    "--server", Arg.Set server, " This is server machine"	;
    "--hostname", Arg.Set_string server_hostname, "X server hostname"	;
    "--sport", Arg.Set_int server_port, "X server port"	;
    "--port", Arg.Set_int my_port, "X my port"	;
  ] 

let variants_exchanged = ref 5
let diversity_selection = ref false
let gen_per_exchange = ref 1
let ring = ref false

let _ = 
  options := !options @ [
  "--num-comps", Arg.Set_int Search.num_comps, "X Distributed: Number of computers to simulate" ;
	"--ring", Arg.Set ring, "X Distributed: use a ring topology" ;
  "--diversity-selection", Arg.Set diversity_selection, " Distributed: Use diversity for exchange";
  "--variants-exchanged", Arg.Set_int variants_exchanged, "X Distributed: Number of variants to send" ;
  "--gen-per-exchange", Arg.Set_int gen_per_exchange, "X Distributed: Number of generations between exchanges" ;
] 

exception Send_Failed

(* A send with some mild error checking *)
let my_send sock str num1 num2 msglst =
  try
    let x = send sock str num1 num2 msglst in
      if (x != (num2-num1)) then 
	raise Send_Failed
  with e ->
    debug "Error: %s\n" (Printexc.to_string e)

(* many many helper functions *)

let domain_of_socket servername port = 
  let hent = Unix.gethostbyname servername in
  let inet_addr = hent.Unix.h_addr_list.(0) in
  let sockaddr = Unix.ADDR_INET(inet_addr, port) in
  let dom = Unix.domain_of_sockaddr sockaddr in 
	sockaddr,dom

let open_socket servername port = 
  try 
	let sockaddr, dom = domain_of_socket servername port in
	let socket = Unix.socket dom Unix.SOCK_STREAM 0 in 
	  setsockopt socket (SO_REUSEADDR) true ;
	  Unix.connect socket sockaddr;
	  Unix.set_nonblock socket;
	  socket 
  with e -> 
	debug "open_socket: %s %d: %s" servername port (Printexc.to_string e) ;
	raise e

let make_server_socket port = 
  let main_socket = socket PF_INET SOCK_STREAM 0 in
  let _ = Unix.setsockopt main_socket (SO_REUSEADDR) true ;
    let server_address = Unix.inet_addr_any in
      Unix.bind main_socket  (ADDR_INET (server_address, port));
      Unix.listen main_socket 10;
      at_exit (fun () -> try close main_socket with _ -> ())
  in
	main_socket

let prep_msg msg = 
  let msg = Printf.sprintf "%4d%s" (String.length msg) msg in
	msg,String.length msg

let send_msg ifdone ifnot fd sofar bytes_written bytes_left = 
  let bytes_written' =
    try
      Unix.write fd sofar bytes_written bytes_left 
    with Unix.Unix_error(e, s1, s2) -> pprintf "WARNING: %s\n" (Unix.error_message e); 0
  in
    if bytes_written' < bytes_left then
      ifnot sofar (bytes_written + bytes_written') (bytes_left - bytes_written')
    else
      ifdone sofar

let total_bytes_read = ref 0

let get_msg ifdone ifnot fd sofar bytes_read bytes_left = 
  let bytes_read' =
    try
      Unix.read fd sofar bytes_read bytes_left 
    with Unix.Unix_error(e, s1, s2) -> pprintf "WARNING: %s\n" (Unix.error_message e); 0
  in
	total_bytes_read := bytes_read' + !total_bytes_read;
    if bytes_read' < bytes_left then
	  ifnot sofar (bytes_read + bytes_read') (bytes_left - bytes_read')
    else
	  ifdone sofar

let get_len fd = 
  let ifdone sofar = my_int_of_string sofar in
  let ifnot _ = failwith "You can't even read 4 bytes?" in
    get_msg ifdone ifnot fd (String.create 4) 0 4

(* We end up calling select a million different ways, trying to listen to people
   or broadcast to people or whatever. Here be a bunch of helper functions so I
   don't have to rewrite that a million times. *)

let done_cond ht () = (Hashtbl.length ht) = 0

let imp _ = failwith "Shouldn't be doing this here!" 
let get_fds ht () = 
  hfold
	(fun fd ->
	  fun _ -> 
		fun lst ->
		  fd :: lst) ht []

let get_empty _ = []

(* spin calls select many times *)
let rec spin done_cond do_read do_write get_reads get_writes on_error =
  try
	if not (done_cond ()) then begin
	  let waiting_for_read = get_reads () in
	  let waiting_for_write = get_writes () in
	  let ready_for_read, ready_for_write, _ =
		Unix.select waiting_for_read waiting_for_write [] (-1.0) 
	  in
		liter do_read ready_for_read;
		liter do_write ready_for_write;
		spin done_cond do_read do_write get_reads get_writes on_error 
	end 
  with e -> on_error e

let do_write write_tbl fd = 
  let info,bw,bl = hfind write_tbl fd in
  let ifdone _ = hrem write_tbl fd in 
  let ifnot sofar bw bl = hrep write_tbl fd (sofar,bw,bl) in
	send_msg ifdone ifnot fd info bw bl

let do_read read_tbl received_info temp_received fd =
  let ifdone sofar = 
	received_info := (fd,sofar) :: !received_info;
	if hmem temp_received fd then hrem temp_received fd;
	hrem read_tbl fd 
  in
  let ifnot sofar bw br = 
	hrep temp_received fd (sofar,br,br)
  in
	if hmem temp_received fd then 
	  let sofar,br,bl = hfind temp_received fd in
		get_msg ifdone ifnot fd sofar br bl 
	else 
	  let len = get_len fd in
		get_msg ifdone ifnot fd (String.create len) 0 len

let broadcast on_error info_to_be_sent = 
  spin (done_cond info_to_be_sent) imp (do_write info_to_be_sent) get_empty 
	(get_fds info_to_be_sent) on_error

let from_everyone on_error read_tbl = 
  let temp_received = hcreate !num_comps in
  let received_info = ref [] in
	spin (done_cond read_tbl) (do_read read_tbl received_info temp_received) imp 
	  (get_fds read_tbl) get_empty on_error;
	!received_info

let to_and_from_all on_error read_tbl write_tbl =
  let temp_received = hcreate !num_comps in
  let received_info = ref [] in
	spin (fun () -> (done_cond read_tbl ()) && (done_cond write_tbl ())) 
	  (do_read read_tbl received_info temp_received) 
	  (do_write write_tbl) (get_fds read_tbl) (get_fds write_tbl) on_error;
	!received_info

(* communicate is basically "to all, from some" *)
let communicate on_error my_do_read read_tbl write_tbl =
  let received_info = ref [] in
  let temp_received = hcreate 5 in
  spin (fun () -> (done_cond read_tbl ()) && (done_cond write_tbl ())) 
	  (my_do_read read_tbl received_info temp_received) (do_write write_tbl) (get_fds read_tbl) 
	  (get_fds write_tbl) on_error;
	!received_info

type client = {
  id : int ;
  fd : Unix.file_descr ;
  addr : Unix.sockaddr ;
}

let info_tbl = hcreate !num_comps 

(* information to be printed at_exit by whomever is the server, whether it's
 * really distributed or properly sequential *)
let server_exit_fun () =
  let total_bytes = ref !total_bytes_read in
  debug "\nTotal test suite evaluations= \n";
  hiter 
	(fun comp ->
	  fun (bytes_read,total_evals, repair_infos) -> 
		total_bytes := !total_bytes + bytes_read;
		debug "Computer %d:" comp;
		debug "\tTotal bytes read: %d\n" bytes_read;
		debug "\tTotal tc evals: %d\n" total_evals;
		debug "\tRepair info: \n" ;
		debug "\t";
		if (llen repair_infos) > 0 then begin
		  liter 
			(fun info -> 
			  debug "\t\tGeneration: %d, test_case_evals: %d\n" 
				info.Search.generation info.Search.test_case_evals)
			repair_infos
		end else debug "No repair found";
	debug "\n") info_tbl;
  debug "Total server bytes read: %d\n" !total_bytes_read;
  debug "Total bytes read overall: %d\n" !total_bytes

let i_am_the_server () = 
  at_exit server_exit_fun;
  let fd_tbl = hcreate !num_comps in
  let client_tbl = hcreate !num_comps in
  let info_to_be_sent = hcreate !num_comps in
  let server_error e = 
	debug "Server error: %s\n" (Printexc.to_string e);
	hiter 
	  (fun _ ->
		fun client -> 
		  try
			my_send client.fd "   1X" 0 5 []
		  with _ -> ()
	  ) client_tbl; exit 1
  in
  (* process the statistics info sent to the server from the clients *)
  let process_stats client_num buffer = 
    let found_repair = 
      match String.sub buffer 0 2 with
		"DN" -> false
      | "DF" when not !Search.continue -> 
		let done_msg = ("X",0,1) in
		  hiter
			(fun id ->
			  fun client ->
				if client.id <> client_num then 
				  hrep info_to_be_sent client.fd done_msg
			) client_tbl; true
      | "DF" when !Search.continue -> true
      | _ -> failwith (Printf.sprintf "Unexpected buffer in process_stats: %s\n" buffer)
    in
    let split = List.tl (Str.split space_regexp buffer) in 
	let bytes_read,split = List.hd split, List.tl split in
    let evals_done,repair_info = 
      if found_repair then begin
		let dash_regexp = Str.regexp_string "-" in
		let repair_info = Str.split dash_regexp (List.hd (List.tl split)) in
		  List.hd split,
		  lmap (fun str -> 
			let pair = String.sub str 1 (String.length str - 2) in
			let split = Str.split comma_regexp pair in
			  { Search.generation = my_int_of_string (List.hd split);
				Search.test_case_evals = my_int_of_string (List.hd (List.tl split))})
			repair_info
      end else (List.hd split),[]
    in
      hrep info_tbl client_num (my_int_of_string bytes_read,my_int_of_string evals_done, repair_info)
  in
  let my_do_read read_tbl received_info temp_received fd = 
	let record = hfind read_tbl fd in 
	do_read read_tbl received_info temp_received fd;
	let fd,msg = List.hd !received_info in
	  process_stats record.id msg 
  in
  let broadcast = broadcast server_error in
  let from_everyone = from_everyone server_error in
  let communicate = communicate server_error my_do_read in
  (* Helper functions *)
  let main_socket = make_server_socket !server_port in
  (* set up communication with client machines. *)
  let rec connect_to_clients client_num = 
    if client_num < !num_comps then begin
      let fd,addr = Unix.accept main_socket  in
      let record = 
		{ id = client_num; fd = fd; addr = addr; } in
		hadd fd_tbl fd record;
		hadd client_tbl client_num record;
		connect_to_clients (client_num + 1)
    end 
  in
	(* helper functions *)
  let reset_fd_tbl () =
	hiter
	  (fun client ->
		fun record ->
		  hrep fd_tbl record.fd record) client_tbl
  in
  (* Step 1: talk to all clients to get their hostnames/ip addresses and 
	 send them to their neighbors *)
  let client_setup () =
	connect_to_clients 0;
	hiter
	  (fun client ->
		fun record ->
		  let msg,len = prep_msg (Printf.sprintf "%d,%d" client !num_comps) in
			hrep info_to_be_sent record.fd (msg, 0, len)) 
	  client_tbl;
	(* send everyone their computer numbers *)
	broadcast info_to_be_sent;
	(* get port numbers from everyone *)
	let ports = 
	  lmap
		(fun (fd,str) ->
		  let record = hfind fd_tbl fd in 
			record.id,str) (from_everyone (Hashtbl.copy fd_tbl)) in
	let as_strings = 
	  lmap (fun (id,port) -> 
		let record = hfind client_tbl id in
		let hostname = 
		  match record.addr with
			ADDR_INET(ia,_) -> (Unix.gethostbyaddr ia).h_name
		  | _ -> failwith "Socket goes to a file descriptor, not a client, which makes no sense"
		in
		Printf.sprintf "%d,%s,%s" id port hostname)
		ports in
	let msg,len = prep_msg (String.concat " " as_strings) in
	  hiter
		(fun client ->
		  fun record ->
			hrep info_to_be_sent record.fd (msg,0,len))
		client_tbl;
	(* send port numbers and address info to everyone *)
	broadcast info_to_be_sent;
	reset_fd_tbl ()
  in
	client_setup ();
	(* now just hang out and wait to hear from people. *)
	ignore(communicate fd_tbl info_to_be_sent);
	exit 1

(* Various helper functions*)

(* Parses messages received from other computers and turns them into reps.
   Variants are separated by a period, '.', and mutations are separated by a space, ' '*)
let message_parse orig msg =
  (* Splits into a list of history lists *)
  let varlst = lmap (fun str -> 
    Str.split (Str.regexp_string " ") str
  ) (Str.split (Str.regexp_string ".") msg) 
  in
    (* Turns said list into a list of variants *)
    lmap 
	  (fun history ->
		let rep = orig#copy() in
		let fitness = float_of_string (List.hd history) in 
		  liter
			(fun hist -> 
			  match hist.[0] with
			  | 'd' ->
				let num = (int_of_string (String.sub hist 2 ((String.index hist ')')-2))) in
				  rep#delete num
			  | 'a' ->
				let tmp = (String.index hist ',') in
				let num1 = (int_of_string (String.sub hist 2 (tmp-2))) in
				let num2 = (int_of_string (String.sub hist (tmp+1) ((String.index hist ')')-tmp-1))) in
				  rep#append num1 num2
			  | 's' ->
				let tmp = (String.index hist ',') in
				let num1 = (int_of_string (String.sub hist 2 (tmp-2))) in
				let num2 = (int_of_string (String.sub hist (tmp+1) ((String.index hist ')')-tmp-1))) in
				  rep#swap num1 num2
			  | 'x' -> ()
			  (*		    debug "Hit a crossover\n";*)
			  |  _  ->  ()
			(*		    debug "Error: This is not a variant, it is:  %s\n" hist;*)
			) (List.tl history);
		  rep#set_fitness fitness;
		  rep,fitness
	  ) varlst

(* Creates the message that the function above parses *)
let make_message lst = 
  let all_histories = 
	lmap 
	  (fun (rep,fit) ->
		let strs = lmap (rep#history_element_to_str) (rep#get_history()) in
		  String.concat " " strs,fit) lst in
  String.concat "." 
    (lmap (fun (ele,fit) -> Printf.sprintf "%g %s" fit ele) all_histories)

(* Chooses variants based on diversity metrics instead of just fitness,
   if the diversity-selection option is enabled *)
let choose_by_diversity (orig : 'a Rep.representation) lst =
  let string_list_describing_history rep : string list =
    let history_list = rep#get_history () in
    lmap (rep#history_element_to_str) history_list
  in 
  let histlist = lmap (fun (ele,fit) -> 
    (ele,fit), 
    (string_list_describing_history ele)
  ) lst in
    
  let setlist =
    lmap (fun (ele,history) -> 
      ele,lfoldl
	(fun ele_set ->
	  fun hist ->
	    StringSet.add hist ele_set)
	(StringSet.empty) history
    ) histlist
  in
    
  (* Add them all to a master set *)
  let allset = 
    lfoldl
      (fun allset ->
	fun (_,oneset) ->
	  StringSet.union allset oneset)
      (StringSet.empty) setlist
  in
  (* Look at which variant has the most changes different from other chosen variants *)
  let rec collect_variants allset setlist sofar =
    (* assumes that !variants_exchanged <= List.length *)
    if sofar = !variants_exchanged then [] 
    else begin
      let sorted = 
	lsort (fun (_,_,a) (_,_,b) -> compare b a)
	  (lmap 
	     (fun (ele,oneset) -> 
	       let intersection = StringSet.inter oneset allset in
		 ele,intersection,StringSet.cardinal intersection)
	     setlist)
      in
      let element,changeset,card = List.hd sorted in
	if card > 0 then begin
	  let a,b = element in
	   debug "Variant: %s\n" (a#name ());
	  element :: 
	    (collect_variants 
	       (StringSet.diff allset changeset) 
	       (lmap (fun (a,b,_) -> a,b) (List.tl sorted))
	       (sofar + 1))
	end
	else 
	  (* If there are no non-taken, non-original variants left, we just
	     make the rest of them originals *)
	  let fit = float_of_int !pos_tests in
	    lmap (fun _ -> begin
	      debug "Variant: %s\n" (orig#name ());
	      orig#copy(),fit
	    end) (1 -- (!variants_exchanged - sofar))
    end
  in
    collect_variants allset setlist 0

(* Gets a message with the best variants from lst and a list of all but the worst*)
let get_exchange orig lst =
  let lst = List.sort (fun (_,f) (_,f') -> compare f' f) lst in
	if !diversity_selection then 
	  choose_by_diversity orig lst
	else 
	  first_nth lst !variants_exchanged

let distributed_client (rep : 'a Rep.representation) incoming_pop = 
  let client_error e = 
	debug "Client error: %s\n" (Printexc.to_string e);
	exit 1
  in
  let broadcast = broadcast client_error in
  let my_do_read read_tbl received_info temp_received fd = 
	do_read read_tbl received_info temp_received fd;
	hclear read_tbl
  in
  let communicate = communicate client_error my_do_read in 
  let setup () = 
	let read_tbl = hcreate 1 in
	let write_tbl = hcreate 1 in
    let server_fd = open_socket !server_hostname !server_port in
    let msg,len = prep_msg (Printf.sprintf "%d" !my_port) in 
	  hadd write_tbl server_fd (msg,0,len);
	  hadd read_tbl server_fd ("",0,0);
	let [my_num;num_comps] = (* FIXME: compiler warning! *) 
	  let split = Str.split comma_regexp 
		(snd (List.hd (communicate read_tbl write_tbl)))
	  in
		lmap my_int_of_string split
	in
	  hadd read_tbl server_fd ("",0,0);
    let all_addrs = 
	  Str.split space_regexp 
		(snd (List.hd (communicate read_tbl write_tbl)))
	in
	let as_addrs = 
	  lmap
		(fun str -> 
		  let [num;port;host] = Str.split comma_regexp str in
			my_int_of_string num, my_int_of_string port, host)
		all_addrs
	in
    let main_listen = make_server_socket !my_port in
    let as_addrs = 
      lsort (fun (num1,_,_) -> fun (num2,_,_) -> compare num1 num2) as_addrs
    in
    let final_tbl = hcreate num_comps in
      (* CLG: this feels hacky to me but it's the solution that has worked the 
	 most reliably of all those I've tried *)
      liter
	(fun (num,port,host) ->
	  if num < my_num then begin
	    let rec spin () =
	      try
		let fd,_ = Unix.accept main_listen in
		  hadd final_tbl num fd
	      with Unix.Unix_error _ -> spin ()
	    in
	      spin ()
	  end else if num > my_num then begin
	    let rec spin () = 
	      try 
		let fd = open_socket host port in
		  hadd final_tbl num fd
	      with Unix.Unix_error _ -> spin ()
	    in spin ()
	  end) as_addrs;
		my_num,num_comps,server_fd,final_tbl
  in
  let my_num,num_comps,server_fd,neighbor_tbl = setup () in
  let _ = 
	at_exit (fun () -> 
	  (* at exit, send statistics to the server *)
	  let final_stat_msg = 
		if !Fitness.successes > 0 then "DF" else "DN" 
	  in
	  let bytes_read = Printf.sprintf "%d" !total_bytes_read in
	  let info_repairs = 
		lmap 
		  (fun info -> Printf.sprintf "(%d,%d)" info.generation info.test_case_evals)
		  !Search.success_info 
	  in
	  let info_repairs = String.concat "-" info_repairs in
	  let total_done = Printf.sprintf "%d" (Rep.num_test_evals_ignore_cache ()) in
	  let msg,len = prep_msg (final_stat_msg^" "^bytes_read^" "^total_done^" "^info_repairs) in
	  let write_tbl = hcreate 1 in
		hadd write_tbl server_fd (msg,0,len);
		broadcast write_tbl;
		hiter
		  (fun _ ->
			fun fd ->
			  Unix.close fd) neighbor_tbl;
		Unix.close server_fd
	)
  in
  let all_neighbors_num,all_neighbors_fd = 
	hfold
	  (fun num ->
		fun fd -> 
		  fun (lst1,lst2) -> 
			num :: lst1,fd::lst2) neighbor_tbl ([],[])
  in
  let exchange_variants msgpop = 
	let msg,len = prep_msg msgpop in
	let sending_to_num = 
	  if !ring then (my_num + 1) mod num_comps 
	  else 
		let rand = Random.int (num_comps - 1) in
		  List.nth all_neighbors_num rand
	in
	let variants_to = hfind neighbor_tbl sending_to_num in
	let write_tbl = hcreate 1 in
	let read_tbl = hcreate num_comps in
	  hadd write_tbl variants_to (msg,0,len);
	  liter (fun fd -> hadd read_tbl fd ("",0,0)) (server_fd :: all_neighbors_fd);
	  let from_others = communicate read_tbl write_tbl in
		lmap
		  (fun (fd,message) -> 
			  if message = "X" then (debug "Server told me to die!\n"; exit 1)
			  else message) from_others
  in
  let rec all_iterations generations (population : 'a Rep.representation list) =
    try
	  if generations < !Search.generations then begin
		let num_to_run = 
		  if (!Search.generations - generations) > !gen_per_exchange then !gen_per_exchange
		  else !Search.generations - generations
		in
		let population = Search.run_ga ~comp:my_num ~start_gen:generations ~num_gens:num_to_run population rep in
		  if num_to_run = !gen_per_exchange then begin
			let population = Search.calculate_fitness (generations + num_to_run) population rep in
			let msgpop = make_message (get_exchange rep population) in
			let from_neighbor = exchange_variants msgpop in
			let population =
			  lfoldl (fun pop -> fun from_neighbor -> 
			    pop @ (message_parse rep from_neighbor)) population from_neighbor
			in
			  all_iterations (generations + !gen_per_exchange) (lmap fst population)
		  end 
	  end
    with Fitness.Found_repair(rep) -> (debug "repair found\n"; exit 1	)
  in
    ignore(all_iterations 1 (Search.initialize_ga ~comp:my_num rep incoming_pop));
    exit 1

(* the sequential distributed algorithm *)
		
let distributed_sequential rep population = 
  at_exit server_exit_fun;
  let computer_index = ref 0 in
  let initial_populations =
	lmap (fun _ -> 
	  debug "Computer %d:\n" !computer_index;
	  let init_pop = Search.initialize_ga ~comp:(!computer_index) rep population in
		hadd info_tbl !computer_index (0,0,!Search.success_info);
		Search.success_info := [] ;
		let retval = !computer_index,init_pop in
		  incr computer_index; retval) (0 -- (!num_comps-1))
  in
  let one_computer_to_exchange gen (computer, population) =
	let _,evals_so_far,current_info = hfind info_tbl computer in
	Search.success_info := current_info;
	let current_evals = Rep.num_test_evals_ignore_cache () in
	try
	  let num_to_run = 
		if (!Search.generations - gen) > !gen_per_exchange then !gen_per_exchange
		else !Search.generations - gen
	  in
	  debug "Computer %d:\n" computer;
	  let population = 
		Search.calculate_fitness (gen + num_to_run)  
		  (Search.run_ga ~comp:computer ~start_gen:gen ~num_gens:num_to_run population rep) rep
	  in
	  let new_evals = Rep.num_test_evals_ignore_cache() in
		hrep info_tbl computer (0, new_evals - current_evals + evals_so_far, !Search.success_info);
		computer,population
	  with Fitness.Found_repair(rep) -> begin
		let new_evals = Rep.num_test_evals_ignore_cache() in
		hrep info_tbl computer (0,new_evals - current_evals + evals_so_far, !Search.success_info);
		  raise (Fitness.Found_repair(rep))
	  end
  in
	  (* Starts loop for the runs where exchange takes place*)
  let rec all_iterations generation populations =
	if generation < !Search.generations then begin
	  let found = ref false in
	  let populations' = 
		lmap 
		  (fun (c,pop) ->
			try
			  one_computer_to_exchange generation (c,pop)
			with Fitness.Found_repair(rep) -> (found := true;c,[])) populations in
		if not !found then begin
		  let to_trade = hcreate !num_comps in
			liter (fun (comp,pop) ->
			  hadd to_trade comp (get_exchange rep pop)) populations';
			let new_pops = 
			  lmap
				(fun (comp,pop) ->
				  let new_vars = hfind to_trade ((comp + 1) mod !num_comps) in
					comp,lmap fst (new_vars @ pop))
				populations'
			in
			  all_iterations (generation + !gen_per_exchange) new_pops
		end else 
		  snd (List.hd populations)
	end else snd (List.hd populations)
  in
	all_iterations 1 initial_populations

(* FIXME: add assertions back in somewhere *)
(*  assert(!gen_per_exchange < !Search.generations);
  assert(!num_comps > 1);
  assert(!variants_exchanged < !Search.popsize);*)

(* Gets all the data in the socket *)
let readall sock size = 
  let count = ref 0 in
  let buffer = String.create (size+1) in
  let rec _readall accum =
    let currcount = (recv sock buffer 0 size []) in
      count := currcount + !count;
      if (!count != size) then
	if (currcount == 0) then begin
	  sleep 5;
	  _readall accum
	end
	else
	  _readall ((String.sub buffer 0 currcount)::accum)
      else
	((String.sub buffer 0 currcount)::accum)
  in
  let str = String.concat "" (List.rev (_readall [])) in
    str

