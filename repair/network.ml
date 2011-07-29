open Global
open Unix
open Search

(* Global variable *)
let server = ref false
let server_hostname = ref "church"
let server_port = ref 65000
let my_hostname = ref "church"
let my_port = ref 65000
let hostisip = ref false
let _ =
  options := !options @
  [
    "--server", Arg.Set server, " This is server machine"	;
    "--shostname", Arg.Set_string server_hostname, "X server hostname"	;
    "--sport", Arg.Set_int server_port, "X server port"	;
    "--hostname", Arg.Set_string my_hostname, "X my hostname"	;
    "--port", Arg.Set_int my_port, "X my port"	;
    "--hostisip", Arg.Set hostisip, " Set if hostname is IP";
  ] 

let variants_exchanged = ref 5
let diversity_selection = ref false
let gen_per_exchange = ref 1
let last_comp = ref 0

let _ = 
  options := !options @ [
  "--num-comps", Arg.Set_int Search.num_comps, "X Distributed: Number of computers to simulate" ;
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

let domain_of_socket servername port = 
  let hent = Unix.gethostbyname servername in
  let inet_addr = hent.Unix.h_addr_list.(0) in
  let sockaddr = Unix.ADDR_INET(inet_addr, port) in
  let dom = Unix.domain_of_sockaddr sockaddr in 
  (sockaddr,dom)

let open_socket servername port = 
  try 
	let sockaddr, dom = (domain_of_socket servername port) in
	let socket = Unix.socket dom Unix.SOCK_STREAM 0 in 
	  Unix.connect socket sockaddr;
	  socket 
  with e -> 
	debug "open_socket: %s %d: %s" servername port (Printexc.to_string e) ;
	raise e

type phase = 
  | Waiting_recv
  | Partial_recv of string * int * int 
  | Waiting_on_send
  | Partial_send of string * int * int
  | Send_kill of string * int * int

(* the bool is whether a repair was found *)

type client = {
  id : int ;
  fd : Unix.file_descr ;
  phase : phase ;
  mutable pop_to : Unix.file_descr ;
}

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

let get_msg ifdone ifnot fd sofar bytes_read bytes_left = 
  let bytes_read' =
    try
      Unix.read fd sofar bytes_read bytes_left 
    with Unix.Unix_error(e, s1, s2) -> pprintf "WARNING: %s\n" (Unix.error_message e); 0
  in
    debug "read: %s\n" sofar;
    if bytes_read' < bytes_left then
	  ifnot sofar (bytes_read + bytes_read') (bytes_left - bytes_read')
    else
	  ifdone sofar

let get_len fd = 
  let ifdone sofar = my_int_of_string sofar, sofar in
  let ifnot _ = failwith "You can't even read 4 bytes?" in
    get_msg ifdone ifnot fd (String.create 4) 0 4

let info_tbl = hcreate !num_comps 

(* information to be printed at_exit by whomever is the server, whether it's
 * really distributed or properly sequential *)
let server_exit_fun () =
  debug "\nTotal test suite evaluations= \n";
  hiter 
	(fun comp ->
	  fun (total_evals, repair_infos) -> 
		debug "Computer %d:" comp;
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
	debug "\n") info_tbl

let i_am_the_server () = 
  at_exit server_exit_fun;
  let fd_tbl = hcreate !num_comps in
  let client_tbl = hcreate !num_comps in
  let info_to_be_sent = hcreate !num_comps in

  (* Helper functions *)
  (* process the statistics info sent to the server from the clients *)
  let process_stats client_num buffer = 
    let found_repair = 
      match String.sub buffer 0 2 with
	"DN" -> false
      | "DF" when not !Search.continue -> 
	let done_msg = "   1X" in
	  hiter
	    (fun id ->
	      fun client ->
		hrep fd_tbl client.fd { client with phase = Send_kill(done_msg,0,5) }
	    ) client_tbl; true
      | "DF" when !Search.continue -> true
      | _ -> failwith (Printf.sprintf "Unexpected buffer in process_stats: %s\n" buffer)
    in
    let split = List.tl (Str.split space_regexp buffer) in 
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
      hrep info_tbl client_num (my_int_of_string evals_done, repair_info)
  in
  (* process a read of info from a client.  If it's not an indication that the client is
     done, pass that info off to the client's neighbor *)
  let process_read read_fd = 
    let record = hfind fd_tbl read_fd in
    let ifdone sofar = 
      match (String.sub sofar 0 1) with
	"D" -> 
	  (try hrem fd_tbl read_fd; Unix.close read_fd with _ -> ()); process_stats record.id sofar
      | _ -> 
	hrep fd_tbl read_fd {record with phase = Waiting_on_send };
	hrep info_to_be_sent record.pop_to sofar
    in
    let ifnot sofar bytes_read bytes_left =
      hrep fd_tbl read_fd
	{record with phase = Partial_recv (sofar,bytes_read,bytes_left) }
    in
    let get_msg = get_msg ifdone ifnot read_fd in 
      match record.phase with
      | Waiting_recv ->
	let len,buff = get_len read_fd in 
	let buff' = String.create len in
	  get_msg buff' 0 len
      | Partial_recv(sofar,bytes_read,bytes_left) -> get_msg sofar bytes_read bytes_left 
      | _ -> failwith "waiting to send when I should be waiting to recv!"
  in
  (* Process of write of info to a client.  Probably hostname/ip address of its neighbor *)
  let process_write write_fd = 
    let record = hfind fd_tbl write_fd in
    let ifdone sofar = 
      hrep fd_tbl write_fd {record with phase = Waiting_recv };
      hrem info_to_be_sent write_fd
    in
    let ifnot sofar bytes_written bytes_left =
      hrep fd_tbl write_fd {record with phase = Partial_send(sofar,bytes_written,bytes_left) }
    in
      match record.phase with
	Waiting_on_send ->
	  let msg = hfind info_to_be_sent write_fd in
	  let len = String.length msg in 
	  let msg = Printf.sprintf "%4d%s" len msg in
	    send_msg ifdone ifnot write_fd msg 0 (String.length msg)
      | Partial_send(message,bytes_sent,bytes_left) -> 
	send_msg ifdone ifnot write_fd message bytes_sent bytes_left
      | Send_kill(message,bytes_sent,bytes_left) ->
	send_msg (fun _ -> hrep fd_tbl write_fd { record with phase = Waiting_recv } )
	  (fun _ bw bl -> hrep fd_tbl write_fd { record with phase = Send_kill(message,bw,bl) })
	  write_fd message bytes_sent bytes_left
      | _ -> failwith "Waiting to send when I should be waiting to recv!\n"
  in 
	
  (* set up communication with client machines *)
  let main_socket = socket PF_INET SOCK_STREAM 0 in
  let _ = Unix.setsockopt main_socket (SO_REUSEADDR) true ;
    let server_address = Unix.inet_addr_any in
      Unix.bind main_socket  (ADDR_INET (server_address, !server_port));
      Unix.listen main_socket 10;
      at_exit (fun () -> try close main_socket with _ -> ())
  in
  let rec connect_to_clients client_num = 
    if client_num < !num_comps then begin
      let fd,_ = Unix.accept main_socket  in
      let record = 
	{ id = client_num; fd = fd; pop_to = fd; phase = Waiting_recv } in
	hadd fd_tbl fd record;
	hadd client_tbl client_num record;
	let msg = Printf.sprintf "%d" client_num in
	let msg = Printf.sprintf "%4d%s" (String.length msg) msg in 
	  debug "sending to %d %s\n" client_num msg;
	  my_send fd msg 0 (String.length msg) [];
	connect_to_clients (client_num + 1)
    end 
  in
    connect_to_clients 0;
	hiter
	  (fun client_num ->
		fun client -> 
		  let send_to = (client_num + 1) mod !num_comps in
			client.pop_to <- (hfind client_tbl send_to).fd
	  ) client_tbl;
    let rec spin () =
      try 
		if (hlen fd_tbl) > 0 then begin
		  let waiting_for_read =
			hfold 
			  (fun fd -> 
				fun record -> 
				  fun waiting_read ->
					match record.phase with
					| Waiting_recv
					| Partial_recv _ -> fd :: waiting_read
					| _ -> waiting_read)
			  fd_tbl []
		  in
		  let waiting_for_write = 
			hfold
			  (fun fd ->
				fun info ->
				  fun waiting_write ->
					let record = hfind fd_tbl fd in
					  match record.phase with
					  | Waiting_on_send
					  | Partial_send _
					  | Send_kill _ -> fd :: waiting_write
					  | _ -> waiting_write)
			  info_to_be_sent []
		  in
		  let ready_for_read, ready_for_write,_ =
			Unix.select waiting_for_read waiting_for_write [] (-1.0) in
			liter process_read ready_for_read;
			liter process_write ready_for_write;
			spin ()
		end
      with e ->
		begin
		  debug "Server error: %s\n" (Printexc.to_string e);
		  hiter 
			(fun _ ->
			  fun client -> 
				try
				  my_send client.fd "   1X" 0 5 []
				with _ -> ()
			) client_tbl; exit 1
		end
	in
	  (* Step 1: talk to all clients to get their hostnames/ip addresses and 
		 send them to their neighbors *)
	  spin ();
	  (* reset the fd table *)
	  hiter
		(fun client_num ->
		  fun client ->
			hadd fd_tbl client.fd client) client_tbl;
	  (* Step 2: wait for the clients to finish and tell us stuff *)
	  spin ()

(* Various helper functions*)

(* Parses messages received from other computers and turns them into reps.
   Variants are separated by a period, '.', and mutations are separated by a space, ' '*)
let message_parse orig msg =
  (* Splits into a list of history lists *)
  let varlst = lmap (fun str -> 
    Str.split (Str.regexp_string " ") str
  ) (Str.split (Str.regexp_string ".") msg) in

    (* Turns said list into a list of variants *)
    lmap 
	(fun history ->
	  let fitness = List.hd history in 
	  lfoldl
	    (fun rep ->
	      fun hist -> begin
		let change = hist.[0] in
		  match change with
		  | 'd' ->
		    let num = (int_of_string (String.sub hist 2 ((String.index hist ')')-2))) in
		      rep#delete num; rep
		  | 'a' ->
		    let tmp = (String.index hist ',') in
		    let num1 = (int_of_string (String.sub hist 2 (tmp-2))) in
		    let num2 = (int_of_string (String.sub hist (tmp+1) ((String.index hist ')')-tmp-1))) in
		      rep#append num1 num2; rep
		  | 's' ->
		    let tmp = (String.index hist ',') in
		    let num1 = (int_of_string (String.sub hist 2 (tmp-2))) in
		    let num2 = (int_of_string (String.sub hist (tmp+1) ((String.index hist ')')-tmp-1))) in
		      rep#swap num1 num2; rep
		  | 'x' -> 
(*		    debug "Hit a crossover\n";*)
		    rep
		  |  _  -> 
(*		    debug "Error: This is not a variant, it is:  %s\n" hist;*)
		    rep
	      end
	    ) (orig#copy()) (List.rev (List.tl history)), 
	    float_of_string fitness
	) varlst

(* Creates the message that the function above parses *)
let make_message lst = 
  String.concat "." 
    (lmap (fun (ele,fit) -> Printf.sprintf "%g %s" fit (ele#name ())) lst)

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
    debug "Variants exchanged:\n";
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
let get_exchange_network orig lst =
  let lst = List.sort (fun (_,f) (_,f') -> compare f' f) lst in
    if (!Search.popsize == !variants_exchanged) then (make_message lst, [])
    else
      if !diversity_selection then
	if (!Search.popsize / 2 < !variants_exchanged) then
	  ((make_message (choose_by_diversity orig lst)), (first_nth lst (!Search.popsize - !variants_exchanged)))
	else
	  ((make_message (choose_by_diversity orig (first_nth lst (!variants_exchanged * 2)))),
	   (first_nth lst (!Search.popsize - !variants_exchanged)))
      else 
	((make_message (first_nth lst !variants_exchanged)), (first_nth lst (!Search.popsize - !variants_exchanged)))

(* Gets a list with the best variants from lst1 and all, but the worst of lst2 *)
let get_exchange orig lst1 lst2 =
  let lst1 = List.sort (fun (_,f) (_,f') -> compare f' f) lst1 in
  let lst2 = List.sort (fun (_,f) (_,f') -> compare f' f) lst2 in
    if (!Search.popsize == !variants_exchanged) then lst1
    else
      if !diversity_selection then
	if (!Search.popsize / 2 < !variants_exchanged) then
	  (choose_by_diversity orig lst1) @ (first_nth lst2 (!Search.popsize - !variants_exchanged))
	else
	  (choose_by_diversity orig (first_nth lst1 (!variants_exchanged * 2))) @  (first_nth lst2 (!Search.popsize - !variants_exchanged))
      else 
	(first_nth lst1 !variants_exchanged) @ (first_nth lst2 (!Search.popsize - !variants_exchanged))
	  
(* Exchange function: Picks the best variants to trade and tosses out the worst *)
let exchange orig poplist =
  let return = ref [] in
    for comps = 1 to !num_comps-1 do
      return :=  (get_exchange orig (List.nth poplist comps) (List.nth poplist (comps-1))) :: !return
    done;
    return := (get_exchange orig (List.nth poplist 0) (List.nth poplist (!num_comps-1))) :: !return;
    !return

let distributed_client (rep : 'a Rep.representation) incoming_pop = 
  let communicate receiving_from sending_to msg = 
    let waiting_for_read = ref receiving_from in
    let waiting_for_write = ref sending_to in 
    let retval = ref "" in
    let msg = Printf.sprintf "%4d%s" (String.length msg) msg in 
    let sending = ref (msg,0,String.length msg) in
    let recving = ref ("",0,0) in
    let rec spin () =
      match !waiting_for_read,!waiting_for_write with
	[],[] -> !retval
      | _ ->
	let ready_for_read, ready_for_write,_ =
	  Unix.select !waiting_for_read !waiting_for_write [] (-1.0) in
	  liter 
	    (fun write_fd ->
	    let ifdone sofar = 
	      waiting_for_write := []
	    in
	    let ifnot sofar bytes_sent bytes_left =
	      sending := (sofar,bytes_sent,bytes_left)
	    in
	    let send_msg = send_msg ifdone ifnot write_fd in
	      match !sending with
		msg,bytes_sent,bytes_left ->
		  send_msg msg bytes_sent bytes_left) ready_for_write ;
	  liter
	    (fun read_fd ->
	      let ifdone sofar = 
		if sofar = "X" then (debug "server told me to die!\n" ; exit 1);
		waiting_for_read := [];
		retval := sofar;
		recving := ("",0,0)
	      in
	      let ifnot sofar bytes_read bytes_left = 
		  recving := (sofar,bytes_read,bytes_left)
		in
		let get_msg = get_msg ifdone ifnot read_fd in
		  match !recving with
		    "",_,_ ->
		      let len,_ = get_len read_fd in 
		      let buff = String.create len in
			get_msg buff 0 len
		  | msg,bytes_read,bytes_left ->
		    get_msg msg bytes_read bytes_left) ready_for_read;
	  spin ()
    in
      spin ()
  in
  let setup () = 
    let sock_fd = open_socket !server_hostname !server_port in
    let main_listen = socket PF_INET SOCK_STREAM 0 in
    let _ = Unix.setsockopt main_listen (SO_REUSEADDR) true ;
      let server_address = Unix.inet_addr_any in
	Unix.bind main_listen  (ADDR_INET (server_address, !my_port));
	Unix.listen main_listen 1;
	Unix.set_nonblock sock_fd;
    in
    let msg = Printf.sprintf "%s %d" !my_hostname !my_port in 
    let my_num = my_int_of_string (communicate [sock_fd] [sock_fd] msg) in 
    let get_from () = 
      let fd,_ = Unix.accept main_listen in
	Unix.close main_listen;
	Unix.set_nonblock fd;
	fd 
    in
    let get_to () = 
      let [hostname;port] = 
	Str.split space_regexp (communicate [sock_fd] [] "")
      in
      let sending_to = open_socket hostname (my_int_of_string port) in
	Unix.set_nonblock sending_to;
	sending_to
    in
      if my_num mod 2 = 0 then begin
	let from = get_from() in
	let sendto = get_to () in
	  my_num,sock_fd,from,sendto
      end else begin
	let sendto = get_to () in
	let from = get_from () in
	  my_num,sock_fd,from,sendto
      end
  in
  let comp_num,sock_fd,variants_from,variants_to = setup () in
  let _ = 
	at_exit (fun () -> 
	  (* at exit, send statistics to the server *)
	  let final_stat_msg = 
		if !Fitness.successes > 0 then "DF" else "DN" 
	  in
	  let info_repairs = 
		lmap 
		  (fun info -> Printf.sprintf "(%d,%d)" info.generation info.test_case_evals)
		  !Search.success_info in
	  let info_repairs = String.concat "-" info_repairs in
	  let total_done = Printf.sprintf "%d" (Rep.num_test_evals_ignore_cache ()) in
	  let msg = final_stat_msg^" "^total_done^" "^info_repairs in
	  let len = String.length msg in 
	  let final_msg = Printf.sprintf "%4d%s" len msg in
	  let rec spin bytes_written bytes_left = 
		let _,ready_for_write,_ = 
		  Unix.select [] [sock_fd] [] (-1.0) in
		  send_msg 
			(fun _ -> ()) (fun _ bw bl -> spin bw bl) 
			(List.hd ready_for_write) final_msg bytes_written bytes_left
	  in
		spin 0 (String.length final_msg);
		Unix.close variants_from;
		Unix.close variants_to;
		Unix.close sock_fd
	)
  in
  let exchange_variants msgpop = 
	let msg = Printf.sprintf "%4d%s" (String.length (fst msgpop)) (fst msgpop) in
	  ignore(communicate [] [variants_to] msg);
	  communicate [sock_fd;variants_from] [] ""
  in
  let rec all_iterations generations (population : ('a Rep.representation * float) list) =
    try
	  if generations < !Search.generations then begin
		let num_to_run = 
		  if (!Search.generations - generations) > !gen_per_exchange then !gen_per_exchange
		  else !Search.generations - generations
		in
		let population = Search.run_ga ~comp:comp_num ~start_gen:generations ~num_gens:num_to_run population in
		  if num_to_run = !gen_per_exchange then begin
			let msgpop = get_exchange_network rep population in
			let from_neighbor = exchange_variants msgpop in
			let population =
			  (message_parse rep from_neighbor) @ (snd msgpop) in
			  all_iterations (generations + !gen_per_exchange) population
		  end 
	  end
    with Fitness.Found_repair(rep) -> (debug "repair found\n"; exit 1	)
  in
    ignore(all_iterations 1 (Search.initialize_ga ~comp:comp_num rep incoming_pop));
    exit 1

(* the sequential distributed algorithm *)
		
let distributed_sequential rep population = 
  at_exit server_exit_fun;
  let computer_index = ref 0 in
  let initial_populations =
	lmap (fun _ -> 
	  debug "Computer %d:\n" !computer_index;
	  let init_pop = Search.initialize_ga ~comp:(!computer_index) rep population in
		hadd info_tbl !computer_index (1, !Search.success_info);
		Search.success_info := [] ;
		let retval = !computer_index,init_pop in
		  incr computer_index; retval) (0 -- (!num_comps-1))
  in
  let one_computer_to_exchange gen (computer, population) =
	Search.success_info := snd (hfind info_tbl computer);
	let curr_gen = !gens_run in 
	try
	  let num_to_run = 
		if (!Search.generations - gen) > !gen_per_exchange then !gen_per_exchange
		else !Search.generations - gen
	  in
	  debug "Computer %d:\n" computer;
	  let population = Search.run_ga ~comp:computer ~start_gen:gen ~num_gens:num_to_run population in
		hrep info_tbl computer ((gen+num_to_run), !Search.success_info);
		computer,population
	  with Fitness.Found_repair(rep) -> begin (* fixme: double-check this arithmetic *)
		hrep info_tbl computer (!gens_run - curr_gen + gen, !Search.success_info); exit 1
	  end
  in
	  (* Starts loop for the runs where exchange takes place*)
  let rec all_iterations generation populations =
	if generation < !Search.generations then begin
	  let populations' = 
		lmap (one_computer_to_exchange generation) populations in
		all_iterations (generation + !gen_per_exchange) populations'
	end else snd (List.hd populations)
  in
	all_iterations 2 initial_populations

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

let networktest () = begin
  let rec getcomps server_socket currcomp = 
    if currcomp < !num_comps then begin
      let (sock,_) = accept server_socket  in
      let str = (Printf.sprintf "%4d" currcomp) in
	debug "Assigning computer %s\n" str;
	my_send sock str 0 4 [];
	sock :: getcomps server_socket (currcomp+1)
    end
    else []
  in

  let main_socket  = socket PF_INET SOCK_STREAM 0 in

  (* Server setup *)
    if !server then begin
      setsockopt main_socket (SO_REUSEADDR) true ;
      let server_address = inet_addr_any in
	bind main_socket  (ADDR_INET (server_address, !server_port));
	listen main_socket  10;
	debug "My name is %s and I am now listening.\n" (gethostname ());
	let str = "start" in 
	let socket_list = getcomps main_socket 1 in

	  (* Starts all computers *)
	  List.iter (fun sock -> begin
	    my_send sock str 0 (String.length str) [];
	    debug "Starting next computer. \n";
	    debug "Address = %s\n" (getnameinfo (getpeername sock) []).ni_hostname
	  end ) socket_list;
	  List.iter (fun sock -> begin
	    let newstr = (readall sock (my_int_of_string (readall sock 4))) in
	      debug "Received string %s\n" newstr
	  end ) socket_list;
    end

    (* Client setup *)
    else begin
      debug "You are the client connecting to %s\n" !server_hostname;
      let server_address = 
		if !hostisip then inet_addr_of_string !server_hostname
		else
	   (gethostbyname !server_hostname).h_addr_list.(0)
	  in
	debug "Address = %s\n" (string_of_inet_addr server_address);

    (* Loops until connected *)
	let b = ref true in
	  while !b do
	    try begin
	      connect main_socket  (ADDR_INET (server_address,!server_port));
	      b := false
	    end
	    with _ ->
	      sleep 5
	  done;

     (* Gets info it needs, then starts main function *)
	  let currcomp = my_int_of_string (readall main_socket 4) in
	    debug "I am computer number %d.\n" currcomp;
	    debug "Received string %s\n" (readall main_socket 5);
	    let str = "Teststring1" in
	      debug "Sending string %s\n" str;
	      my_send main_socket (Printf.sprintf "%4d" (String.length str)) 0 4 [];
	      my_send main_socket str 0 (String.length str) []
    end
end
