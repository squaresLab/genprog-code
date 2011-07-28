open Global
open Unix
(* globals from the top of search.ml: *)

(*Global(ish) variables necessary for splitting up the search space, recording
  the total number of generations and variants evaluated before exit (respectively)*)
let totgen = ref (-1)

(* Global variable *)
let gens_used = ref 0
let server = ref false
let hostname = ref "church"
let port = ref 65000
let reset_seed = ref false
let hostisip = ref false
let _ =
  options := !options @
  [
    "--server", Arg.Set server, " This is server machine"	;
    "--hostname", Arg.Set_string hostname, "X Hostname to connect to"	;
    "--port", Arg.Set_int port, "X Port used"	;
    "--hostisip", Arg.Set hostisip, " Set if hostname is IP";
    "--reset-seed", Arg.Set reset_seed, " Distributed: Resets seed between each generation";
  ] 

let distributed = ref ""
let variants_exchanged = ref 5
let diversity_selection = ref false
let num_comps = ref 2
let split_search = ref false
let gen_per_exchange = ref 1
let listevals = ref (Array.make_matrix 1 1 0)
let last_comp = ref 0
let currentevals = ref 0

let _ = 
  options := !options @ [
  "--distributed", Arg.Set_string distributed, " Distributed: distributed GA mode. seq for sequential, net for networked." ;
  "--num-comps", Arg.Set_int num_comps, "X Distributed: Number of computers to simulate" ;
  "--split-search", Arg.Set split_search, " Distributed: Split up the search space" ;
  "--diversity-selection", Arg.Set diversity_selection, " Distributed: Use diversity for exchange";
  "--variants-exchanged", Arg.Set_int variants_exchanged, "X Distributed: Number of variants exchanged" ;
  "--gen-per-exchange", Arg.Set_int gen_per_exchange, "X Distributed: Number of generations between exchanges" ;
] 

exception Client_repair of Unix.file_descr
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
    Waiting_recv
  | Partial_recv of string * int * int 
  | Waiting_on_send
  | Partial_send of string * int * int

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
    match sofar with
      "Done" -> raise (Client_repair(fd))
    | _ ->
      if bytes_read' < bytes_left then
	ifnot sofar (bytes_read + bytes_read') (bytes_left - bytes_read')
      else
	ifdone sofar

let get_len fd = 
  let ifdone sofar =
    match sofar with
      "Done" -> raise (Client_repair(fd))
    | _ -> my_int_of_string sofar, sofar
  in
  let ifnot _ = failwith "You can't even read 4 bytes?" in
    get_msg ifdone ifnot fd (String.create 4) 0 4

let i_am_the_server () = 
  let fd_tbl = hcreate !num_comps in
  let client_tbl = hcreate !num_comps in
  let info_to_be_sent = hcreate !num_comps in

  (* sets up communication with client machines *)
  let main_socket = socket PF_INET SOCK_STREAM 0 in
  let _ = Unix.setsockopt main_socket (SO_REUSEADDR) true ;
    let server_address = Unix.inet_addr_any in
      Unix.bind main_socket  (ADDR_INET (server_address, !port));
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
	connect_to_clients (client_num + 1)
    end 
  in
    connect_to_clients 0;
    try 
	  hiter
	(fun client_num ->
	  fun client -> 
	    let send_to = (client_num + 1) mod !num_comps in
	      client.pop_to <- (hfind client_tbl send_to).fd
	) client_tbl;
      let process_read read_fd = 
	let record = hfind fd_tbl read_fd in
	let ifdone sofar = 
	  debug "SOFAR: %s\n" sofar;
	  if sofar = "   9No repair" then begin
	    hrem fd_tbl read_fd;
	    try Unix.close read_fd with _ -> ();
	  end else begin
	    hrep fd_tbl read_fd {record with phase = Waiting_on_send };
	    hrep info_to_be_sent record.pop_to sofar
	  end
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
	      get_msg (buff^buff') 4 len
	  | Partial_recv(sofar,bytes_read,bytes_left) -> get_msg sofar bytes_read bytes_left 
	  | _ -> failwith "waiting to send when I should be waiting to recv!"
      in
      let process_write write_fd = 
	let record = hfind fd_tbl write_fd in
	let ifdone sofar = 
	  hrep fd_tbl write_fd {record with phase = Waiting_recv };
	  hrem info_to_be_sent write_fd
	in
	let ifnot sofar bytes_written bytes_left =
	  hrep fd_tbl write_fd {record with phase = Partial_send(sofar,bytes_written,bytes_left) }
	in
	let send_msg = send_msg ifdone ifnot write_fd in
	  match record.phase with
	    Waiting_on_send ->
	      let msg = hfind info_to_be_sent write_fd in
	      let len = String.length msg in 
		send_msg msg 0 len
	  | Partial_send(message,bytes_sent,bytes_left) -> 
	    send_msg message bytes_sent bytes_left
	  | _ -> failwith "Waiting to send when I should be waiting to recv!\n"
      in 
      let rec spin () =
	if (hlen fd_tbl) > 0 then begin
	let waiting_for_read =
	  hfold 
	    (fun fd -> 
	      fun record -> 
		fun waiting_read ->
		  match record.phase with
		    Waiting_recv
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
		      Waiting_on_send
		    | Partial_send(_) -> fd :: waiting_write
		    | _ -> waiting_write)
	    info_to_be_sent []
	in
	  debug "read_wait: %d, read_write: %d\n" (llen waiting_for_read) (llen waiting_for_write);
	let ready_for_read, ready_for_write,_ =
	  Unix.select waiting_for_read waiting_for_write [] (-1.0) in
	  liter process_read ready_for_read;
	  liter process_write ready_for_write;
	  spin ()
	end else begin
	  debug "Clients are done, exiting\n"; exit 1
	end
      in 
	spin ()
    with Client_repair(_) ->
      begin
	debug "Repair found on a client, telling everyone else to stop!\n"; 
	hiter 
	  (fun _ ->
	    fun client -> 
	      try
		my_send client.fd "   4" 0 4 [];
		my_send client.fd "Done" 0 4 []
	      with _ -> ()
	  ) client_tbl; exit 1
      end
    | e ->
      begin
	debug "Server error: %s\n" (Printexc.to_string e);
	hiter 
	  (fun _ ->
	    fun client -> 
	      try
		my_send client.fd "   4" 0 4 [];
		my_send client.fd "Done" 0 4 []
	      with _ -> ()
	  ) client_tbl; exit 1
      end

(*************************************************************************
 *************************************************************************
                     Distributed computation
 *************************************************************************
 *************************************************************************)
(* Various helper functions*)

(* Parses messages received from other computers and turns them into reps.
   Variants are separated by a period, '.', and mutations are separated by a space, ' '*)
let message_parse orig msg =
  (* Splits into a list of history lists *)
  let varlst = lmap (fun str -> 
    Str.split (Str.regexp_string " ") str
  ) (Str.split (Str.regexp_string ".") msg) in

    (* Turns said list into a list of variants *)
    let variantlist lst =
      lmap 
	(fun history ->
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
		    debug "Hit a crossover\n";
		    rep
		  |  _  -> 
		    debug "Error: This is not a variant, it is:  %s\n" hist;
		    rep
	      end
	    ) (orig#copy()) (List.rev history)) lst
    in
      (* Returns variant list with the variants associated fitness *)
      (Search.calculate_fitness (variantlist varlst))


(* Creates the message that the function above parses *)
let make_message lst = 
  String.concat "." (lmap (fun (ele,fit) -> 
    ele#name ()) lst)

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


let distributed_client (rep : 'a Rep.representation) (incoming_pop : ('a Rep.representation * float) list) = begin
  let sock_fd = open_socket !hostname !port in
  let _ = Unix.set_nonblock sock_fd; at_exit (fun () -> Unix.close sock_fd) in
  let communicate_with_server msgpop = 
    let sizemsg = Printf.sprintf "%4d" (String.length (fst msgpop)) in
    let msg = sizemsg^(fst msgpop) in
    let message_length = String.length msg in 
    let sending = ref (msg,0,message_length) in
    let recving = ref ("",0,0) in
    let retval = ref "" in
    let waiting_for_read = ref [sock_fd] in
    let waiting_for_write = ref [sock_fd] in
    let rec spin () = 
      match !waiting_for_read,!waiting_for_write with
	[],[] -> !retval 
      | _ -> 
	let ready_for_read, ready_for_write,_ =
	  Unix.select !waiting_for_read !waiting_for_write [] (-1.0) in
	  if ready_for_read <> [] then begin
	    let read_fd = List.hd ready_for_read in
	    let ifdone sofar = begin
	      waiting_for_read := [];
	      retval := sofar
	    end in
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
		get_msg msg bytes_read bytes_left
	  end;
	  if ready_for_write <> [] then begin
	    let write_fd = List.hd ready_for_write in
	    let ifdone sofar = 
	      waiting_for_write := []
	    in
	    let ifnot sofar bytes_sent bytes_left =
	      sending := (sofar,bytes_sent,bytes_left)
	    in
	    let send_msg = send_msg ifdone ifnot write_fd in
	      match !sending with
		msg,bytes_sent,bytes_left ->
		  send_msg msg bytes_sent bytes_left 
	  end;
	  spin ()
    in
      spin ()
  in
  let exchange_iters = !Search.generations / !gen_per_exchange in
  let rec all_iterations total_exchange_iters generations (population : ('a Rep.representation * float) list) =
    try
    if total_exchange_iters < exchange_iters then begin
	let population = Search.genetic_algorithm ~generations:(!gen_per_exchange) rep (lmap fst population) (* FIXME: don't recalculate the fitness! *) in
	let msgpop = get_exchange_network rep population in
	let from_server = communicate_with_server msgpop in
	let population =
	  (message_parse rep from_server) @ (snd msgpop) in
	  all_iterations (total_exchange_iters + 1) (generations + !gen_per_exchange) population
    end
	  (* Goes through the rest of the generations requested*)
    else if generations < !Search.generations then begin
      incr gens_used;
      ignore(Search.genetic_algorithm ~generations:(!Search.generations - generations) rep (lmap fst population))
    end;
    with Fitness.Found_repair(rep) -> begin
      debug "Repair found, telling server\n";
      (try
	my_send sock_fd "   4" 0 4 [];
	my_send sock_fd "Done" 0 4 []
       with _ -> ()); 
      exit 1
    end

  in
    all_iterations 0 0 incoming_pop;
    debug "No repair found, telling server\n"; 
    (try
	my_send sock_fd "   9" 0 4 [];
	my_send sock_fd "No repair" 0 9 []
     with _ -> ()); 
    exit 1

end
		
let distributed_sequential search_strategy rep population = 
  begin
	  (* the sequential distributed algorithm *)
    let totgen = !Search.generations in
    let in_pop = ref [] in
	  Search.generations := !Search.gen_per_exchange;
	  let exchange_iters = totgen / !Search.gen_per_exchange in
    (* Sets the original value of in_pop to be the incoming_population for all computers *)
		for comps = 0 to (!Search.num_comps - 1) do
		  in_pop := population :: !in_pop;
		done; 
		
    (* Main function Start *)
    (* Starts loop for the runs where exchange takes place*)
		listevals := Array.make_matrix !Search.num_comps (exchange_iters + 1) 0;
		let rec all_iterations gen population =
		  let run_search comps population = 
			let comma = Str.regexp "," in 
      
	(* Apply the requested search strategies in order. Typically there
	 * is only one, but they can be chained. *) 
			let what_to_do = Str.split comma search_strategy in

			  (List.fold_left (fun population strategy ->
				let pop = List.map fst population in
				  match strategy with
				  | "brute" | "brute_force" | "bf" -> 
					Search.brute_force_1 rep pop
				  | "ga" | "gp" | "genetic" -> 
					Search.genetic_algorithm rep pop
				  | "multiopt" | "ngsa_ii" -> 
					Multiopt.ngsa_ii rep pop
				  | x -> failwith x
			   ) population what_to_do)
		  in
		  let rec one_iteration comps =
			if comps < !Search.num_comps then begin
			  last_comp := comps;
			  debug "Computer %d:\n" (comps+1);
			  Fitness.varnum := 0;
			  let returnval = run_search comps (List.nth population comps) in
				!listevals.(comps).(gen) <- Rep.num_test_evals_ignore_cache () - !currentevals;
				currentevals := Rep.num_test_evals_ignore_cache ();
				Fitness.success_rep := "";
				returnval :: (one_iteration (comps + 1))
			end else
			  if (!Fitness.finish_gen && (!Fitness.successes > 0)) then
				exit 1
			  else
				[]
		  in
			if gen < exchange_iters then 
 			  let returnval = one_iteration 0 in
				gens_used := 1 + !gens_used;
				all_iterations (gen + 1) (exchange rep returnval)
			else if (totgen mod !Search.gen_per_exchange) <> 0 then begin
		  (* Goes through the rest of the generations requested*)
			  Search.generations := (totgen mod !Search.gen_per_exchange);
			  ignore(one_iteration 0);
			  gens_used := 1 + !gens_used
			end
		in
		  all_iterations 0 !in_pop;
		  gens_used := !gens_used - 1
  end



let distributed_search search_strategy (rep : 'a Rep.representation) (population : ('a Rep.representation * float) list) = begin
  assert(!gen_per_exchange < !Search.generations);
  assert(!num_comps > 1);
  assert(!variants_exchanged < !Search.popsize);
  (* print distributed info at exit *)
  at_exit (fun () -> 
	let partgen = (float !Fitness.varnum) /. (float !Search.popsize) in
		  (* Test evaluations per computer for Distributed algorithm *)
	  if !listevals.(!last_comp).(!gens_used) == 0 then
		!listevals.(!last_comp).(!gens_used) <- Rep.num_test_evals_ignore_cache () - !currentevals;
	  Array.iteri 
		(fun comps ->
		  fun _ -> debug "Computer %d:\t" (comps+1)) !listevals;
	  debug "\n";
	  
	  for gen=0 to !gens_used do
		for comps=0 to pred !num_comps do
		  debug "%d\t\t" !listevals.(comps).(gen) 
		done;
		debug "\n"
	  done;
      
	  debug "\nTotal = \n";
	  Array.iteri 
		(fun comps ->
		  fun listevals ->
			let total = 
			  Array.fold_left 
				(fun total ->
				  fun eval ->
					total + eval) 0 listevals
			in
			  debug "%d\t\t" total
			) !listevals;
		  debug "\n\n";
		  debug "Total generations run = %d\n" (!gens_used * !gen_per_exchange);
		  if !Fitness.finish_gen then begin
			debug "Partial gens = %g\n" ((float !Fitness.min_varnum) /. (float !Search.popsize));
			debug "Last gen variants = %d\n" !Fitness.min_varnum;
			debug "Successes=%d\n\n" (!Fitness.successes)
		  end
		  else begin
			debug "Partial gens = %g\n" partgen;
			debug "Last gen variants = %d\n" !Fitness.varnum
		  end;
		  debug "Last computer = %d\n\n" (!last_comp+1);
		  (match !distributed with
			"net" ->
			  debug "Total generations run = %d\n" (!gens_used * !gen_per_exchange);
			  debug "Partial gens = %g\n" partgen;
		  debug "Last gen variants = %d\n" !Fitness.varnum;
		  | "seq" ->
			if !totgen > -1 then begin
			  debug "Total generations run = %d\n" !totgen;
			  debug "Partial gen = %g\n" partgen;
			  debug "Last gen variants = %d\n\n" !Fitness.varnum
			end
		  | _ -> ()));

  match !distributed with
	"net" -> if !server then i_am_the_server() else distributed_client rep population
  | "seq" -> distributed_sequential search_strategy rep population
  | _ -> debug "Unrecognized distributed mode %s.  Options: net, seq\n" !distributed; exit 1
end

(* from search.ml: *)


(*************************************************************************
 *************************************************************************
                     Distributed computation
 *************************************************************************
 *************************************************************************)
(* Various helper functions*)


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
    if currcomp < !Search.num_comps then begin
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
	bind main_socket  (ADDR_INET (server_address, !port));
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
      debug "You are the client connecting to %s\n" !hostname;
      let server_address = ref inet_addr_any in
	if !hostisip then
	  server_address := inet_addr_of_string !hostname
	else
	  server_address := (gethostbyname !hostname).h_addr_list.(0);
	debug "Address = %s\n" (string_of_inet_addr !server_address);

    (* Loops until connected *)
	let b = ref true in
	  while !b do
	    try begin
	      connect main_socket  (ADDR_INET (!server_address,!port));
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
