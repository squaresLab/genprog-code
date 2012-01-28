open Global
open Unix
open Search

(* Options*)
let server = ref false
let hostname = ref "127.0.0.1"
let server_port = ref 65000
let my_port = ref 60000
let variants_exchanged = ref 50
let gen_per_exchange = ref 1
(* Type 1 uses purely diversity, Type 2 uses Diversity and Fitness *)
let diversity_selection = ref 0

let _ = 
  options := !options @ [
    "--server", Arg.Set server, " This is server machine"	;
    "--hostname", Arg.Set_string hostname, "X server ip"	;
    "--sport", Arg.Set_int server_port, "X server port"	;
    "--port", Arg.Set_int my_port, "X my port"	;
    "--num-comps", Arg.Set_int Search.num_comps, "X Distributed: Number of computers to simulate" ;
    "--diversity-selection", Arg.Set_int diversity_selection, "X Distributed: Use diversity for exchange";
    "--variants-exchanged", Arg.Set_int variants_exchanged, "X Distributed: Number of variants to send" ;
    "--gen-per-exchange", Arg.Set_int gen_per_exchange, "X Distributed: Number of generations between exchanges" ;
  ] 

exception Send_Failed

let distributed_sequential rep pop = 
  debug "This is currently deprecated. Don't use it. The program will exit now.\n";
  exit 1;
  List.map (fun a -> (a,1.0)) pop

(* Various helper functions*)

(* Parses messages received from other computers and turns them into reps.
   Variants are separated by a period, '.', and mutations are separated by a space, ' '*)
let message_parse orig msg =
  let totbytes = ref 0 in
  (* Splits into a list of history lists *)
  let varlst = lmap (fun str -> 
    Str.split (Str.regexp_string " ") str
  ) (Str.split (Str.regexp_string ".") msg) 
  in
    (* Turns said list into a list of variants *)
  let retlist = lmap 
	  (fun history ->
		let rep = orig#copy() in
		let fitness = float_of_string (List.hd history) in 
		  liter
			(fun hist -> 
			  match hist.[0] with
			  | 'd' ->
				let num = (int_of_string (String.sub hist 2 ((String.index hist ')')-2))) in
				  totbytes := 4 + !totbytes;
				  rep#delete num
			  | 'a' ->
				let tmp = (String.index hist ',') in
				let num1 = (int_of_string (String.sub hist 2 (tmp-2))) in
				let num2 = (int_of_string (String.sub hist (tmp+1) ((String.index hist ')')-tmp-1))) in
				  totbytes := 6 + !totbytes;
				  rep#append num1 num2
			  | 's' ->
				let tmp = (String.index hist ',') in
				let num1 = (int_of_string (String.sub hist 2 (tmp-2))) in
				let num2 = (int_of_string (String.sub hist (tmp+1) ((String.index hist ')')-tmp-1))) in
				  totbytes := 6 + !totbytes;
				  rep#swap num1 num2
			  | 'x' -> ()
			  (*		    debug "Hit a crossover\n";*)
			  |  _  ->  ()
			(*		    debug "Error: This is not a variant, it is:  %s\n" hist;*)
			) (List.tl history);
		  rep#set_fitness fitness;
		  rep,fitness
	  ) varlst
  in
    retlist,!totbytes

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
	  (* DEBUG: debug "Variant: %s\n" (a#name ());*)
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

(* Gets a list of the population that we wish to exchange*)
let get_exchange orig lst =
	if !diversity_selection == 1 then 
	  choose_by_diversity orig (random_order lst)
	else if !diversity_selection == 2 then
	  let lst = List.sort (fun (_,f) (_,f') -> compare f' f) lst in
	    choose_by_diversity orig lst
	else 
	  first_nth (random_order lst) !variants_exchanged

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

(* A send with some mild error checking *)
let my_send sock str num1 num2 msglst =
  try
    let x = send sock str num1 num2 msglst in
      if (x != (num2-num1)) then 
	raise Send_Failed
  with e ->
    debug "Error in send: %s\n" (Printexc.to_string e)

(* Handles the internal formatting to make it easier *)
let fullread sock =
  let size = my_int_of_string (readall sock 4) in
    readall sock size

let fullsend sock str =
  let len = String.length(str) in
  let newstr = Printf.sprintf "%4d%s" len str in
    my_send sock newstr 0 (len+4) []

let rec spin socklist accum =
  try
    match socklist with
    | [] -> accum 
    | _ -> begin
      let ready_for_read, _, _ =
	Unix.select socklist [] [] (-1.0) 
      in
      let lst = lfoldl (fun acc sock -> (fullread sock) :: acc) [] ready_for_read in
      let socklist = List.filter (fun sock -> not (List.mem sock ready_for_read)) socklist in
	spin socklist (accum @ lst)
    end 
  with e -> 
    debug "Error in spin: %s\n" (Printexc.to_string e);
    accum

(* Global variables *)
let server_socket  = socket PF_INET SOCK_STREAM 0

let i_am_the_server ()= begin
  let client_tbl = Hashtbl.create !Search.num_comps in
  let info_tbl = Hashtbl.create !Search.num_comps in
  (* Adds all client computers to client_tbl (and set info_tbl to its default)*)
  let rec getcomps sock =
    for currcomp=0 to (!Search.num_comps-1) do
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
  listen server_socket (!Search.num_comps+5);
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
      match String.sub buffer 0 1 with
      | "X" -> ()
      | _ -> bool := false;
      
	if !bool then
	  ()
	else begin
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
	    | _ -> failwith (Printf.sprintf "Unexpected buffer in process_stats: %s\n" buffer)
	  in
	  let split = List.tl split in 
	  let bytes_read,split =  (my_int_of_string (List.hd split)), List.tl split in
	  let evals_done,split =  (my_int_of_string (List.hd split)), List.tl split in
	  let gens = (my_int_of_string (List.hd split)) in
	    hrep info_tbl comp (found_repair,bytes_read,evals_done,gens)
	end
	
    in

    let rec inform_neighbours () =
      let rec lstmaker curr =
	if curr < !Search.num_comps then
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
      let last = List.nth lst (!Search.num_comps-1) in
	sender lst last
	
    in

    (* Let's spin until we find a client who's done? *)
      while !bool do
	let msglist = (spin !socketlist []) in
	  liter process_stats msglist;
	  if !bool then
	    inform_neighbours()
      done;
      exit 1
end


let distributed_client rep incoming_pop = begin
  let client_tbl = Hashtbl.create (!Search.num_comps+3) in
  let totbytes = ref 0 in
  let my_comp = ref 0 in
  (*Client exit function *)
  let client_exit_fun () =
    (* at exit, send statistics to the server *)
    let final_stat_msg = 
      if !Fitness.successes > 0 then "DF" else "DN"
    in
    let bytes_read = Printf.sprintf "%d" !totbytes in
    let gens = match !Search.success_info with
      | [] -> 0
      | hd :: tl -> hd.generation
    in
    let test_suite_evals = Printf.sprintf "%d" ((Rep.num_test_evals_ignore_cache ())/(!pos_tests + !neg_tests)) in
    let str = Printf.sprintf "%d %s %s %s %d" !my_comp final_stat_msg bytes_read test_suite_evals gens in
      fullsend server_socket str; 
    try
      close server_socket
    with _ -> ();
  in

  (* Connect to a given socket (loops until connected) *)
  let rec connect_to_sock sock addr =
    try 
      connect sock addr;
    with _ ->
      sleep 5;
      connect_to_sock sock addr
  in

  at_exit client_exit_fun;
  (* Setting up client socket *)
  let main_socket = socket PF_INET SOCK_STREAM 0 in
    setsockopt main_socket (SO_REUSEADDR) true ;
  bind main_socket  (ADDR_INET (inet_addr_any, !my_port));
  listen main_socket (!Search.num_comps+5);

  (* Connecting to server *)
  let server_address = inet_addr_of_string !hostname in
  connect_to_sock server_socket (ADDR_INET(server_address,!server_port));
  my_comp := my_int_of_string (fullread server_socket);
    fullsend server_socket (Printf.sprintf "%d" !my_port);

    (* Populates the client_tbl with the keys being the computer number and the value being their sockaddr *)
    for i=1 to !Search.num_comps do
      let strlist = Str.split (Str.regexp " ") (fullread server_socket) in
	Hashtbl.add client_tbl (my_int_of_string (List.hd strlist)) (ADDR_INET((inet_addr_of_string (List.nth strlist 1)),(my_int_of_string (List.nth strlist 2))))
    done;

    let exchange_variants str =
      let buffer = (fullread server_socket) in
      let sendto = match buffer with
	| "X" -> begin
	  debug "\n\nServer has ordered termination\n\n";
	  exit 1
	end
	| a -> my_int_of_string a
	| _ -> failwith (Printf.sprintf "Unexpected buffer in exchange_variants: %s\n" buffer)
      in
	if (!my_comp == !num_comps-1) then begin
	  let (sock,_) = accept main_socket in
	  let tempstr = (fullread sock) in
	    (* DEBUG: debug "Received: %s\n" tempstr; *)
	  let pop,bytes = message_parse rep tempstr in
	  let newsock = socket PF_INET SOCK_STREAM 0 in
	    connect_to_sock newsock (Hashtbl.find client_tbl sendto);
	    fullsend newsock str;
	    pop, bytes
	end
	else begin
	  let newsock = socket PF_INET SOCK_STREAM 0 in
	    connect_to_sock newsock (Hashtbl.find client_tbl sendto);
	    fullsend newsock str;
	    let (sock,_) = accept main_socket in
	    let tempstr = (fullread sock) in
	      (* DEBUG: debug "Received: %s\n" tempstr; *)
	    let pop,bytes = message_parse rep tempstr in
	      pop, bytes
	end
    in

  (* Starting iterations of genetic algorithm *)
    let rec all_iterations generations (population : ('a Rep.representation * float) list) =
      try
	if generations < (!Search.generations+1) then begin
	  let num_to_run = 
	    if (!Search.generations + 1 - generations) > !gen_per_exchange then !gen_per_exchange
	    else !Search.generations - generations
	  in
	  let population = Search.run_ga ~comp:!my_comp ~start_gen:generations ~num_gens:num_to_run population rep in
	    if num_to_run <> (!Search.generations - generations) then begin
	      fullsend server_socket "X";
	      let msgpop = make_message (get_exchange rep population) in
	      let from_neighbor,bytes = exchange_variants msgpop in
		totbytes := bytes + !totbytes;
	      let population = population @ from_neighbor in
		all_iterations (generations + !gen_per_exchange) population
	    end 
	end
      with Fitness.Found_repair(rep) -> (exit 1)
    in
      ignore(all_iterations 1 (Search.initialize_ga ~comp:!my_comp rep incoming_pop));
      debug "\n\nNo repair found.\n\n";
      exit 1
end
