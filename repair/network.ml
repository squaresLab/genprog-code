open Global
open Distglobal
open Unix
open Population
open Search

(* Options*)
let server = ref false
let hostname = ref "127.0.0.1"
let my_port = ref 60000
let variants_exchanged = ref 50
let gen_per_exchange = ref 1
(* Type 1 uses purely diversity, Type 2 uses Diversity and Fitness *)
let diversity_selection = ref 0
let split_search = ref 0

let _ = 
  options := !options @ [
    "--hostname", Arg.Set_string hostname, "X server ip"	;
    "--sport", Arg.Set_int server_port, "X server port"	;
    "--port", Arg.Set_int my_port, "X my port"	;
    "--num-comps", Arg.Set_int num_comps, "X Distributed: Number of computers to simulate" ;
    "--diversity-selection", Arg.Set_int diversity_selection, "X Distributed: Use diversity for exchange";
    "--variants-exchanged", Arg.Set_int variants_exchanged, "X Distributed: Number of variants to send" ;
    "--gen-per-exchange", Arg.Set_int gen_per_exchange, "X Distributed: Number of generations between exchanges" ;
(* is split search ever different from num_comps? *)
	"--split-search", Arg.Set_int split_search, "X Distributed: Split up the search space" ;

  ] 


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
		  rep
	  ) varlst
  in
    retlist,!totbytes

(* Creates the message that the function above parses *)
let make_message (lst : ('a,'b) GPPopulation.t) = 
  let all_histories = 
	lmap 
	  (fun rep ->
		let strs = lmap (rep#history_element_to_str) (rep#get_history()) in
		  String.concat " " strs,get_opt (rep#fitness())) lst in
  String.concat "." 
    (lmap (fun (ele,fit) -> Printf.sprintf "%g %s" fit ele) all_histories)

(* Chooses variants based on diversity metrics instead of just fitness,
   if the diversity-selection option is enabled *)
let choose_by_diversity (orig : ('a,'b) Rep.representation) (lst : ('a,'b) GPPopulation.t) : ('a,'b) GPPopulation.t =
  let string_list_describing_history rep : string list =
    let history_list = rep#get_history () in
    lmap (rep#history_element_to_str) history_list
  in 
  let histlist = lmap (fun ele -> 
    ele, 
    string_list_describing_history ele
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
      (fun allset (_,oneset) ->
	  StringSet.union allset oneset)
      (StringSet.empty) setlist
  in
  (* Look at which variant has the most changes different from other chosen variants *)
  let rec collect_variants (allset) (setlist) (sofar) : ('a,'b) GPPopulation.t =
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
		  let copy = orig#copy() in
			copy#set_fitness fit; copy
	    end) (1 -- (!variants_exchanged - sofar))
    end
  in
    collect_variants allset setlist 0

(* Gets a list of the population that we wish to exchange*)
let get_exchange orig (lst : ('a,'b) GPPopulation.t) : ('a,'b) GPPopulation.t =
	if !diversity_selection == 1 then 
	  choose_by_diversity orig (random_order lst)
	else if !diversity_selection == 2 then
	  let lst = List.sort (fun i i' -> 
		let f = get_opt (i#fitness()) in
		let f' = get_opt (i'#fitness()) in
		  compare f' f) lst in
	    choose_by_diversity orig lst
	else 
	  first_nth (random_order lst) !variants_exchanged


let distributed_client rep incoming_pop = begin
  let client_tbl = Hashtbl.create (!num_comps+3) in
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
  listen main_socket (!num_comps+5);

  (* Connecting to server *)
  let server_address = inet_addr_of_string !hostname in
  connect_to_sock server_socket (ADDR_INET(server_address,!server_port));
  my_comp := my_int_of_string (fullread server_socket);
    fullsend server_socket (Printf.sprintf "%d" !my_port);

    (* Populates the client_tbl with the keys being the computer number and the value being their sockaddr *)
    for i=1 to !num_comps do
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
    let rec all_iterations generations (population : ('a,'b) GPPopulation.t) =
      try
	if generations < (!Search.generations+1) then begin
	  let num_to_run = 
	    if (!Search.generations + 1 - generations) > !gen_per_exchange then !gen_per_exchange
	    else !Search.generations - generations
	  in
	  let population = Search.run_ga ~start_gen:generations ~num_gens:num_to_run population rep in
	    if num_to_run <> (!Search.generations - generations) then begin
	      fullsend server_socket "X";
	      let msgpop = make_message 
			(get_exchange rep population) in
	      let from_neighbor,bytes = exchange_variants msgpop in
		totbytes := bytes + !totbytes;
	      let population = population @ from_neighbor in
			all_iterations (generations + !gen_per_exchange) population
	    end 
	end
      with Found_repair(rep) -> (exit 1)
    in
	let mut_ids = rep#get_faulty_atoms () in
	let splitting_function x length comp =
      if (comp < !num_comps-1) then
		(x >= length*comp / !num_comps) &&
		  (x < length*(comp+2) / !num_comps)
      else
		(x >= length*comp / !num_comps) ||
		  (x < length / !num_comps)
	in
	let reduce_func (x, prob) = 
	  match !split_search with
		1 ->  (x mod !num_comps) = !my_comp
	  | 2 -> (x mod !num_comps) = !my_comp || prob = 1.0
	  | 3 when !num_comps > 2 ->
		let len = llen mut_ids in
		  prob = 1.0 || (splitting_function x len !my_comp)
	  | _ -> true
	in
	  (* fixme: length of mut_ids might be wrong based on promut *)
	  rep#reduce_search_space reduce_func false;
      ignore(all_iterations 1 (Search.initialize_ga rep incoming_pop));
      debug "\n\nNo repair found.\n\n";
      exit 1
end
