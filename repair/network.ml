open Global
open Unix

(* Global variable *)
let gens_used = ref 0

let server = ref false
let hostname = ref "church"
let port = ref 65000
let reset_seed = ref false
let set_computer = ref (-1)
let hostisip = ref false
let _ =
  options := !options @
  [
    "--server", Arg.Set server, " This is server machine"	;
    "--hostname", Arg.Set_string hostname, "X Hostname to connect to"	;
    "--port", Arg.Set_int port, "X Port used"	;
    "--hostisip", Arg.Set hostisip, " Set if hostname is IP";
    "--reset-seed", Arg.Set reset_seed, " Distributed: Resets seed between each generation";
    "--set-comp", Arg.Set_int set_computer, "X Distributed: Sets the computer number instead of assigning it automatically."
  ] 

exception Send_Failed

let setup incoming_pop rep = begin
  let msgout = open_out (Printf.sprintf "message.%d" !random_seed) in 
  (* Helper functions *)
  (* A send with some mild error checking *)
  let my_send sock str num1 num2 msglst =
    try
      let x = send sock str num1 num2 msglst in
	if (x != (num2-num1)) then 
	  raise Send_Failed
    with e ->
      debug "Error: %s\n" (Printexc.to_string e);
  in

  (* If someone finds the repair, they tell the server *)
  let last_send socket_list =
    let strlen = (String.length !Fitness.success_rep) in
    (* Server tells clients that everyone's done *)
    if !server then
      if (strlen == 0) then ()
      else
	List.iter (fun sock ->
	  try
	    my_send sock "   4" 0 4 [];
	    my_send sock "Done" 0 4 [];
	    my_send sock (Printf.sprintf "%4d" strlen) 0 4 [];
	    my_send sock !Fitness.success_rep 0 strlen [];
	  with _ -> ();
	) socket_list

    (* Client tells server that it's done *)
    else
      if (strlen == 0) then ()
      else
	try
	  let sock = List.hd socket_list in
	    ignore((getnameinfo (getpeername sock) []).ni_hostname);
	    my_send sock "   4" 0 4 [];
	    my_send sock "Done" 0 4 [];
	    my_send sock (Printf.sprintf "%4d" strlen) 0 4 [];
	    my_send sock !Fitness.success_rep 0 strlen [];
	with _ -> ();
  in

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
  in


 (* Helps the server exchange populations *)
  let rec serv_pop_exchange currcomp sock socket_list =
    let time_at_start = Unix.gettimeofday () in
    debug "Waiting for client %d\n" currcomp;
    if currcomp < !Search.num_comps-1 then begin
      let str = (readall sock (my_int_of_string (readall sock 4))) in
      debug "Time waited: %g\n" ((Unix.gettimeofday ()) -. time_at_start);
      if ((String.compare str "Done") == 0) then begin
	debug "\nRepair found - Check computer %d\n\n" currcomp;
	ignore(Search.message_parse rep (readall sock (my_int_of_string (readall sock 4))));
	exit 1
      end;
      (serv_pop_exchange (currcomp+1) (List.hd socket_list) (List.tl socket_list)) @ [str]
    end
    else begin
      let str = (readall sock (my_int_of_string (readall sock 4))) in
      debug "Time waited: %g\n" ((Unix.gettimeofday ()) -. time_at_start);
      if ((String.compare str "Done") == 0) then begin
	debug "\nRepair found - Check computer %d\n\n" currcomp;
	ignore(Search.message_parse rep (readall sock (my_int_of_string (readall sock 4))));
	exit 1
      end;
	[str]
    end
  in

  (* This is where all the exchange takes place *)
  let pop_exchange pop socket_list =
    let msgpop = Search.get_exchange_network rep pop in
    Printf.fprintf msgout "%s\n" (fst msgpop);

    (* Server: *)
    if !server then begin
      let sock = List.hd socket_list in
      let msglst = serv_pop_exchange 1 sock (List.tl socket_list) in
      my_send sock (Printf.sprintf "%4d" (String.length (fst msgpop))) 0 4 [];
      my_send sock (fst msgpop) 0 (String.length (fst msgpop)) [];
      List.iter2 (fun s msg ->
	my_send s (Printf.sprintf "%4d" (String.length msg)) 0 4 [];
	my_send s msg 0 (String.length msg) [];
      ) (List.tl socket_list) (List.rev (List.tl msglst));
      (Search.message_parse rep (List.hd msglst)) @ (snd msgpop)
    end

    (* Client: *)
    else begin

      (* Sends the size, then the variantlist to the server *)
      let sock = List.hd socket_list in
	my_send sock (Printf.sprintf "%4d" (String.length (fst msgpop))) 0 4 [];
	my_send sock (fst msgpop) 0 (String.length (fst msgpop)) [];

      (* Receives a message back. If it's Done, exit, else continue *)
      let time_at_start = Unix.gettimeofday () in
      debug "Waiting for server\n";
      let str = (readall sock (my_int_of_string (readall sock 4))) in
      debug "Time waited: %g\n" ((Unix.gettimeofday ()) -. time_at_start);
      if ((String.compare str "Done") == 0) then begin
	debug "\nRepair found - Check Server\n\n";
	ignore(Search.message_parse rep (readall sock (my_int_of_string (readall sock 4))));
	exit 1
      end;

      (Search.message_parse rep str) @ (snd msgpop)
    end
  in

  (* Main function call *)
  let startfunction computer socket_list =
    let totgen = !Search.generations in
    let exchange_iters = totgen / !Search.gen_per_exchange in
      Search.generations := !Search.gen_per_exchange;
      let rec all_iterations gen population =
	if gen < exchange_iters then begin
	  if !reset_seed then
	    Random.init !random_seed;
	  debug "I am computer %d:\n" computer;
	  Search.varnum := 0;	    
	  gens_used := 1 + !gens_used;
	  all_iterations (gen + 1) (pop_exchange (Search.genetic_algorithm rep (lmap fst population) ~comp:computer) socket_list)
	end
      (* Goes through the rest of the generations requested*)
	else if (totgen mod !Search.gen_per_exchange) <> 0 then begin
	  Search.generations := (totgen mod !Search.gen_per_exchange);
	  ignore(Search.genetic_algorithm rep (lmap fst population) ~comp:computer);
	  gens_used := 1 + !gens_used
	end
      in
	all_iterations 0 incoming_pop;
  in

  (* Assigns all computers a number and sets up communication *)
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

    (* Makes it close all sockets at exit *)
      at_exit(fun () -> 
	gens_used := !gens_used - 1;
	close_out msgout;
	last_send socket_list;
	try
	  close main_socket
	with _ -> ();
	  List.iter (fun sock -> try close sock with _ -> ()) socket_list;
      );

    (* Starts all computers *)
    List.iter (fun sock -> begin
      my_send sock str 0 (String.length str) [];
      debug "Starting next computer. \n";
      debug "Address = %s\n" (getnameinfo (getpeername sock) []).ni_hostname
    end ) socket_list;

      (*Starts main function *)
    if !set_computer >= 0 then
      startfunction !set_computer socket_list
    else
      startfunction 0 socket_list
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
      
     (* Closes socket at exit*)
     at_exit(fun () -> 
       gens_used := !gens_used - 1;
       close_out msgout;
       last_send [main_socket];
       try
	 close main_socket ;
       with _ -> ()
     );

     (* Gets info it needs, then starts main function *)
     let currcomp = ref !set_computer in
       if !currcomp < 0 then
	 currcomp := my_int_of_string (readall main_socket 4)
       else
	 ignore(readall main_socket 4);
       debug "I am computer number %d.\n" !currcomp;
       assert ((String.compare (readall main_socket 5) "start") == 0);
       startfunction !currcomp [main_socket]
  end
end

