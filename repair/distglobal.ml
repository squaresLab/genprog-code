(** Distglobal provides network options/utilities for the distributed GA server
	and client implementations.  Because we compile the server separately, we
	separate these utilities out into this module to avoid repetitiveness *)
open Global
open Unix

let num_comps = ref 2
let server_port = ref 65000

exception Send_Failed

(** readall socket message_size reads message_size bytes from socket and returns
	it as a string.  readall can throw an exception/abort if any of the system
	calls failed; it is typically called from another, higher-level function
	that will catch such exceptions and handle them gracefully. *)
let readall sock size = 
  let count = ref 0 in
  let buffer = String.create (size+1) in
  let rec _readall accum =
    let currcount = recv sock buffer 0 size [] in
      count := currcount + !count;
      if !count <> size then
		if currcount = 0 then begin
		  sleep 5;
		  _readall accum
		end
		else
		  _readall ((String.sub buffer 0 currcount)::accum)
      else
		((String.sub buffer 0 currcount)::accum)
  in
	String.concat "" (List.rev (_readall [])) 

(** my_send socket message start num_bytes flags sends num_bytes of message over
	socket, starting at byte start, with flags, and does some mild error checking *)
let my_send sock str num1 num2 msglst =
  try
    let x = send sock str num1 num2 msglst in
      if x <> (num2-num1) then 
		raise Send_Failed
  with e ->
    debug "Error in send: %s\n" (Printexc.to_string e)

(** fullread socket does internal formatting by reading the size of the message
	in bytes from a socket before dispatching to readall to read the rest of the
	message *)
let fullread sock =
  let size = my_int_of_string (readall sock 4) in
    readall sock size

(** fullsent socket message sends the length of the message on socket and then
	sends the message *)
let fullsend sock str =
  let len = String.length(str) in
  let newstr = Printf.sprintf "%4d%s" len str in
    my_send sock newstr 0 (len+4) []

(** spin socket_list result does a select on socket_list where socket_list is a
	list of sockets from which we hope to read, reads all available data from
	those that return, and spins until all sockets in socket_list have been read
	from successfully.  It will catch any exception thrown by the helper/reading
	functions and print an error message, but not abort. *)
let rec spin socklist accum =
  try
    match socklist with
    | [] -> accum 
    | _ -> begin
      let ready_for_read, _, _ =
		Unix.select socklist [] [] (-1.0) 
      in
      let lst = 
		lfoldl (fun acc sock -> (fullread sock) :: acc) [] ready_for_read 
	  in
      let socklist = 
		List.filter (fun sock -> not (List.mem sock ready_for_read)) socklist 
	  in
		spin socklist (accum @ lst)
    end 
  with e -> 
    debug "Error in spin: %s\n" (Printexc.to_string e);
    accum

let server_socket = socket PF_INET SOCK_STREAM 0


(** {b connect_to_sock} socket addr loops until it successfully connects to
	socket.  It therefore can, potentially, loop infinitely. *)
let rec connect_to_sock sock addr =
  try 
    connect sock addr;
  with _ ->
    sleep 5;
    connect_to_sock sock addr
