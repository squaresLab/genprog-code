(*
 * Program Repair Prototype (v2) 
 *
 * Networked Hash Table -- a networked cache for storing <key,value> pairs,
 * such as the results of program test case evaluations. 
 *
 * This is handy for cloud computing or other distributed searches where
 * the various searchers do not share local filesystem access, and thus do
 * not share "repair.cache". 
 *
 * The NHT is event-based (i.e., non-blocking on connections) and can
 * handle multiple incoming requests at a time (implicitly serializing
 * them). 
 *
 * STILL TODO: add persistent support (i.e., load/save the hash table) 
 *)
open Global
open Unix

let debug fmt = 
  let k result = begin
    output_string Pervasives.stdout result ; 
    flush Pervasives.stdout ; 
  end in
  Printf.kprintf k fmt 

let the_global_ht = ref (Hashtbl.create 4095) 

let get_ht_of_name name = 
  try
    Hashtbl.find !the_global_ht name 
  with _ -> begin 
    let result = Hashtbl.create 4095 in
    Hashtbl.add !the_global_ht name result ;
    result 
  end 

type connection_state = 
  | Server_Socket
  | Reading 
  | Writing 
  | Closing 

type connection = {
          socket : file_descr ;   (* non-blocking *) 
  mutable state : connection_state ;
  mutable payload : Buffer.t ; 
} 

let port = ref 51000
let listen_max = ref 16
let select_timeout = ref 1.0 
let newline_regexp = Str.regexp "[\r\n]+" 

let process_payload payload = 
  let lines = Str.split newline_regexp payload in
  let output_buffer = Buffer.create 2048 in 
  let state = match lines with
    | "e" :: rest -> (* ECHO *) 
      List.iter (fun line ->
        Printf.bprintf output_buffer "%s\n" line ;
      ) rest ;
      (Writing) 

    | "p" :: ht_name :: entry_key :: entry_value :: [] -> (* PUT *) 
      let local_ht = get_ht_of_name ht_name in
      Hashtbl.replace local_ht entry_key entry_value ;
      (Closing) 

    | "g" :: ht_name :: entry_key :: [] -> (* GET *) 
      let local_ht = get_ht_of_name ht_name in 
      begin try 
        let entry_value = Hashtbl.find local_ht entry_key in
        Printf.bprintf output_buffer "1\n%s\n" entry_value 
      with _ -> 
        Printf.bprintf output_buffer "0\n" 
      end ; (Writing) 

    | verb :: rest -> 
      Printf.bprintf output_buffer "Unknown verb: %s\n\n" verb ; 
      (Writing)

    | _ -> 
      Printf.bprintf output_buffer "Empty verb received:\n\n" ; 
      (Writing)
  in
  state, output_buffer

let event_loop ss_connection = begin
  let connections = Hashtbl.create 255 in
  Hashtbl.add connections ss_connection.socket ss_connection ; 

  let my_close socket = 
    (try close socket with _ -> ()) ;
    Hashtbl.remove connections socket 
  in 

  let buffer_len = 2048 in 
  let buffer = String.make buffer_len '\000' in 

  while true do
    try 
      (* Process all current connections into read/write/error lists *) 
      let read_fds = ref [] in
      let write_fds = ref [] in
      let error_fds = ref [] in 
      Hashtbl.iter (fun socket c -> 
        match c.state with
        | Server_Socket 
        | Reading 
        -> read_fds := socket :: !read_fds 
        | Writing
        -> write_fds := socket :: !write_fds
        | Closing 
        -> error_fds := socket :: !error_fds 
      ) connections;

      List.iter my_close !error_fds ; 

      (* Wait until something is ready for us *) 
      let reads, writes, errors = 
        select !read_fds !write_fds [] !select_timeout 
      in 

(*
      debug "%d reads.  %d writes.  %d errors.\n" 
        (List.length reads )  
        (List.length writes )  
        (List.length errors ) ; 
*) 

      List.iter (fun read_socket ->
        let c = Hashtbl.find connections read_socket in 
        match c.state with
        | Server_Socket -> 
          let client_socket, client_sockaddr = accept read_socket in 
          set_nonblock client_socket ; 
          let new_connection = {
            socket = client_socket ;
            state = Reading ;
            payload = Buffer.create 2048 ; 
          } in
          Hashtbl.add connections client_socket new_connection 

        | Reading -> 
          let amount = recv read_socket buffer 0 buffer_len [] in 
          if amount = 0 then begin
            my_close read_socket 
          end else begin 
            Buffer.add_substring c.payload buffer 0 amount ;
            let payload_length = Buffer.length c.payload in 
            if payload_length > 2 then begin 
              let last_chars = Buffer.sub c.payload 
                (payload_length - 3) 2 in 
              (* debug "last_chars = %S\n" last_chars ;  *)
              if last_chars = "\n\n" then begin
                let payload_string = Buffer.contents c.payload in 
                let new_state, new_payload = process_payload payload_string in
                c.state <- new_state ; 
                c.payload <- new_payload ; 
              end 
            end 
          end 

        | _ -> () 
      ) reads ; 

      List.iter (fun write_socket ->
        let c = Hashtbl.find connections write_socket in 
        match c.state with
        | Writing -> 
          let payload_length = Buffer.length c.payload in
          let payload_string = Buffer.contents c.payload in 
          let amount = send write_socket payload_string 0 payload_length [] in
          if amount = payload_length then begin
            (* done writing! *) 
            my_close write_socket 
          end else begin
            let remainder = Str.string_after payload_string amount in 
            let new_payload = Buffer.create payload_length in 
            Buffer.add_string new_payload remainder ;
            c.payload <- new_payload 
          end 

        | _ -> () 
      ) writes ;

      List.iter (fun error_socket ->
        my_close error_socket 
      ) errors ; 

    with e -> 
      debug "ERROR: %s\n" (Printexc.to_string e) 
  done 
end ;; 

let main () = begin
  let options = [
    "--port", Arg.Set_int port, "X use port X" ;
    "--select_timeout", Arg.Set_float select_timeout, "X select timeout X (seconds)" ;
    "--max-listen", Arg.Set_int listen_max, "X up to X pending listen requests" ;
  ] 
  in 
  let aligned = Arg.align options in 
  let usage_msg = "Program Repair Prototype -- Networked Hash Table" in 
  Arg.parse aligned (fun x -> 
    Arg.usage aligned usage_msg ; exit 1 
  ) usage_msg;

  let server_socket = socket (PF_INET) (SOCK_STREAM) 0 in 
  setsockopt server_socket (SO_REUSEADDR) true ; 
  set_nonblock server_socket ; 
  let server_sockaddr = (ADDR_INET(inet_addr_any,!port)) in 
  bind server_socket server_sockaddr ; 
  listen server_socket !listen_max ; 

  let ss_connection = {
    socket = server_socket ;
    state = Server_Socket ;
    payload = Buffer.create 1 ;
  } in
  event_loop ss_connection ; 

end ;; 

main () ;;
