(*
 *
 * Copyright (c) 2012-2018,
 *  Wes Weimer          <weimerw@umich.edu>
 *  Stephanie Forrest   <steph@asu.edu>
 *  Claire Le Goues     <clegoues@cs.cmu.edu>
 *  Eric Schulte        <eschulte@cs.unm.edu>
 *  Jeremy Lacomis      <jlacomis@cmu.edu>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. The names of the contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)
(** Networked Hash Table -- implements a networked cache for storing <key,value>
    pairs, such as the results of program test case evaluations.

    This is handy for cloud computing or other distributed searches where the
    various searchers do not share local filesystem access, and thus do not
    share "repair.cache".

    The NHT is event-based (i.e., non-blocking on connections) and can handle
    multiple incoming requests at a time (implicitly serializing them).

    Every few seconds, if the store has been updated, it is saved to the
    disk. This is also done in a non-blocking manner so that the server is still
    available while saving large stores.

    This is a separate utility from the rest of GenProg, written by Wes, that
    CLG did not look at very hard during the March 2012 refactor.
*)

open Global
open Unix

let verbose = ref false

let debug fmt =
  let k result = begin
    output_string Stdlib.stdout result ;
    flush Stdlib.stdout ;
  end in
  Printf.kprintf k fmt

let the_global_ht = ref (Hashtbl.create 4095)
let global_ht_filename = ref "repair.nht.cache"
let unsaved_writes = ref false
let currently_saving = ref false

let sockaddr_to_str s =
  match s with
  | Unix.ADDR_UNIX(s) -> "unix:" ^ s
  | Unix.ADDR_INET(ia,port) -> (string_of_inet_addr ia) ^ ":" ^
                               (string_of_int port)

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
  | Saving
  | Closing

type connection = {
  socket : file_descr ;   (* non-blocking *)
  mutable state : connection_state ;
  mutable read_payload : Buffer.t ;
  mutable write_payload : string ;
  mutable write_offset : int * int; (* amount, length *)
}

let port = ref 51000
let listen_max = ref 16
let select_timeout = ref 1.0
let newline_regexp = Str.regexp "[\r\n]+"
let save_timeout = ref 1.0

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
      (if !verbose then
         debug "put: %s %s %s\n" ht_name entry_key entry_value) ;
      unsaved_writes := true ;
      (Closing)

    | "g" :: ht_name :: entry_key :: [] -> (* GET *)
      let local_ht = get_ht_of_name ht_name in
      begin try
          let entry_value = Hashtbl.find local_ht entry_key in
          (if !verbose then
             debug "get: %s %s = %s\n" ht_name entry_key entry_value) ;
          Printf.bprintf output_buffer "1\n%s\n" entry_value
        with _ ->
          (if !verbose then
             debug "get: %s %s\n" ht_name entry_key) ;
          Printf.bprintf output_buffer "0\n"
      end ; (Writing)

    | verb :: rest ->
      Printf.bprintf output_buffer "Unknown verb: %s\n\n" verb ;
      (Writing)

    | _ ->
      Printf.bprintf output_buffer "Empty verb received:\n\n" ;
      (Writing)
  in
  state, (Buffer.contents output_buffer)

let event_loop ss_connection = begin
  let connections = Hashtbl.create 255 in
  Hashtbl.add connections ss_connection.socket ss_connection ;

  let my_close socket =
    (try close socket with _ -> ()) ;
    (if !verbose then debug "my_close: socket closed\n");
    Hashtbl.remove connections socket
  in

  let buffer_len = 2048 in
  let buffer = String.make buffer_len '\000' in
  let last_save_time = ref (Unix.gettimeofday ()) in

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
          | Saving
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
              read_payload = Buffer.create 2048 ;
              write_payload = "" ;
              write_offset = 0,0;
            } in
            if !verbose then begin
              debug "accepted: %s\n" (sockaddr_to_str client_sockaddr)
            end ;
            Hashtbl.add connections client_socket new_connection

          | Reading ->
            let amount = recv read_socket (Bytes.of_string buffer) 0 buffer_len [] in
            if amount = 0 then begin
              my_close read_socket
            end else begin
              Buffer.add_substring c.read_payload buffer 0 amount ;
              let payload_length = Buffer.length c.read_payload in
              if payload_length > 2 then begin
                let last_chars = Buffer.sub c.read_payload
                    (payload_length - 3) 2 in
                (* debug "last_chars = %S\n" last_chars ;  *)
                if last_chars = "\n\n" then begin
                  let payload_string = Buffer.contents c.read_payload in
                  let new_state, new_payload = process_payload payload_string in
                  c.state <- new_state ;
                  c.write_payload <- new_payload ;
                  c.write_offset <- 0, (String.length new_payload);
                end
              end
            end

          | _ -> ()
        ) reads ;

      List.iter (fun write_socket ->
          let c = Hashtbl.find connections write_socket in
          match c.state with
          | Saving
          | Writing ->
            let payload_string = (Bytes.of_string c.write_payload) in
            let offset, length = c.write_offset in
            let to_send = length - offset in
            let amount =
              if c.state = Writing then
                send write_socket payload_string offset to_send []
              else
                single_write write_socket payload_string offset to_send
            in
            (* debug "written: %d\n" amount ;  *)
            c.write_offset <- (offset + amount, length) ;
            if offset + amount = length then begin
              (* done writing! *)
              my_close write_socket ;
              if c.state = Writing then begin
                (* debug "Saving ends\n" ;  *)
                currently_saving := false
              end
            end

          | _ -> ()
        ) writes ;

      List.iter (fun error_socket ->
          my_close error_socket
        ) errors ;

      if !unsaved_writes && not !currently_saving then begin
        let now = Unix.gettimeofday () in
        if now -. !last_save_time >= !save_timeout &&
           !save_timeout >= 0.0 then begin
          last_save_time := now ;
          currently_saving := true ;
          let string_to_save = Marshal.to_string !the_global_ht
              [Marshal.No_sharing] in
          let len = String.length string_to_save in
          (* debug "Saving begins (%d bytes)\n" len ;  *)
          let fd = openfile !global_ht_filename [O_WRONLY;O_NONBLOCK;O_CREAT]
              0o660 in
          let save_connection = {
            socket = fd ;
            state = Saving ;
            read_payload = Buffer.create 1 ;
            write_payload = string_to_save ;
            write_offset = 0,len;
          } in
          Hashtbl.add connections fd save_connection
        end
      end ;

    with e ->
      debug "ERROR: %s\n" (Printexc.to_string e)
  done
end ;;

let main () = begin
  let options = [
    "--port", Arg.Set_int port, "X use port X" ;
    "--select-timeout", Arg.Set_float select_timeout, "X select timeout X (seconds)" ;
    "--save-timeout", Arg.Set_float select_timeout, "X save-to-disk timeout X (seconds)" ;
    "--max-listen", Arg.Set_int listen_max, "X up to X pending listen requests" ;
    "--verbose", Arg.Set verbose, " note incomming connections" ;
    "--filename", Arg.Set_string global_ht_filename, "X use X as on-disk filename" ;
  ]
  in
  let aligned = Arg.align options in
  let usage_msg = "Program Repair Prototype -- Networked Hash Table" in
  Arg.parse aligned (usage_function aligned usage_msg) usage_msg;

  begin try
      let inchan = open_in_bin !global_ht_filename in
      the_global_ht := Marshal.from_channel inchan ;
      close_in inchan ;
      debug "%s: loaded\n" !global_ht_filename
    with e ->
      debug "%s: %s\n" !global_ht_filename (Printexc.to_string e)
  end ;

  let server_socket = socket (PF_INET) (SOCK_STREAM) 0 in
  setsockopt server_socket (SO_REUSEADDR) true ;
  set_nonblock server_socket ;
  let server_sockaddr = ADDR_INET(inet_addr_any,!port) in
  bind server_socket server_sockaddr ;
  listen server_socket !listen_max ;

  debug "%s: listening on port %d\n" Sys.argv.(0) !port ;

  let ss_connection = {
    socket = server_socket ;
    state = Server_Socket ;
    read_payload = Buffer.create 1 ;
    write_payload = "" ;
    write_offset = 0,0;
  } in
  event_loop ss_connection ;

end ;;

main () ;;
