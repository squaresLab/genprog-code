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
(** [Distserver] implements the server for the distributed GA search algorithm.
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
    "--sport", Arg.Set_int server_port, "X server port" ;
  ] in
  let aligned = Arg.align options in
  let usage_msg = "Program repair prototype -- Distributed GA server" in
  Arg.parse aligned (usage_function aligned usage_msg) usage_msg;
  let debug_str = Printf.sprintf "repair.debug.distserver" in
  debug_out := open_out debug_str ;

  assert(!num_comps >= 2);

  let client_tbl = Hashtbl.create !num_comps in
  let info_tbl = Hashtbl.create !num_comps in
  (* Adds all client computers to client_tbl (and set info_tbl to its
     default)*)
  let getcomps sock =
    for currcomp=0 to (!num_comps-1) do
      debug "Awaiting connection from client %d of %d\n"
        currcomp (!num_comps-1) ;
      let sock,address = accept sock  in
      let addr = match address with
        | ADDR_INET(addr,port) -> (string_of_inet_addr addr)
        | _ -> failwith("Client did not have an inet_addr")
      in
      let str = Printf.sprintf "%d" currcomp in
      fullsend sock str;
      let str = Printf.sprintf "%d" !num_comps in
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
  debug "All clients have connected\n" ;

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
  let ready_clients = ref 0 in
  let exited_clients = ref 0 in

  (* Let's spin until we find a client who's done *)
  while true do
    debug "Waiting for client communication, %d/%d clients ready to exchange\n"        !ready_clients !num_comps ;
    let msglist = spin !socketlist [] in

    let process_client_communication buffer =
      debug "Client: %S\n" buffer ;
      let words = Str.split whitespace_regexp buffer in
      match words with
      | from :: "Ready" :: rest -> incr ready_clients
      | from :: "Repair_Found" :: rest ->
        exit 0
      | from :: "No_Repair_Found" :: rest -> incr exited_clients
      | _ -> failwith "unknown message in client-server protocol"
    in

    liter process_client_communication msglist;

    if !exited_clients >= !num_comps then begin
      debug "All clients have exited; terminating.\n" ;
      exit 0
    end else if !ready_clients >= !num_comps then begin
      ready_clients := 0 ;
      debug "Instructing clients to exchange variants.\n" ;
      let counting = 0--(!num_comps - 1) in
      let permutation = random_order counting in
      let rec sender list last =
        match list with
        | [] -> ()
        | hd :: tl ->
          let sock,_,_ = Hashtbl.find client_tbl last in
          debug "\tinstructing %d to send to %d\n" last hd ;
          fullsend sock (Printf.sprintf "%d" hd);
          sender tl hd
      in
      debug "Exchange order: " ;
      liter (fun x -> debug "%d -> " x) permutation ;
      debug "\n" ;
      let last = List.nth permutation (!num_comps-1) in
      sender permutation last
    end
  done
end ;;

main () ;;
