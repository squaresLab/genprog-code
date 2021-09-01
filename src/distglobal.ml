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
(** [distglobal] provides network options/utilities for the distributed GA server
    and client implementations.  Because we compile the server separately, we
    separate these utilities out into this module to avoid repetitiveness *)
open Global
open Unix

(** number of participating nodes *)
let num_comps = ref 2

(** port for the server *)
let server_port = ref 65000
let server_socket = socket PF_INET SOCK_STREAM 0

exception Send_Failed

(** @param socket socket on which to read; should be ready
    @param message_size integer, number of bytes to read
    @return bytes read as a string
    @raise Abort if any system calls fail; usually such failures will be
    caught/handled gracefully by the calling function, as readall is typically
    called from something higher-level. *)
let readall sock size =
  let count = ref 0 in
  let buffer = Bytes.create (size+1) in
  let rec _readall accum =
    let currcount = recv sock buffer 0 size [] in
    count := currcount + !count;
    if !count <> size then
      if currcount = 0 then begin
        sleep 5;
        _readall accum
      end
      else
        _readall ((String.sub (Bytes.to_string buffer) 0 currcount)::accum)
    else
      ((String.sub (Bytes.to_string buffer) 0 currcount)::accum)
  in
  String.concat "" (List.rev (_readall []))

(** Performs a basic Unix.send with some small error checking.  If the send
    fails in any way, prints the error gracefully but does not abort.

    @param socket on which to send
    @param message string message to send
    @param start integer, which byte to start send from
    @param num_bytes number of bytes to send, starting from start
    @param flags flags to pass to Unix.send  *)
let my_send sock str num1 num2 msglst =
  try
    let x = send sock str num1 num2 msglst in
    if x <> (num2-num1) then
      raise Send_Failed
  with e ->
    debug "Error in send: %s\n" (Printexc.to_string e)

(** our send protocol sends the size of the message in number of bytes first (in
    a 4 byte message) and then the full message.  This function reads the number
    of bytes before dispatching to [readall] to read the rest of the message.

    @param socket from which to read
    @return message read from the socket as a string.
*)
let fullread sock =
  let size = my_int_of_string (readall sock 4) in
  readall sock size

(** Our send protocol sends the size of the message in number of bytes first (in
    a 4 byte message) and then the full message.  This function prepends the
    length of the message to the message and then dispatches to [my_send] to
    send the message.

    @param socket socket on which to send
    @param message string message to send on socket *)
let fullsend sock str =
  let len = String.length(str) in
  let newstr = Printf.sprintf "%4d%s" len str in
  my_send sock (Bytes.of_string newstr) 0 (len+4) []

(** Performs a select on the socketlist, reads all available data from those
    that return, and spins until all sockets in socketlist have been read from
    successfully.  It will catch any exception thrown by the helper/reading
    functions and print an error message, but not abort.

    @param socketlist list of sockets on which we are waiting to read.
    @param accum result list of strings as read from the sockets
    @return accum the result list of strings as read from the sockets
*)
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


(** loops until it successfully connects to socket.  This function can
    potentially loop infinitely.

    @param socket socket to which we'd like to connect
    @param addr address to bind to socket *)
let rec connect_to_sock sock addr =
  try
    connect sock addr;
  with _ ->
    (* Neal: I used print_endline here instead of debug so the
       repair.debug file doesn't balloon up to astronomical size. *)
    print_endline "Failed to connect to socket. Retrying in 5 seconds.";
    sleep 5;
    connect_to_sock sock addr
