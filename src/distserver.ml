(*
 *
 * Copyright (c) 2012-2013, 
 *  Wes Weimer          <weimer@cs.virginia.edu>
 *  Stephanie Forrest   <forrest@cs.unm.edu>
 *  Claire Le Goues     <legoues@cs.virginia.edu>
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
    "--tweet", Arg.Set tweet, " set up to tweet"
  ] in
  let aligned = Arg.align options in 
  let usage_msg = "Program repair prototype -- Distributed GA server" in 
    Arg.parse aligned (usage_function aligned usage_msg) usage_msg;

    let client_tbl = Hashtbl.create !num_comps in
    let info_tbl = Hashtbl.create !num_comps in
    (* Adds all client computers to client_tbl (and set info_tbl to its
       default)*)
    let rec getcomps sock =
      for currcomp=0 to (!num_comps-1) do
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
              Printf.printf "Computer %d:\n" comp;
              if found then begin
                Printf.printf "\tRepair found!\n";
                Printf.printf "\tGeneration:%d\n" gens;
                repair_found := comp :: !repair_found
              end
              else
                Printf.printf "\tNo repair found.\n";
              Printf.printf "\tTotal test suite evals:%d\n" total_evals;
              Printf.printf "\tTotal bytes received:%d\n\n" bytes_read;
          ) info_tbl;
        Printf.printf "Total bytes sent as messages: %d\n" !total_bytes;

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
          let do_tweet msg = 
            let cmd = Printf.sprintf "echo \"%s\" > tweet.txt" msg in
              (try ignore(Unix.system cmd) with e -> ());
                   Printf.printf "tweeting: %s\n" msg;
                     Pervasives.flush Pervasives.stdout
(*              let cmd = Printf.sprintf "perl ttyer.pl -noratelimit -noprompt -slowpost 10 -script tweet.txt" in
                try ignore(Unix.system cmd) with e -> ()*)
          in
(*          Printf.printf "Buffer = %s\n" buffer;
          Pervasives.flush Pervasives.stdout;*)
          (match String.sub buffer 0 1 with
          | "X" -> ()
          | "T" when !tweet -> 
            if (String.sub buffer 1 1) = "R" then begin
              let num,repair = Scanf.sscanf buffer "TR %d %s@\n" (fun num var -> (num,var)) in
              let msg = Printf.sprintf "#%d found a repair: %s" num repair in
              let msg = 
                if (String.length msg) > 140 then (String.sub msg 0 137)^"..."
                else msg
              in
                do_tweet msg
            end else begin
              let num,var = Scanf.sscanf buffer "T %d%s@\n" (fun num var -> (num,var)) in
             let split = Str.split (Str.regexp_string ".") var in
             let vars = 
               lmap (fun str ->
                 let edits = Str.split whitespace_regexp str in 
                 let fitness = int_of_string (List.hd edits) in 
                 let edits = List.tl edits in
                 let first_edit = List.hd edits in
                 let char_first_edit = first_edit.[0] in
                 let nums_first_edit = String.sub first_edit 2 ((String.length first_edit) - 3 ) in
                 let start_str = Printf.sprintf "%c(%s" char_first_edit nums_first_edit in
                 let compressed,_ =
                   lfoldl (fun (edits,curr_edit) var ->
                     let edit = var.[0] in 
                     let nums = String.sub var 2 ((String.length var) - 3) in
                       if edit == curr_edit then
                         Printf.sprintf "%s;%s" edits nums, edit
                       else 
                         Printf.sprintf "%s)%c(%s" edits edit nums, edit
                   ) (start_str,char_first_edit) (List.tl edits)
                 in
                 let compressed = compressed^")" in
                   Printf.sprintf ("[%s:%d] ") compressed fitness)
                 split
             in
             let msg = Printf.sprintf "#%d:" num in
             let msgs,last_msg =
               lfoldl
                 (fun (msgs,curr_msg) variant ->
                   let var_length = String.length variant in 
                   let current_msg_len = String.length curr_msg in 
                     if var_length + current_msg_len <= 140 then
                       msgs,curr_msg^variant
                     else 
                       curr_msg::msgs,Printf.sprintf "#%d:%s" num variant)
                 ([],msg) vars
             in
             let vars_to_tweet = lrev (last_msg :: msgs) in
               liter do_tweet vars_to_tweet 
            end
          | _ -> bool := false);
          if not !bool then begin
            let split = (Str.split space_regexp buffer) in
              match (List.hd split) with
                "T" -> ()
              | _ ->
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
                  | _ -> failwith 
                    (Printf.sprintf "Unexpected buffer in process_stats: %s\n" buffer)
                in
                let split = List.tl split in 
                let bytes_read,split = 
                  my_int_of_string (List.hd split), List.tl split in
                let evals_done,split = 
                  my_int_of_string (List.hd split), List.tl split in
                let gens = my_int_of_string (List.hd split) in
                  hrep info_tbl comp (found_repair,bytes_read,evals_done,gens)
          end
            
        in

        let rec inform_neighbours () =
          let rec lstmaker curr =
            if curr < !num_comps then
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
          let last = List.nth lst (!num_comps-1) in
            sender lst last
              
        in
          (* Let's spin until we find a client who's done *)
          while !bool do
            let msglist = spin !socketlist [] in
              liter process_stats msglist;
              if !bool then
                inform_neighbours()
          done
end ;;

main () ;;
