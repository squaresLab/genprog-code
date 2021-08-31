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
(** The Network module implements the client for a distributed GA search, which
    runs a preset number of GA generations (potentially with a reduced search
    space) and then communicates with other agents doing the same thing to
    combine partial answers.  The distributed server (a separate utility in this
    directory; primarily implemented in distserver.ml) is required to coordinate
    the distributed GA clients.  *)

open Global
open Distglobal
open Unix
open Population
open Search

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
      "--hostname", Arg.Set_string hostname, "X server ip" ;

      "--sport", Arg.Set_int server_port, "X server port" ;

      "--port", Arg.Set_int my_port, "X my port"  ;

      "--num-comps", Arg.Set_int num_comps,
      "X Distributed: Number of computers to simulate" ;

      "--diversity-selection", Arg.Set_int diversity_selection,
      "X Distributed: Use diversity for exchange";

      "--variants-exchanged", Arg.Set_int variants_exchanged,
      "X Distributed: Number of variants to send" ;

      "--gen-per-exchange", Arg.Set_int gen_per_exchange,
      "X Distributed: Number of generations between exchanges" ;

      (* CLG FIXME: is split search ever different from num_comps? *)
      "--split-search", Arg.Set_int split_search,
      "X Distributed: Split up the search space" ;

    ]

exception Server_shutdown

(** {b message_parse} original_variant message parses messages recieved from
    other clients and converts them into variants.  The message must be
    formatted such that variants are separated by a period '.' and mutations by
    a space ' '. Returns the constructed variants and the number of bytes
    required for the message (in theory). *)
let message_parse (orig : ('a,'b) Rep.representation) (msg : string)
  : ('a,'b) Rep.representation list * int =
  let totbytes = ref 0 in
  (* Splits the message into a list of history lists *)
  let varlst = lmap (fun str ->
      Str.split (Str.regexp_string " ") str
    ) (Str.split (Str.regexp_string ".") msg)
  in
  (* Turns said list into a list of variants *)
  let retlist =
    lmap
      (fun history ->
         let rep = orig#copy() in
         let fitness = float_of_string (List.hd history) in
         liter
           (fun hist ->
              match hist.[0] with
              | 'd' ->
                let num =
                  int_of_string (String.sub hist 2 ((String.index hist ')')-2))
                in
                totbytes := 4 + !totbytes;
                rep#delete num
              | 'a' ->
                let tmp = String.index hist ',' in
                let num1 = int_of_string (String.sub hist 2 (tmp-2)) in
                let num2 = int_of_string
                    (String.sub hist (tmp+1) ((String.index hist ')')-tmp-1))
                in
                totbytes := 6 + !totbytes;
                rep#append num1 num2
              | 's' ->
                let tmp = String.index hist ',' in
                let num1 = int_of_string (String.sub hist 2 (tmp-2)) in
                let num2 =
                  int_of_string
                    (String.sub hist (tmp+1) ((String.index hist ')')-tmp-1))
                in
                totbytes := 6 + !totbytes;
                rep#swap num1 num2
              | 'r' ->
                let tmp = String.index hist ',' in
                let num1 = int_of_string (String.sub hist 2 (tmp-2)) in
                let num2 =
                  int_of_string
                    (String.sub hist (tmp+1) ((String.index hist ')')-tmp-1))
                in
                totbytes := 6 + !totbytes;
                rep#replace num1 num2
              |  _  -> failwith "unknown edit operation for distributed algorithm"
           ) (List.tl history);
         rep#set_fitness fitness;
         rep
      ) varlst
  in
  retlist,!totbytes

(** make_message variant_list converts a population to a string message to send
    to other clients participating in the GA search; this message is parsed by
    message_parse at the other end.  Assumes that all variants know their
    fitnesses. *)
let make_message (lst : ('a,'b) GPPopulation.t) =
  let all_histories =
    lmap
      (fun (rep : ('a,'b) Rep.representation) ->
         let strs = lmap (rep#history_element_to_str) (rep#get_history()) in
         String.concat " " strs,get_opt (rep#fitness())) lst in
  String.concat "."
    (lmap (fun (ele,fit) -> Printf.sprintf "%g %s" fit ele) all_histories)


(** {b choose_by_diversity} selects a subset of variants based on diversity
    metrics instead of just fitness, if the diversity-selection flag is set *)
(* FIXME: it is not clear to CLG that the behavior differs based on that
   diversity-selection flag *)
let choose_by_diversity (orig : ('a,'b) Rep.representation)
    (lst : ('a,'b) GPPopulation.t) : ('a,'b) GPPopulation.t =

  let history_string_list rep : string list =
    let history_list = rep#get_history () in
    lmap (rep#history_element_to_str) history_list
  in
  let histlist =
    lmap
      (fun (ele : ('a,'b) Rep.representation) ->
         (ele,get_opt (ele#fitness())),
         (history_string_list ele)
      ) lst
  in

  let variants =
    lmap (fun (ele,history) ->
        ele,
        lfoldl
          (fun ele_set ->
             fun hist ->
               StringSet.add hist ele_set)
          (StringSet.empty) history
      ) histlist
  in

  (* Add them all to a master set *)
  let all_edits =
    lfoldl
      (fun allset ->
         fun (_,oneset) ->
           StringSet.union allset oneset)
      (StringSet.empty) variants
  in
  (* Look at which variant has the most changes different from other chosen
     variants *)
  let rec collect_variants (all_edits) (variants) (sofar) : ('a,'b) GPPopulation.t =
    (* assumes that !variants_exchanged <= List.length *)
    if sofar = !variants_exchanged then []
    else begin
      let sorted =
        lsort (fun (_,_,a) (_,_,b) -> compare b a)
          (lmap
             (fun (ele,history) ->
                let intersection = StringSet.inter history all_edits in
                ele,intersection,StringSet.cardinal intersection)
             variants)
      in
      let (a,b),changeset,card = List.hd sorted in
      if card > 0 then
        a ::  (collect_variants
                 (StringSet.diff all_edits changeset)
                 (lmap (fun (a,b,_) -> a,b) (List.tl sorted))
                 (sofar + 1))
      else
        (* If there are no non-taken, non-original variants left, we just
           make the rest of them originals *)
        let fit = float_of_int !pos_tests in
        lmap (fun _ ->
            debug "Variant: %s\n" (orig#name ());
            orig#set_fitness fit;
            orig#copy()
          ) (1 -- (!variants_exchanged - sofar))
    end
  in
  collect_variants all_edits variants 0

(** {b get_exchange} original_variant population selects a portion of population
    to exchange with another distributed GA client *)
(* this assumes that the reps know their fitnesses! *)
let get_exchange (orig : ('a,'b) Rep.representation) (lst : ('a,'b) GPPopulation.t)
  : ('a,'b) GPPopulation.t =
  match !diversity_selection with
    1 -> choose_by_diversity orig (random_order lst)
  | 2 -> let lst = List.sort
             (fun (i : ('a,'b) Rep.representation) (i' : ('a,'b) Rep.representation) ->
                let f = get_opt (i#fitness()) in
                let f' = get_opt (i'#fitness()) in
                compare f' f) lst
    in
    choose_by_diversity orig lst
  | _ -> first_nth (random_order lst) !variants_exchanged

(** {b distributed_client} original_variant incoming_population acts as one
    client in a distributed GA search.  Communicates with a server that
    coordinates all such clients.  Does not return *)
(* this can fail if the network calls do or if the client receives a corrupted
   or improperly-formatted message from a neighbor or the server *)
let distributed_client rep incoming_pop =
  let client_tbl = Hashtbl.create (!num_comps+3) in
  let totbytes = ref 0 in
  let my_comp = ref 0 in
  let found_repair = ref false in
  let repair = ref "" in

  let client_exit_fun () =
    (* at exit, send statistics to the server *)
    let final_stat_msg =
      if !found_repair then "Repair_Found" else "No_Repair_Found"
    in
    let bytes_read = Printf.sprintf "%d" !totbytes in
    let gens = match !Search.success_info with
      | [] -> 0
      | hd :: tl -> hd.generation
    in
    let test_suite_evals =
      (Rep.num_test_evals_ignore_cache ())/(!pos_tests + !neg_tests)
    in
    let test_suite_evals = Printf.sprintf "%d" test_suite_evals in
    let str =
      Printf.sprintf "%d %s %s %s %d"
        !my_comp final_stat_msg bytes_read test_suite_evals gens
    in
    fullsend server_socket str;
    try
      close server_socket
    with _ -> ()
  in

  (* Setting up client socket *)
  let main_socket = socket PF_INET SOCK_STREAM 0 in
  setsockopt main_socket (SO_REUSEADDR) true ;
  bind main_socket  (ADDR_INET (inet_addr_any, !my_port));
  listen main_socket (!num_comps+5);

  (* Connecting to server *)
  let server_address = inet_addr_of_string !hostname in
  debug "distclient: connecting to server %s:%d\n" !hostname !server_port;
  connect_to_sock server_socket (ADDR_INET(server_address,!server_port));
  my_comp := my_int_of_string (fullread server_socket);
  debug "distclient%d: this is client %d\n" !my_comp !my_comp ;
  let server_num_comps = my_int_of_string (fullread server_socket ) in
  assert(server_num_comps = !num_comps) ;
  fullsend server_socket (Printf.sprintf "%d" !my_port);

  (* Populates the client_tbl with the keys being the computer number and the value being their sockaddr *)
  for i=1 to !num_comps do
    let strlist = Str.split (Str.regexp " ") (fullread server_socket) in
    let client_num = my_int_of_string (List.hd strlist) in
    let addr_inet =
      ADDR_INET(inet_addr_of_string (List.nth strlist 1),
                my_int_of_string (List.nth strlist 2))
    in
    Hashtbl.add client_tbl client_num addr_inet
  done;

  let exchange_variants str =
    debug "distclient%d: about to exchange variants (%S), waiting on server\n"
      !my_comp str ;
    let buffer = fullread server_socket in
    debug "distclient%d: server instruction: %S\n" !my_comp buffer ;
    let sendto = match buffer with
      | "Exit" -> debug "\n\nServer has ordered termination\n\n";
        raise (Server_shutdown)
      | a -> my_int_of_string a
    in
    (* FIXME: CLG is a little confused by this; I know we need to
       alternate the order in which we send and receive, but it looks like
       this will domino/be unecessarily slow.  Why is only one computer
       receiving and then sending while everyone else sends and then
       receives?  *)
    let do_send () =
      debug "distclient%d: sending to %d\n" !my_comp sendto ;
      let newsock = socket PF_INET SOCK_STREAM 0 in
      connect_to_sock newsock (Hashtbl.find client_tbl sendto);
      fullsend newsock str;
      close newsock ;
      debug "distclient%d: sending to %d done\n" !my_comp sendto ;
    in
    let do_receive () =
      debug "distclient%d: receiving\n" !my_comp ;
      let sock,_ = accept main_socket in
      let tempstr = fullread sock in
      close sock ;
      debug "distclient%d: received %S\n" !my_comp tempstr ;
      debug "distclient%d: receiving done\n" !my_comp ;
      message_parse rep tempstr
    in
    if !my_comp = 0 then begin
      do_send () ;
      do_receive () ;
    end else begin
      let result = do_receive () in
      do_send () ;
      result
    end
  in

  let rec all_iterations generations (population : ('a,'b) GPPopulation.t) =
    try
      if generations <= !Search.generations then begin
        let num_to_run =
          if (!Search.generations + 1 - generations) > !gen_per_exchange
          then !gen_per_exchange
          else !Search.generations - generations
        in
        debug "distclient%d: running GA for %d generations\n"
          !my_comp num_to_run ;
        let population = Search.run_ga ~start_gen:generations
            ~num_gens:num_to_run population rep
        in
        debug "distclient%d: have resulting population from GA\n" !my_comp ;

        if num_to_run <> (!Search.generations - generations) then begin
          debug "distclient%d: creating message for population exchange\n" !my_comp ;
          let msgpop = make_message (get_exchange rep population) in
          fullsend server_socket (Printf.sprintf "%d Ready" !my_comp) ;
          let from_neighbor,bytes = exchange_variants msgpop in
          totbytes := bytes + !totbytes;
          let population = population @ from_neighbor in
          all_iterations (generations + !gen_per_exchange) population
        end else begin
          debug "distclient%d: no exchange at end of search\n" !my_comp ;
        end
      end
    with | Found_repair(rep) -> (found_repair := true; repair := rep)
         | Server_shutdown -> ()
  in

  let mut_ids = rep#get_faulty_atoms () in
  (* split the search space if specified *)
  let splitting_function x length comp =
    if comp < !num_comps-1 then
      (x >= length*comp / !num_comps) &&
      (x < length*(comp+2) / !num_comps)
    else
      (x >= length*comp / !num_comps) ||
      (x < length / !num_comps)
  in
  let reduce_func (x, prob) =
    match !split_search with
      1 -> (x mod !num_comps) = !my_comp
    | 2 -> (x mod !num_comps) = !my_comp || prob = 1.0
    | 3 when !num_comps > 2 ->
      let len = llen mut_ids in
      prob = 1.0 || (splitting_function x len !my_comp)
    | _ -> true
  in

  at_exit client_exit_fun;
  (* fixme: length of mut_ids might be wrong based on promut *)
  rep#reduce_search_space reduce_func false;
  all_iterations 1 (Search.initialize_ga rep incoming_pop)
