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

module Make (Ord : Map.OrderedType) =
struct

  module OrdMap = Map.Make(Ord)

  type key = Ord.t

  type 'a t = Trie of 'a t OrdMap.t * 'a option

  let empty = Trie(OrdMap.empty, None)

(*
let debug m =
  let rec helper indent index (Trie(map, value)) =
    begin match value with
    | Some(value) -> Printf.printf "%s[%s] (%s)\n" indent index value
    | None        -> Printf.printf "%s[%s]\n" indent index
    end;
    OrdMap.iter (fun i m ->
      helper (indent ^ "  ") (string_of_int i) m
    ) map
  in
  helper "" "" m
*)

  (**/**)
  let rec unzip ?(create=false) crumbs x m =
    match x, m with
    | hd::tl, Trie(map, value) ->
      let child =
        try
          (OrdMap.find) hd map
        with Not_found ->
          if create then empty else raise Not_found
      in
      unzip ~create ((hd, map, value)::crumbs) tl child
    | [], Trie(map, value) ->
      crumbs, map, value

  let rec zip crumbs map value =
    if (value = None) && ((OrdMap.is_empty) map) then
      match crumbs with
      | (hd, map', value')::crumbs ->
        zip crumbs (OrdMap.remove hd map') value'
      | [] -> empty
    else
      let child = Trie(map, value) in
      match crumbs with
      | (hd, map', value')::crumbs ->
        zip crumbs ((OrdMap.add) hd child map') value'
      | [] -> child
  (**/**)

  let mem x m =
    try
      match unzip [] x m with
      | _, _, Some(_) -> true
      | _, _, None    -> false
    with Not_found -> false

  let find x m =
    match unzip [] x m with
    | _, _, Some(value) -> value
    | _, _, None -> raise Not_found

  let add x y m =
    let crumbs, map, value = unzip ~create:true [] x m in
    zip crumbs map (Some(y))

  let remove x m =
    try
      let crumbs, map, value = unzip [] x m in
      zip crumbs map None
    with Not_found -> m

  let singleton x y = add x y empty

  let fold f m a =
    let rec helper path m a =
      match m with
      | Trie(map, value) ->
        let a =
          match value with
          | Some(value) -> f (List.rev path) value a
          | None -> a
        in
        OrdMap.fold (fun i m a -> helper (i::path) m a) map a
    in
    helper [] m a

  let iter f m = fold (fun k v _ -> f k v) m ()

  let size m = fold (fun _ _ n -> n + 1) m 0

  let find_prefix x m =
    let _, map, value = unzip [] x m in
    List.rev (fold (fun k v lst -> (k, v) :: lst) m [])

  let contains_prefix_of x m =
    let rec helper crumbs list map value =
      match list with
        l :: ls ->
        begin
          try
            let crumbs', map', value = unzip crumbs [l] map in
            helper crumbs' ls (Trie(map', value)) value
          with Not_found ->
            crumbs, list, map, value
        end
      | [] ->
        crumbs, list, map, value
    in
    let crumbs, _, _, value = helper [] x m None in
    match value with
    | Some(_) ->
      List.fold_left (fun acc (key, _, _) ->
          key :: acc
        ) [] crumbs
    | None ->
      []
end
