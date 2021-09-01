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
open Sys
open Global
(* sourcereader.ml
 *
 * Read a source file, creating an
 * array of strings to represent its
 * lines. Support operations of insertion
 * and deletion of arbitrary strings or
 * lists of strings, to be recognized as
 * lines of code. Keep track of total lines
 * as appropriate, updating this number
 * and using it to make changes to the program.*)

let orig_file = ref ""
let orig_rev = ref ""

let _ =
  options := !options @ [
      "--change-original", Arg.Set_string orig_file, "X Try to automatically apply repairs to original file X";
      "--original-revision", Arg.Set_string orig_rev, "X Set the revision number X of the original revision, for change-original";
    ]

let source_code = ref [] (* Original source code *)
let changed_source_code = ref [] (* Repaired code *)

(* Used to keep track
 * of line location when
 * changes are made (DEPRECATED) *)
let global_line_adjustment = ref 0

(* If it gets triggered, then we should not try to write the
 * repaired file, and let the user know. *)
let bad_flag = ref false

(* The earliest line that
 * is changed. Used to
 * determine if the
 * global adjustment should
 * be used. (DEPRECATED) *)
let pivot_line = ref max_int

(* Keeps track of the relative positions of original source code
 * references. So when we say "insert at line x", we know where x
 * is in our patched code. Need to clarify some things here, namely
 * the way deletions and insertions work. If we delete every line
 * from the last one to the first, the relative position array will
 * declare every modifier to be 0. This could result in out-of-bounds
 * insertions on an empty patch, or something like that. Even with
 * checks the behavior could be weird. Requires more thought. *)
let relative_positions = ref []

(* Keep track of inserts between lines, for poorly done scripts. *)
let inserts_between_lines = ref []

(* Holds the file name of the current file on which we're operating. *)
let global_filename = ref ""

(* Used for multi-file repairs. This should be called between
 * processing scripts. *)
let reset_data () = begin
  relative_positions := [];
  inserts_between_lines := [];
  source_code := [];
  changed_source_code := [];
end

(* source_to_str_list
 * Initialize the source_code list to
 * contain the source code of the file
 * identified by filename, as a list of
 * strings representing lines of code
 * INPUT: Source file name as a string *)
let source_to_str_list filename = begin
  let c = open_in filename in
  try
    while true do
      let next_line = input_line c in
      source_code := next_line :: !source_code;
      changed_source_code := next_line :: !changed_source_code;
      relative_positions := 0 :: !relative_positions;
      inserts_between_lines := 0 :: !inserts_between_lines
    done;
    close_in c;
  with
    End_of_file -> close_in c;
    source_code := (List.rev !source_code);
    changed_source_code :=  (List.rev !changed_source_code);
    inserts_between_lines := (List.tl !inserts_between_lines);
    ()
end

(* print_original_file
 * Prints out the source_code
 * string list, mimicking the
 * source code from the file. *)
let print_file () = begin
  let count = ref 1 in
  List.iter (fun x -> Printf.printf "%d: %s\n" !count x; incr count
            ) !source_code
end

(* print_changed_file
 * Prints out the changed_source_code
 * string list, mimicking the
 * altered source code from the original
 * file. *)
let print_changed_file () = begin
  let count = ref 1 in
  List.iter (fun x -> Printf.printf "%d: %s\n" !count x; incr count
            ) !changed_source_code
end

(* modify_positions
 * Adjusts the relative position buffer
 * based on a starting line number and
 * a modifier (always -1 or 1)
 * INPUT: Line number; all lines after
 * it are modified (int)
 * INPUT: Modifier (always -1 or 1, int) *)
let modify_positions line_number num = begin
  let count = ref 0 in
  relative_positions :=
    List.map(fun x ->
        if (!count)>line_number then (x+num)
        else (incr count; x)
      ) !relative_positions
end

(* modify_inserts
 * Adjust the number of inserts between two lines
 * (really after a line). Always going to do +1
 * but I don't know a better way of just changing
 * the nth position so I'll just do it insanely
 * inefficiently for now.
 * INPUT: Line number; modifies the line number - 1
 * position in the buffer by +1 *)
let modify_inserts line_number = begin
  let count = ref 0 in
  inserts_between_lines :=
    List.map(fun x ->
        if (!count)=(line_number-1) then (incr count; (x+1))
        else (incr count; x)
      ) !inserts_between_lines
end
(* debug_tuple
 * Print out the action tuples. *)
let debug_tuple tup = begin
  List.iter(fun (a,b,c,d) ->
      Printf.printf "%s\n" a;
      Printf.printf "%d\n" b;
      Printf.printf "%d\n" c;
      List.iter(fun s -> Printf.printf "%s\n" s
               ) d
    ) tup
end

(* insert_line_helper
 * Does the actual insertion for insert_line via recursion and pattern matching.
 * INPUT: (always !changed_source_code)
 * INPUT: Line of code to insert as string
 * INPUT: Line number where code is to be inserted *)
let rec insert_line_helper (theList : string list) line line_number = begin
  if  line_number = 1  then line::theList else
    match theList with
      []  -> []
    |   h::t    -> h::( insert_line_helper t line (line_number - 1))
end

(* insert_line
 * Insert a line of code into the modified code.
 * Assumption: We want it inserted at line x where x
 * is a line from the ORIGINAL source code.
 * INPUT: Line of code to insert as string
 * INPUT: Line number where code is to be inserted *)
let insert_line line line_number = begin
  let modifier = List.nth !relative_positions line_number in
  (*
    if (line_number>(!pivot_line)) then (!global_line_adjustment)
    else 0
    in
    (pivot_line :=
    if (line_number<=(!pivotline)) then line_number
    else !pivote_line);
  *)
  changed_source_code :=
    (insert_line_helper !changed_source_code line (line_number + modifier + 1));
  modify_positions line_number 1
end

(* insert_line_list
 * Insert a list of lines into the source code.
 * INPUT: Lines of code to insert as string list
 * INPUT: Line number where code is to be inserted *)
let insert_line_list line_list line_number = begin
  List.iter (fun x -> insert_line x line_number
            ) line_list
end

(* delete_line_helper
 * Does the actual deletion for delete_line via recursion and pattern matching.
 * INPUT: (always !changed_source_code)
 * INPUT: Line number where code is to be deleted *)
let rec delete_line_helper (theList : string list) line_number = begin
  match theList with
    []  -> []
  |   h::t    -> if  line_number = 1 then t
    else h::( delete_line_helper t ( line_number - 1) )
end

(* delete_line
 * Delete a line of code from the modified code.
 * Assumption: We want it deleted at line x where x
 * is a line from the original source file
 * INPUT: line number to delete *)
let delete_line line_number = begin
  let modifier =
    (List.nth !relative_positions line_number) +
    (List.nth !inserts_between_lines line_number) in
  (*
    if (line_number>(!pivot_line)) then (!global_line_adjustment)
    else 0
    in
    (pivot_line :=
    if (line_number<=(!pivot_line)) then line_number
    else !pivot_line);
  *)
  changed_source_code :=
    (delete_line_helper !changed_source_code (line_number + modifier));
  modify_positions line_number (-1) ;
  modify_inserts line_number
end

(* delete_line_list
 * Delete numerous lines of source code.
 * Assumption: The list refers to line numbers from
 * the ORIGINAL source file.
 * INPUT: Line number to start deletion
 * INPUT: Number of lines to delete *)
let delete_line_list line_number total_lines = begin
  for i=line_number to (total_lines+line_number-1) do
    delete_line i
  done
end

(* process_change_action
 * Go through one insert or delete based on an
 * input script (moves will be handled seperately).
 * The script will be a tuple containing an action
 * ("Insert" or "Delete"), a line number referring
 * to the appropriate location in the original source
 * file, a number representing amount of lines to delete
 * (_ for inserts), and a list of strings representing
 * lines to insert (_ for deletes).
 * INPUT: (string, int, int, string list) tuple *)
let process_change_action (action, line, deletes, toInsert) = begin
  match action with
  |  "Insert" ->
    insert_line_list toInsert line;
    ()
  |  "Delete" ->
    delete_line_list line deletes;
    ()
  |   _ -> ()
end

(* process_change_script
 * Go over the list of tuples representing the change
 * script. Pass each one to process_change_action
 * INPUT: List of (string, int, int, string list) *)
let process_change_script changeScript = begin
  List.iter (fun (a,b,c,d) -> process_change_action (a,b,c,d)
            ) changeScript
end

(* derive_change_script
 * Go over a formatted input file and derive
 * the list of changes to make to the source.
 * Return this list of tuples. The format of the
 * input file is:
 *   Action
 *   Line number
 *   # of deletes (0 for insert)
 *   Code to insert (several lines, nonexistent for deletes)
 *   ###
 * etc..
 * INPUT: Filename of file describing changes to be made
 * OUTPUT: (string, int, int, string list) tuple list *)

exception Bad_op

let derive_change_script filename = begin
  let tupleList = ref [] in
  let currentLine = ref "" in
  let c = open_in filename in
  try
    while true do
      let insertList = ref [] in
      currentLine := input_line c;
      if (!currentLine)="FLAGGED! BAD NODE OPERATION!" then raise Bad_op;
      let action = !currentLine in
      currentLine := input_line c;
      let line_number = !currentLine in
      let line_numberR = int_of_string line_number in
      currentLine := input_line c;
      let delete_number = !currentLine in
      let delete_numberR = int_of_string delete_number in
      currentLine := input_line c;
      (* Printf.printf "%s\n" !currentLine; *)
      while !currentLine<>"###" do
        (* Printf.printf "%s\n" !currentLine; *)
        insertList := !currentLine :: !insertList;
        currentLine := input_line c
      done;
      insertList := List.rev !insertList;
      tupleList :=
        (action, line_numberR, delete_numberR, !insertList) :: !tupleList
    done;
    close_in c;
    List.rev !tupleList
  with
    End_of_file -> close_in c; List.rev !tupleList
  |  Bad_op -> close_in c;
    Printf.printf "Bad node operation. Repair cannot be automatically applied.\n";
    bad_flag := true;
    List.rev !tupleList

end

(* write_file
 * writes the changed file to a new one
 * with the same name/extension, but with ".repaired" *)
let write_file filename = begin
  let base = Filename.chop_extension filename in
  let ext = String.sub filename ((String.length base)+1)
      ((String.length filename) - ((String.length base)+1))
  in
  let output_name = "Change_Original/"^base^".repaired."^ext in
  ensure_directories_exist output_name;
  let oc = open_out output_name in
  List.iter(fun x ->
      Printf.fprintf oc "%s\n" x) !changed_source_code;
  close_out oc
end

(* repair_files
 * Takes as input a list of files, representing the original
 * files of a given repair. For each file in the list, get the
 * change tuple, and process those changes. Make sure to reset
 * the data between files. Possibly could just use this as a
 * wrapper for sourcereader, even for single file repairs - just
 * a one element list. NOTE: This list should be created in diffprocessor
 * as it creates the scripts.
 * INPUT: List of strings (script file names [look-original.script, etc.]) *)
let repair_files script_list = begin
  List.iter (fun script_name ->
      reset_data ();
      let filename = Filename.chop_extension script_name in
      source_to_str_list (filename^".c"^"-"^(!orig_rev));
      let the_tuple_script = derive_change_script ("Change_Original/"^script_name) in
      process_change_script the_tuple_script;
      write_file (filename^".c");
    ) script_list
end
;;
