(*
 * Simple script for performing mutation.
 *
 * Usage: mutation program.c "mutation string"
 *   where "mutation string" is along the lines of "d(45)" or some such
 *
 * Results saved in m_program.c
 *
 *)
open Printf
open Cil
open Global
IFDEF ELF THEN
open Elf
END

let abort fmt = begin Printf.printf fmt; exit 1 end
let mutate program mut (rep : 'c Rep.representation) = begin
  let ios = my_int_of_string in
    rep#from_source program;
    List.iter
      (fun m ->
         let ms = Str.global_replace (Str.regexp "[^asd0-9]") " " m in
         let ml = Str.split (Str.regexp_string " ") ms in
           match List.nth ml 0 with
             | "a"   -> rep#append (ios (List.nth ml 1)) (ios (List.nth ml 2))
             | "s"   -> rep#swap   (ios (List.nth ml 1)) (ios (List.nth ml 2))
             | "d"   -> rep#delete (ios (List.nth ml 1))
             | other -> abort "invalid mutation string\n")
      (Str.split (Str.regexp_string " ") mut);
    rep#output_source ("m_"^program);
end

let prog = try ref Sys.argv.(1) with
    e -> abort "Missing first argument (program to repair).\n" in
let mut  = try ref Sys.argv.(2) with
    e -> abort "Missing second argument (mutation to perform).\n" in

let base, filetype = split_ext !prog in
  Global.extension := filetype;
  match String.lowercase filetype with
    | "c"   | "i" -> mutate !prog !mut ((new Cilrep.cilRep) :> 'a Rep.representation);
    | "asm" | "s" -> mutate !prog !mut ((new Asmrep.asmRep) :> 'b Rep.representation);
    | "exe" | ""  ->
        IFDEF ELF THEN
          mutate !prog !mut ((new Elfrep.elfRep) :> 'b Rep.representation)
        END
    | other -> abort "%s: unknown file type";
