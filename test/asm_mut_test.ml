open Global

(* Load mutate and compile an ASM .s file, return 0 on success *)
let main () = begin
  let rep = new Asmrep.asmRep in
    (* load *)
    rep#from_source "gcd.s";
    (* read in fault localization information *)
    (* rep#process_line_or_weight_file "gcd.64.coverage" "line"; *)
    (* mutate *)
    rep#swap 18 20;
    (* write to disk *)
    rep#output_source "temp.s" ;
    (* compile *)
    let compile_success = rep#compile "temp.s" "gcd" in
      if compile_success then exit 0 else exit 1;
end ;;

main () ;;
