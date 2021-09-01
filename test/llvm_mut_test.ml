(* Load mutate and compile an ASM .s file, return 0 on success *)
let main () = begin
  let rep = new Llvmrep.llvmRep in
    (* load *)
    rep#from_source "gcd.ll";
    (* mutate *)
    rep#replace 29 51;
    (* write to disk *)
    rep#output_source "temp.ll" ;
    (* compile *)
    let compile_success = rep#compile "temp.ll" "gcd" in
      if compile_success then exit 0 else exit 1;
end ;;

main () ;;
