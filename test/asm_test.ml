open Global

(* Load and compile an ASM .s file, return 0 on success *)
let main () = begin
  let rep = (new Asmrep.asmRep) in
    rep#from_source("gcd.s");
    let compile_success = rep#compile "gcd.s" "gcd" in
      if compile_success then exit 0 else exit 1;
end ;;

main () ;;
