(* Load and compile an ASM .s file, return 0 on success *)
let main () = begin
  let rep = new Llvmrep.llvmRep in
    rep#from_source "gcd.ll";
    rep#output_source "temp.ll";
    let compile_success = rep#compile "temp.ll" "gcd" in
      if compile_success then exit 0 else exit 1;
end ;;

main () ;;
