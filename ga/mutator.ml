(* 
 * Produce many random mutant variants of a given C program. Initially
 * created for research with John Regehr. 
 *
 * Uses CABS instead of CIL so that compiler bugs that really depend on the
 * ABS can be examined. 
 *)
open Cabs
open Cabsvisit

(* This makes a deep copy of an arbitrary Ocaml data structure *) 
let copy (x : 'a) = 
  let str = Marshal.to_string x [] in
  (Marshal.from_string str 0 : 'a) 
  (* Cil.copyFunction does not preserve stmt ids! Don't use it! *) 

let exp_ht = Hashtbl.create 255 
let exp_count = ref 0 

class noteExpVisitor = object
  inherit nopCabsVisitor
  method vexpr e = 
    let i = !exp_count in
    incr exp_count ;
    Hashtbl.add exp_ht i (copy e) ;
    DoChildren
end 

class mutExpVisitor (to_swap) 
                    (exp_count : int ref) = object
  inherit nopCabsVisitor
  method vexpr e = 
    let i = !exp_count in
    incr exp_count ;
        if Hashtbl.mem to_swap i then begin
          let j = Hashtbl.find to_swap i in
          let new_exp = copy (Hashtbl.find exp_ht j) in 
          let sub_count = ref !exp_count in 
          let subVisitor = new mutExpVisitor to_swap sub_count in  
          ChangeTo(visitCabsExpression subVisitor new_exp) 
        end else DoChildren
end 

let main () = begin
  let usageMsg = "Prototype C Program Mutator\n" in 
  let filenames = ref [] in 
  let num_mutations = ref 1 in 
  let num_variants = ref 10 in 
  Random.self_init () ; 
  let seed = ref (Random.bits ()) in  

  let argDescr = [
    "--seed", Arg.Set_int seed, "X use X as random seed";
    "--mut", Arg.Set_int num_mutations, "X use X expression mutations per variant";
    "--num", Arg.Set_int num_variants, "X create X variants";
  ] in 
  let handleArg str = filenames := str :: !filenames in 
  Arg.parse (Arg.align argDescr) handleArg usageMsg ; 

  Cil.initCIL () ; 
  Random.init !seed ; 
  List.iter (fun arg -> 
    begin
      Whitetrack.enabled := true ; 
      let cabs_file, _ = Frontc.parse_with_cabs arg () in 
      Printf.printf "%s parsed\n" arg ; 

      let note_visitor = new noteExpVisitor in 
      let cabs_file = visitCabsFile note_visitor cabs_file in 
      Printf.printf "%s: %d expressions\n" arg !exp_count ; 

      for v = 1 to !num_variants do

        let mut_ht = Hashtbl.create 255 in 
        for m = 1 to !num_mutations do
          let src = Random.int !exp_count in 
          let dst = Random.int !exp_count in 
          Hashtbl.replace mut_ht src dst
        done ; 
        let local_exp_count = ref 0 in
        let mut_visitor = new mutExpVisitor mut_ht local_exp_count in 
        let cabs_file = visitCabsFile mut_visitor cabs_file in 

        let cabs_out = 
          Printf.sprintf "%s-s%d-m%02d-v%04d.c" arg !seed !num_mutations v 
        in 
        let fout = open_out cabs_out in 
        Whitetrack.setOutput fout ; 
        Cprint.printFile fout cabs_file ; 
        Printf.printf "%s mutated and written to %s\n" arg cabs_out ; 
        close_out fout ; 
      done 
    end 
  ) !filenames ; 

end ;;

main () ;;
