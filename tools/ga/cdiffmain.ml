(*
 * Structural Diff on C Programs
 *
 * --generate: given two C files, produce a data file and a text patch file
 *   that can be used to turn one into the other
 *
 * --use: given the data file and some subset of the text file, apply that
 *   subset of the changes to turn the first file into (something like) the
 *   second file 
 *
 * Used by Weimer's prototype GP project to post-mortem minimize a 
 * candidate patch. Typically used in conjunction with delta-debugging 
 * to produce a 1-minimal subset of the original patch that still has the
 * desired behavior. 
 *)
open Pretty
open Printf
open Cil
open Cdiff


let main () = begin
  Cil.initCIL () ; 

  let filename = ref [] in 
  let generate = ref "" in 
  let use = ref "" in 
  let usageMsg = "Prototype Difference Minimizer\n" in 

  let argDescr = [
    "--generate", Arg.Set_string generate, "X output minimization data file to X";
    "--use", Arg.Set_string use, "X use minimization data file X";
  ] in 
  let handleArg str = 
    filename := str :: !filename 
  in 
  Arg.parse (Arg.align argDescr) handleArg usageMsg ; 
  match !filename with 
  | two :: one :: rest when !generate <> "" -> begin

    (* Cil.printCilAsIs := true ; *)
    let f1 = Frontc.parse one () in 
    visitCilFileSameGlobals my_num f1 ; 

    let f2 = Frontc.parse two () in 
    visitCilFileSameGlobals my_num f2 ; 

    let diffname = !generate ^ ".diff" in 
    let diff_out = open_out diffname in 
    let data_out = open_out_bin !generate in 


    gendiff f1 f2 diff_out data_out ;
    close_out diff_out ; 
    close_out data_out 

  end 

  | diff :: rest when !use <> "" -> begin

    (* Cil.printCilAsIs := true; *)
    let data_in = open_in_bin (!use) in 
    let diff_in = open_in diff in 

    let file_out = open_out "cdiff-output.c" in 

    usediff diff_in data_in file_out ;

  end

  | _ -> begin

  end

end ;;
main () ;;
