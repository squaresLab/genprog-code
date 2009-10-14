open Cil 
open Printf
open Utils
open Gp
open Bf

(*sect: main*)
let main() = (
  port := 800 + (R.int 800);
  let chrome_orig = super_init() in

  if !use_alg = alg_GP then gp_start chrome_orig
  else if !use_alg = alg_SIMPL then simpl_start chrome_orig
  else if !use_alg = alg_BF || !use_alg = alg_BF_w_del then 
	bf_start chrome_orig
  else (debug "unknown algorithm input\n";	exit 1);
  
  continue:=false; print_results () ;
);;

main();;
