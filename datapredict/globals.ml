open Cil

module OrderedString =
struct
  type t = string
  let compare = compare
end

module StringMap = Map.Make(OrderedString)

(* we copy all debugging output to a file and to stdout *)
let debug_out = ref stdout 
let debug fmt = 
  let k result = begin
    output_string !debug_out result ; 
    output_string stdout result ; 
    flush stdout ; 
  end in
  Printf.kprintf k fmt 

(* split "filename.dat" into ["filename";"dat"] *) 
let split_ext name =
  try 
    let base = Filename.chop_extension name in
    let ext = String.sub name ((String.length base)+1)
      ((String.length name) - ((String.length base)+1))
    in 
    base,ext
  with _ -> name,""

(* This makes a deep copy of an arbitrary Ocaml data structure *) 
let copy (x : 'a) = 
  let str = Marshal.to_string x [] in
  (Marshal.from_string str 0 : 'a) 
  (* Cil.copyFunction does not preserve stmt ids! Don't use it! *) 

let space_regexp = Str.regexp "[ \t]+" 

(* actual program-specific global types and variables *)

(* this naming scheme is probably a really crap idea because the two words
   actually mean the same thing, but I'm using one to be just like "truth-valued
   statement" and one to be "Slightly more general" *)

type predicate = CilExp of exp | ReturnVal of exp
type invariant = General of predicate | Specific of predicate * location | RunFailed

type rank = { (* sum or record? *)
  val f_P : float;
  val s_P : float;
  val failure_P : float;
  val context : float;
  val increase : float;
  val f_P_obs : float;
  val s_P_obs : float;
  val numF : int;
  val importance : float;
}

type memV = Int of int | Float of float 
let mval_of_string str = 
  try (Float(float_of_string str)) with _ -> (Int(int_of_string str))



