open Cil
open Pretty
open Globals

(* FIXME: ReturnVal is making less and less sense, but what can you
 * do? *)
type predicate = CilExp of exp
		 | ReturnVal of exp 
		 | RunFailed
		 | RunSucceeded 
		 | Undefined

let rec pred_vars pred =
  let rec lhost_vars lhost = 
    match lhost with
    | Var(vi) -> [vi.vname]
    | Mem(e) -> exp_vars e
  and off_vars offset = 
    match offset with
      Field(fi, off) -> off_vars off
    | Index(e, off) -> (exp_vars e) @ (off_vars off)
    | _ -> []
  and exp_vars exp = 
    match exp with 
      Lval(lhost,off)
    | AddrOf(lhost,off)
    | StartOf(lhost,off) -> (lhost_vars lhost) @ (off_vars off)
    | CastE(_, e)
    | SizeOfE(e)
    | AlignOfE(e) -> exp_vars e
    | UnOp(u, e, t) -> exp_vars e
    | BinOp(b, e1, e2, t) -> (exp_vars e1) @ (exp_vars e2)
    | _ -> []
  in
    match pred with
      CilExp(exp) -> exp_vars exp
    | _ -> []

let d_pred p =
  match p with
    CilExp(e) -> 
      let exp_str = Pretty.sprint 80 (d_exp () e) in
	pprintf "cilexp: %s\n" exp_str;
	flush stdout
  | ReturnVal(e) -> pprintf "returnval\n"; flush stdout
  | RunFailed -> pprintf "Run Failed\n"; flush stdout
  | RunSucceeded -> pprintf "Run Succeeded\n"; flush stdout
  | Undefined -> pprintf "Undefined\n"; flush stdout

let opposite pred =
  match pred with 
    CilExp(e) -> 
	  begin
		match e with
		| BinOp(Gt, lvar, rvar, typ) -> CilExp(BinOp(Le, lvar, rvar, typ))
		| BinOp(Lt, lvar, rvar, typ) -> CilExp(BinOp(Ge, lvar, rvar, typ))
		| BinOp(Eq, lvar, rvar, typ) -> CilExp(BinOp(Ne, lvar, rvar, typ))
		| _ -> failwith "Unexpected expression in CilExp predicate" 
	  end
  | ReturnVal(e) -> failwith "Not implemented three"
  | RunFailed -> RunSucceeded
  | RunSucceeded -> RunFailed
  | Undefined -> Undefined
