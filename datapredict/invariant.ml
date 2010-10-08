open Cil
open Pretty
open Utils
open Globals

type predicate = 
	BranchTrue of exp
  | BranchFalse of exp
  | CilExp of exp 
  | ReturnVal of exp 
  | Executed
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
	  let estr = Pretty.sprint 80 (d_exp () e) in
		Printf.sprintf "CilExp of %s" estr 
  | ReturnVal(e) -> 
	  let estr = Pretty.sprint 80 (d_exp () e) in
		Printf.sprintf "ReturnVal of %s" estr
  | RunFailed -> "Run Failed"
  | RunSucceeded -> "Run Succeeded"
  | Undefined -> "Undefined"
  | BranchFalse(e) -> 
	  let exp_str = Pretty.sprint 80 (d_exp () e) in
		Printf.sprintf "Branch condition false: %s" exp_str
  | BranchTrue(e) ->
	  let exp_str = Pretty.sprint 80 (d_exp () e) in
		Printf.sprintf "Branch condition true: %s" exp_str
  | Executed -> "Statement executed"

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
  | BranchTrue(e) -> BranchFalse(e)
  | BranchFalse(e) -> BranchTrue(e)
  | Executed -> failwith "Not implemented executed"
