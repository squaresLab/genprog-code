(*
 * Fabio Pellacini's "User-Configurable Automatic Shader Simplification"
 *
 *
 * Goal: Generate a sequence of increasingly-simple shaders 
 * Input: 
 *  Shader S
 *  Set of [a,b] ranges for all input parameters 
 *
 * Preprocess: (not done here!) 
 *  Duplicate any sub-procedures P defined in S so that each
 *  callsite c_i to P becomes a call to a fresh P_i 
 *
 * Given shader S, to compute next shader in sequence
 *  1. If all expressions in S are constants then EXIT 
 *  2. Determine average values for all expressions e in S 
 *    simulate the shader in software 
 *    use 1000 random trials 
 *    over user-provided input parameter ranges
 *    (cf. Monte Carlo integration)
 *  3. Produce set A = { S' : S' = one_edit(S) } 
 *  4. Produce set B = map normalize A 
 *    where normalize(S') is: 
 *      determine average values for all expressions e in 
 *      replace "return x" in S' with
 *              "return x - average(S',x) + average(S,x)" 
 *  5. For each S' in B, calculate Error(S') 
 *    convert S' to shader source code, run it on video hardware
 *    using only ONE VALUE for each parameter
 *  6. Let N = S' in B with smallest Error(S') 
 *  7. if error(N) <= error(S) then EXIT 
 *     else output N as next shader in sequence
 *)
open Cil
open Global

let funs = Hashtbl.create 255 

class simpleNumVisitor count = object
  inherit nopCilVisitor 
  method vstmt v =
    ChangeDoChildrenPost(v,(fun v ->
      v.sid <- !count;
      incr count ;
      v 
    ))
  method vfunc f = 
    Hashtbl.add funs f.svar.vname f ;
    DoChildren
end 
let my_simple_num_visitor = new simpleNumVisitor

class allConstantVisitor = object
  inherit nopCilVisitor 
  method vexpr e = match e with
    | UnOp _ 
    | BinOp _ 
    -> failwith "found" 
    | _ -> DoChildren
end
let my_all_constant = new allConstantVisitor

let is_all_constant file = 
  try
    visitCilFileSameGlobals my_all_constant file ; 
    true
  with _ -> false 

(*************************************************************************
 * Symbolic Execution
 *************************************************************************)

let env = Hashtbl.create 255 
let output = Hashtbl.create 255 
let output_values = Hashtbl.create 255 
let sid = ref 0 

let update_env lv res = 
  match lv with
  | Var(va),x -> Hashtbl.replace env lv res 
  | _ -> failwith "updatE_env" 

let get_from_env lv = 
  match lv with
  | Var(va),x -> begin
    try Hashtbl.find env lv 
    with _ -> 
      debug "get_from_env: %s not found" va.vname ;
      failwith "get_from_env" 
  end 
  | _ -> failwith "get_from_env" 

let note_value e res = 
  Hashtbl.replace output_values (!sid,e) () ;
  Hashtbl.add output (!sid,e) res 

exception My_Break 
exception My_Continue 
exception My_Return of constant option 

let rec eval_block b = 
  List.iter (fun stmt -> eval_stmt stmt ) b.bstmts 

and ee e = 
  let res = 
    match e with 
    | Const(c) -> c
    | Lval(lv) -> get_from_env lv 
    | UnOp(Neg,e,_) -> begin match ee e with
      | CInt64(i,k,_) -> CInt64(Int64.neg i,k,None) 
      | CReal(f,k,_) -> CReal(0.0 -. f,k,None) 
      | _ -> failwith "unop neg" 
      end 
    | UnOp(LNot,e,_) -> begin match ee e with
      | CInt64(i,k,_) -> 
        let n = if i = 0L then 1L else 0L in 
        CInt64(n,k,None) 
      | CReal(f,k,_) -> 
        let n = if f = 0. then 1L else 0L in 
        CInt64(n,IInt,None) 
      | _ -> failwith "unop lnot" 
      end 

    | BinOp(PlusA,e1,e2,_) -> begin match ee e1, ee e2 with
      | CInt64(i,k,_), CInt64(j,_,_) -> CInt64(Int64.add i j,k,None)
      | CReal(i,k,_), CReal(j,_,_) -> CReal(i +. j,k,None) 
      | _ -> failwith "binop " 
      end 

    | _ -> failwith "ee" 
  in
  note_value e res ;
  res 

and eval_instr i = match i with
  | Set(lv,exp,_) ->
    let res = ee exp in 
    update_env lv res 
  | Call(lvopt,(Lval(Var(va),NoOffset)),args,_) -> begin
      try 
        let fname = va.vname in 
        let fundec = Hashtbl.find funs fname in  
        assert(List.length args = List.length fundec.sformals); 
        let arg_vals = List.map (fun arg -> ee arg) args in 
        List.iter2 (fun formal actual ->
          update_env ((Var(formal),NoOffset)) actual 
        ) fundec.sformals arg_vals ;
        eval_block fundec.sbody 
      with My_Return (ropt) -> begin
        match lvopt, ropt with
        | Some(lv), Some(v) -> update_env lv v 
        | None, None -> () 
        | _, _ -> failwith "call assignment" 
      end 
    end 
  | _ -> failwith "eval_instr" 

and eval_stmt s = 
  sid := s.sid ; 
  match s.skind with
  | Instr(il) -> List.iter eval_instr il 
  | Return(Some(e),_) -> 
    let c = ee e in 
    raise (My_Return (Some(c)))
  | If(e,b1,b2,_) -> ()
  | Break _  -> raise My_Break 
  | Continue _ -> raise My_Continue
  | Loop(b,_,_,_) -> 
    let finished = ref false in
    while not !finished do
      try
        eval_block b 
      with My_Break -> finished := true
         | My_Continue -> () 
    done 
  | Block(b) -> eval_block b 
  | Return(None,_) -> raise (My_Return (None))
  | _ -> failwith "eval_stmt" 

let rec random_value_of_type tau = match tau with
  | TFloat(k,_) -> CReal(Random.float 1.0,k,None) 
  | TInt(k,_) -> CInt64((if Random.bool () then 1L else 0L),k,None) 
  | _ -> failwith "random_value_of_type" 

let compute_average_values ast methods = 
  let final_averages = Hashtbl.create 255 in 
  Hashtbl.clear output ; 
  Hashtbl.clear output_values ; 
  List.iter (fun meth ->
    let fundec = Hashtbl.find funs meth in 
    for trial = 1 to 1000 do
      Hashtbl.clear env ; 
      let args = List.map (fun formal ->
        Const(random_value_of_type formal.vtype)
      ) fundec.sformals in 
      let instr = Call(None,(Lval(Var(fundec.svar),NoOffset)),args,locUnknown) in 
      eval_instr instr ;
      () 
    done ;
  ) methods ; 
  Hashtbl.iter (fun (expr) _ ->
    let all_observed = Hashtbl.find_all output expr in 
    let num_observed = ref 0 in 
    let total = List.fold_left (fun acc elt -> 
      match elt with
      | CInt64(i,_,_) -> incr num_observed ; (acc +. (Int64.to_float i))
      | CReal(f,_,_) -> incr num_observed ; (acc +. f)
      | _ -> acc 
    ) 0.0 all_observed in 
    if !num_observed > 0 then begin
      let avg = total /. (float_of_int !num_observed) in 
      Hashtbl.replace final_averages expr avg 
    end 
  ) output_values ; 
  final_averages

(*************************************************************************
 * Mutation
 *************************************************************************)
class ruleOneVisitor count desired = object
  inherit nopCilVisitor 
  method vstmt v =
    sid := v.sid ; 
    ChangeDoChildrenPost(v,
      (fun v ->
        v 
      ))
  method vexpr e = 
    ChangeDoChildrenPost(e,
      (fun e ->
        incr count ;
        match e with
        | BinOp(op,Const(c),e,t) 
        | BinOp(op,e,Const(c),t) when !count = desired ->
          e 
        | _ -> e
      ))
end 
let my_rule_one_visitor = new ruleOneVisitor

(*************************************************************************
 * Control Loop
 *************************************************************************)

let pellacini_loop original seqno = 
  if is_all_constant original then 
    None (* we're done *) 
  else begin
    let methods = [ "FIXME" ] in 
    let original_averages = compute_average_values original methods in 
    None
  end 

let pellacini (original : Cil.file) = 
  let stmt_number = ref 1 in 
  let current = ref (original) in 
  visitCilFileSameGlobals (my_simple_num_visitor stmt_number) !current ; 
  let finished = ref false in 
  while not !finished do
    match pellacini_loop !current !stmt_number with
    | Some(next) -> 
      current := next ;
      incr stmt_number 
    | None -> finished := true 

  done 
