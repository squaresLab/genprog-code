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

(*************************************************************************
 * Preprocessing and Expression Numbering 
 *************************************************************************)
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
  let return_averages = Hashtbl.create 255 in 
  Hashtbl.clear output ; 
  List.iter (fun meth ->
    let fundec = Hashtbl.find funs meth in 
    let retvals = ref [] in 
    for trial = 1 to 1000 do
      Hashtbl.clear env ; 
      let args = List.map (fun formal ->
        Const(random_value_of_type formal.vtype)
      ) fundec.sformals in 
      let instr = Call(None,(Lval(Var(fundec.svar),NoOffset)),args,locUnknown) in 
      let retval = try 
        eval_instr instr ;
        debug "compute_average_values: WARNING: did not return\n" ;
        (CReal(0.0,FFloat,None))
      with My_Return(None) -> 
            debug "compute_average_values: WARNING: returned None\n" ; 
            (CReal(0.0,FFloat,None))
         | My_Return(Some(c)) -> c
      in 
      retvals := retval :: !retvals 
    done ;
    let num_retvals = ref 0 in 
    let total = List.fold_left (fun acc elt -> 
      match elt with
      | CInt64(i,_,_) -> incr num_retvals ; (acc +. (Int64.to_float i))
      | CReal(f,_,_) -> incr num_retvals ; (acc +. f)
      | _ -> acc 
    ) 0.0 !retvals in 
    if !num_retvals > 0 then begin
      let avg = total /. (float_of_int !num_retvals) in 
      Hashtbl.replace return_averages meth avg 
    end 
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
  final_averages, return_averages

(*************************************************************************
 * Parsing & Pretty-Printing
 *************************************************************************)
class removeCastsVisitor = object
  inherit nopCilVisitor 
  method vexpr v =
    ChangeDoChildrenPost(v,(fun v ->
      match v with
      | CastE(tau,e) -> e
      | _ -> v
    ))
end 
let my_remove_casts = new removeCastsVisitor

let print_cg ast filename = 
  let fout = open_out filename in
  lineDirectiveStyle := None ; 
  visitCilFileSameGlobals my_remove_casts ast ; 
  iterGlobals ast (fun glob ->
    let loc = get_globalLoc glob in 
    if loc.file = "INTERNAL" || loc.file = "<compiler builtins>" then
      ()
    else match glob with
      | GType(_) -> () (* do not print out typedefs! *) 
      | GFun(fundec,l) -> 
        let new_type = 
          match fundec.svar.vtype with
          | TFun(ret,Some(args),b,a) -> 
            let args = List.map2 (fun formal_va (x,arg_tau,arg_attr) -> 
              match arg_tau with
              | TPtr(tau,attr) -> (x,(TArray(tau,(Some (integer 4)),attr)),arg_attr) 
              | _ ->  (x,arg_tau,arg_attr)
            ) fundec.sformals args in
            TFun(ret,(Some args),b,a) 
          | x -> x 
        in 
        let svar = { fundec.svar with vtype = new_type } in 
        let new_glob = GFun({fundec with svar = svar},l) in 
        dumpGlobal defaultCilPrinter fout new_glob 
      | _ -> 
        dumpGlobal defaultCilPrinter fout glob ;
  ) ; 
  close_out fout ;
  let main_file_string = file_to_string filename in 
  let funattr_regexp = Str.regexp "([ \t\r]*:[ \t\r]+\\([A-Z0-9]+\\)[ \t\r]+\\([A-Za-z0-9_]+\\))\\(([^)]+)\\)"in 
  let main_file_string = Str.global_replace funattr_regexp "\\2 \\3 : \\1 " main_file_string in 
  let fieldattr_regexp = Str.regexp "\\(float[^:]*\\): \\([A-Za-z0-9_]+\\)\\([^;,{]+\\)" in 
  let main_file_string = Str.global_replace fieldattr_regexp "\\1 \\3 : \\2 " main_file_string in 
  let cast_regexp = Str.regexp "(struct f" in 
  let main_file_string = Str.global_replace cast_regexp "(f" main_file_string in 
  let cast_regexp = Str.regexp "(\\([A-Za-z0-9_]+\\)[ \t\r]+:[^)]*)" in 
  let main_file_string = Str.global_replace cast_regexp "(\\1)" main_file_string in 
  let dummy_regexp = Str.regexp ".dummy_field" in 
  let main_file_string = Str.global_replace dummy_regexp "" main_file_string in 
  let woof_array = Str.regexp "(\\([A-Za-z0-9_]+\\))\\[" in 
  let main_file_string = Str.global_replace woof_array "\\1[" main_file_string in 
  let fout = open_out filename in
  Printf.fprintf fout "%s" main_file_string ; 
  close_out fout ; 
  () 

let parse_cg filename = 
  debug "\nparse_cg: %s\n" filename ;
  let main_file_string = file_to_string filename in 
  (* change 
       struct X { } ; 
     to 
       typedef struct X { } X ; 
  *) 
  let typedef_regexp = Str.regexp "struct[ \t\r]+\\([^ \t\r]+\\)\\([^}]+\\)}" in 
  let main_file_string = Str.global_replace typedef_regexp "typedef struct \\1 \\2} \\1" main_file_string in
  let cast_regexp = Str.regexp "\\(float[0-9]\\)(" in 
  let main_file_string = Str.global_replace cast_regexp "\\1_(" main_file_string in 
  let outfile = "temp.c" in (* Filename.temp_file "cg" ".c" in  *)
  let fout = open_out outfile in 
  Printf.fprintf fout "#line 1 \"INTERNAL\" 
 typedef struct float2 {
  float dummy_field;
 } float2; 
 typedef struct float3 {
  float dummy_field;
 } float3; 
 typedef struct float4 {
  float dummy_field;
 } float4;
 typedef struct float4x4 {
  float dummy_field;
 } float4x4;
 typedef struct texobj2D {
  float dummy_field;
 } texobj2D; 
 float4 mul(float4x4 a, float4 b); 
 float3 normalize(float3 b); 
 float dot(float3 a, float3 b); 
 float min(float a, float b); 
 float exp(float a); 
 float pow(float a, float b); 
 float sqrt(float a); 
 float saturate(float a); 
 float3 floor(float3 a); 
 float2 float2_(float a, float b); 
 float3 float3_(float a, float b, float c); 
 float4 tex2D(texobj2D a, float2 b); 

#line 1 \"%s\"\n" filename ;
  Printf.fprintf fout "%s" main_file_string ; 
  close_out fout ; 

  debug "Frontc.parse: begins\n" ; 
  let file = Frontc.parse outfile () in 
  debug "Frontc.parse: ends\n" ; 
  file 

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

class ruleThreeVisitor count desired averages = object
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
        (if Hashtbl.mem averages (!sid,e) then incr count) ;
        match e with
        | BinOp(op,Const(c),e,t) 
        | BinOp(op,e,Const(c),t) when !count = desired ->
          let newval = 
            try Hashtbl.find averages (!sid,e) 
            with _ -> 
              debug "pellacini: no average found for %s on sid %d\n"
                (Pretty.sprint 80 (d_exp () e)) !sid ; 0.0
          in 
          Const(CReal(newval,FFloat,None))
        | _ -> e
      ))
end 
let my_rule_three_visitor = new ruleThreeVisitor

(*************************************************************************
 * Normalization
 *************************************************************************)
class normalizeVisitor method_name delta_retval = object
  inherit nopCilVisitor 
  method vfunc fd = 
    if fd.svar.vname = method_name then
      DoChildren
    else
      SkipChildren
  method vstmt v =
    match v.skind with
    | Return(Some(e),loc) -> 
      let e' = failwith "FIXME" in
      ChangeTo({ v with skind = Return(Some(e'),loc) })
    | _ -> DoChildren
end 
let my_normalize_visitor = new normalizeVisitor

(*************************************************************************
 * Control Loop
 *************************************************************************)

let pellacini_loop original incoming seqno = 
  if is_all_constant incoming then 
    None (* we're done *) 
  else begin
    let methods = [ "FIXME" ] in 
    let variants = ref [] in 
    let incoming_averages, _ = compute_average_values incoming methods in 

    let rule_one_count = ref 0 in 
    visitCilFileSameGlobals (my_rule_one_visitor rule_one_count (-1)) incoming;
    debug "pellacini: #%02d: Rule #1 (BinOp) yields %d possible variants\n"
      seqno !rule_one_count ; 
    for i = 1 to !rule_one_count do
      let count = ref 0 in 
      let newv = copy incoming in 
      visitCilFileSameGlobals (my_rule_one_visitor count i) newv ;
      variants := newv :: !variants 
    done ;
    debug "pellacini: #%02d: Rule #1 variant generation done\n" seqno ;

    let rule_three_count = ref 0 in 
    visitCilFileSameGlobals (my_rule_three_visitor rule_three_count (-1)
      incoming_averages
      ) incoming;
    debug "pellacini: #%02d: Rule #3 (Averages) yields %d possible variants\n"
      seqno !rule_three_count ; 
    for i = 1 to !rule_three_count do
      let count = ref 0 in 
      let newv = copy incoming in 
      visitCilFileSameGlobals (my_rule_three_visitor count i
        incoming_averages) newv ;
      variants := newv :: !variants 
    done ;
    debug "pellacini: #%02d: Rule #3 variant generation done\n" seqno ;

    (* TODO: unique-ify by produced ASM before normalizing *) 

    debug "pellacini: %d variants to normalize\n" 
      (List.length !variants) ; 


    None
  end 


let pellacini (original_filename : string) = 
  let original = parse_cg original_filename in
  let _ = print_cg original "output.cg" in 
  exit 0 ; 
  let stmt_number = ref 1 in 
  let current = ref (copy original) in 
  visitCilFileSameGlobals (my_simple_num_visitor stmt_number) !current ; 
  let finished = ref false in 
  while not !finished do
    match pellacini_loop original !current !stmt_number with
    | Some(next) -> 
      current := next ;
      incr stmt_number 
    | None -> finished := true 

  done 
