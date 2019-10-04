(*
 *
 * Copyright (c) 2012-2018,
 *  Wes Weimer          <weimerw@umich.edu>
 *  Stephanie Forrest   <steph@asu.edu>
 *  Claire Le Goues     <clegoues@cs.cmu.edu>
 *  Eric Schulte        <eschulte@cs.unm.edu>
 *  Jeremy Lacomis      <jlacomis@cmu.edu>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. The names of the contributors may not be used to endorse or promote
 * products derived from this software without specific prior written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *)
(** Fabio Pellacini's "User-Configurable Automatic Shader Simplification".  CLG
    knows virtually nothing about this module so she didn't touch it very much
    in March 2012 *)
(*
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
open Rep
open Global

let pellacini_method_name = ref ""


let _ =
  options := !options @
             [
               "--pellacini-method", Arg.Set_string pellacini_method_name, "X operate on Fragment shader method X" ;
             ]

(*************************************************************************
                                                                          * Preprocessing and Expression Numbering
 *************************************************************************)
let funs = Hashtbl.create 255
let get_fun fname =
  try
    Hashtbl.find funs fname
  with e ->
    debug "ERROR: pellacini: function %s not found\n" fname ;
    failwith "get_fun"

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
                                                                          * Symbolic Math
 *************************************************************************)
let array_ify e1 =
  match e1 with
  | CReal(i,_,_) -> CFloatArray([| i |])
  | CInt64(i,_,_) -> CFloatArray([| Int64.to_float i |])
  | CFloatArray(x) -> e1
  | _ -> failwith "array_ify"

let real_ify e1 =
  match e1 with
  | CReal(i,_,_) -> i
  | CInt64(i,_,_) -> Int64.to_float i
  | CFloatArray(a) -> a.(0)
  | _ ->
    ignore (Pretty.printf "%a\n" d_const e1) ;
    failwith "real_ify"

let rec binop iop fop e1 e2 =
  match e1, e2 with
  | CReal(i,k,_), CReal(j,_,_) -> CReal(fop i j,k,None)
  | CInt64(i,k,_), CInt64(j,_,_) -> CInt64(iop i j,k,None)

  | CInt64(i,_,_), CReal(j,k,_) ->
    let i = Int64.to_float i in
    CReal(fop i j,k,None)

  | CReal(i,k,_), CInt64(j,_,_) ->
    let j = Int64.to_float j in
    CReal(fop i j,k,None)

  | CFloatArray(a), CFloatArray(b) ->
    (*    assert(Array.length a = Array.length b); *)
    let lA = Array.length a in
    let lB = Array.length b in
    if lA == lB then
      CFloatArray(Array.init (Array.length b)
                    (fun i -> fop a.(i) b.(i)))
    else
      (*  let _ = debug "LEN: %d %d\n" lA lB in *)
      CFloatArray(Array.init lA
                    (fun i -> fop a.(i) b.(0)))


  | CFloatArray(a), CInt64(j,_,_) ->
    let alen = Array.length a in
    let jarr = Array.init alen (fun i -> Int64.to_float j) in
    binop iop fop e1 (CFloatArray(jarr))

  | CInt64(j,_,_),CFloatArray(a) ->
    let alen = Array.length a in
    let jarr = Array.init alen (fun i -> Int64.to_float j) in
    binop iop fop (CFloatArray(jarr)) e2

  | CFloatArray(a), CReal(j,_,_) ->
    let alen = Array.length a in
    let jarr = Array.init alen (fun i -> j) in
    binop iop fop e1 (CFloatArray(jarr))

  | CReal(j,_,_),CFloatArray(a) ->
    let alen = Array.length a in
    let jarr = Array.init alen (fun i -> j) in
    binop iop fop (CFloatArray(jarr)) e2

  | CStr(x),_
  | _,CStr(x)
    -> CStr(x)

  | _,_ ->
    ignore (Pretty.printf "binop:\n%a\n%a\n" d_const e1 d_const e2) ;
    failwith "binop"

let sumlist lst =
  List.fold_left (fun acc elt ->
      binop (Int64.add) (+.) acc elt
    ) (CInt64(0L,IInt,None)) lst

let average_list lst =
  let len = List.length lst in
  if len <= 0 then
    None
  else begin
    let sum = sumlist lst in
    Some(
      binop (Int64.div) (/.) sum (CInt64(Int64.of_int len,IInt,None))
    )
  end

let floatarray_size_of_ti name =
  match name with
  | "float2" -> 2
  | "float3" -> 3
  | "float4" -> 4
  | x -> failwith ("floatarray_size_of: " ^ x)

let docast tau c = match tau, c with
  | TVoid(_), _ -> c
  | TPtr(_), CFloatArray(a) -> c
  | TPtr(_), CStr(a) -> c
  | TNamed(ti,_), CInt64(j,_,_) ->
    CFloatArray(
      Array.init (floatarray_size_of_ti ti.tname) (fun i -> Int64.to_float j)
    )

  | TNamed(ti,_), CReal(j,_,_) ->
    CFloatArray (Array.init (floatarray_size_of_ti ti.tname) (fun i -> j) )

  | TNamed(ti,_), CFloatArray(a) ->
    let wanted = floatarray_size_of_ti ti.tname in
    let have = Array.length a in
    assert(have >= wanted);
    if have = wanted then
      c
    else
      CFloatArray(Array.sub a 0 wanted)

  | TComp(ci,_), _ when ci.cname = "texobj2D" -> c
  | TComp(ci,_), _ when ci.cname = "texobjCUBE" -> c
  | TComp(ci,_), _ when ci.cname = "texobjCube" -> c



  | TComp(ci,_), CFloatArray(a) ->
    let wanted = floatarray_size_of_ti ci.cname in
    let have = Array.length a in
    if have <= wanted then
      c
    else
      CFloatArray(Array.sub a 0 wanted)

  | TComp(ci,_), CInt64(j,_,_) ->
    let wanted = floatarray_size_of_ti ci.cname in
    CFloatArray(Array.init wanted (fun i -> Int64.to_float j))

  | TInt(k,_), x ->
    let v = real_ify x in
    CInt64(Int64.of_float v,k,None)

  | TComp(ci,_), CReal(j,_,_) ->
    let wanted = floatarray_size_of_ti ci.cname in
    CFloatArray(Array.init wanted (fun i -> j))

  | TFloat(k,_), CInt64(j,_,_) -> CReal(Int64.to_float j,k,None)
  | TFloat(k,_), CReal(j,_,_) -> c


  | TComp(ci,_), _ when ci.cname = "float3x3" -> c
  | TComp(ci,_), _ when ci.cname = "float4x4" -> c

  | TFloat(k,_), CFloatArray(a) ->
    (* this represents a cast inserted by a "parsing error" *)
    c

  | x,y ->
    ignore (Pretty.printf "docast: (%a) %a\n"
              d_plaintype tau
              d_const c) ;
    failwith "docast"

let const_is_zero c = match c with
  | CReal(j,_,_) -> j = 0.
  | CInt64(j,_,_) -> j = Int64.zero
  | CFloatArray(a) -> a = [| 0. |]
  | _ -> failwith "const_is_zero"

(*************************************************************************
                                                                          * Texture Handling
 *************************************************************************)
let tex_ht = Hashtbl.create 255

let read_ppm filename =
  let ic = open_in filename in
  let line = input_line ic in
  if line <> "P6" then invalid_arg "not a P6 ppm file";
  let line = input_line ic in
  let line =
    try if line.[0] = '#'  (* skip comments *)
      then input_line ic
      else line
    with _ -> line
  in
  let width, height =
    Scanf.sscanf line "%d %d" (fun w h -> (w, h))
  in
  let line = input_line ic in
  if line <> "255" then invalid_arg "not a 8 bit depth image";
  let all_channels =
    let kind = Bigarray.int8_unsigned
    and layout = Bigarray.c_layout
    in
    Bigarray.Array3.create kind layout 3 width height
  in
  let r_channel = Bigarray.Array3.slice_left_2 all_channels 0
  and g_channel = Bigarray.Array3.slice_left_2 all_channels 1
  and b_channel = Bigarray.Array3.slice_left_2 all_channels 2
  in
  for y = 0 to pred height do
    for x = 0 to pred width do
      r_channel.{x,y} <- (input_byte ic);
      g_channel.{x,y} <- (input_byte ic);
      b_channel.{x,y} <- (input_byte ic);
    done;
  done;
  close_in ic;
  (width, height,
   all_channels,
   r_channel,
   g_channel,
   b_channel)

let load_texture tname =
  try
    Hashtbl.find tex_ht tname
  with _ -> begin
      try
        let res = read_ppm tname in
        Hashtbl.replace tex_ht tname res;
        res
      with e ->
        abort "%s: %s -- cannot simulate without !\n" tname
          (Printexc.to_string e)
    end


(*************************************************************************
                                                                          * Symbolic Execution
 *************************************************************************)
let is_not_const e =  match e with
  | Const(_) -> false
  | _ -> true


exception My_Break
exception My_Continue
exception My_Return of constant option

let env = Hashtbl.create 255
let output = Hashtbl.create 255
let output_values = Hashtbl.create 255
let sid = ref 0
let the_method = ref ""

let is_xyz_field fi =
  let l = String.length fi.fname in
  let is = ref true in
  for i = 0 to pred l do
    if fi.fname.[i] = 'x' ||
       fi.fname.[i] = 'y' ||
       fi.fname.[i] = 'z' ||
       fi.fname.[i] = 'w' then ()
    else
      is := false
  done ;
  !is


let rec random_value_of_type va tau =
  (*  let _ = ignore (Pretty.printf "VNAME = %s; TYPE = %a\n" va.vname d_type tau) in*)
  let vname = va.vname in
  match tau with
  | TFloat(k,_) -> CReal(Random.float 1.0,k,None)
  | TInt(k,_) -> CInt64((if Random.bool () then 1L else 0L),k,None)
  | TNamed(ti,_) when ti.tname = "float2" ->
    CFloatArray(Array.init 2 (fun i -> Random.float 1.0))
  | TNamed(ti,_) when ti.tname = "float3" ->
    CFloatArray(Array.init 3 (fun i -> Random.float 1.0))
  | TNamed(ti,_) when ti.tname = "float4" ->
    CFloatArray(Array.init 4 (fun i -> Random.float 1.0))
  | TNamed(ti,_) when ti.tname = "texobj2D" ->
    CStr(vname)
  | TNamed(ti,_) when ti.tname = "Texture2D" ->
    CStr(vname)
  | TNamed(ti,_) when ti.tname = "TextureCube" ->
    CStr(vname)
  | TNamed(ti,_) when ti.tname = "texobjCUBE" ->
    CStr(vname)
  | TNamed(ti,_) when ti.tname = "ReflectionInfo" ->
    CStr(vname)
  | TNamed(ti,_) when ti.tname = "float3x3" ->
    for i = 0 to pred 3 do
      let lval = Var(va),Index(integer i,NoOffset) in
      let rval = CFloatArray(Array.init 3 (fun i -> Random.float 1.0)) in
      update_env lval rval
    done ; CStr(vname)
  | TNamed(ti,_) when ti.tname = "float4x4" ->
    for i = 0 to pred 4 do
      let lval = Var(va),Index(integer i,NoOffset) in
      let rval = CFloatArray(Array.init 4 (fun i -> Random.float 1.0)) in
      update_env lval rval
    done ; CStr(vname)
  | TArray(TFloat(_,_),Some(Const CInt64(x,_,_)),_) ->
    for i = 0 to pred (Int64.to_int x) do
      let lval = Var(va),Index(integer i,NoOffset) in
      update_env lval (CReal(Random.float 1.0,FFloat,None))
    done ; CStr(vname)
  | TArray(TNamed(ti,_),Some(Const CInt64(x,_,_)),_) ->
    for i = 0 to pred (Int64.to_int x) do
      let inner_size = floatarray_size_of_ti ti.tname in
      let lval = Var(va),Index(integer i,NoOffset) in
      update_env lval
        (CFloatArray(Array.init (inner_size) (fun i -> Random.float 1.0)))
    done ; CStr(vname)
  | TNamed(_,_) ->
    CStr(vname)
  | _ ->
    ignore (Pretty.printf "VNAME = %s; TYPE = %a\n" vname d_type tau) ;
    failwith "random_value_of_type"

and update_env lv res =
  let str = Pretty.sprint ~width:80 (d_lval () lv) in
  (*
    Pretty.printf "update_env: %s : %s <- %a\n"
    !the_method (str) d_const res ;
  *)
  match lv with
  | Var(va),NoOffset -> Hashtbl.replace env str res
  | Var(va),Field(fi,NoOffset) when is_xyz_field fi -> begin
      let outer_lv = Var(va),NoOffset in
      let outer = get_from_env outer_lv in
      match outer, array_ify res with
      | CFloatArray(olda), CFloatArray(newa) ->
        let len = String.length fi.fname in
        for i = 0 to pred len do
          let xyz = fi.fname.[i] in
          let idx = match xyz with
            | 'x' -> 0
            | 'y' -> 1
            | 'z' -> 2
            |  _  -> 3
          in
          olda.(idx) <- newa.(i)
        done ;
        let res = CFloatArray(olda) in
        let str = Pretty.sprint ~width:80 (d_lval () outer_lv) in
        Hashtbl.replace env str res

      | _ -> failwith "update_env .xyz"
    end

  | Var(va),Field(fi,NoOffset) when fi.fname = "dummy_field" -> begin
      let outer_lv = Var(va),NoOffset in
      let outer = get_from_env outer_lv in
      match outer, array_ify res with
      | CFloatArray(olda), CFloatArray(newa) ->
        for i = 0 to pred (Array.length olda) do
          olda.(i) <- newa.(i mod (Array.length newa))
        done ;
        let res = CFloatArray(olda) in
        let str = Pretty.sprint ~width:80 (d_lval () outer_lv) in
        Hashtbl.replace env str res
      | _ -> failwith "update_env .dummy_field"
    end

  | Var(va),Index(e,off)
  | Mem(BinOp(IndexPI,Lval(Var(va),NoOffset),e,_)),off
    ->
    let idx = ee e in
    let new_vname = Pretty.sprint ~width:80
        (Pretty.dprintf "%s[%a]" va.vname d_const idx) in
    let new_va = {va with vname = new_vname} in
    update_env (Var(new_va),off) res

  | e ->
    ignore (Pretty.printf "update_env: %a <- %a\n" d_plainlval lv
              d_const res ) ;
    failwith "update_env"

and get_from_env ?(warn=true) lv =
  let str = Pretty.sprint ~width:80 (d_lval () lv) in
  let res =
    try begin
      match lv with
      | Var(va),NoOffset -> Hashtbl.find env str
      | Var(va),Field(fi,NoOffset) when is_xyz_field fi -> begin
          let outer = get_from_env (Var(va),NoOffset) in
          match outer with
          | CFloatArray(fa) ->
            let len = String.length fi.fname in
            let arr = Array.init len (fun i ->
                let xyz = fi.fname.[i] in
                match xyz with
                | 'x' -> fa.(0)
                | 'y' -> fa.(1)
                | 'z' -> fa.(2)
                |  _  -> fa.(3)
              ) in
            CFloatArray(arr)

          | _ -> failwith ".xyz on wrong type of thing"
        end

      | Var(va),Index(e,off)
      | Mem(BinOp(IndexPI,Lval(Var(va),NoOffset),e,_)),off
        ->
        let idx = ee e in
        let new_vname = Pretty.sprint ~width:80
            (Pretty.dprintf "%s[%a]" va.vname d_const idx) in
        let new_va = {va with vname = new_vname} in
        get_from_env (Var(new_va),off)

      | _ -> raise Not_found
    end with e ->
      (if warn then debug "get_from_env: %s not found\n" str );
      raise e
  in
  (*
    Pretty.printf "get_from_env: %s %s is %a\n"
    !the_method (str) d_const res ;
  *)
  res

and note_value e res =
  match res with
  | CInt64(_)
  | CReal(_)
  | CFloatArray(_) -> begin
      Hashtbl.replace output_values (!sid,e) () ;
      Hashtbl.add output (!sid,e) res
    end
  | _ -> () (* not useful to cache *)

and eval_block b =
  List.iter (fun stmt -> eval_stmt stmt ) b.bstmts

and ee e =
  let res =
    match e with
    | Const(c) -> c
    | StartOf(lv)
    | Lval(lv) -> get_from_env lv
    | UnOp(Neg,e,_) -> begin match ee e with
        | CInt64(i,k,_) -> CInt64(Int64.neg i,k,None)
        | CReal(f,k,_) -> CReal(0.0 -. f,k,None)
        | CFloatArray(fa) -> CFloatArray(Array.map (fun i -> 0.0 -. i) fa)
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


    | BinOp(PlusA,e1,e2,_) -> binop (Int64.add) (+.) (ee e1) (ee e2)
    | BinOp(MinusA,e1,e2,_) -> binop (Int64.sub) (-.) (ee e1) (ee e2)
    | BinOp(Mult,e1,e2,_) -> binop (Int64.mul) ( *. ) (ee e1) (ee e2)
    | BinOp(Div,e1,e2,_) -> binop (Int64.div) ( /.) (ee e1) (ee e2)
    | BinOp(Mod,e1,e2,_) -> binop (Int64.rem) ( mod_float ) (ee e1) (ee e2)

    | BinOp(Lt,e1,e2,_) -> binop (fun x y -> if x < y then 1L else 0L)
                             (fun x y -> if x < y then 1. else 0.)
                             (ee e1) (ee e2)
    | BinOp(Le,e1,e2,_) -> binop (fun x y -> if x <= y then 1L else 0L)
                             (fun x y -> if x <= y then 1. else 0.)
                             (ee e1) (ee e2)
    | BinOp(Gt,e1,e2,_) -> binop (fun x y -> if x > y then 1L else 0L)
                             (fun x y -> if x > y then 1. else 0.)
                             (ee e1) (ee e2)
    | BinOp(Ge,e1,e2,_) -> binop (fun x y -> if x >= y then 1L else 0L)
                             (fun x y -> if x >= y then 1. else 0.)
                             (ee e1) (ee e2)
    | BinOp(Eq,e1,e2,_) -> binop (fun x y -> if x = y then 1L else 0L)
                             (fun x y -> if x = y then 1. else 0.)
                             (ee e1) (ee e2)
    | BinOp(Ne,e1,e2,_) -> binop (fun x y -> if x <> y then 1L else 0L)
                             (fun x y -> if x <> y then 1. else 0.)
                             (ee e1) (ee e2)

    | CastE(tau,e) -> docast tau (ee e)

    | _ ->
      ignore (Pretty.printf "\n%a\n%a\n" d_exp e d_plainexp e) ;
      failwith "ee"
  in
  if is_not_const e then begin
    (*
      Pretty.printf "ee: %d's %a -> %a\n" !sid d_exp e d_const res ;
    *)
    note_value e res ;
  end ;
  res

and eval_instr ?(raise_retval=false) i =
  try begin
    match i with
    | Set(lv,exp,_) ->
      let res = ee exp in
      update_env lv res
    | Call(lvopt,(Lval(Var(va),NoOffset)),args,_) -> begin
        let old = !the_method in
        let to_restore = ref [] in
        try
          let fname = va.vname in
          let arg_vals = List.map (fun arg -> ee arg) args in
          begin match fname with
            | "normalize" -> begin
                match arg_vals with
                | [CFloatArray(fa)] ->
                  let sum = Array.fold_left (fun acc elt -> acc +. elt) 0.0 fa in
                  let retval = CFloatArray(Array.map (fun i -> i /. sum) fa) in
                  raise (My_Return(Some retval))
                | _ -> failwith "call normalize"
              end

            | "pow" -> begin
                match arg_vals with
                | [CReal(i,k,_); CReal(j,_,_)] ->
                  let retval = CReal((i ** j),FFloat,None) in
                  raise (My_Return(Some (retval)))
                | [CFloatArray(fa); CReal(j,_,_)] ->
                  let retval = CFloatArray(Array.map (fun i -> i ** j) fa) in
                  raise (My_Return(Some retval))
                | _ -> failwith "call pow"
              end

            | "pow3" -> begin
                match arg_vals with
                | [CReal(i,k,_); CReal(j,_,_)] ->
                  let retval = CReal((i ** j),FFloat,None) in
                  raise (My_Return(Some (retval)))
                | [CFloatArray(fa); CReal(j,_,_)] ->
                  let retval = CFloatArray(Array.map (fun i -> i ** j) fa) in
                  raise (My_Return(Some retval))
                | _ -> failwith "call pow"
              end

            | "dot" -> begin
                match arg_vals with
                | [CFloatArray(fa); CFloatArray(fb)] ->
                  assert(Array.length fa = Array.length fb);
                  let sofar = ref 0.0 in
                  for i = 0 to pred (Array.length fa) do
                    sofar := !sofar +. (fa.(i) *. fb.(i))
                  done ;
                  let retval = CReal(!sofar,FFloat,None) in
                  raise (My_Return(Some retval))
                | _ -> failwith "call dot"
              end
            | "mul3x3" -> begin
                (*
             Weimer notes: Given that CG files seem to use

             float3x3 mTan;
             float3 vBumpNormal;
             float3 wBumpNormal = mul3x3 ( mTan, vBumpNormal ) ;

             ... one can infer that mul3x3 is standard matrix
             vector multiply.
          *)
                match arg_vals with
                | [ CStr(matrix_name) ; CFloatArray(vec) ] -> begin
                    let va = makeVarinfo false matrix_name voidType in
                    let lval x = Var(va),Index(integer x,NoOffset) in
                    let row_0 = get_from_env (lval 0) in
                    let row_1 = get_from_env (lval 1) in
                    let row_2 = get_from_env (lval 2) in
                    match row_0, row_1, row_2 with
                    | CFloatArray(r0), CFloatArray(r1), CFloatArray(r2) ->
                      let out0 = (r0.(0) *. vec.(0)) +.
                                 (r1.(0) *. vec.(1)) +.
                                 (r2.(0) *. vec.(2)) in
                      let out1 = (r0.(1) *. vec.(0)) +.
                                 (r1.(1) *. vec.(1)) +.
                                 (r2.(1) *. vec.(2)) in
                      let out2 = (r0.(2) *. vec.(0)) +.
                                 (r1.(2) *. vec.(1)) +.
                                 (r2.(2) *. vec.(2)) in
                      let retval = CFloatArray([| out0 ; out1 ; out2 |]) in
                      raise (My_Return(Some retval))

                    | _ -> failwith
                             (Printf.sprintf "call mul3x3 -- %s is not a float3x3 ?"
                                matrix_name)
                  end
                | _ -> failwith "call mul3x3"
              end
            | "mul" -> begin
                (*
             Weimer notes: Given that CG files seem to use

             float3x3 mTan;
             float3 vBumpNormal;
             float3 wBumpNormal = mul3x3 ( mTan, vBumpNormal ) ;

             ... one can infer that mul3x3 is standard matrix
             vector multiply.
          *)
                match arg_vals with
                | [ CStr(matrix_name) ; CFloatArray(vec) ] -> begin
                    let va = makeVarinfo false matrix_name voidType in
                    let lval x = Var(va),Index(integer x,NoOffset) in
                    let row_0 = get_from_env (lval 0) in
                    let row_1 = get_from_env (lval 1) in
                    let row_2 = get_from_env (lval 2) in
                    match row_0, row_1, row_2 with
                    | CFloatArray(r0), CFloatArray(r1), CFloatArray(r2) ->
                      let out0 = (r0.(0) *. vec.(0)) +.
                                 (r1.(0) *. vec.(1)) +.
                                 (r2.(0) *. vec.(2)) in
                      let out1 = (r0.(1) *. vec.(0)) +.
                                 (r1.(1) *. vec.(1)) +.
                                 (r2.(1) *. vec.(2)) in
                      let out2 = (r0.(2) *. vec.(0)) +.
                                 (r1.(2) *. vec.(1)) +.
                                 (r2.(2) *. vec.(2)) in
                      let retval = CFloatArray([| out0 ; out1 ; out2 |]) in
                      raise (My_Return(Some retval))

                    | _ -> failwith
                             (Printf.sprintf "call mul3x3 -- %s is not a float3x3 ?"
                                matrix_name)
                  end
                | _ -> failwith "call mul3x3"
              end


            | "float2_" -> begin
                match arg_vals with
                | [a;b] ->
                  let a = real_ify a in
                  let b = real_ify b in
                  raise (My_Return(Some(CFloatArray( [|a;b|] ))))
                | _ -> failwith "float2_"
              end

            | "float3_" -> begin
                match arg_vals with
                | [a;b;c] ->
                  let a = real_ify a in
                  let b = real_ify b in
                  let c = real_ify c in
                  raise (My_Return(Some(CFloatArray( [|a;b;c|] ))))
                | _ -> failwith "float3_"
              end
            | "float4_" -> begin
                match arg_vals with
                | [a;b;c;d] ->
                  let a = real_ify a in
                  let b = real_ify b in
                  let c = real_ify c in
                  let d = real_ify d in
                  raise (My_Return(Some(CFloatArray( [|a;b;c;d|] ))))
                | _ -> failwith "float4_"
              end
            | "cos" -> begin
                match arg_vals with
                | [CReal(i,k,_)] -> raise (My_Return(Some(CReal(cos i,k,None))))
                | [CFloatArray(fa)] -> raise (My_Return(Some(
                    CFloatArray(Array.map (fun i -> cos i) fa))))
                | _ -> failwith "cos"
              end
            | "acos" -> begin
                match arg_vals with
                | [CReal(i,k,_)] -> raise (My_Return(Some(CReal(acos i,k,None))))
                | [CFloatArray(fa)] -> raise (My_Return(Some(
                    CFloatArray(Array.map (fun i -> acos i) fa))))
                | _ -> failwith "acos"
              end
            | "sqrt" -> begin
                match arg_vals with
                | [CReal(i,k,_)] -> raise (My_Return(Some(CReal(sqrt i,k,None))))
                | _ -> failwith "sqrt"
              end
            | "floor" -> begin
                match arg_vals with
                | [CReal(i,k,_)] -> raise (My_Return(Some(CReal(floor i,k,None))))
                | [CFloatArray(fa)] -> raise (My_Return(Some(
                    CFloatArray(Array.map (fun i -> floor i) fa))))
                | _ -> failwith "floor"
              end
            | "saturate" -> begin
                match arg_vals with
                | [CReal(i,k,_)] ->
                  raise (My_Return(Some(CReal(clamp 0. i 1.,k,None))))
                | _ -> failwith "saturate"
              end
            | "exp" -> begin
                match arg_vals with
                | [CReal(i,k,_)] ->
                  raise (My_Return(Some(CReal(exp i,k,None))))
                | _ -> failwith "exp"
              end

            | "min" -> begin
                match arg_vals with
                | [a;b] -> raise (My_Return(Some(binop min min a b )))
                | _ -> failwith "call min"
              end
            (*  | "max3" -> begin
                match arg_vals with
                | [CFloatArray(fa); CFloatArray(fb)] ->
                assert(Array.length fa = Array.length fb);
                let sofar = ref 0.0 in
                for i = 0 to pred (Array.length fa) do
                sofar := !sofar +. (fa.(i) *. fb.(i))
                done ;
                let retval = CReal(!sofar,FFloat,None) in
                raise (My_Return(Some CFloatArray(Array.map (fun i -> max    ))
                | _ -> failwith "call max"
                end*)
            | "max" -> begin
                match arg_vals with
                | [a;b] -> raise (My_Return(Some(binop max max a b)))
                | _ -> failwith "call max"
              end
            | "tex2D" -> begin
                match arg_vals with
                | [CStr(image);CFloatArray(ra)] ->
                  let w,h,all,r,g,b = load_texture (image ^ ".ppm") in
                  let x = int_of_float ra.(0) in
                  let y = int_of_float ra.(1) in
                  let x = clamp 0 x (pred w) in
                  let y = clamp 0 y (pred h) in

                  let a1 = r.{x,y} in
                  let a2 = g.{x,y} in
                  let a3 = b.{x,y} in
                  let a4 = (a1+a2+a3)/3 in
                  let map x = (float_of_int x) /. 256.0 in
                  raise (My_Return(Some(CFloatArray(
                      [| map a1 ;  map a2 ; map a3 ; map a4 |] ))))

                | _ -> failwith "call tex2d"
              end
            | "texCUBE" -> begin
                match arg_vals with
                | [CStr(image);CFloatArray(ra)] ->
                  let w,h,all,r,g,b = load_texture (image ^ ".ppm") in
                  let x = int_of_float ra.(0) in
                  let y = int_of_float ra.(1) in
                  let x = clamp 0 x (pred w) in
                  let y = clamp 0 y (pred h) in

                  let a1 = r.{x,y} in
                  let a2 = g.{x,y} in
                  let a3 = b.{x,y} in
                  let a4 = (a1+a2+a3)/3 in
                  let map x = (float_of_int x) /. 256.0 in
                  raise (My_Return(Some(CFloatArray(
                      [| map a1 ;  map a2 ; map a3 ; map a4 |] ))))
                | _ -> failwith "call texCUBE"
              end
            | _ -> let fundec = get_fun fname in
              assert(List.length args = List.length fundec.sformals);
              (*         ignore (Pretty.printf "FNAME = %s\n" fname );*)
              List.iter2 (fun formal actual ->
                  (try
                     let x = get_from_env ~warn:false ((Var(formal),NoOffset)) in
                     to_restore := (x,formal) :: !to_restore
                   with _ -> ()) ;
                  update_env ((Var(formal),NoOffset)) actual
                ) fundec.sformals arg_vals ;
              List.iter (fun local ->
                  let actual = random_value_of_type local local.vtype in
                  (try
                     let x = get_from_env ~warn:false ((Var(local),NoOffset)) in
                     to_restore := (x,local) :: !to_restore
                   with _ -> ()) ;
                  update_env ((Var(local),NoOffset)) actual
                ) fundec.slocals ;
              the_method := fundec.svar.vname ;
              let result = eval_block fundec.sbody in
              the_method := old ;
              List.iter (fun (v,name) ->
                  update_env ((Var(name),NoOffset)) v
                ) !to_restore ;
              result
          end
        with My_Return (ropt) -> begin
            the_method := old ;
            List.iter (fun (v,name) ->
                update_env ((Var(name),NoOffset)) v
              ) !to_restore ;
            match lvopt, ropt with
            | Some(lv), Some(v) -> update_env lv v
            | None, None -> ()
            | _ when raise_retval -> raise (My_Return(ropt))
            | _ ->
              ignore (Pretty.printf "%a\n" d_instr i) ;
              failwith "call + assignment mismatch"
          end
      end
    | _ -> failwith "eval_instr"
  end with e -> begin
      match e with
      | My_Return(_)
      | My_Break | My_Continue -> raise e
      | _ ->
        (ignore (Pretty.printf "eval_instr: %a\n" d_instr i)) ;
        raise e
    end

and eval_stmt s =
  sid := s.sid ;
  match s.skind with
  | Instr(il) -> List.iter eval_instr il
  | Return(Some(e),_) ->
    let c = ee e in
    raise (My_Return (Some(c)))
  | If(e,b1,b2,_) ->
    let pred = ee e in
    if const_is_zero pred then
      eval_block b2
    else
      eval_block b1
  | Break _  -> raise My_Break
  | Continue _ -> raise My_Continue
  | Loop(b,_,_,_) ->
    let finished = ref false in
    let loop_count = ref 0 in
    while not !finished do
      try
        incr loop_count ;
        (if !loop_count > 16 then finished := true) ;
        eval_block b
      with My_Break -> finished := true
         | My_Continue -> failwith "eval_stmt continue"
    done
  | Block(b) -> eval_block b
  | Return(None,_) -> raise (My_Return (None))
  | _ -> failwith "eval_stmt"


let compute_average_values ?(trials=1000) ast meth =
  debug "pellacini: Computing Average\n" ;
  let final_averages = Hashtbl.create 255 in
  let return_averages = ref (CReal(0.0,FFloat,None)) in
  Hashtbl.clear output ;

  let fundec = get_fun meth in
  let retvals = ref [] in
  for trial = 1 to trials do
    if trial mod 200 = 0 then debug "trial %d\n" trial ;
    Hashtbl.clear env ;
    iterGlobals ast (fun glob -> match glob with
        | GVar(va,init,_) ->
          (*
            Pretty.printf "GVar %s %a\n" va.vname
            d_plaintype va.vtype;
          *)
          update_env (Var(va),NoOffset)
            (random_value_of_type va va.vtype)
        | _ -> ()
      ) ;

    let args = List.map (fun formal ->
        Const(random_value_of_type formal formal.vtype)
      ) fundec.sformals in
    let instr =
      Call(None,(Lval(Var(fundec.svar),NoOffset)),args,locUnknown)
    in
    let retval = try
        (*
          debug "compute_average_values: Evaluating %s\n"
          (Pretty.sprint ~width:80 (d_instr () instr)) ;
        *)
        eval_instr ~raise_retval:true instr ;
        debug "compute_average_values: ERROR: %s did not return\n" meth ;
        failwith "compute_average_values"
      with My_Return(None) ->
        debug "compute_average_values: WARNING: returned None\n" ;
        (CReal(0.0,FFloat,None))
         | My_Return(Some(c)) -> c
    in
    retvals := retval :: !retvals ;
  done ;

  (match average_list !retvals with
   | None -> ()
   | Some(v) -> return_averages := v ) ;

  Hashtbl.iter (fun (sid,expr) _ ->
      let all_observed = Hashtbl.find_all output (sid,expr) in
      match average_list all_observed with
      | None -> ()
      | Some(avg) ->
        let expr_str = Pretty.sprint ~width:80 (d_exp () expr) in
        Hashtbl.replace final_averages (sid,expr_str) avg
    ) output_values ;

  final_averages, !return_averages

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

let print_cg_func ast filename =
  let fout = open_out filename in
  lineDirectiveStyle := None ;
  visitCilFileSameGlobals my_remove_casts ast ;

  let _ =
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
                let ret = TFun(ret,(Some args),b,a) in
                ret
              | x -> x
            in
            let svar = { fundec.svar with vtype = new_type } in
            let new_glob = GFun({fundec with svar = svar},l) in
            dumpGlobal defaultCilPrinter fout new_glob
          | _ ->
            dumpGlobal defaultCilPrinter fout glob ;
      ) ;


    close_out fout ;
  in
  let main_file_string = file_to_string filename in

  let funattr_regexp =
    let funattr_str =
      "([ \t\r]*:[ \t\r]+\\([A-Z0-9]+\\)"
      ^"[ \t\r]+\\([A-Za-z0-9_]+\\))\\(([^)]+)\\)"
    in
    Str.regexp funattr_str
  in
  let main_file_string =
    Str.global_replace funattr_regexp "\\2 \\3 : \\1 " main_file_string
  in
  let fieldattr_regexp =
    Str.regexp "\\(float[^:]*\\): \\([A-Za-z0-9_]+\\)\\([^;,{]+\\)"
  in
  let main_file_string =
    Str.global_replace fieldattr_regexp "\\1 \\3 : \\2 " main_file_string
  in
  let cast_regexp = Str.regexp "(struct f" in
  let main_file_string =
    Str.global_replace cast_regexp "(f" main_file_string
  in
  let cast_regexp = Str.regexp "(\\([A-Za-z0-9_]+\\)[ \t\r]+:[^)]*)" in
  let main_file_string =
    Str.global_replace cast_regexp "(\\1)" main_file_string
  in
  let dummy_regexp = Str.regexp ".dummy_field" in
  let main_file_string =
    Str.global_replace dummy_regexp "" main_file_string
  in
  let woof_array = Str.regexp "(\\([A-Za-z0-9_]+\\))\\[" in

  let func_cast_regexp = Str.regexp "\\(lerp\\|max\\|pow\\|min\\)[0-9]" in
  let main_file_string =
    Str.global_replace func_cast_regexp "\\1" main_file_string
  in

  let float_cast_regexp = Str.regexp "float\\([0-9]\\)_(" in
  let main_file_string =
    Str.global_replace float_cast_regexp "float\\1(" main_file_string
  in
  let main_file_string =
    Str.global_replace woof_array "\\1[" main_file_string
  in
  let fout = open_out filename in
  Printf.fprintf fout "%s" main_file_string ;
  close_out fout ;
  ()

let print_cg ast filename =
  try
    print_cg_func ast filename;
  with _ ->
    debug "pellacini: print_cg fail\n"



let parse_cg filename =
  debug "\nparse_cg: %s\n" filename ;
  let main_file_string = file_to_string filename in
  (* change
     struct X { } ;
     to
     typedef struct X { } X ;
  *)
  let typedef_regexp =
    Str.regexp "struct[ \t\r]+\\([^ \t\r]+\\)\\([^}]+\\)}"
  in
  let main_file_string =
    Str.global_replace typedef_regexp "typedef struct \\1 \\2} \\1" main_file_string
  in
  let cast_regexp = Str.regexp "\\(float[0-9]\\)(" in
  let main_file_string =
    Str.global_replace cast_regexp "\\1_(" main_file_string
  in
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

 typedef struct float3x3 {
  float t[3][3];
 } float3x3;
//  typedef float3 float3x3[3];

//  typedef float4 float4x4[4];

// typedef struct float3x3 {
//  float dummy_field;
// } float3x3;

 typedef struct texobj2D {
  float dummy_field;
 } texobj2D, Texture2D;

 typedef struct texobjCube {
  float dummy_field;
 } texobjCUBE, texobjCube, TextureCube;

 float3 mul(float3x3 a, float3 b);
 float3 mul3x3(float3x3 a, float3 b);
 float3 normalize(float3 b);
 float dot(float3 a, float3 b);
 float3 min(float3 a, float3 b);
// float min(float a, float b);


 float max(float a, float b);
 float3 max3(float3 a, float3 b) {
   a.x = max(a.x, b.x);
   a.y = max(a.y, b.y);
   a.z = max(a.z, b.z);
   return a;
 }
 float3 pow3(float3 a, float b);
 float pow(float a, float b);
 float3 lerp3(float3 a, float3 b, float f)  { return (1.0f - f)*a + b*f;};
 float lerp(float a, float b, float f)      { return (1.0f - f)*a + b*f;};

/*
 float3 max(float3 a, float3 b);
  float3 lerp(float3 a, float3 b, float3 f) {
   return (float3_(1.0f,1.0f,1.0f) - f)*a + b*f;
 };
*/

 float3 exp(float3 a);
 float sqrt(float a);
 float cos(float a);
 float acos(float a);
 float saturate(float a);
 float3 floor(float3 a);
 float2 float2_(float a, float b);
 float3 float3_(float a, float b, float c);
 float4 float4_(float a, float b, float c, float d);


 float4 tex2D(texobj2D a, float2 b);
 float4 tex2D(Texture2D a, float2 b);

 float4 texCUBE(TextureCube a, float3 b);

 float f1texcompare2D(texobj2D a, float3 sz) { return tex2D(a, sz.xy).x >sz.z; };
 float3 reflect(float3 a, float3 n) {
   return a - 2.0f* n*dot(a,n);
 }
#define __REPAIR__

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
    match v.skind with
    | Return _ -> SkipChildren (* handled by "normalization" *)
    | _ -> DoChildren
  method vexpr e =
    ChangeDoChildrenPost(e,
                         (fun e ->
                            match e with
                            | BinOp(op,Const(c),e',t)
                            | BinOp(op,e',Const(c),t) ->
                              incr count ;
                              if !count = desired then e' else e
                            | _ -> e
                         ))
end
let my_rule_one_visitor = new ruleOneVisitor

class ruleThreeVisitor count desired averages = object
  inherit nopCilVisitor
  method vstmt v =
    sid := v.sid ;
    DoChildren
  method vexpr e =
    ChangeDoChildrenPost(e,
                         (fun e ->
                            let expr_str = Pretty.sprint ~width:80 (d_exp () e) in
                            if (Hashtbl.mem averages (!sid,expr_str))
                            && (is_not_const e) then begin
                              incr count;
                              if !count = desired then
                                Const(Hashtbl.find averages (!sid,expr_str))
                              else
                                e
                            end else e
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
    | Return(Some(BinOp(PlusA,e,(Const foo),tau)),loc) ->
      let new_foo = binop (Int64.add) (+.) delta_retval foo in
      let e' = BinOp(PlusA,e,(Const new_foo),tau) in
      ChangeTo({ v with skind = Return(Some(e'),loc) })

    | Return(Some(e),loc) ->
      let e' = BinOp(PlusA,e,(Const delta_retval),(typeOf e)) in
      ChangeTo({ v with skind = Return(Some(e'),loc) })
    | _ -> DoChildren
end
let my_normalize_visitor = new normalizeVisitor

(*************************************************************************
                                                                          * Error Evaluation
 *************************************************************************)
let eval_error source_name =
  let port_arg = Printf.sprintf "%d" !port in
  change_port () ;
  let base_command = !test_command in
  let fitness_file = source_name ^ ".fitness" in
  let exe_name = source_name in
  let cmd = Global.replace_in_string base_command
      [
        "__TEST_SCRIPT__", !test_script ;
        "__EXE_NAME__", exe_name ;
        "__TEST_NAME__", (test_name Single_Fitness) ;
        "__SOURCE_NAME__", (source_name) ;
        "__FITNESS_FILE__", (fitness_file) ;
        "__PORT__", port_arg ;
      ]
  in
  let real_valued = ref [| 0. |] in
  debug "%s\n" cmd ;
  begin
    match Stats2.time "test" system cmd with
    | Unix.WEXITED(0) -> (real_valued := [| 1.0 |])
    | _ -> (real_valued := [| 0.0 |])
  end ;
  (try
     let str = file_to_string fitness_file in
     let parts = Str.split (Str.regexp "[, \t\r\n]+") str in
     let values = List.map (fun v ->
         try
           float_of_string v
         with _ -> begin
             debug "%s: invalid\n%S\nin\n%S"
               fitness_file v str ;
             0.0
           end
       ) parts in
     if values <> [] then
       real_valued := Array.of_list values
   with _ -> ()) ;
  (if not !always_keep_source then
     (try Unix.unlink fitness_file with _ -> ())) ;
  (* return the results *)
  !real_valued

(*************************************************************************
                                                                          * Control Loop
 *************************************************************************)
let original_retval = ref None

let pellacini_loop original method_name incoming seqno =
  debug "pellacini: #%02d: computation begins\n" seqno ;
  if seqno > 100 then begin
    debug "pellacini: max sequence number: done!\n" ;
    None (* we're done *)
  end else if is_all_constant incoming then begin
    debug "pellacini: only constants remain: done!\n" ;
    None (* we're done *)
  end else begin
    let variants = ref [] in
    let incoming_averages,
        this_retval = compute_average_values incoming method_name in
    (match !original_retval with
     | None -> original_retval := Some(this_retval) ;
     | Some(e) -> ()
    ) ;

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
                               incoming_averages) incoming;
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

    (* let to_normalize = first_nth !variants 3 in  *)
    let to_normalize = !variants in

    debug "pellacini: %d variants to normalize\n"
      (List.length to_normalize) ;
    let post_normalize = List.map (fun variant ->
        let _, this_retval = compute_average_values ~trials:10
            incoming method_name in
        let original_retval = match !original_retval with
          | None -> failwith "normalize to original retval"
          | Some(e) -> e
        in
        let delta = binop (Int64.sub) (-.) original_retval this_retval in
        visitCilFileSameGlobals (my_normalize_visitor method_name delta) variant;
        variant
      ) to_normalize in

    let vcount = ref 0 in
    let errors = List.map (fun var ->
        let name = Printf.sprintf "p%02d-%04d.cg" seqno !vcount in
        incr vcount ;
        print_cg var name ;
        let error = eval_error name in
        let error = if error = [| |] then infinity else error.(0) in
        debug "%s -- %g\n" name error ;
        error, name, var
      ) post_normalize in
    let errors = List.sort (fun (e,_,_) (e',_,_) -> compare e e') errors in
    match errors with
    | [] ->
      debug "pellacini: no new variants; done\n" ;
      None
    | (error,name,var) :: rest ->
      debug "\npellacini: #%02d WINNER is %s with error %g\n\n"
        seqno name error ;
      if error = infinity then  begin
        abort "pellacini: ERROR = infinity, stopping\n" ;
      end ;
      Some(error,name,var)
  end

let pellacini (original_filename : string) =
  let original = parse_cg original_filename in
  (* let _ = print_cg original "output.cg" in  *)
  let stmt_number = ref 1 in
  let current = ref (copy original) in
  visitCilFileSameGlobals (my_simple_num_visitor stmt_number) !current ;
  let meth = !pellacini_method_name in
  if meth = "" then
    abort "use --pellacini-method to set the shader method\n" ;
  let finished = ref false in
  let seqno = ref 1 in
  let counter = ref 1 in
  while not !finished do
    match pellacini_loop
            original meth !current !seqno with
    | Some(error,name,var) ->
      (* Around Mon Nov 22 09:23:21 EST 2010, Yam noticed that
         Deadcodeelimination was producing incorrect values. Since
         Pellacini just includes it as an optimization (i.e., it
         leads to faster convergence but not really better results),
         we're dropping it.
         Deadcodeelim.dce var ;
      *)
      let newname = Printf.sprintf "final-%02d-%g.cg" !counter error in
      incr counter ;
      ignore (system(Printf.sprintf "cp %s %s" name newname)) ;
      current := var ;
      incr seqno
    | None -> finished := true
  done

let _ =
  global_filetypes := !global_filetypes @
                      [ ("pellacini",(fun () -> Cil.initCIL(); pellacini !program_to_repair)) ]
