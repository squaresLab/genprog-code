open Global
open Cil
open Cilprinter

(* lots of useful utilities *)

let exp_str exp = Pretty.sprint ~width:80 (printExp defaultCilPrinter () exp) 
let mk_lval vi = Lval(Var(vi),NoOffset)

let complete_xform map = 
  let the_xform _ stmt = 
    if IntMap.mem stmt.sid map then
      IntMap.find stmt.sid map
    else stmt 
  in
    visitCilStmt (my_xform the_xform nop_bxform)

(* 
 * Myoungkyu Song     <mksong1117@utexas.edu>
 *
 * Template 02: integer overflow checker
 * -----------
 * 
 * 1. We find Set instructions that would perform arithmetic calculation such 
 *    as, addition and multiplication to void "integer overflow". Suppose that 
 *    there appears an arithmetic expression, c = a * b. When we detect this 
 *    expression, we add a checker using an If conditional expression: value 'a' 
 *    must be equal to the result of a new calculation 'c divided by b' or 
 *    'c/b'.
 * 
 *    To limit the searches that we need to look for, we check if operands in 
 *    the right part of arithmetic calculations are used in If conditional 
 *    expressions in the subsequent statements. Otherwise, we just skip. There
 *    is difficulty of adding a Return statement along with an If statement as
 *    we are hard to determine what value is to be associated with it as a
 *    negative return value. This is the reason why we modify an existing If
 *    conditional expression, rather than adding a new If statement.
 *
 *  2. We look at Set instructions and If statements subsequently, and stores
 *     relevant expressions and statements in a hash table, if they are related
 *     as described above. At the moment, we only take care of multiplying
 *     calculation.
 *
 *  3. Given an id of the If statement we found above, we find it in a change
 *     module, where we modify the If conditional expression by adding a new 
 *     "BinOp" expression as a new required condition to avoid potential fault,
 *     integer overflow.
 *
 *  4. More various and complex arithmetic calculating expression will be 
 *     handled.


    Example.

   cc = (tsize_t )(dir->tdir_count * (uint32 )w);
-  if (! dir->tdir_count) {
+  if (! dir->tdir_count || (uint32 )w != cc / dir->tdir_count) { .. }
   
   
 *
 *)

class collectLvals retval = object
  inherit nopCilVisitor

  method vlval lv = retval := lv :: !retval; DoChildren
end

class exprVisitor retval = object
  inherit nopCilVisitor

  method vexpr e = 
    match e with 
    | BinOp(Mult,exp1,exp2,_) -> 
      let lvs = ref [] in
      let my_collect = new collectLvals lvs in 
      (* this is totally not how I want to do this, hm *)
      ignore(visitCilExpr my_collect exp1);
      ignore(visitCilExpr my_collect exp2);
      retval := (exp1,exp2,!lvs)::!retval;
      SkipChildren
    | _ -> DoChildren
end

class template02Visitor retval = object
  inherit nopCilVisitor
  
  val mutable preceding_set = false 
  val mutable preceding_exp_info = None

  method vstmt s =
    (match s.skind with
    | Instr([Set(lv, exp, location)]) -> 
      let exp_retval = ref [] in 
      let _ =
        ignore(visitCilExpr (new exprVisitor exp_retval) exp)
      in
        if (llen !exp_retval) > 0 then
          (preceding_set <- true; preceding_exp_info <- Some(lv,!exp_retval))
    | If(UnOp(LNot,e,t),bl1,bl2,loc) when preceding_set ->
      let lv,lst = match preceding_exp_info with Some(lv,exp_retval) -> lv,exp_retval | None -> failwith "failwhale"
      in
      let math_lvals = lfoldl (fun acc (_,_,c) -> c @ acc) [] lst in
      let guard_lvals = ref [] in
      let _ = ignore(visitCilExpr (new collectLvals guard_lvals) e) in
      let any_overlap = 
        (* CLG notes that she can't ever remember the difference between = and
           == and is only about 50% confident that the below will work in any
           case *)
        List.exists (fun math_lv -> (List.exists (fun guard_lv -> guard_lv == math_lv) !guard_lvals)) math_lvals 
      in
        if any_overlap then begin
          let exps = lmap (fun (a,b,_) -> s,lv,a,b,!currentLoc) lst in
            retval := exps@ !retval
        end
    | _ -> preceding_set <- false); DoChildren
end

let template02 fd stmt get_fun_by_name = begin
  let retval = ref [] in 
  let _ = ignore(visitCilStmt (new template02Visitor retval) stmt) in 
  let newstmts = 
    List.fold_left 
      (fun acc (s,lv,exp1,exp2,loc) -> 
        let divide = BinOp(Div,Lval(lv),exp1,intType) in
        let ne = BinOp(Ne,exp2,divide,intType) in
        let new_skind = 
          match s.skind with
            If(guard,bl1,bl2,loc) -> 
                If(BinOp(BOr,guard,ne,intType),bl1,bl2,loc) 
          | _ -> failwith "major failwhale"
        in
          IntMap.add s.sid ({s with skind = new_skind}) acc
      ) (IntMap.empty) !retval 
  in 
    complete_xform newstmts stmt 
end

      
(* 
 * Myoungkyu Song     <mksong1117@utexas.edu>
 *
 * Template 03: string copy checker
 * -----------
 * 
 * 1. We find a standard API function call to "strcpy" that would occur a 
 *    potential problem of overwriting a near memory space due to a longer 
 *    length of a source string than the one of a destination string. 
 *    Suppose that there appears "strcpy(s1,s2)"; the length of variable 's1' 
 *    could be shorter than the length of variable 's2', while possibly making 
 *    a corruption near neighbor memory. To prevent it from causing this 
 *    potential problem, we replace a call to "strcpy" with a call to "strncpy" 
 *    with the specified length of the destination variable.
 *
 *    To limit the searches that are required to look for, we find a pair of
 *    Call instructions: a call to "strcpy" along with "strlen". We investigate 
 *    whether a call to "strlen" is followed by a call to "strcpy" to add a 
 *    null-terminated character to a destination argument in the latter call. 
 *
 * 2. Given a set of two statements we found above, we replace these statements 
 *    with the new statements that we create by using the same argument: (1)
 *    a call to "strcpy" with a call to "strncpy" by using the length of its
 *    destination argument, and (2) a call to "strlen" with a call to "sizeof"
 *    when adding a null-terminated character to the end of an array.
 *
 *    Need to perform data flow analysis to determine whether a variable is 
 *    used in invoking both calls to "strcpy" and "strlen" as an argument, 
 *    respectively.
 

    Example.

- strcpy((char * )(t2p->pdf_datetime + 2), (char const * )optarg);
- tmp___9 = strlen((char const * )optarg);

+ __builtin_strncpy((char * )(t2p->pdf_datetime + 2), (char const * )optarg, 
                    sizeof((char * )(t2p->pdf_datetime + 2)) - 1);
+ tmp___9 = sizeof((char * )(t2p->pdf_datetime + 2)) - 1;
   
 *
 *)
class template03Visitor retval = object
  inherit nopCilVisitor

  val current_strcpy = ref (mkEmptyStmt())
  val strcpy_arguments = ref []
  val strcpy_loc = ref builtinLoc
  val mutable preceding_strcpy = false

  method vstmt s = 
    (** Get a relevant element from a list of arguments. **)
    let getRelevantName args nth = begin
      (* FIXME: matching on the names is probably a bad idea? *)
      let dest = exp_str (List.nth args nth) in
      try
        let par_right_index = (String.rindex dest ')') + 1 in
        let len = String.length dest in
        let len = len - par_right_index in
        let only_name = String.sub dest par_right_index len in
        let only_name = String.trim only_name in
        only_name        
      with
      | _ -> dest
    end in
    match s.skind with
      (* we're not handling anything but the most straightforward of calls to the
         functions in question *)
      Instr([Call(ret, Lval((Var(v),o)), arguments, location)]) when v.vname = "strcpy" -> begin
        current_strcpy := s;
        strcpy_loc := !currentLoc;
        strcpy_arguments := arguments;
        preceding_strcpy <- true;
        SkipChildren
      end
    | Instr([Call(ret, Lval((Var(v),o)), strlen_args, location)]) 
        when preceding_strcpy && v.vname = "strlen" && 
      (getRelevantName !strcpy_arguments 1) = (getRelevantName strlen_args 0) ->
      retval := ((!current_strcpy, !strcpy_arguments, !strcpy_loc),(s, !currentLoc)) :: !retval;
      SkipChildren
    | _ -> preceding_strcpy <- false; DoChildren
end

let template03 (_: Cil.fundec) stmt get_fun_by_name =
  let old_directive_style = !Cil.lineDirectiveStyle in
    Cil.lineDirectiveStyle := None ; 
  let pairs = ref [] in
  let _ = ignore(visitCilStmt (new template03Visitor pairs) stmt) in
  let newstmts = 
    List.fold_left
      (fun acc ((strcpy_s, strcpy_args, strcpy_loc), (strlen,loc)) ->
        (* first, strncpy *)
        let dest_exp = List.hd strcpy_args in
        let src_exp = List.nth strcpy_args 1 in

        let subtraction_exp = BinOp(MinusA,SizeOfE(dest_exp),one,intType) in
        let strncpy_varinfo = get_fun_by_name "__builtin_strncpy" in
        let strncpy_lval = mk_lval strncpy_varinfo in

        let arguments = [dest_exp;src_exp;subtraction_exp] in

        let strncpy_instr = Instr([Call(None,strncpy_lval,arguments,strcpy_loc)]) in
        let strncpy_stmt = mkStmt strncpy_instr in

        let sizeof_exp = BinOp(MinusA,SizeOfE(dest_exp),one,intType) in
        let sizeof_stmt =
          match strlen.skind with
            Instr[(Call(Some(retval),presumed_strlen,args,loc))] -> begin
              let new_inst = Set(retval,sizeof_exp,loc) in
                { strlen with skind = Instr[new_inst] }
            end
          | _ -> abort "major fail"
        in
        let acc = IntMap.add strcpy_s.sid strncpy_stmt acc in
          IntMap.add strlen.sid sizeof_stmt acc
      ) (IntMap.empty) !pairs
  in
    Cil.lineDirectiveStyle := old_directive_style;
    complete_xform newstmts stmt
      

(* 
 * Myoungkyu Song     <mksong1117@utexas.edu>
 *
 * Template 04: paired function call checker
 * -----------
 * 
 * 1. We find a function call that must be associate with a particular call. If
 *    there does not appear the associated subsequent call that is required to
 *    be paired, we add a required call so that we make a call that we predefine
 *    be follow by its paired call.
 *
 *    Need to conduct manual inspection on data set to find which function need 
 *    to be specified in advance in a predefined set of paired functions that 
 *    we match with a function name.
 *
 *    In this template, we includes several function names in a predefine set,
 *    which must be wrapped with two statements: (1) a call statement to notify
 *    entering its control execution, and (2) one to declare leaving it.
 *
 * 2. Once we find a call that we would like to wrap with paired calls, we add 
 *    the first paired call before executing the call, and add the second paired
 *    call that is followed by the call that we found.
 *
 *    When it comes to the first paired call, we add an If statement that can 
 *    determine if the call is appropriately executed. If it gets a negative 
 *    sign, it no longer moves forward to the execution and return.
 

    Example.

+  __cil_tmp10 = Py_EnterRecursiveCall("");
+  if (__cil_tmp10 != 0) {
+    return (-1);
+  }
   tmp___0 = encoder_listencode_obj(s, rval, obj, indent_level);
+  Py_LeaveRecursiveCall();

 *
 *)

let predefined_fname_list = [ "encoder_listencode_obj";"encoder_listencode_dict";"encoder_listencode_obj" ]
(* I assume that this has the function definitions in it, filled in in a
   currently non-existent preprocessing step *)
let function_ht = hcreate 10

class template04Visitor calls = object(self)
  inherit nopCilVisitor

  method vstmt s = 
    let _ = 
      match s.skind with
      | Instr([Call(Some (Var(vi), NoOffset),fun_exp,arguments,loc)]) 
          when List.mem vi.vname predefined_fname_list -> 
        calls := (s.sid,!currentLoc) :: !calls
      | _ -> ()
    in
      DoChildren
end

let template04 fd stmt get_fun_by_name =
  let calls = ref [] in
  let _ = ignore(visitCilStmt (new template04Visitor calls) stmt) in
  let newstmts = 
    List.fold_left
      (fun acc (sid,loc) ->
        let ret_var = makeTempVar fd intType in 
        let enter_exp = mk_lval(hfind function_ht "Py_EnterRecursiveCall") in
        let leave_exp = mk_lval (hfind function_ht "Py_LeaveRecursiveCall") in

        let enter_call = mkStmt (Instr([Call(Some(Var(ret_var),NoOffset),enter_exp,[Const(CStr(""))],loc)])) in
        let leave_call = mkStmt (Instr([Call(None,leave_exp,[],loc)])) in

        let guard = BinOp (Ne,Lval(Var(ret_var),NoOffset),zero,intType) in
        let ret_stmt = mkStmt (Return(Some mone,loc)) in
        let block1 = mkBlock([ret_stmt]) in
        let block2 = mkBlock([]) in
        let if_stmt = mkStmt (If(guard,block1,block2,loc)) in
        let block = mkStmt (Block(
          mkBlock 
            [enter_call; if_stmt;leave_call])) in
          IntMap.add sid block acc) (IntMap.empty) !calls
  in
    complete_xform newstmts stmt

(*
 * Myoungkyu Song     <mksong1117@utexas.edu>
 *
 * Template 06: arguments (call-by-references) checker
 * -----------
 *
 * 1. We find a call statement that passes arguments to a function copies the
 *    address of an argument. If we would detect, we add an If statement that
 *    check the argument described above to ensure that it isn't less then zero.
 * 
 *    To limit searches that we need to do, we check the variable returned from
 *    the function call to determine if it is type long and it is used in an If
 *    conditional expression that represents equivalent comparison. In addition,
 *    we check to see if the found If statement include a Return statement.
 * 
 * 2. Given the variables, We add If statements that can ensure the variables
 *    are not less than zero. Otherwise, these If statements return. To add 
 *    them, we find a block that includes the If statement we found above. We
 *    modify the block we found by adding new If statements we created 
 *    after execution of the If statement.
 *

   Example.

   tmp = zend_parse_parameters(ht, "ll", & tv_sec, & tv_nsec);
   if (tmp == (int __attribute__((__visibility__("default")))  )-1) {
     return;
   } else {
 
+  }
+  if (tv_nsec < 0) {
+    return;
+  }
+  if (tv_sec < 0) {
+    return;
   }

 *)

(* visitor *)
class template06Visitor retval = object
  inherit nopCilVisitor

  val mutable preceding_call = false
  val mutable preceding_vid = -1
  val preceding_info = ref None

  method vstmt s = 
    let return_but_no_loop stmts = 
      let has_ret, has_loop = List.fold_left(fun (has_ret, has_loop) s ->
        match s.skind with
        | Return _ -> (true, has_loop)
        | Loop _ -> (has_ret,true)
        | _ -> (has_ret, has_loop)
      ) (false,false) stmts 
      in
        has_ret && not has_loop
    in
    let _ =
      match s.skind with 
      | Instr([Call(Some (Var(vi), _),fun_exp,arguments,loc)]) -> 
        let reffed_args = 
          List.fold_left
            (fun acc arg -> 
              match arg with
              | AddrOf(Var vi, _) when isIntegralType vi.vtype -> vi :: acc
              | _ -> acc
            ) [] arguments 
        in
          if (llen reffed_args) > 0 then begin
            preceding_call <- true; 
            preceding_vid <- vi.vid;
            preceding_info := Some(s,reffed_args)
          end
      | If(BinOp((Eq,Lval(Var vi, os),ex2,typ)),b1,b2,loc) when 
          preceding_call && vi.vid == preceding_vid && return_but_no_loop b1.bstmts -> 
        let stmt,args =
          match !preceding_info with
            Some(p) -> p
          | None -> failwith "this shouldn't be none!"
        in
          retval := (stmt,args,loc) :: !retval
      | _ -> preceding_call <- false
    in
      DoChildren
end


let template06 fd stmt get_fun_by_name = begin
  let retval = ref [] in
  let _ = ignore(visitCilStmt (new template06Visitor retval) stmt) in
  let newstmts = 
    List.fold_left
      (fun map (stmt,reffed_args,loc) ->
        let new_ifs =
          List.map 
            (fun vi ->
              let retblock = mkBlock ([mkStmt (Return(None,loc))]) in
              let elseblock = mkBlock ([]) in
              let new_var = Lval(Var(vi),NoOffset) in
              let guard = BinOp (Lt,new_var,zero,intType) in
                mkStmt(If(guard,retblock,elseblock,loc))
            ) reffed_args
        in
        let block =
          Block({
          battrs = [] ;
          bstmts = ({stmt with sid = 0 }) :: new_ifs })
        in
          IntMap.add stmt.sid (mkStmt block) map)
      (IntMap.empty) !retval
  in
    complete_xform newstmts stmt
end

(* 
 * Myoungkyu Song     <mksong1117@utexas.edu>
 *
 * Template 07: memory leak checker
 * -----------
 *
 * 1. We find Set statements and Call statements to deallocate memory spaces
 *    before assigning a new value to avoid resulting in wasted memory. If
 *    found, we add a call to the memory deallocation API, checking if the
 *    argument to be freed is non-zero.
 *     
 *    To reduce the searches that we need to do, we look into arguments that
 *    pass to a function to determine if these arguments are used in Set or
 *    Call statements that we found above. Otherwise, we no long move forward to
 *    a search.
 *     
 *    Suppose that there appears a Set statement like "buf->num = data". We 
 *    check arguments of the enclosed function to see whether they are identical
 *    to "buf" and "data" used in the the Set statement.
 *     
 * 2. We find a function whose argument type is a pointer type. And, in the 
 *    function, we find (1) a Set statement that assign a function argument with
 *    another argument, and (2) a Call statement that returns to a function 
 *    argument passing another argument[s].
 *     
 * 3. Given the information that we analyzed above, we update a block that
 *    entails a particular statement such as Set or Call statement that we 
 *    found above. When we detect a block of statements we would like to update, 
 *    we divide it into the front and rear parts by a given statement. 
 *    We create an If statement including a call to the memory deallocation API 
 *    in its body, and insert them between the front and rear fragments, 
 *    which becomes a new block replacing an existing block.
 *     
      
   Example.

 static void spl_array_set_pos_array(spl_array_object *intern , 
                                     HashPosition position ,
                                     zval *arr1 , zval *arr2 , int type ) 
 { 
+  if (intern->pos != 0) {
+    _efree(intern->pos);
+  }
   intern->pos = position;

+  if (intern->array != 0) {
+    _efree(intern->array);
+  }
   intern->array = spl_array_read_dimension(arr1, arr2, type);
 
 *
 *)

class template07Visitor formals just_pointers retval = object
  inherit nopCilVisitor

  
  (** Visitor to find the function we'll change **)
  method vstmt s = 
    let rec unroll_exp lst exp =
      let unroll_exps es = 
        List.fold_left
          (fun acc e -> 
            match acc with
              Some _ -> acc | None -> unroll_exp lst e) None es
      in
      match exp with
      | AddrOf(l) | StartOf(l) | Lval(l) -> unroll_lval lst l
      | SizeOfE(e) | AlignOfE(e) | CastE(_,e) | UnOp(_,e,_) -> unroll_exp lst e
      | BinOp(_,e1,e2,_) -> unroll_exps [e1;e2]
      | Question(e1,e2,e3,_) -> unroll_exps [e1;e2;e3]
      | _  -> None
    and unroll_lval lst lval = 
      match fst lval with
        Var(v) when List.exists (fun p -> p.vid == v.vid) lst -> Some(v)
      | Mem(e) -> unroll_exp lst e
      | _ -> None
    in
      (match s.skind with
      | Instr([Set((Mem left_exp,Field(fi,o)),right_exp,loc)]) -> begin
        match (unroll_exp just_pointers left_exp) with
          Some(left_vi) -> begin
            match (unroll_exp formals right_exp) with
              Some(right_vi) -> retval := (s,(Mem left_exp, Field(fi,o)),!currentLoc) :: !retval
            | None -> ()
          end
        | None -> ()
      end
      | Instr([Call(Some (Mem left_exp,Field(fi,o)),fun_exp,arguments,loc)]) -> begin
        match (unroll_exp just_pointers left_exp) with
          Some(left_vi) -> 
            retval := (s, (Mem left_exp,Field(fi,o)), !currentLoc) :: !retval
        | None -> ()
      end
      | _ -> ()); DoChildren
end

let template07 fd stmt get_fun_by_name = begin
  let just_pointers = List.filter (fun vi -> isPointerType vi.vtype) fd.sformals in
    match just_pointers with
      [] -> stmt
    | _ when (llen fd.sformals) > 2 -> 
      let retval = ref [] in
      let _ = 
        ignore(visitCilStmt (new template07Visitor fd.sformals just_pointers retval) stmt) 
      in
      let newstmts = 
        List.fold_left (fun map (stmt,lval,loc) ->
          let as_exp = Lval lval in
          let guard = BinOp(Ne,as_exp,zero,intType) in
          let free_lval = hfind function_ht "_efree" in
          let thenblock = mkBlock ([mkStmt (Instr([Call(None,mk_lval free_lval,[as_exp],loc)]))]) in
          let elseblock = mkBlock ([]) in
          let ifstmt = mkStmt (If(guard,thenblock,elseblock,loc)) in
          let newstmt = { stmt with skind = Block(mkBlock [ ({stmt with sid = 0}) ; ifstmt ]) } in
            IntMap.add stmt.sid newstmt map
        ) (IntMap.empty) !retval 
      in
        complete_xform newstmts stmt
    | _ -> stmt
end

(* 
 * Myoungkyu Song     <mksong1117@utexas.edu>
 *
 * Template 09: null and size checker
 * -----------
 *
 * 1. We find a field of a struct variable that would indicate the size of
 *    another field, when walking through an If conditional expression, to 
 *    update the current expression by adding another conditional expression 
 *    with the found field appeared in the body of the If statement.
 *       
 *    Suppose that there appears an If statement like,
 *       
 *      if (buf->data) { 
 *        while (buf->data_size > 0) { .. }
 *      }
 *       
 *    We examine a struct variable and its field, "buf" and "data" in the If
 *    conditional expression, as well as we look for another field "data_size",
 *    indicating the size which is involved in same struct variable "buf". To
 *    add an awareness of relationship between size and resource, we implement
 *    its mappings in the template.
 *       
 * 2. We find an If conditional expression checking if a field of a struct
 *    variable is NULL. Once found, we go through each statement in the body of
 *    the If statement to find another field of the variable. We, then, match
 *    two fields using a predefined mapping information to see if there is a
 *    relationship between them like resource and its size.
 *       
 * 3. Given variable info instances, we create new conditional expressions, 
 *    which we append to the existing expression with the And operation.
 *       

   Example.

-  if (tif->tif_fields) {
+  if (tif->tif_fields && tif->tif_nfields > 0) { /* In this example, we have */
     i = 0U;                                      /* a pre-defined mappings   */ 
     while (i < tif->tif_nfields) {               /* between the fields       */
       fld = *(tif->tif_fields + i);              /* "tif_fields" and         */
  ...                                             /* "tif_nfields".           */

 *
 *)

let field_pairs = StringMap.empty

class lvalVisitor fields = object
  inherit nopCilVisitor

  method vlval (lhost,offset) = 
    (match lhost,offset with 
      Var(vi),Field(fi,_) -> fields := (vi,fi) :: !fields
    | _ -> ()) ; DoChildren
end

(** Filter the list by using the passed variable names of a struct and a field. **)
let filter_lval_list vfList vi fi = begin

  let split_str instr token = Str.split (Str.regexp token) instr in
  let contains_reg str = Str.regexp (".*"^str^".*") in
  let contains str substr = Str.string_match (contains_reg substr) str 0 in

  (** The 1st filtering for the integer type. **)
  let _= vfList := List.filter(fun (v,f) -> 
    (match f.ftype with
    | TInt((IInt|IUInt),_) -> true
    | TNamed (t,_) -> 
      (match t.ttype with
      | TInt((IInt|IUInt),_) -> true;
      | _ -> false);
    | _ -> false)
  )!vfList in

  (** The 2nd filtering using variable names, holding same struct variable and 
  different field names. **)
  let _= vfList := List.filter(fun (v,f) -> 
    if (v.vname = vi.vname) && (not (f.fname = fi.fname)) then
      true
    else false
  )!vfList in

  (** The 3rd filtering for paring two variable names that we pre-define as
  a mapping information. Added one case like PREFIX_NAME and PREFIX_nNAME. **)
  vfList := List.filter(fun (v,f) -> 
    let strlst1 = split_str fi.fname "_" in
    let strlst2 = split_str f.fname "_" in
    if not ((List.length strlst1) == 2) then false
    else if not ((List.length strlst2) == 2) then false
    else begin
      let s1_token0 = List.nth strlst1 0 in
      let s2_token0 = List.nth strlst2 0 in
      let s1_token1 = List.nth strlst1 1 in
      let s2_token1 = List.nth strlst2 1 in
  
      if (s1_token0 = s2_token0) && (s2_token1 = ("n"^s1_token1)) then true
      else false
    end
  )!vfList
end

class template09Visitor retval = object
  inherit nopCilVisitor

  method vstmt s = 
    (match s.skind with
      | If(exp,bl1,bl2,loc) ->
        begin
          match exp with
            Lval(Mem(Lval(Var vi,_)),Field(fi,NoOffset)) -> 
              let then_lvals = ref [] in
              let _ = ignore (visitCilBlock (new lvalVisitor then_lvals) bl1) in
              let _ = filter_lval_list then_lvals vi fi in
                if (llen !then_lvals) > 0 then 
                  retval := (s,exp,!then_lvals,bl1,bl2,!currentLoc) :: !retval
          | _ -> ()
        end
      | _ -> ()) ; DoChildren
end


let template09 fd stmt get_fun_by_name = begin
  let retval = ref [] in 
  let _ = ignore(visitCilStmt (new template09Visitor retval) stmt) in
    assert((llen !retval) == 1); (* I think this should be true? *)
  let newstmts = 
    List.fold_left
      (fun map (stmt,exp,lvals,bl1,bl2,loc) ->
        let binop = 
        List.fold_left 
          (fun binop (vi,fi) ->
          let new_lval = Lval (Mem (Lval(Var vi,NoOffset)), Field(fi, NoOffset)) in
          let new_gaurd = BinOp(Gt,new_lval,zero,intType) in
          BinOp(LAnd,binop,new_gaurd,intType)
          ) exp lvals
        in
        let new_stmt = 
          { stmt with skind = If(binop,bl1,bl2,loc) }
        in
          IntMap.add stmt.sid new_stmt map 
      ) (IntMap.empty) !retval
  in
    complete_xform newstmts stmt
end

let templates :
    (Cil.fundec -> Cil.stmt -> (string -> Cil.varinfo) -> Cil.stmt) StringMap.t
  =
  List.fold_left (fun m (n,f) -> StringMap.add n f m) StringMap.empty [
    ("template02", template02);
    ("template03", template03);
    ("template04", template04);
    ("template06", template06);
    ("template07", template07);
    ("template09", template09);
  ]

