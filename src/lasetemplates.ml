open Printf
open Global
open Cil
open Cilprinter

(* lots of useful utilities *)

let exp_str exp = Pretty.sprint ~width:80 (printExp defaultCilPrinter () exp) 
let mk_lval vi = Lval(Var(vi),NoOffset)

let complete_xform map = 
  let the_xform stmt = 
    if IntMap.mem stmt.sid map then
      IntMap.find stmt.sid map
    else stmt 
  in
    visitCilStmt (my_xform the_xform nop_bxform)


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
        SkipChildren
      end
    | Instr([Call(ret, Lval((Var(v),o)), strlen_args, location)]) 
        when v.vname = "strlen" && 
      (getRelevantName !strcpy_arguments 1) = (getRelevantName strlen_args 0) ->
      retval := ((!current_strcpy, !strcpy_arguments, !strcpy_loc),(s, !currentLoc)) :: !retval;
      SkipChildren
    | _ -> DoChildren
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


let template06 stmt = begin
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
