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
open Global
open Cil

(**/**)
let dealloc_api = ref "free"
let paired_function_file = ref ""
let paired_functions = ref []

let _ =
  options := !options @
             [
               "--lase-deallocation-api", Arg.Set_string dealloc_api,
               "function API for deallocating memory to prevent leaks" ;

               "--lase-paired-functions", Arg.Set_string paired_function_file,
               "CSV contains function-to-wrap, prefix-function, and suffix-function"
             ]
(**/**)

let configure_templates () =
  if !paired_function_file <> "" then
    iter_lines !paired_function_file (fun line ->
        match Str.split comma_regexp line with
        | [wrappee; prefix; suffix] ->
          paired_functions := (wrappee, prefix, suffix) :: !paired_functions
        | _ ->
          failwith ("invalid syntax in " ^ !paired_function_file ^ ": must be 3 columns")
      )

let fun_exists fn str = try ignore(fn str); true with Not_found -> false

let isVoidTFun fd = match fd.svar.vtype with
  | TFun(rt,_,_,_) -> (isVoidType rt) | _ -> false

(* lots of useful utilities *)
let stmt_str stmt = Pretty.sprint ~width:80 (printStmt defaultCilPrinter () stmt)
let exp_str exp = Pretty.sprint ~width:80 (printExp defaultCilPrinter () exp)
let lval_str lv = Pretty.sprint ~width:80 (printLval defaultCilPrinter () lv)
let typ_str typ = Pretty.sprint ~width:80 (printType defaultCilPrinter () typ)
let clamp str = if String.length str > 114 then (Str.string_before str 114) ^ "..." else str
let one_line instr = Str.global_replace (Str.regexp "[\r\n]+") " " (clamp instr)

let contains_reg str = Str.regexp (".*"^str^".*")
let contains str substr = Str.string_match (contains_reg substr) str 0
let comp_str st1 st2 = (String.compare st1 st2) == 0

let lhead = List.hd
let lapnd = List.append
let ltail = List.tl
let lnth = List.nth
let hfinda = Hashtbl.find_all
let lu = locUnknown

(* String.trim is only since 4.00.0. But our experiments need to run on systems
   with older versions of Ocaml than that... *)
let trim_str s =
  let needs_trim =
    Str.string_match (Str.regexp "^[ \t]*\\([^ \t]\\(.*[^ \t]\\)?\\)[ \t]*$") s 0
  in
  if needs_trim then Str.matched_group 1 s else s

let mk_lval vi = Lval(Var(vi),NoOffset)

(* FIXME: we should probably do a structural equivalence check here *)
let cmp_exp e1 e2 = compare (exp_str e1) (exp_str e2)

let append_after_stmt stmt new_stmts =
  let lst = ({stmt with sid = 0}) :: new_stmts in
  let b = Block (mkBlock lst) in
  { stmt with skind = b }

let prepend_before_stmt stmt new_stmts =
  let lst = List.rev (({stmt with sid = 0}) :: (List.rev new_stmts)) in
  let b = Block (mkBlock lst)  in
  { stmt with skind = b }

let rec get_varinfo_exp exp =
  let handle_exps es =
    List.fold_left
      (fun acc e ->
         match acc with
           Some _ -> acc | None -> get_varinfo_exp e) None es
  in
  match exp with
  | AddrOf(l) | StartOf(l) | Lval(l) -> get_varinfo_lval l
  | SizeOfE(e) | AlignOfE(e) | CastE(_,e) | UnOp(_,e,_) -> get_varinfo_exp e
  | BinOp(_,e1,e2,_) -> handle_exps [e1;e2]
  | Question(e1,e2,e3,_) -> handle_exps [e1;e2;e3]
  | _  -> None

and get_varinfo_lval lval =
  match fst lval with
    Var(v) -> Some(v)
  | Mem(e) -> get_varinfo_exp e

let visitGetList visitFun visitor construct =
  let retval = ref [] in
  let _ = visitFun (visitor retval) construct in
  !retval

let visitFnGetList visitor construct = visitGetList visitCilFunction visitor construct
let visitExprGetList visitor expr = visitGetList visitCilExpr visitor expr
let visitBlkGetList visitor bl = visitGetList visitCilBlock visitor bl
let visitStmtGetList visitor st = visitGetList visitCilStmt visitor st

let visitGetBool visitFun visitor construct =
  let retval = ref false in
  let _ = visitFun (visitor retval) construct in
  !retval

let visitBlkGetBool visitor bl = visitGetBool visitCilBlock visitor bl
let visitExprGetBool visitor exp = visitGetBool visitCilExpr visitor exp

(* generic template, which really all do the same thing:
   (1) collect info from the function in a list
   (2) iterate over elements of that info list to construct new statements to
   replace old statements
   (3) put those new statements in an IntMap *)
(* generic template, given the visitor result *)
let pre_template retval mapper =
  List.fold_left
    (fun acc (sid,stmt) ->
       IntMap.add sid stmt acc)
    (IntMap.empty)
    (List.map mapper retval)

let template visitor mapper fd =
  let retval = visitFnGetList visitor fd in
  pre_template retval mapper

class chkConstructVisitor looking_for retval = object
  inherit nopCilVisitor

  method vstmt s =
    match s.skind,looking_for with
    | Return (None,_), "void_return"
    | Break _, "break"
    | Loop _, "loop"
    | Return _, "return"
    | Instr([Call (_,_,_,_)]), "call"
      -> retval := true; SkipChildren
    | _ -> DoChildren

  method vexpr e =
    match e, looking_for with
    | Lval _, "lval"
    | BinOp (_,_,_,_), "binop"
    | SizeOfE _, "sizeof"
    | SizeOfStr _, "sizeof"
    | SizeOf _, "sizeof"
      -> retval := true; SkipChildren
    | _ -> DoChildren

  method voffs o =
    match o, looking_for with
    | Field _, "offset"
    | Index _, "offset" -> retval := true; SkipChildren
    | _ -> DoChildren
end

let chkBrkStmtVisitor = new chkConstructVisitor "break"
let chkRetStmtVisitor = new chkConstructVisitor "return"
let exprLvalVisitor = new chkConstructVisitor "lval"
let boExprVisitor = new chkConstructVisitor "binop"
let chkOffsetFieldIndexVisitor = new chkConstructVisitor "offset"
let collectSizeOfVisitor = new chkConstructVisitor "sizeof"
let chkCallStmtVisitor = new chkConstructVisitor "call"
let chkLoopStmtVisitor = new chkConstructVisitor "loop"

let has_call blk = visitBlkGetBool chkCallStmtVisitor blk

class chkStmtLstVisitor looking_for retval = object
  inherit nopCilVisitor

  method vstmt s =
    match s.skind, looking_for with
    | Instr([Set(_,_,_)]), "set"
    | If (BinOp(Eq,_,_,_),_,_,_), "if_eq"
    | If _, "if"
    | _, "all_stmts"
    | Return _, "call_ret"
      -> retval := s :: !retval; DoChildren
    | Instr([(Call (_,fun_exp,_,_))]), "call_ret" ->
      let fun_exp_str = exp_str fun_exp in
      if (contains fun_exp_str "free") || (contains fun_exp_str "Free") || (contains fun_exp_str "FREE") then
        retval := s :: !retval;
      DoChildren
    | _ -> DoChildren
end

let chkSetStmtVisitor = new chkStmtLstVisitor "set"
let chkIfStmtVisitor  = new chkStmtLstVisitor "if"
let stmtChkCallRetVisitor = new chkStmtLstVisitor "call_ret"
let stmtIfThenBlkExpVisitor = new chkStmtLstVisitor "if_eq"
let stmtVisitor = new chkStmtLstVisitor "all_stmts"

class chkExpLstVisitor looking_for retval = object
  inherit nopCilVisitor

  method vstmt s =
    match s.skind, looking_for with
      If (exp, _,_,_), "if_expr"
      -> retval := exp :: !retval; DoChildren
    | _ -> DoChildren

  method vexpr e =
    match e, looking_for with
    | BinOp (Gt,e1,_,_), "binop_expr"
    | UnOp (LNot,e1,_), "uolnot"
      -> retval := e1 :: !retval; DoChildren

    | BinOp ((PlusPI|IndexPI),e1,Const(CInt64 (value,_,_)),_), "binop_indexpi" ->
      retval := e :: !retval; DoChildren

    | _ -> DoChildren

end

let chkIfExprBlockVisitor = new chkExpLstVisitor "if_expr"
let chkBinopExprVisitor =  new chkExpLstVisitor "binop_expr"
let chkBoIndexPIExprVisitor = new chkExpLstVisitor "binop_indexpi"
let chkUoLNotExprVisitor = new  chkExpLstVisitor "uolnot"

class chkBinopMinExprVisitor retval = object
  inherit nopCilVisitor

  method vexpr = function
    | BinOp ((MinusA|MinusPP|MinusPI),e1,e2,_) ->
      let _ =
        match getInteger e2 with
        | Some i -> retval := (e1,(cilint_to_int i + 1))::!retval
        | None -> ()
      in
      DoChildren
    | _ -> DoChildren
end

class usedVarVisitor retval = object
  inherit nopCilVisitor

  method vlval = function
    | Var vi, NoOffset -> retval := vi::!retval; DoChildren
    | _ -> DoChildren

end

class gotoLocVisitor retval = object
  inherit nopCilVisitor

  method vstmt s =
    match s.skind with
    | Goto(sref,loc) -> retval := (s,sref,loc)::!retval; DoChildren
    | _ -> DoChildren
end


class cmpIDVisitor looking_for id retval = object
  inherit nopCilVisitor
  method vstmt s =
    match s.skind,looking_for with
    | If _, "if"
    | _, "any" when id == s.sid -> retval := true; SkipChildren
    | _ -> DoChildren
end

let blkLoopVisitor = new cmpIDVisitor "if"
let cmpSidVisitor = new cmpIDVisitor "any"

class checkIfThenVisitor condition_check retval = object
  inherit nopCilVisitor

  method vstmt s =
    match s.skind with
    | If(exp,bl1,bl2,loc) when condition_check exp bl1 bl2 ->
      retval := true; SkipChildren
    | _ -> DoChildren
end

let chkIfStmtExprVisitor =
  new checkIfThenVisitor (fun exp _ _ -> visitExprGetBool exprLvalVisitor exp)
let chkThenBlockIfStmtVisitor =
  new checkIfThenVisitor (fun _ bl1 _ -> has_call bl1)


class labelAndRetVisitor goto_ret_htbl = object(self)
  inherit nopCilVisitor

  val mutable prec_label_name = ""

  method vstmt s =
    let mk_null_stmt () =
      { skind = Instr [];
        labels = [];
        sid = -1114; succs = []; preds = [] } in
    let _ = match s.labels with
        Label(nm,loc,bl)  :: rest -> prec_label_name <- nm;
        hadd goto_ret_htbl nm (mk_null_stmt(),lu)
      | _ -> ()
    in
    match s.skind with
    | Return (Some exp,loc) ->
      if hmem  goto_ret_htbl prec_label_name then
        hadd goto_ret_htbl prec_label_name (s,loc);
      DoChildren
    | _ -> DoChildren
end

class constVisitor usedVars = object
  inherit nopCilVisitor

  method vexpr = function
    | Const(CInt64 (c, _, _)) -> usedVars := c::!usedVars; DoChildren
    | _ -> DoChildren
end

let get_goto bl = visitBlkGetList (new gotoLocVisitor) bl

let is_cil_label_name nm =
  (comp_str nm "_L") || (contains nm "_L__") || (contains nm "Cont" )

let get_line_label s =
  if (llen s.labels) > 0 then begin
    let lb = lhead s.labels in
    match lb with
    | Label (str,loc,bol) when not (is_cil_label_name str) -> loc.line
    | _ -> (-1114)
  end else (-1114)

let get_label_name s =
  match s.labels with
    Label(str,loc,bol) :: lbs -> str
  | _ -> ""

let is_cil_label st = is_cil_label_name (get_label_name st)

let has_return_in_goto s goto_ret_htbl =
  if (get_line_label s) <> (-1114) then begin
    let label_nm = get_label_name s in
    try
      let stmt,loc = hfind goto_ret_htbl label_nm in
      if (loc.line == (-1)) || (comp_str (stmt_str stmt) "") then false else true
    with
    | _ -> false
  end else false


(*
 * Myoungkyu Song     <mksong1117@utexas.edu>
 *
 * Template 01: branch label checker
 * -----------
 *
 *    #1 match
 *   -----------
 *
 * 1. We find function calls to make correction on potential wrong branch labels
 *    by comparing with a set of predefined functions, matching them by the
 *    function name and the return type. Suppose that there appear two Goto
 *    statements, goto bad and goto done within each If statements, respectively.
 *    When we would detect one of them is wrong, we replace the wrong one by
 *    using the label information of the other Goto statement.
 *
 *    There is limitation of lack of an idea which Goto statement is wrong
 *    till fault localization let us know. So, we predefine a function call
 *    information, although it is a little naive strategy due to a requirement
 *    of manual observation.
 *
 * 2. We find an IF statement whose expression includes a variable returning
 *    from a call that we found above; the conditional expression checks if
 *    the variable is less then zero, which means the predefined function calls
 *    must return a value that is greater than zero, as long as it works without
 *    any errors.
 *
 * 3. We check the body of the IF statement that we found above whether it
 *    includes a Goto statement, and we determine if it is involved in an
 *    opposite Goto statement, for example, there are two Goto statements; the
 *    one returns negative or '-1', and the other returns positive or '1'.
 *    If possible, we find a pair of the Goto statements to replace the one with
 *    the other.
 *
 * 4. Based on the information we analyzed above in terms of the Goto statement
 *    within the body of an If statement, we change the label in the Goto
 *    statement with the one in an opposite Goto statement, since we assume that
 *    the found Goto statement would cause a fault by using the opposite one.


  @Example 1: case01/1tiffcp

   while (row < imagelength) {
     tmp___1 = TIFFReadScanline(in, buf, row, (unsigned short)0);
     if (tmp___1 < 0) {
       if (! ignore) {
-        goto done;
+        goto bad;
     ..

     tmp___2 = TIFFWriteScanline(out, buf, row, (unsigned short)0);
     if (tmp___2 < 0) {
       goto bad;
     }
   }

   done:
     _TIFFfree(buf);
     return (1);
   bad:
     _TIFFfree(buf);
     return (0);

 *
 *    #2 match
 *   -----------
 *
 *  1. We find an If conditional statement that includes a branch jumping to the
 *     failure case. And, we find the defect case that the program goes to the
 *     same failure case, although the program does not go into the If block.
 *
 *  2. We find an If conditional statement, particularly whose enclosing
 *     function has void return type, which is one of restrictions to avoid
 *     false negatives. If its block includes a Goto statement, we store
 *     the Goto statement into a temporary place.
 *
 *  3. We visit the Goto statement that we found above in a If block to store
 *     the statement ID and the location of the line.
 *
 *  4. We check each labeled statement whose line location is identical to the
 *     one that we found above in the Goto statement, where we store the
 *     reference ID to the labeled statement. Once we find, we check whether
 *     this labeled statement has a Return or a Goto statement until the next
 *     labeled statement. For this checking routine, we create the Hash table
 *     for a enclosing function, mapping a labeled statement with a Return
 *     statement that appear till the next labeled statement.
 *
 *  5. If no Return between the current and next Goto statements, we insert
 *     the next Goto statement that has a Return.

  @Example 2: case01/2tiff2pdf.c

    if ((unsigned int )t2p->t2p_error != 0U) {
      TIFFError("tiff2pdf", "An error occurred creating output PDF file");
      goto fail;
    }

+ goto success;

  fail:
    ret = 1;
  success:
    if ((unsigned int )input != (unsigned int )((void * )0)) {
      TIFFClose(input);
    }

 *
 *)


class collectGotosVisitor (retval : ((int * stmt ref) list) ref)  = object(self)
  inherit nopCilVisitor

  method vstmt s =
    match s.skind with
      Goto(stmtref,loc) -> retval := (!stmtref.sid,stmtref) :: !retval; DoChildren
    | _ -> DoChildren
end

class template01Pattern01 gotos retval = object
  inherit nopCilVisitor

  val mutable preceding_call_lv = []
  val mutable in_interesting_if = false

  method vstmt s =
    match s.skind with
    | Instr([Call(Some lv, exp1, args, loc)]) ->
      let _ =
        match get_varinfo_lval lv with
          Some(vi) -> preceding_call_lv <- [vi.vid]
        | None -> ()
      in
      DoChildren
    | Instr([Set(lv, exp1, loc)]) when (llen preceding_call_lv) > 0 ->
      let _ =
        match (get_varinfo_lval lv),(get_varinfo_exp exp1) with
          Some(setvi),Some(fromvi) ->
          let doadd = List.exists (fun vid -> fromvi.vid = vid) preceding_call_lv in
          if doadd then
            preceding_call_lv <- setvi.vid :: preceding_call_lv
        | _,_ -> ()
      in
      DoChildren
    | If(BinOp(Lt,e1,e2,_),bl1,bl2,loc) when (llen preceding_call_lv) > 0 ->
      begin
        match get_varinfo_exp e1 with
          Some(evi) ->
          if List.exists (fun vid -> evi.vid = vid) preceding_call_lv then
            begin
              in_interesting_if <- true ;
              ChangeDoChildrenPost(s,(fun s -> in_interesting_if <- false;s))
            end
          else DoChildren
        | None ->  DoChildren
      end
    | Goto(stmtref,loc) when in_interesting_if && not (is_cil_label !stmtref) ->
      let cur_lablename = get_label_name !stmtref in
      let different_stmts = random_order (lfilt (fun (sid,sref) ->
          not (comp_str cur_lablename (get_label_name !sref) || (is_cil_label !sref))
        ) gotos) in
      if (llen different_stmts) > 0 then begin
        let _,newstmt = List.hd different_stmts in
        preceding_call_lv <- [];
        retval := (s,newstmt,loc) :: !retval;
      end; DoChildren
    | _ -> DoChildren

end


(* only called on functions without a void return type *)
class template01Pattern02 goto_ret_htbl stmts retval2  = object(self)
  inherit nopCilVisitor

  val mutable prec_if_with_goto = []
  val mutable prec_srefId = []
  val mutable prec_goto = []
  val mutable prec_goto_line = (-1114)

  method vstmt s =
    let htable_size tb =
      let counter = ref [] in
      let _ = hiter(fun k v ->
          match v with
          | (s,loc) when not (s.sid == (-1114)) -> counter := k::!counter
          | _ -> ()
        ) tb in
      llen (uniq !counter)
    in
    let _ =
      match s.skind with
      | If(exp,bl1,bl2,loc) -> prec_if_with_goto <- get_goto bl1
      | Goto(sref,loc) when (llen prec_if_with_goto) > 0 ->
        if (llen !sref.labels) > 0 then
          prec_goto_line <- get_line_label !sref ;
        prec_srefId <- !sref.sid::prec_srefId;
        prec_goto <- s::prec_goto
      | _ -> ()
    in
    let _ =
      (* reach at the label which does not include a Return statement. *)
      if (llen prec_srefId) > 0 && (htable_size goto_ret_htbl) < 2 then (begin
          let line_label = get_line_label s in
          if line_label <> (-1114) && line_label == prec_goto_line then (begin
              let prec_goto_stmt = lhead prec_goto in
              (* this labeled statement has not return. *)
              if not ( has_return_in_goto s goto_ret_htbl) then
                (begin
                  ignore(List.fold_left (fun bs st ->
                      if st.sid == prec_goto_stmt.sid then true
                      else if st.sid == s.sid then false
                      else if bs then
                        let _ =
                          match st.skind with
                          | Return _ | Goto _ -> ()
                          | _ -> retval2 := (s,!currentLoc)::!retval2;
                            prec_srefId <- [];prec_goto <- []
                        in
                        bs
                      else bs
                    ) false stmts)
                end)
            end)
        end)
    in
    DoChildren
end


let template01 get_fun_by_name fd =
  (* *************************************************************
     let gotos = visitFnGetList (new collectGotosVisitor) fd in
     let one_ele (s,new_stmt,loc) =
      let rep_stmt = mkStmt (Goto(new_stmt,loc)) in
      s.sid,rep_stmt
     in
      template (new template01Visitor gotos) one_ele fd
    ************************************************************* *)
  let gotos = visitFnGetList (new collectGotosVisitor) fd in
  let labels_infunc =
    lfoldl (fun acc (sid,sref) ->
        match !sref.labels with
          Label(labelstr,_,_) :: rest -> labelstr :: acc
        | _ -> acc) [] gotos
  in

  let goto_ret_htbl = hcreate 255 in
  let _ = ignore(visitCilFunction(new labelAndRetVisitor goto_ret_htbl) fd) in
  let stmts = lrev (visitFnGetList stmtVisitor fd) in
  let retval1 =
    if (llen labels_infunc == 2) &&
       (llen  (uniq labels_infunc)) == 1 then
      visitFnGetList  (new template01Pattern01 gotos) fd
    else []
  in
  if (llen retval1) > 0 then
    pre_template retval1 (fun (s,new_stmt,loc) -> s.sid,mkStmt (Goto(new_stmt,loc)))
  else if (not (isVoidTFun fd)) && ((llen (uniq labels_infunc)) == 2) then
    let one_ele (stmt,loc) =
      let label_nm = get_label_name stmt in
      (* find the other label name. *)
      let other_nm = hfold(fun k v other_nm ->
          if not (comp_str k label_nm) then k else other_nm) goto_ret_htbl "" in
      (* find the statement which is annotated with the found label. *)
      let sref = lfoldl(fun sref s ->
          if (llen s.labels) > 0 then begin
            let cur_label_nm = get_label_name s in
            if comp_str cur_label_nm other_nm then s
            else sref
          end  else sref
        ) (mkEmptyStmt()) stmts in
      (* make a new Goto statement to insert in a location before the given statement. *)
      let newGotoStmt = mkStmt(Goto(ref sref,lu)) in
      stmt.sid, {newGotoStmt with skind = Block (mkBlock([newGotoStmt;stmt]))}
    in
    template (new template01Pattern02 goto_ret_htbl stmts) one_ele fd
  else IntMap.empty

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

  method vlval lv = retval := (lval_str lv) :: !retval; DoChildren
end


let get_arithmetictype_var lst = lfilt(fun vi -> isArithmeticType vi.vtype) lst
let get_arithmetictype_fld lst = lfilt(fun fi -> isArithmeticType fi.ftype) lst


class usedFieldVisitor retval = object
  inherit nopCilVisitor

  method voffs (o:offset) =
    match o with
    | Field (f, NoOffset) -> retval := f::!retval; DoChildren;
    | _ -> DoChildren;
end


class exprVisitor retval = object
  inherit nopCilVisitor

  method vexpr e =
    match e with
    | BinOp(Mult,exp1,exp2,_) ->
      let lvs = ref [] in
      let my_collect = new collectLvals lvs in
      (* FIXME: this is totally not how I want to do this, hm *)
      ignore(visitCilExpr my_collect exp1);
      ignore(visitCilExpr my_collect exp2);

      let ret_var1 = visitExprGetList (new usedVarVisitor) exp1 in (* for the first expression. *)
      let ret_fld1 = visitExprGetList (new usedFieldVisitor) exp1 in (* for the second expression. *)

      (* filter out not arithmetic type variables. *)
      let ret_val1_arith = get_arithmetictype_var ret_var1 in
      let ret_fld1_arith = get_arithmetictype_fld ret_fld1 in

      let ret_var2 = visitExprGetList (new usedVarVisitor) exp2 in
      let ret_fld2 = visitExprGetList (new usedFieldVisitor) exp2 in

      let ret_val2_arith = get_arithmetictype_var ret_var2 in
      let ret_fld2_arith = get_arithmetictype_fld ret_fld2 in

      let x1 = (llen ret_val1_arith) == 1 || (llen ret_fld1_arith) == 1  in
      let x2 = (llen ret_val2_arith) == 1 || (llen ret_fld2_arith) == 1  in
      if x1 && x2 then begin
        retval := (exp1,exp2,!lvs)::!retval;
        SkipChildren
      end else DoChildren
    | _ -> DoChildren
end

class lvalVisitor retval retfld = object
  inherit nopCilVisitor

  method vlval lval =
    match lval with
    | _, Field(fi, _) -> retfld := fi::!retfld; DoChildren
    | Var vi, _ -> retval := vi::!retval; DoChildren
    | _ -> DoChildren
end

(* FIXME: how to deal with fact that CIL often uses a single variable to hold
   onto something checked in an if condition?  Under which circumstances does
   CIL do that? We think it only does it with a function call, so it may not
   matter *)

class template02Visitor retval = object
  inherit nopCilVisitor

  val mutable preceding_exp_info = None

  method vstmt s =
    let _ =
      match s.skind with
      | Instr([Set(lv, exp, location)]) ->
        let lv_retval = ref [] in
        let lv_retfld = ref [] in
        ignore(visitCilLval (new lvalVisitor lv_retval lv_retfld) lv);

        let lv_retval_arith = get_arithmetictype_var !lv_retval in
        let lv_retfld_arith = get_arithmetictype_fld !lv_retfld in

        let exp_retval = visitExprGetList (new exprVisitor) exp in
        if ((llen exp_retval) > 0) &&
           ((llen lv_retval_arith) == 1 || (llen lv_retfld_arith) == 1) then
          preceding_exp_info <- Some(lv,exp_retval)
      | If(UnOp(LNot,e,t),bl1,bl2,loc) -> begin
          match preceding_exp_info with
            Some(lv,lst) ->
            let lv,lst = get_opt preceding_exp_info in
            let math_lvals = lfoldl (fun acc (_,_,c) ->  c @ acc) [] lst in
            let guard_lvals = visitExprGetList (new collectLvals) e in
            let any_overlap =
              List.exists
                (fun math_lv ->
                   List.exists (fun guard_lv -> guard_lv = math_lv) guard_lvals)
                math_lvals
            in
            let lhs_on_rhs = List.mem (lval_str lv) math_lvals in
            if any_overlap && not lhs_on_rhs then
              let exps = lmap (fun (a,b,_) -> s,lv,a,b,!currentLoc) lst in
              retval := exps@ !retval
          | None -> ()
        end
      | _ -> preceding_exp_info <- None
    in
    DoChildren
end

let template02 get_fun_by_name =
  let one_ele (s,lv,exp1,exp2,loc) =
    let divide = BinOp(Div,Lval(lv),exp1,intType) in
    let ne = BinOp(Ne,exp2,divide,intType) in
    let new_skind =
      match s.skind with
        If(guard,bl1,bl2,loc) ->
        If(BinOp(LOr,guard,ne,intType),bl1,bl2,loc)
      | _ -> failwith "major failwhale"
    in
    s.sid, ({s with skind = new_skind})
  in
  template (new template02Visitor) one_ele

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
    (* Get a relevant element from a list of arguments. *)
    let getRelevantName args nth = begin
      (* FIXME: matching on the names is probably a bad idea? *)
      let dest = exp_str (List.nth args nth) in
      try
        let par_right_index = (String.rindex dest ')') + 1 in
        let len = String.length dest in
        let len = len - par_right_index in
        let only_name = String.sub dest par_right_index len in
        trim_str only_name
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

let template03 get_fun_by_name fd =
  if not (fun_exists get_fun_by_name "__builtin_strncpy") then IntMap.empty
  else begin
    let old_directive_style = !Cil.lineDirectiveStyle in
    Cil.lineDirectiveStyle := None ;
    let retval3 = visitFnGetList (new template03Visitor) fd in
    List.fold_left
      (fun acc ((strcpy_s, strcpy_args, strcpy_loc), (strlen,loc)) ->
         (* first, strncpy *)
         let dest_exp :: src_exp :: _ = strcpy_args in
         let subtraction_exp = BinOp(MinusA,SizeOfE(dest_exp),one,intType) in
         let strncpy_lval = mk_lval (get_fun_by_name "__builtin_strncpy") in

         let arguments = [dest_exp;src_exp;subtraction_exp] in
         let strncpy_stmt =
           mkStmt (Instr([Call(None,strncpy_lval,arguments,strcpy_loc)]))
         in
         let sizeof_exp = BinOp(MinusA,SizeOfE(dest_exp),one,intType) in
         let sizeof_stmt =
           match strlen.skind with
             Instr[(Call(Some(retval),presumed_strlen,args,loc))] -> begin
               let new_inst = Set(retval,sizeof_exp,loc) in
               { strlen with skind = Instr[new_inst] }
             end
           | _ -> abort "major fail"
         in
         Cil.lineDirectiveStyle := old_directive_style;
         let acc = IntMap.add strcpy_s.sid strncpy_stmt acc in
         IntMap.add strlen.sid sizeof_stmt acc)
      (IntMap.empty) retval3
  end
(*
 * Myoungkyu Song     <mksong1117@utexas.edu>
 *
 * Template 04: paired function call checker
 * -----------
 *
 *    #2 match
 *   -----------
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

  @ File name: python-bug-70019-70023_json.c
  @ The number of edit location to be repair: 5
  @ The number of edit location that the template has fixed correctly: 5
  @ The number of edit location that the template has changed: 5
  @ An Edit Example:

+   int __cil_tmp10 ;

+   {
+   __cil_tmp10 = Py_EnterRecursiveCall("");
+   if (__cil_tmp10 != 0) {
+     return (-1);
+   }
    tmp___0 = encoder_listencode_obj(s, rval, obj, indent_level);
+   Py_LeaveRecursiveCall();
+   }

 *
 *    #2 match
 *   -----------
 *
 *
 * 1. In this match, we find a particular pattern, consisting of a structure like
 *    a Loop block including a Call instruction and a If statement. We check if
 *    the Then block of the If statement includes function calls whose names are
 *    specified in a list of predefined function names, and check if the Else
 *    block includes a Break statement.
 *
 *    When we match the If statement, we check its conditional expression whether
 *    it has a binary operation.
 *
 *    When we find the Break statement in the location we visited above, we store
 *    the location of the enclosing If statement and its expression.
 *
 * 2. In terms of the changes by this template, we remove the found If statement
 *    along with its Then body block except a Break statement in the Else block.


  Example.

  @ File name: python-bug-69609-69616-_collectionsmodule.c

*** 1572,1589 ****
              }
              Py_DECREF(key);
          }
      } else {
          while (1) {
              key = PyIter_Next(it);
              if (key == NULL) {
-                 if (PyErr_Occurred() && PyErr_ExceptionMatches(PyExc_StopIteration))
-                     PyErr_Clear();
-                 else
                      break;
              }
              oldval = PyObject_GetItem(mapping, key);
              if (oldval == NULL) {
                  if (!PyErr_Occurred() || !PyErr_ExceptionMatches(PyExc_KeyError))
                      break;
                  PyErr_Clear();
                  Py_INCREF(one);

 *
 *    #3 match
 *   -----------
 *
 * 1. In this match, we find a particular pattern of a macro. Without purely
 *    relying on the name of the macro, we match a structure consisting of two
 *    subsequent instructions, an If statement and a Loop statement. Regarding
 *    to the relationship of these If and Loop statements, an IF body contains
 *    a Loop statement. Additionally we check if the Loop body includes another
 *    If statement.
 *
 * 2. In terms of the changes by this template, we remove the found Loop
 *    statement along with all statements in its body. If this removes other
 *    irrelevant code fragments, we will add more restrictions such as naming of
 *    function calls.


  Example.

  @ File name: php-bug-2012-03-06-6237456cae-5e80c05deb-fileinfo.c

*** 274,294 ****

    {
    object = this_ptr;
    tmp___0 = zend_parse_parameters();
    tmp = (int __attribute__((__visibility__("default")))  )tmp___0;
    if (tmp == (int __attribute__((__visibility__("default")))  )-1) {
/*    macro     */
-     while (1) {
-       if (object) {
-         zend_object_store_ctor_failed();
-         zval_dtor();
-         object = (zval * )0;
-       }
      while (1) {
        __z = return_value;
        __z->value.lval = 0L;
        __z->type = (char)3;
        break;


 *
 *    #4 match
 *   -----------
 *
 *
 * 1. In terms of this match, we use specification that we predefined where we
 *    match the function name of a call which returns a value which is assigned
 *    to a variable. In the preceding statement, we check if there is a function
 *    call that does not have any returned variable.
 *
 * 2. Next we check whether there is an If statement whose expression uses a
 *    variable that is returned from a function call that we found above. At this
 *    point, we store the subsequent two function calls that appear until this
 *    If statement, and the current statement.
 *
 *    In terms of changes by this template, we switch the order of these two
 *    function calls, while removing the If statement, we found above, using the
 *    variable that the second function call returns.
 *
 * 3. Similarly, we find two subsequent function calls where the first call does
 *    not have a returned variable, and the second call does the one that is used
 *    in an If conditional expression.
 *
 *    Regarding to changes, we remove statements in the Then block, add the first
 *    function call we found above, and make the Else block inserting error
 *    handing code fragments that we find at the beginning part of the enclosing
 *    function.


  Example.

  @ File name: php-bug-2012-02-12-3d898cfa3f-af92365239-array.c

*** 1560,1580 ****
    }

    /* allocate an array for return */
    array_init_size(return_value, num);

    num--;
-   zval_add_ref(&val);
-   if (zend_hash_index_update(Z_ARRVAL_P(return_value), start_key, &val, sizeof(zval * ), NULL) == FAILURE) {
-     zval_ptr_dtor(&val);
-   }

+   zend_hash_index_update(Z_ARRVAL_P(return_value), start_key, &val, sizeof(zval * ), NULL)
+   zval_add_ref(&val);


-   zval_add_ref__((void * )(& val));
    tmp___1 = zend_hash_next_index_insert__(start_key);
-   if (tmp___1 == 0) {
-     zval_ptr_dtor__((void * )(& val));
-   }

    tmp___1 = zend_hash_next_index_insert__(start_key);
    if (tmp___1 == 0) {
+     zval_add_ref__((void * )(& val));
+   } else {
+     php_error_docref__();
+     return;
+   }

*)

let predefined_fname_list_pt1 = [ "encoder_listencode_obj";"encoder_listencode_dict";
                                  "encoder_listencode_obj";"__tmp__foo__"]
let predefined_fname_list_pt2 = [ "zend_hash_index_update";"zval_ptr_dtor";"zend_hash_index_update__";"zval_ptr_dtor__";"_zend_hash_index_update_or_next_insert";"_zval_ptr_dtor" ]
let predefined_fname_list_pt3 = [ "zend_hash_next_index_insert";"zval_ptr_dtor";"zend_hash_next_index_insert__";"zval_ptr_dtor__" ]
let predefined_fname_prefix = ["PyErr";"PyIter"]


class usedPointerVisitor usedVars = object
  inherit nopCilVisitor

  method vlval = function
    | Mem exp, Field _ -> (* debug "%s\n" (exp_str exp); *)
      let retval = visitExprGetList (new usedVarVisitor) exp in
      let ptrval = lfilt (fun v -> isPointerType v.vtype) retval in
      let _ = usedVars := ptrval in
      DoChildren
    | _ -> DoChildren
end

class chkIfThenElseBlkVisitor retval = object
  inherit nopCilVisitor

  method vstmt s =
    let is_empty_block bl = begin
      if (llen bl.bstmts) == 1 then begin
        let str = stmt_str (lhead bl.bstmts) in
        comp_str str ""
      end else false
    end in
    match s.skind with
    | If (_,bl1,bl2,_) ->
      let retvalbrk2 = visitBlkGetBool chkBrkStmtVisitor bl2 in
      if ((is_empty_block bl1) && retvalbrk2) ||
         (not (is_empty_block bl2)) then (retval := false; SkipChildren)
      else DoChildren
    | _ -> DoChildren
end

class template04Pattern01 retval1 = object
  inherit nopCilVisitor
  method vstmt s =
    match s.skind with
    | Instr([Call(Some (Var(_), NoOffset),Lval(Var(vi),NoOffset),arguments,loc)])
      when (List.mem vi.vname predefined_fname_list_pt1) ->
      retval1 := (s,!currentLoc) :: !retval1; DoChildren
    | _ -> DoChildren
end

class template04Pattern02 retval2 = object
  inherit nopCilVisitor
  val vi_call_ht = hcreate 255
  val preceding_loop = ref []
  val preceding_loop_blk = ref []
  val loop_if_list = ref []
  val lp_if_ht = hcreate 255

  method vstmt s =
    let has_binop exp = visitExprGetBool boExprVisitor exp in
    let has_break blk = visitBlkGetBool chkBrkStmtVisitor blk in

    let has_var_call exp =
      let retPtrUsedVar = visitExprGetList (new usedPointerVisitor) exp in
      let retUsedVar = visitExprGetList (new usedVarVisitor) exp in
      (llen retPtrUsedVar) == 0 && (List.exists (fun vi -> hmem vi_call_ht vi.vid) retUsedVar)
    in

    let has_cur_if loopst curst = begin
      match loopst.skind with
      | Loop (blk,_,_,_) -> visitBlkGetBool (blkLoopVisitor curst.sid) blk
      | _ -> false
    end in

    let _ =
      match s.skind with
      | Instr([Call(Some (Var(vi), NoOffset),fun_exp,arguments,loc)]) ->
        let fname = exp_str fun_exp in
        liter(fun prefix ->
            try
              if contains fname prefix then
                hadd vi_call_ht vi.vid ()
            with
            | _ -> ()
          ) predefined_fname_prefix
      | Loop (blk,loc,_,_) ->
        preceding_loop := s::!preceding_loop;
        preceding_loop_blk := blk::!preceding_loop_blk;
      | If(exp,bl1,bl2,loc) when has_binop exp && has_call bl1 && has_break bl1 ->
        if (has_var_call exp) && (llen !preceding_loop) > 0 then begin
          let pre_loop = lhead !preceding_loop in
          if has_cur_if pre_loop s then begin
            loop_if_list := (pre_loop,s)::!loop_if_list;
            hadd lp_if_ht pre_loop s;
          end;
        end;
      | Break(loc) -> (begin
          (* see if the expression includes the binary operation. *)
          match !loop_if_list with
            (lpSt,ifSt) :: rest -> (begin
              if hmem lp_if_ht lpSt then (begin
                  let foundIfSt  = hfind lp_if_ht lpSt in
                  let If(exp,bl1,bl2,loc) = foundIfSt.skind in
                  (* foundIfSt includes an if statement whose Then block contains a Call. *)
                  let retval = visitBlkGetBool chkThenBlockIfStmtVisitor bl1 in
                  if retval then
                    retval2 := (foundIfSt,exp,loc)::!retval2
                end)
            end)
          | _ -> ()
        end)
      | _ -> ()
    in
    DoChildren
end

class template04Pattern03 retval3 = object
  inherit nopCilVisitor
  (* used for the 3rd pattern. *)
  val prec_set_vi = ref []
  (* OK, there are other places in template04 that prec_call_vi is set, but it
     looks to CLG as though those places should not be relevant to pattern 03
     and so she didn't move them to match here, nor did she move the ones here
     into the other patterns.  It's possible that this is subtly incorrect, so
     if template04 is acting funny, check my work...*)
  val prec_call_vi = ref []
  val prec_ifSt = ref []
  val prec_loopSt = ref []

  method vstmt s =
    (* check the given block if it includes a Return statement. *)
    let has_voidreturn = visitBlkGetBool (new chkConstructVisitor "void_return") in
    (* check the given expression if it includes at least an element in a set of variables. *)
    let has_var_call exp vars = begin
      let retUsedVar = visitExprGetList (new usedVarVisitor) exp in
      List.exists (fun vid ->
          List.exists (fun vii -> vid == vii.vid) retUsedVar
        ) vars
    end in
    let has_loop_if loopBlk = visitBlkGetBool chkIfStmtExprVisitor loopBlk in
    (* check the given block if it includes a Set instruction. *)
    let has_sets bl1 =
      let retval = visitBlkGetList chkSetStmtVisitor bl1 in
      (llen retval) > 0
    in
    (* the If includes loop? *)
    let has_if_loop ifSt st =
      match ifSt.skind with
      | If(exp,bl1,bl2,loc) -> visitBlkGetBool (cmpSidVisitor st.sid) bl2
      | _ -> false
    in
    (* instead of the naming pair, the following uses a pair of the execution flow..*)
    match s.skind with
    | Instr([Set((Var(vi), NoOffset),e,l)]) -> prec_set_vi := vi.vid::!prec_set_vi ; DoChildren
    | Instr([Call(Some (Var(vi), NoOffset),_,_,_)]) when (llen !prec_set_vi) > 0 ->
      prec_call_vi := vi.vid::!prec_call_vi; DoChildren
    | If(exp,bl1,bl2,loc) when (has_voidreturn bl1) &&
                               ((has_var_call exp !prec_set_vi) || (has_var_call exp !prec_call_vi)) ->
      prec_ifSt := s::!prec_ifSt; DoChildren
    | Loop(bl,loc,None,None) when (has_loop_if bl) && (has_sets bl) ->
      if (llen !prec_ifSt) > 0 && (has_if_loop (lhead !prec_ifSt) s) then begin
        (* check inner If statements. *)
        let ifstmts = visitBlkGetList chkIfStmtVisitor bl in
        (* check block: the block includes an If statement, whose Then block
           should not be empty and whose Else block should not include a Break
           statement should not be empty. *)
        let xretval = ref true in
        let _ = ignore(visitCilBlock (new chkIfThenElseBlkVisitor xretval) bl) in
        (* check loop existence. *)
        let hasloop = visitBlkGetBool chkLoopStmtVisitor bl in
        (* check empty body. *)
        if !xretval && (not hasloop)  && (llen ifstmts) < 2 then
          retval3 := (s,loc)::!retval3
      end; DoChildren
    | _ -> DoChildren
end

class template04Pattern04 retval4 retval7 = object
  inherit nopCilVisitor

  (* these are also used for pattern03, but it looked like the information
     should be tracked separately for each pattern.  So I didn't copy both
     assignments between patterns, but rather separated out the usage *)
  val prec_call_vi = ref []
  val prec_call = ref []
  val prec_call_predefined_2 = ref []
  val prec_ifSt_blk = ref []

  (* used for the 4/5/6th pattern. *)
  val prec_call_predefined = ref []

  method vstmt s =
    let has_varId_call exp varIDs = begin
      let retUsedVar = visitExprGetList(new usedVarVisitor) exp in
      List.exists (fun vi -> lmem vi.vid varIDs) retUsedVar
    end in
    let _ =
      (* the naming pair using a list of the predefined function calls. *)
      match s.skind with
      | Instr([Call(None,_,_,_)]) -> prec_call := s::!prec_call;
      | Instr([Call(Some (Var(vi), NoOffset),exp,_,loc)])
        when lmem (exp_str exp) predefined_fname_list_pt2 ->
        prec_call_predefined := s::!prec_call_predefined;
        prec_call_vi := vi.vid::!prec_call_vi;
      | If(exp,bl1,bl2,loc)
        when (llen !prec_call_predefined) > 0 && (has_varId_call exp !prec_call_vi) ->
        if (llen !prec_call) > 0 then begin
          match !prec_call_predefined with
            fst_call :: rest ->
            prec_call_predefined := rest;
            let snd_call = lhead !prec_call in
            (* addition of the two preceding calls by switching the order. *)
            (* CLG notes: this used to be 3 retvals, but they were redundant *)
            retval4 := (s,fst_call,snd_call)::!retval4
          | [] -> ()
        end
      | Instr([Call(Some (Var(vi), NoOffset),exp,_,loc)])
        when lmem (exp_str exp) predefined_fname_list_pt3 ->
        prec_call_predefined_2 := s::!prec_call_predefined_2;
        prec_call_vi := vi.vid::!prec_call_vi
      (* let snd_call = (lhead !prec_call) in
         debug "***\t %s\n" (stmt_str s);
         debug "---\t %s\n" (stmt_str snd_call); *)
      | If(exp,bl1,bl2,loc)
        when (llen !prec_ifSt_blk) > 0 && (llen !prec_call) > 0 &&
             (llen !prec_call_predefined_2) > 0 && (has_varId_call exp !prec_call_vi) ->
        let filtered_ifSt_blk = lrev (lfilt(fun b ->
            visitBlkGetBool chkRetStmtVisitor b)
            !prec_ifSt_blk) in
        let filtered_ifSt_blk = lfoldl(fun acc b ->
            lapnd b.bstmts acc
          ) [] filtered_ifSt_blk in
        let prv_ifSt_blk = mkBlock(filtered_ifSt_blk) in
        let prv_call = lhead !prec_call in
        retval7 := (s,exp,prv_call,prv_ifSt_blk,loc)::!retval7
      | If(exp,bl1,bl2,loc) -> prec_ifSt_blk := bl1::!prec_ifSt_blk
      | _ -> ()
    in
    DoChildren
end

let template04 get_fun_by_name fd =
  (* ***********************************************************
     let one_ele (s,loc,name) =
      let ret_var = makeTempVar fd intType in
      let _, prefix, suffix =
        List.hd (List.filter (fun x -> (fst3 x) = name) !paired_functions)
      in
      let enter_exp = mk_lval (get_fun_by_name prefix) in
      let leave_exp = mk_lval (get_fun_by_name suffix) in

      let enter_call = mkStmt (Instr([Call(Some(Var(ret_var),NoOffset),enter_exp,[Const(CStr(""))],loc)])) in
      let leave_call = mkStmt (Instr([Call(None,leave_exp,[],loc)])) in

      let guard = BinOp (Ne,Lval(Var(ret_var),NoOffset),zero,intType) in
      let ret_stmt = mkStmt (Return(Some mone,loc)) in
      let block1 = mkBlock([ret_stmt]) in
      let block2 = mkBlock([]) in
      let if_stmt = mkStmt (If(guard,block1,block2,loc)) in
      let block = {s with skind =
          Block(mkBlock
                  [enter_call; if_stmt;{s with sid = 0 };leave_call]) }
      in
        s.sid, block
     in
      template (new template04Visitor) one_ele fd
   *********************************************************** *)
  let retval1 =
    if (fun_exists get_fun_by_name "Py_EnterRecursiveCall")  &&
        (fun_exists get_fun_by_name  "Py_LeaveRecursiveCall") then
      visitFnGetList (new template04Pattern01) fd
    else [] in
  if (llen retval1) > 0 then begin
    let one_ele (stmt,loc) =
      let ret_var = makeTempVar fd intType in
      let enter_exp = mk_lval (get_fun_by_name  "Py_EnterRecursiveCall") in
      let leave_exp = mk_lval (get_fun_by_name "Py_LeaveRecursiveCall") in

      let enter_call = mkStmt (Instr([Call(Some(Var(ret_var),NoOffset),enter_exp,[Const(CStr(""))],loc)])) in
      let leave_call = mkStmt (Instr([Call(None,leave_exp,[],loc)])) in

      let guard = BinOp (Ne,Lval(Var(ret_var),NoOffset),zero,intType) in
      let ret_stmt = mkStmt (Return(Some mone,loc)) in
      let if_stmt = mkStmt (If(guard,mkBlock([ret_stmt]),mkBlock([]),loc)) in
      stmt.sid, mkStmt (Block(mkBlock [enter_call;if_stmt;stmt;leave_call])) in
    pre_template retval1 one_ele
  end else
    let retval2 = visitFnGetList (new template04Pattern02) fd in
    if (llen retval2) > 0 then begin
      let one_ele (stmt,exp,loc) =
        let brk = {(mkEmptyStmt()) with skind = Break(lu)} in
        stmt.sid, {stmt with skind = If(exp,mkBlock([brk]),mkBlock([]),loc)}
      in
      pre_template retval2 one_ele
    end else
      let retval3 = visitFnGetList (new template04Pattern03) fd in
      if (llen retval3) > 0 then
        pre_template retval3
          (fun (stmt,loc) -> stmt.sid, {stmt with skind = Block (mkBlock([]))})
      else
        let retval4 = ref [] in
        let retval7 = visitFnGetList (new template04Pattern04 retval4) fd in
        let newstmts =
          lfoldl(fun map(s,stmt,addedst) ->
              let newstmt = {stmt with skind = Block (mkBlock([stmt;addedst]))} in
              let map' = IntMap.add stmt.sid newstmt map in
              let newstmt = {stmt with skind = Block (mkBlock([]))} in
              let map'' = IntMap.add addedst.sid newstmt map' in
              let newstmt = {stmt with skind = Block (mkBlock([]))} in
              IntMap.add s.sid newstmt map''
            ) (IntMap.empty) !retval4
        in
        lfoldl(fun map(stmt,exp,call,blk,loc) ->
            let newstmt = {stmt with skind = If(exp,mkBlock([call]),blk,loc)} in
            let map' = IntMap.add stmt.sid newstmt map in
            let newstmt = {stmt with skind = Block (mkBlock([]))} in
            IntMap.add call.sid newstmt map'
          ) newstmts retval7

(*
 * Myoungkyu Song     <mksong1117@utexas.edu>
 *
 * Template 05: wrong condition remover
 * -----------
 *
 *    #1 match
 *   -----------
 *
 * 1. We find an If statement in which its conditional expression is wrong and
 *    is necessary to be removed, while we would like to keep the statements
 *    within its body blocks.
 *
 *    We have a rule that detects a particular If statement that we described
 *    above: (1) we, first, take look at Call statements, which returns a value
 *    and assigns it to a variable, which we store to match with variables in
 *    an If conditional expression, and (2) we, then, look for an If continual
 *    expression that compares the variable that we found above with zero.
 *
 *    Lack of information in bug reports describing why it's a fault, but I
 *    would guess, reviewing the function definition, the statements in the body
 *    of the If statement should always be executed regardless of the returned
 *    value.
 *
 *    Need to add more constraints like checking statements of the If statement
 *    that found above.
 *
 * 2. Based on given information, we modify the If statement that we found by
 *    replacing it with a list of statements in its body block.
 *    We will take care of in case when there appears an else-block that is not
 *    empty.

  @Example 1:

   iterator = spl_filesystem_object_to_iterator(dir_object);

-  if ((unsigned long )iterator->intern.data == (unsigned long )((void * )0)) {
-    iterator->intern.data = (void * )object;
-    iterator->intern.funcs = & spl_filesystem_dir_it_funcs;
-    iterator->current = object;
-  } else {
-
-  }
+  iterator->intern.data = (void * )object;
+  iterator->intern.funcs = & spl_filesystem_dir_it_funcs;
+  iterator->current = object;

 *
 *    #2 match
 *   -----------
 *
 *
 *  1. In terms of this match, we take a look at an inner If statement that five
 *     or six If Then blocks wrap. We check whether this If statement does not
 *     include any Loop statement in its Else block but does a Loop statement in
 *     its Then block.
 *
 *     We store the Then block of its second child If statement, removing the
 *     Return statement that appear at the last, which we call X. We store the
 *     Else block of its first child If statement, which we call Y.
 *
 *  2. With regard to changes by this template, we replace the If Then block, which
 *     we found, appending Y after X.


  @Example 2:

    see a detail example at the bottom part of this file.

      tmp___1 = __builtin_expect((long )tmp___0, 1L);
   -  if (tmp___1) {
   -    if ((unsigned int )get_props == (unsigned int )(& zend_std_get_properties)) {
   -      zobj = (zend_object * )(executor_globals.objects_store.object_buckets + pz->value.obj.handle)->bucket.obj.object;
   -      if (! zobj->properties) {
            n = (zobj->ce)->default_properties_count;
            while (1) { /* .. */ }
            i = 0;
            while (i < n) { /* .. */ }
   -        return;
   -      } else {
   -        p = (zobj->properties)->pListHead;
   -      }
   -    } else {
          tmp = ( *get_props)(pz);
          props = tmp;
          if (! props) {
            return;
          }
          p = props->pListHead;
   -    }
   -  }

 *)


class stmtLoopVisitor stmt retval = object
  inherit nopCilVisitor

  method vstmt s =
    let _ =
      (match s.skind with
       | Loop _ when s.sid <> stmt.sid -> retval := s::!retval
       | _ -> ())
    in
    DoChildren
end


class findHoleVisitor bgnStmt endStmt retHoles = object
  inherit nopCilVisitor

  val found = ref false

  method vstmt s =
    if s.sid == endStmt.sid then begin
      found := false;
      SkipChildren
    end else
    if !found then begin
      retHoles := s::!retHoles;
      DoChildren;
    end else
    if s.sid == bgnStmt.sid then begin
      ChangeDoChildrenPost(s, (fun s -> ignore(found := true) ; s))
    end else DoChildren
end

let get_thenblock stmt = begin
  match stmt.skind with
  | If (e,b1,b2,loc) -> b1
  | _ -> mkBlock([])
end

let del_lastreturn bl = begin
  let rec get_lastone head rest =
    match rest with
    | h::[] -> h
    | h::t -> get_lastone (lhead t) (List.tl t)
    | [] -> head
  in
  (* let _ = liter(fun s -> debug "~~~ %s\n" (one_line (stmt_str s))) bl.bstmts in *)

  let lastone = get_lastone (lhead bl.bstmts) (ltail bl.bstmts) in
  match lastone.skind with
  | Return _ ->
    (* ___dbug___; *)
    let newstmts = ltail (lrev bl.bstmts) in
    let newstmts = lrev newstmts in
    (* let _ = liter (fun s -> debug "### %s\n" (one_line (stmt_str s))) newstmts; in *)
    let newbl = mkBlock newstmts in
    newbl;
  | _ -> (mkBlock([]))
end

let get_elseblock stmt = begin
  match stmt.skind with
  | If (e,b1,b2,loc) -> b2
  | _ -> mkBlock([])
end

let get_nth_ifstmt nth stmts = begin
  let inc = ref 0 in
  let stmts = lrev(stmts) in
  let nth_ifstmt =
    lfoldl (fun acc s ->
        match s.skind with
        | If _ ->
          let _ =
            inc := (!inc + 1);
          in
          if !inc == (nth + 1) then
            s::acc
          else
            acc
        | _ -> acc
      ) [] stmts in
  if (llen nth_ifstmt) > 0 then
    (lhead nth_ifstmt)
  else
    (mkEmptyStmt())
end

class chkStmtInBlkVisitor  stmt hasStmt = object
  inherit nopCilVisitor

  method vstmt s =
    match s.skind with
    | If _ when (stmt.sid == s.sid) -> hasStmt := true; DoChildren
    | _ -> DoChildren
end

class chkStackIfStmtWalkVisitor retval = object
  inherit nopCilVisitor

  val stack = ref []

  method vstmt s =
    let has_stmt_inBlock stmt blk =
      visitBlkGetBool(new chkStmtInBlkVisitor stmt) blk
    in

    let get_ifStmtParents stmts s =
      let ifStmtParents = lfilt(fun st ->
          match st.skind with
          | If (_,bl1,_,_) -> has_stmt_inBlock s bl1
          | _ -> false
        ) stmts in
      ifStmtParents in

    match s.skind with
    | If _ -> let _ = stack := s::!stack in
      ChangeDoChildrenPost(s, (fun s -> ignore(
          if (llen (ltail !stack)) > 0 then stack := (ltail !stack)
          else stack := [];
          if (llen !stack) > 0 then begin
            let ifStmtParents = get_ifStmtParents !stack s in
            retval := (s,ifStmtParents)::!retval
          end
        ) ; s))
    | _ -> DoChildren
end

let get_ifContainment fd numPar numOrder = begin
  let retval_containment = visitFnGetList (new chkStackIfStmtWalkVisitor) fd in
  let nth_ifstmt = lfoldl(fun acc(s,ifStmtParents) ->
      if (llen ifStmtParents) == numPar then s::acc else acc
    ) [] retval_containment in
  if (llen nth_ifstmt) > (numOrder - 1) then (lnth nth_ifstmt (numOrder - 1))
  else (mkEmptyStmt())
end

let rec is_empty_block b =
  match b.bstmts with
  | [s] ->
    (match s.skind with
     | Block(b') -> is_empty_block b'
     | Instr([]) -> true
     | _         -> false)
  | []            -> true
  | _             -> false

class template05Pattern01 retval1 = object
  inherit nopCilVisitor

  val mutable preceding_call_lv = []

  method vstmt s =
    let check_compare_zero e1 e2 =
      if isZero e2 then
        match get_varinfo_exp e1 with
        | Some(evi) ->
          List.exists (fun vid -> evi.vid = vid) preceding_call_lv
        | None -> false
      else
        false
    in
    match s.skind with
    | Instr([Call(Some ret, Lval((Var(v),o)), args, loc)]) ->
      (match get_varinfo_lval ret with
       | Some(vi) -> preceding_call_lv <- [vi.vid]
       | None -> ()); DoChildren
    | Instr([Set(lv, exp1, loc)]) when (llen preceding_call_lv) > 0 ->
      (match (get_varinfo_lval lv), (get_varinfo_exp exp1) with
       | Some(setvi), Some(fromvi) ->
         let doadd =
           List.exists (fun vid -> fromvi.vid = vid) preceding_call_lv
         in
         if doadd then
           preceding_call_lv <- setvi.vid :: preceding_call_lv
       | _ -> ()); DoChildren
    | If(BinOp(Eq,e1,e2,_), bl1, bl2, loc)
      when (llen preceding_call_lv) > 0 && (is_empty_block bl2)
           && ( (check_compare_zero e1 e2) || (check_compare_zero e2 e1) )
           && (not (has_call bl1)) ->
      let num_setstmt = llen (visitBlkGetList chkSetStmtVisitor bl1) in
      let num_ifstmts = llen (visitBlkGetList chkIfStmtVisitor bl1) in
      (* check the left-hand of each Set statement if same variables are used. *)
      if (num_setstmt > 2) && (num_setstmt == (llen bl1.bstmts)) && (num_ifstmts == 0) then
        (* get a list of unique variable names, and create a hash table that holds
           variable name as a key and the number of occurrence as a value. *)
        let names_lefthand_set = (lfoldl(fun acc s ->
            match s.skind with
            | Instr([Set((Mem ex, (Field (fi,_))), exp, loc)]) -> (exp_str ex) :: acc
            | _ -> acc
          ) [] bl1.bstmts)
        in
        let uniqnms = uniq names_lefthand_set in
        let morethanthree =
          List.exists (fun nm ->
              let filtered = lfilt(fun e -> comp_str e nm) names_lefthand_set in
              (llen filtered) > 2
            ) uniqnms in
        if morethanthree then begin
          retval1 := (s, bl1) :: !retval1;
          SkipChildren
        end else DoChildren
      else DoChildren
    | _ -> DoChildren
end

class template05Pattern02 (nth_ifstmt1,nth_ifstmt2) retval2 = object
  inherit nopCilVisitor

  method vstmt s =
    (* start to investigate starting from here at nth statement. *)
    if not ((s.sid == nth_ifstmt1.sid) || (s.sid == nth_ifstmt2.sid)) then DoChildren
    else
      match s.skind with
      | If (exp,bl1,bl2,loc) ->
        (* check this If block includes two LOOP statements. *)
        let loopstmts_then = visitBlkGetBool chkLoopStmtVisitor bl1 in
        let loopstmts_else = visitBlkGetBool chkLoopStmtVisitor bl2 in
        let ifstmts_then = visitBlkGetList chkIfStmtVisitor bl1 in
        let ifstmts_else =  visitBlkGetList chkIfStmtVisitor bl2 in

        if loopstmts_then && (not loopstmts_else) && (llen ifstmts_then) > 1 && (llen ifstmts_else) == 0 then begin
          let ifstmts = lrev ifstmts_then in
          let ifstmt1_child = lhead ifstmts in
          let ifstmt2_child = lhead (List.tl ifstmts) in
          (* 1st : need this Then block *)
          let ifstmt2_child_thenblock = get_thenblock ifstmt2_child in
          (* 2nd : remove a Return statement if included at the last statement. *)
          let ifstmt2_child_thenblock = del_lastreturn ifstmt2_child_thenblock in
          (* 3rd : need this Else block *)
          let ifstmt1_child_elseblock = get_elseblock ifstmt1_child in
          (* 4th : combine two blocks, replacing with the current one. *)
          if not (is_empty_block ifstmt2_child_thenblock) ||
             not (is_empty_block ifstmt1_child_elseblock) then
            retval2 := (s,ifstmt2_child_thenblock,ifstmt1_child_elseblock,loc)::!retval2;
        end; DoChildren
      | _ -> DoChildren
end

let template05 get_fun_by_name fd =
  (************************************************
     let one_ele (s, bl1) =
      (* This will, of course, break the link between Gotos and this statemtent,
         but at least the labels will be kept. So the code should still compile...
       *)
      s.sid, {s with skind = Block bl1}
     in
      template (new template05Visitor) one_ele
   *************************************************)
  (* let stmts = get_stmts fd in
     let nth_ifstmt1 = get_nth_ifstmt 5 stmts in
     let nth_ifstmt2 = get_nth_ifstmt 6 stmts in
     let nth_ifstmt3 = get_nth_ifstmt 8 stmts in
     let nth_ifstmt = (nth_ifstmt1,nth_ifstmt1,nth_ifstmt1) in

     three parents of the If statements with the second order, and
     four parents of the one with the second order. *)

  let retval1 = visitFnGetList (new template05Pattern01) fd in
  if (llen retval1) > 0 then
    pre_template retval1 (fun (stmt,bl1) -> stmt.sid, {stmt with skind = Block bl1})
  else
    let nth_ifstmt1 = get_ifContainment fd 3 2 in
    let nth_ifstmt2 = get_ifContainment fd 4 2 in
    let nth_ifstmt = nth_ifstmt1,nth_ifstmt2 in
    let retval2 =  visitFnGetList (new template05Pattern02 nth_ifstmt) fd  in
    let one_ele (stmt,block1,block2,location) =
      let newblstmts = lapnd block1.bstmts block2.bstmts in
      stmt.sid, {stmt with skind = Block (mkBlock(newblstmts))}
    in
    pre_template retval2 one_ele

(*
 * Myoungkyu Song     <mksong1117@utexas.edu>
 *
 * Template 06: arguments (call-by-references) checker
 * -----------
 *
 *    #1 match
 *   -----------
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

  @Example 1:

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

 *
 *    #2 match
 *   -----------
 *
 *  1. We find a particular structure that consists of a Switch, a Call, and a
 *     Return statements.
 *
 *     The defects exist in the Call arguments. We fix it by adding Set
 *     statements in the Switch default case according to our observation during
 *     manual inspection of data set.
 *
 *     In the Switch statement, we store used variables in its block. We check
 *     whether there exists a Default case in the Switch statement.
 *
 *     Under condition that we do not find a Default case in the Switch statement,
 *     we walk through the Call statement, where we take a look at its argument
 *     and collect ones whose type are integer. And we check whether one of
 *     arguments is used in the preceding Switch statement.
 *
 *     When it comes to the variable of the Return statement, we check whether
 *     the Return statement returns the variable assigned by the preceding Call.
 *
 *  2. In terms of changes by this template, we create a new Default case with a
 *     Block where there are two Set statements and a Break. We create the first
 *     Set with the found variable and a number, eight which is randomly assigned
 *     yet we decided such number according to our manual inspection of data set.
 *     We create the second Set with a variable we newly created and a number with
 *     the same reason above.


  @Example 2:

    default:
    {
+     radix = 8;
+     __cil_tmp16 = 1;
      break;
    }
    }
    if (radix != 0) {
      tmp___1 = fb_wstr_CalcDiff(p, r);
      tmp___2 = fb_WstrRadix2Longint(r, len - tmp___1, radix);
      return ((double )tmp___2);
    } else {

 *)

class vnameCollector vlist = object
  inherit nopCilVisitor

  method vlval = function
    | Var vi, NoOffset -> vlist := vi :: !vlist;
      DoChildren
    | _ -> DoChildren
end

(* visitor *)
class template06Pattern01 retval1 = object
  inherit nopCilVisitor
  val mutable preceding_call = false
  val mutable preceding_vid = -1
  val mutable preceding_info = None

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
      if (llen reffed_args) > 1 (* 0 *) then begin
        (* one pattern *)
        let str_pattern = "sec" in
        let onepattern_args = lfilt (fun vi -> contains vi.vname str_pattern) reffed_args in
        if (llen onepattern_args) == (llen reffed_args) then begin
          preceding_call <- true;
          preceding_vid <- vi.vid;
          preceding_info <- Some(s,reffed_args)
        end;

        (* one pattern *)
        let str_pattern = "_len" in
        let onepattern_args = lfilt (fun vi -> contains vi.vname str_pattern) reffed_args in
        if (llen onepattern_args) == (llen reffed_args) then begin
          preceding_call <- true;
          preceding_vid <- vi.vid;
          preceding_info <- Some(s,reffed_args)
        end
      end; DoChildren
    | If(BinOp((Eq,Lval(Var vi, os),ex2,typ)),b1,b2,loc) when
        preceding_call && vi.vid == preceding_vid && return_but_no_loop b1.bstmts ->
      let stmt,args = get_opt preceding_info
      in
      retval1 := (stmt,args,loc) :: !retval1;
      DoChildren
    | _ -> preceding_call <- false; DoChildren
end

class template06Pattern02 retval2 = object
  inherit nopCilVisitor

  val mutable is_preceding_default_label = true
  val mutable preceding_returned_call = false
  val mutable preceding_returned_vid = -1
  val mutable preceding_returned_info = None
  val mutable preceding_info_switch = None
  val mutable preceding_vars_switch = []

  method vstmt s =
    (* Add match patterns. *)
    let _ =
      match s.skind with
      | Switch (e, b, stmts, l) ->
        (* collect all variable names from Switch statement. *)
        let vnames = lmap (fun vi -> vi.vname) (visitBlkGetList (new vnameCollector) b) in
        let _ = preceding_vars_switch <- vnames in
        (* check Default-labeld statement *)
        let default_stmts = lfilt(fun st ->
            let default_label_cases =
              lfoldl (fun acc lb ->
                  match lb with
                  | Default (l) -> l::acc;
                  | _ -> acc
                ) [] st.labels
            in
            (llen default_label_cases) > 0
          ) stmts in
        if (llen preceding_vars_switch) > 0 && (llen default_stmts) == 0 then begin
          is_preceding_default_label <- false;
          preceding_info_switch <- Some(s,stmts,l);
        end
      | Instr([Call(Some (Var(vi), _),fun_exp,arguments,loc)]) when (not is_preceding_default_label) ->
        (* check the types of arguments of a function call *)
        let reffed_args = lfoldl(fun acc arg ->
            match arg with
            | Lval(Var vi,_) when isIntegralType vi.vtype -> vi :: acc
            | _ -> acc
          ) [] arguments
        in
        (* check which one of arguments is used in the preceding Switch statement. *)
        let reffed_args_A = uniq (lfilt(
            fun vi -> (lmem vi.vname preceding_vars_switch)
          ) reffed_args) in
        let reffed_args = reffed_args_A in
        if (llen reffed_args) > 0 then begin
          preceding_returned_call <- true;
          preceding_returned_vid <- vi.vid;
          preceding_returned_info <- Some(s,reffed_args)
        end
      | Return(Some ((* Lval(Var vi, _) *) exp),_) when
          (not is_preceding_default_label) && preceding_returned_call ->
        (* return Switch, its containing statements, and the location. *)
        let vlist = visitExprGetList (new vnameCollector) exp in
        if (llen vlist) > 0 then begin
          let vi = List.hd vlist in
          if vi.vid == preceding_returned_vid then begin
            let stmt_switch,stmts,loc = get_opt preceding_info_switch in
            (* return the preceding call and its arguments used in Switch. *)
            let stmt_call,reffed_args = get_opt preceding_returned_info in
            retval2 := (stmt_call,reffed_args,stmt_switch,stmts,loc) :: !retval2;
          end
        end
      | _ -> ()
    in
    DoChildren
end

let template06 get_fun_by_name fd = begin
  (***********************************************************
     let one_ele (stmt,reffed_args,loc) =
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
      let newstmt = append_after_stmt stmt new_ifs in
        stmt.sid, newstmt
     in
      template (new template06Visitor) one_ele
   ***********************************************************)
  let retval1 = visitFnGetList (new template06Pattern01) fd in
  let retval2 = visitFnGetList (new template06Pattern02) fd in

  let check_used_args = lfilt(fun (stmt_call,reffed_args,stmt_switch,stmts_in_switch,loc) ->
      let var_update = List.hd reffed_args in
      let is_used_args x args =
        let flt_args = lfilt (fun vi -> vi.vid == x.vid) args in
        (llen flt_args) > 0
      in
      is_used_args var_update fd.sformals
    ) retval2 in

  if (llen retval2) > 0 && (llen check_used_args) == 0 then begin
    let newstmts = lfoldl(fun map (stmt_call,reffed_args,stmt_switch,stmts_in_switch,loc) ->
        let is_const, vi_switch_exp =
          match stmt_switch.skind with
          | Switch (exp, _, stmts, _) ->
            (* debug "exp_str exp? %s\n" (exp_str exp); *)
            let vlist = visitExprGetList (new vnameCollector) exp in
            (contains (exp_str exp) "const"), (List.hd vlist)
          (* collect all variable names from Switch expression. *)
          (* liter (fun vi -> debug "vi? %s\n" vi.vname) !vlist *)
          | _ -> false, (makeTempVar fd intType)
        in
        let default_value = integer (8) in (* used 'specific_value_eight' intentionally yet need to get this value from configuration. *)
        let break_stmt = mkStmt (Break(lu)) in
        let default_case_stmt = begin
          if is_const then begin
            (* make the first statement. *)
            let default_case_stmt = mkStmt (Instr [Set((var (List.hd reffed_args)), default_value, lu)]) in
            (* make the second statement. *)
            let lval_switch_exp = var vi_switch_exp in
            let incr_switch_exp = increm (Lval(var vi_switch_exp)) 1 in
            let stmt_inc_switch_exp = mkStmtOneInstr (Set(lval_switch_exp, incr_switch_exp, lu)) in
            (* combine them into one and store it to some place. *)
            let default_case_stmt = { default_case_stmt with
                                      skind = Block (mkBlock [default_case_stmt;stmt_inc_switch_exp;break_stmt])
                                    } in
            default_case_stmt
          end else begin
            (* make the first statement. in this case, one statement is needed. *)
            let default_case_stmt = mkStmt (Instr [Set((var (List.hd reffed_args)), default_value, lu)]) in
            (* make the second statement. *)
            let tempVar = makeTempVar fd intType in
            let lval_tmpVar = var tempVar in
            (* used 'specific_value_eight' intentionally yet need to get this value from configuration. *)
            let default_value = integer 1 in
            let stmt_tmpVar = mkStmtOneInstr(Set(lval_tmpVar, default_value, lu)) in
            (* let default_case_stmt = mkStmt (Instr [Set(lval_tmpVar, default_value, lu)]) in *)
            let default_case_stmt = { default_case_stmt with
                                      skind = Block (mkBlock [default_case_stmt;stmt_tmpVar;break_stmt])
                                    } in
            default_case_stmt
          end
        end in
        let _ = default_case_stmt.labels <- [Default(lu)] in
        match stmt_switch.skind with
        | Switch (exp, block, stmts, location) ->
          (* update both 'block' and 'stmts' by adding Default statement. *)
          let new_stmts = List.append block.bstmts [default_case_stmt] in
          let new_block = mkBlock new_stmts in
          let new_sw = mkStmt(Switch(exp,new_block,new_stmts,loc)) in
          IntMap.add stmt_switch.sid new_sw map
        | _ -> IntMap.add stmt_switch.sid (mkEmptyStmt()) map;

      ) (IntMap.empty) retval2
    in
    newstmts
  end else (* if retval1 is empty this will default to the empty map, so the check isn't necessary *)
    lfoldl (fun map (stmt,reffed_args,loc) ->
        let new_ifs = List.map (fun vi ->
            let retblock = mkBlock ([mkStmt (Return(None,loc))]) in
            let elseblock = mkBlock ([]) in
            let new_var = Lval(Var(vi),NoOffset) in
            let guard = BinOp (Lt,new_var,zero,intType) in
            mkStmt(If(guard,retblock,elseblock,loc))
          ) reffed_args
        in
        let newstmt = append_after_stmt stmt new_ifs in
        IntMap.add stmt.sid newstmt map
      ) (IntMap.empty) retval1
end

(*
 * Myoungkyu Song     <mksong1117@utexas.edu>
 *
 * Template 07: memory leak checker
 * -----------
 *
 *    #1 match
 *   -----------
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

  @Example 1.

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
 *    #2 match
 *   -----------
 *
 *
 *  1. We find a variable used by one of the Call arguments and appeared at the
 *     right part of the Set statement within a Loop statement.
 *
 *     We fix defects by adding a function call for reseting memory space of the
 *     found variable before starting the Loop statement we found above.
 *
 *     When walking through the Call statement, we store it in a temporary place
 *     to check a subsequent Set statement.
 *
 *     When a Set statement, we check the right part if it is operated as an array
 *     accessing an element with an index of the array.
 *
 * 2.  In terms of changes by this template, we create a function call name
 *     'memset' with arguments we found above, and insert it at the location
 *     before going into a Loop statement that we found.


  @Example 2.

+ memset(srcbuffs, 0, sizeof(srcbuffs));
  while (1) {
    if ((int )s < (int )spp) {
      if ((int )s < 8) {

      } else {
        break;
      }
    } else {
      break;
    }
    srcbuffs[s] = (unsigned char * )((void * )0);
    tmp___0 = _TIFFmalloc((long )src_rowsize);
    buff = (unsigned char * )tmp___0;
    if (! buff) {
      TIFFError("... %d",(int )s);
      i = 0;
      while (i < (int )s) {
        _TIFFfree((void * )srcbuffs[i]);
        i ++;
      }
      return (0);
    } else {

    }
    srcbuffs[s] = buff;
    s = (tsample_t )((int )s + 1);
  }

 *
 *    #3 match
 *   -----------
 *
 *  1. We find a variable used in a Loop statement and appeared at the last
 *     statement of its block.
 *
 *     We fix defects by inserting a If statement as Null checker and adding a
 *     function call as memory deallocation.
 *
 *     We go through a Loop statement and collection used variables while
 *     filtering out global variables, function parameters, and ones with
 *     arithmetic type. We check if the Loop block has an If statement.
 *
 *  2. In terms of changes by this template, we insert a specific function call
 *     at the last part of the Loop statement that we found along with an If
 *     statement checking the variable's validation.


  @Example 3:

  fd_head = (fragment_data * )value;
  while ((unsigned int )fd_head != (unsigned int )((void * )0)) {
    tmp_fd = fd_head->next;
    if (fd_head->data) {
      if (! (fd_head->flags & 32U)) {
        g_free((void * )fd_head->data);
      } else {

      }
    } else {

    }
    while (1) {
      g_slice_free1(sizeof(fragment_data ), (void * )fd_head);
      break;
    }
    fd_head = tmp_fd;
+   if (fd_head == 0) {
+     g_slice_free1(sizeof(fragment_data ), fd_head);
+   }
  }
  return (1);

 *
 *)

class declVarVisitor decVars = object
  inherit nopCilVisitor

  method vvdec (vi : varinfo) =
    decVars := vi::!decVars;
    DoChildren
end

class offsetVisitor retval = object
  inherit nopCilVisitor

  method voffs o =
    match o with
    | Field (fi,NoOffset) when (isPointerType fi.ftype) ->
      retval := true; SkipChildren
    | _ -> DoChildren
end

class memFieldVisitor retval = object
  inherit nopCilVisitor

  method vlval lval =
    match lval with
    | Mem e, Field _ -> retval := e::!retval; DoChildren
    | _ -> DoChildren
end

(* Add a visitor to collect a relevant variable. *)
class chkSetCallBlockVisitor decVarIDs decVars memset_vars = object
  inherit nopCilVisitor
  val preceding_instr = ref 0
  val preceding_call = ref 0

  method vstmt s =
    match s.skind with
    (* check if a variable is local variable. *)
    | Instr([Set((Var vi,Index(expr_index_array,_)),exp,location)])
      (* check if it is assigned to an element of array. *)
      when (lmem vi.vid decVarIDs) ->
      incr preceding_instr;
      if !preceding_instr > 1 && !preceding_call > 0 then begin
        (* when satisfying all requirements, get a variable. *)
        let mset_var = lhead(lfilt(fun decVi -> decVi.vid == vi.vid) decVars) in
        let is_integer_type expr_index_array =
          match expr_index_array with (* Myoungkyu: will this work on a 32-bit system? *)
          | Const(CInt64(_,_,_)) -> true
          | _ -> false in

        (* checking the integer type in the index of the array.  *)
        if not (is_integer_type expr_index_array) then
          memset_vars := mset_var::!memset_vars;
      end;
      DoChildren
    (* check if there is a function call between assignment statements. *)
    | Instr([Call(Some(Var vi,_),fun_exp,arguments,loc)]) when !preceding_instr > 0 ->
      incr preceding_call; DoChildren
    | _ -> DoChildren
end

(* Add a visitor to inspect If conditional expression. *)
class chkIfExpLoopBlockVisitor usedVars = object
  inherit nopCilVisitor
  method vstmt s =
    match s.skind with
    | If(exp, blk1, blk2, loc) ->
      (match exp with
       | UnOp(LNot,_,_) | BinOp(Ne,_,_,_) | Lval(Mem _,Field _) ->
         let _ = visitCilExpr (new usedVarVisitor usedVars) exp in
         DoChildren
       | _ -> DoChildren)
    | _ -> DoChildren
end

class template07Pattern01 fd just_pointers retval1 = object
  inherit nopCilVisitor

  method vstmt s =
    let check_vi vi lst =
      match vi with
        Some(vi) when List.exists (fun p -> p.vid = vi.vid) lst -> true
      | _ -> false in
    let hasPointerType os = begin
      let retv = ref false in
      let _ = ignore(visitCilOffset(new offsetVisitor retv) os) in
      !retv
    end in
    match s.skind with
    | Instr([i]) -> begin
        match i with
          Set((Mem left_exp,Field(fi,o)),right_exp,loc) when not (isArithmeticType fi.ftype) ->
          let left_vi = get_varinfo_exp left_exp in
          let right_vi = get_varinfo_exp right_exp in
          if (check_vi left_vi just_pointers) && (check_vi right_vi fd.sformals) && (hasPointerType o) then
            retval1 := (s,Lval(Mem left_exp, Field(fi,o)),!currentLoc) :: !retval1;
          DoChildren
        | Call(Some (Mem left_exp,Field(fi,o)),fun_exp,arguments,loc) when not (isArithmeticType fi.ftype) ->
          let left_vi = get_varinfo_exp left_exp in
          if (check_vi left_vi just_pointers) && (hasPointerType o) then
            retval1 := (s, Lval(Mem left_exp,Field(fi,o)), !currentLoc) :: !retval1;
          DoChildren
        | _ -> DoChildren
      end
    | _ -> DoChildren

end

class template07Pattern02 decVarIDs decVars retval2 = object
  inherit nopCilVisitor

  method vstmt s =
    match s.skind with
    | Loop (blk, location, s1, s2) ->
      (* walk into a block to check each instruction. *)
      let memset_vars = visitBlkGetList (new chkSetCallBlockVisitor decVarIDs decVars) blk in
      if (llen memset_vars) > 0 then
        retval2 := (s, (Var (lhead memset_vars),NoOffset), !currentLoc)::!retval2;
      DoChildren
    | _ -> DoChildren
end

class template07Pattern03 fd retval3 = object
  inherit nopCilVisitor

  method vstmt s =
    let hasPointerType_exp exp =
      llen (visitExprGetList (new memFieldVisitor) exp) > 0
    in
    let hasPointerType_lval lval = begin
      let retv = ref [] in
      let _ = ignore (visitCilLval(new usedVarVisitor retv) lval) in
      List.exists (fun vi -> isPointerType vi.vtype) !retv
    end in
    (* Add match patterns. *)
    match s.skind with
    | Loop (blk, location, s1, s2) ->
      (* pattern: the block of a loop has more than one If conditional exprresion, where
         one same variable is used in three different manners. *)
      let uniqUsedVars = uniq (visitBlkGetList  (new chkIfExpLoopBlockVisitor) blk) in

      (* filter out the global variables, and not isArithmeticType types *)
      let uniqUsedVars = lfilt (fun vi -> not (vi.vglob || isArithmeticType vi.vtype)
                               ) uniqUsedVars in

      (* filter out the function parameters. *)
      let uniqUsedVars =
        lfilt(fun vi -> not (List.exists (fun fun_vid -> fun_vid.vid == vi.vid) fd.sformals)) uniqUsedVars
      in
      (* check the last statement in the loop block. *)
      let lastone = List.hd (List.rev blk.bstmts) in
      let isSetLastone =
        match lastone.skind with
        | Instr([Set (left_lval,right_exp,loc)])
          when (hasPointerType_lval left_lval) && (not (hasPointerType_exp right_exp)) -> true
        | _ -> false
      in
      (* check if the one statement has same variables in both left and right sides. *)
      let has_same_var st =
        match st.skind with
        | Instr([Set ((Var ltvi,_),BinOp (_,Lval(Var rtvi, NoOffset),_,_),loc)]) ->
          ltvi.vid == rtvi.vid
        | _ -> false
      in
      (* check if the loop block includes an If statement. *)
      let if_stmts = visitBlkGetList chkIfStmtVisitor blk in

      if (llen uniqUsedVars) == 1 && isSetLastone && (llen if_stmts) > 1 && not (has_same_var lastone) then begin
        retval3 := (s, (lhead uniqUsedVars), !currentLoc)::!retval3;
      end; DoChildren
    | _ -> DoChildren
end

let template07 get_fun_by_name fd =
  (********************************************************************
     let just_pointers =
      List.filter (fun vi -> isPointerType vi.vtype) fd.sformals
     in
      if (llen just_pointers) > 0 && (llen fd.sformals > 2) then
        let one_ele (stmt,as_exp,loc) =
          let guard = BinOp(Ne,as_exp,zero,intType) in
          let free_lval = get_fun_by_name !dealloc_api in
          let thenblock = mkBlock ([mkStmt (Instr([Call(None,mk_lval free_lval,[as_exp],loc)]))]) in
          let elseblock = mkBlock ([]) in
          let ifstmt = mkStmt (If(guard,thenblock,elseblock,loc)) in
          let newstmt = prepend_before_stmt stmt [ifstmt] in
            stmt.sid, newstmt
        in
          template (new template07Visitor fd.sformals just_pointers) one_ele fd
      else IntMap.empty
   ********************************************************************)
  let just_pointers = List.filter (fun vi -> isPointerType vi.vtype) fd.sformals in
  if (llen just_pointers) == 0 || (llen fd.sformals) < 3 then IntMap.empty
  else begin
    let retval1 =
      if fun_exists get_fun_by_name "_efree" then
        visitFnGetList (new template07Pattern01 fd just_pointers) fd
      else [] in
    if (llen retval1) > 0 then
      let one_ele (stmt,lval,loc) =
        let efree_varinfo = get_fun_by_name "_efree" in
        let guard = BinOp(Ne,lval,zero,intType) in
        let free_lval = mk_lval efree_varinfo in
        let thenblock = mkBlock ([mkStmt (Instr([Call(None,free_lval,[lval],loc)]))]) in
        let elseblock = mkBlock ([]) in
        let ifstmt = mkStmt (If(guard,thenblock,elseblock,loc)) in
        stmt.sid,  { stmt with skind = Block(mkBlock [ ({stmt with sid = 0}) ; ifstmt ]) }
      in
      pre_template retval1 one_ele
    else
      let retval2 =
        if fun_exists get_fun_by_name "memset" then
          let decVars = visitFnGetList (new declVarVisitor) fd in
          let decVarIDs = lmap (fun vi -> vi.vid) decVars in
          visitFnGetList (new template07Pattern02 decVarIDs decVars) fd
        else [] in
      if (llen retval2) > 0 then
        let one_ele (stmt,lval,loc) =
          let as_exp = Lval lval in
          let exp_sizeof = SizeOfE(as_exp) in
          let fun_decl = get_fun_by_name "memset" in
          let fun_lval = Lval(Var(fun_decl),NoOffset) in
          let fun_inst = Call(None,fun_lval,[as_exp;zero;exp_sizeof],loc) in
          let fun_stmt = mkStmtOneInstr(fun_inst) in
          stmt.sid, { stmt with skind = Block(mkBlock [ fun_stmt; ({stmt with sid = 0}) ]) }
        in
        pre_template retval2 one_ele
      else
        let retval3 =
          if(fun_exists get_fun_by_name "g_slice_free1") then
            visitFnGetList (new template07Pattern03 fd) fd
          else [] in
        let one_ele  (stmt,usedVar,loc) =
          let fun_lval = mk_lval (get_fun_by_name "g_slice_free1") in
          let lval_usedVar = (Var usedVar,NoOffset) in
          let typeUsedVar =
            (match usedVar.vtype with
             | TPtr (t, _) -> t
             | _ -> voidPtrType) in

          let exp_sizeof = SizeOf(typeUsedVar) in
          let as_exp = Lval lval_usedVar in
          let fun_inst = Call(None,fun_lval,[exp_sizeof;as_exp],lu) in
          let fun_stmt = mkStmtOneInstr(fun_inst) in

          let guard = BinOp(Eq,as_exp,zero,intType) in
          let thenblock = mkBlock([fun_stmt]) in
          let elseblock = mkBlock ([]) in
          let ifstmt = mkStmt (If(guard,thenblock,elseblock,lu)) in
          match stmt.skind with
          | Loop (blk, loc, s1, s2) ->
            (* debug "-->%s\n" (stmt_str ifstmt); *)
            stmt.sid, mkStmt(Loop(mkBlock(List.append blk.bstmts [ifstmt]),loc,s1,s2))
          | _ -> failwith "Unexpected match in pattern 3 of template 7"
        in
        pre_template retval3 one_ele
  end

(*
 * Myoungkyu Song     <mksong1117@utexas.edu>
 *
 * Template 08: memory reset adder
 * -----------
 *
 *    #1 match
 *   -----------
 *
 * 1. We find a block of Set statements that assign each field of a struct
 *    variable to insert a call to a memory reset API like "memset" before
 *    starting to execute the statement block.
 *
 *    Suppose that there appear three Set statements for a variable with its
 *    fields such as "buf->nm = name", "buf->ad = addr", and "buf->sz = size".
 *    We detect them in a function, where we insert a call to "memset" at the
 *    beginning of these statements.
 *
 *    To reduce a search scope, we inspect each code fragment as the function-
 *    level granularity. Although it sounds like working, inserting a call to
 *    "memset" is not always precise. [Todo] I think that we need to change it
 *    to the block level granularity to locate more precise edit positions.
 *
 * 2. We find a list of Set statement sets from blocks when leaving each block.
 *    When leaving each function, we store each of the first Set statement from
 *    every set, and return them. For example, when we have sets
 *
 *      "buf->x = A1; buf->y = B1; buf->z = C1"    and
 *      "data->p = A2; data->q = B2; data->r = C2"
 *
 *    we need each first one like "buf->x = A1" and "data->p = A2", and those
 *    are used to locate to insert a call to "memset" using variables "buf"
 *    and "data".
 *
 * 3. Given the information we searched, we update a block of statements that
 *    encludes the statement that we found above. When we go through each block
 *    we would like to update, we divide it into two parts by the given
 *    statement. We create a new Call statement invoking the memory reset API
 *    "memset", add it at the end of the front part, and append the resulting
 *    fragment to the rear part. It then becomes a new block, replacing an
 *    existing block we just walked through.
 *

  @Example 1.

+  __builtin_memset((dateobj->time)->relative, 0, sizeof(struct timelib_rel_time ));
   (dateobj->time)->relative.y = 0LL - (intobj->diff)->y * (timelib_sll )bias;
   (dateobj->time)->relative.m = 0LL - (intobj->diff)->m * (timelib_sll )bias;
   (dateobj->time)->relative.d = 0LL - (intobj->diff)->d * (timelib_sll )bias;
   (dateobj->time)->relative.h = 0LL - (intobj->diff)->h * (timelib_sll )bias;
   (dateobj->time)->relative.i = 0LL - (intobj->diff)->i * (timelib_sll )bias;
   (dateobj->time)->relative.s = 0LL - (intobj->diff)->s * (timelib_sll )bias;

 *
 *    #2 match
 *   -----------
 *
 *  1. We find the Then block of an If statement and take a look at a variable to
 *     see if it should be reset its memory space before having it hold data.
 *
 *     We fix defects by adding a Set statement to insert a Null termination
 *     value at the end of the array.
 *
 *     When it comes to investigation of the If statement, we collect all used
 *     variables its conditional expression. We check whether those variables are
 *     used in the parameters in the enclosing function. We take a look at the
 *     Else block to see whether there is any variable used in the parameters in
 *     the enclosing function, and see whether a returned variable of a Call is
 *     used in an inner If conditional expression.
 *
 *  2. In terms of changes of this template, we check the length of the variable
 *     that we found above, checking if its range is from zero to 512.
 *     Specifically, its range being greater than 512 is cut by a Null terminator.
 *     We insert a function call returning the string length, two If statements,
 *     and a Set instruction in the body of the inner If statement.


  @Example 2.

    if ((int )t2p->pdf_author[0] != 0) {
+     __cil_tmp54 = strlen(t2p->pdf_author);
+     if (__cil_tmp54 > 0) {
+       if (__cil_tmp54 > 511) {
+         t2p->pdf_author[512] = 0;
+       }
        tmp___15 = t2pWriteFile(output, (tdata_t )"/Author ", (tmsize_t )8);
        written += tmp___15;
        tmp___16 = t2p_write_pdf_string(t2p->pdf_author, output);
        written += tmp___16;
        tmp___17 = t2pWriteFile(output, (tdata_t )"\n", (tmsize_t )1);
        written += tmp___17;
+     }
    } else {
      tmp___22 = TIFFGetField(input, (uint32 )315, & info);
      if (tmp___22 != 0) {
        goto _L;
      } else {
        tmp___23 = TIFFGetField(input, (uint32 )33432, & info);


 *
 *)

class newLvalExprVisitor constIndex usedVarPtr usedVarIndex = object
  inherit nopCilVisitor

  method vexpr exp =
    match exp with
    | Lval (Mem (Lval (Var vi, _)), Field (fi, Index(_,_))) ->
      if (isArithmeticType fi.ftype) || (isFunctionType fi.ftype) then begin
        (* debug "[DBG] X \t %s %s\n" vi.vname fi.fname; *)
        DoChildren
      end else begin
        (* debug "[DBG] O \t %s %s\n" vi.vname fi.fname; *)
        let ptrHead = Mem (Lval (Var vi, NoOffset)) in
        let expPtr = Lval(ptrHead, Field (fi,NoOffset)) in
        let _ = usedVarPtr := expPtr::!usedVarPtr in
        let newlval = ptrHead, Field(fi,Index(constIndex, NoOffset)) in
        let _ = usedVarIndex := newlval::!usedVarIndex in
        DoChildren
      end;
    | _ -> DoChildren
end

let contains_vars xVars yVars =
  List.exists (fun vi ->
      List.exists (fun yVar -> yVar.vid == vi.vid) yVars
    ) xVars

(* visitor for a block collecting used variables *)
class usedVarBlockVisitor argsFun retUsedVars  = object
  inherit nopCilVisitor

  val preceding_ret_var = ref []
  val contain_arguments_expression = ref false

  method vstmt s =
    match s.skind with
    | Instr([Call(Some (Var vi,_),fun_exp,arguments,loc)]) ->
      let _ = preceding_ret_var := [vi] in
      (* check if one of arguments is used in the call. *)
      let var_args = lfoldl(fun acc exp -> match exp with
          | Lval(Var vi,_) -> vi::acc
          | _ -> acc) [] arguments in
      let _ = contain_arguments_expression := contains_vars var_args argsFun in
      DoChildren
    | If(exp, blk1, blk2, loc) ->
      let usedVars = visitExprGetList (new usedVarVisitor) exp in
      (* check if preceding function call's returned variable is used in the If expression. *)
      let vid_usedVars = lfoldl (fun acc vi -> vi.vid :: acc)[] usedVars in
      let rst_vars = List.exists (fun vi -> lmem vi.vid vid_usedVars) !preceding_ret_var in
      if rst_vars && !contain_arguments_expression then begin
        retUsedVars := true; SkipChildren
      end else    DoChildren
    | _ -> DoChildren
end

let get_opt opt retval =
  match opt with
  | Some(o) -> retval := o :: !retval
  | None -> () (* failwith "Get_opt called on non-Some value." *)

class chkVarWithinBlockVisitor retval = object
  inherit nopCilVisitor

  method vvrbl v =
    retval := v::!retval; DoChildren;
end

class template08Pattern01 retval1 = object
  inherit nopCilVisitor

  val mutable preceding_ifStmt = false
  val mutable preceding_ifStmt_list = []

  val mutable preceding_vars_in_ifStmt = []
  val mutable preceding_vars_in_SetStmt = []


  (* find statements that set fields of struct pointers. *)
  method vblock b =
    (* given the address expression in the field access, figure out its type,
       since that's what we'll need to pass to the memset *)
    let rec addr_type addr =
      let lval_type = function
        | _,Field(fi,_) -> Some(fi.ftype)
        | Var(vi),_ -> Some(vi.vtype)
        | Mem(e),_ -> addr_type e
      in
      match addr with
      | Lval(l) | AddrOf(l) | StartOf(l) -> lval_type l
      | CastE(t,_) -> Some(t)
      | _ -> None
    in
    let _,groups =
      (* this awful fold walks the list of statements and groups interesting and
         related set instructions *)
      lfoldl (fun (base, groups) s ->
          (* if we find another Set that is different from the preceding Set, or
             an instruction that is not a Set, we take the current group of Set
             instructions and tack them onto groups, making a new group. *)
          let get_type e =
            (* returns whether type was found to avoid a compilation error and runtime exception. *)
            match addr_type e with
              None -> false, voidType
            | Some(TPtr(t,_)) -> true, t
            | Some (_) -> false, voidType
          in
          match s.skind with
          | If (_,blk1,_,_) -> begin
              let retval = visitBlkGetList (new chkVarWithinBlockVisitor) blk1 in
              preceding_vars_in_ifStmt <- (lapnd retval preceding_vars_in_ifStmt);
              None, groups;
            end
          | Instr([Set((Mem addr, Field(fi, _)), exp, loc)]) ->

            let do_get_type, gottype = get_type addr in

            if not do_get_type then
              None, groups
            else
              begin
                let newitem = addr, gottype, s, loc in
                begin match base with
                  (* no previous group; start a new one *)
                    None -> Some(addr), [newitem]::groups
                  | Some(base_exp) ->
                    if (cmp_exp base_exp addr) <> 0 then
                      (* different group; start a new one *)
                      Some(addr), [newitem]::groups
                    else
                      (* same group of sets, add to the current list *)
                      base, (newitem::(List.hd groups)) :: (List.tl groups)
                end
              end
          | _ -> None, groups (* not a Set instruction or not the right kind of Set instruction,
                                 so the current group ends if it exists *)
        ) (None, []) b.bstmts
    in
    let groups =
      filter_map (fun group ->
          if (llen group) > 3 then Some(List.hd (lrev group)) else None
        ) groups
    in
    (* collect variables in the Set statements. *)
    let usedVarsGrp =
      lfoldl(fun acc (_,_,s,_) ->
          match s.skind with
          | Instr([Set(_,exp1,_)]) ->
            let retUsedVars = visitExprGetList (new usedVarVisitor) exp1 in
            lapnd retUsedVars acc
          | _ -> acc
        ) [] groups in
    (* check if there is overwrappings. *)
    let check_overwrap_lists lst1 lst2 =
      let lst1 = lfoldl(fun acc vi -> vi.vid::acc)[] lst1 in
      let lst2 = lfoldl(fun acc vi -> vi.vid::acc)[] lst2 in
      List.exists (fun via -> lmem via lst2) lst1
    in
    (* check if each statement includes a case label. *)
    let check_case_labled groups =
      let rst_bstmts =
        lfilt(fun st ->
            let rst_labels = lfilt(fun lb -> match lb with
                | Case (exp,_) -> true
                | _ -> false
              ) st.labels
            in (llen rst_labels) > 0
          ) b.bstmts
      in (llen rst_bstmts) > 0
    in
    (* let's check *)
    if (check_overwrap_lists usedVarsGrp preceding_vars_in_ifStmt) && not (check_case_labled groups) then
      retval1 := groups @ !retval1;
    DoChildren
end

(* visitor for template 8 *)
class template08Pattern02 arguments retval2 = object
  inherit nopCilVisitor

  method vstmt s =
    match s.skind with
    | If(exp, blk1, blk2, loc) ->
      (* collect all variables used in an If expression. *)
      let usedVars = visitExprGetList (new usedVarVisitor) exp in
      (* check if an used variable in expression is identical to one of arguments. *)
      let contain_arguments_expression = contains_vars usedVars arguments in
      (* check the Else block if it contains a function call and an If statement, and
         the return variable of a call is used in the If conditional expression.
         check if one of arguments is used in an If conditional expression. *)
      let contain_arguments_else = visitBlkGetBool (new usedVarBlockVisitor arguments) blk2 in
      (* return statement if patterns are matched *)
      if(contain_arguments_expression && contain_arguments_else) then begin
        let usedVarPtr = ref [] in
        let usedVarIndex = ref [] in
        let _ = ignore(visitCilExpr (new newLvalExprVisitor (integer 512) usedVarPtr usedVarIndex) exp) in
        if (llen !usedVarPtr) > 0 && (llen !usedVarIndex) > 0 then
          retval2 := (s,exp,!usedVarPtr,!usedVarIndex,blk1,blk2,loc)::!retval2
      end;
      DoChildren
    | _ -> DoChildren
end

let template08 get_fun_by_name fd =
  (****************************************************************
     let one_ele  (addr,t,stmt,loc) =
      let args = [ addr; zero;SizeOf(t)] in
      let fn = Lval(Var(get_fun_by_name "memset"),NoOffset) in
      let instr = mkStmt (Instr([Call(None,fn,args,loc)])) in
      let newstmt = append_after_stmt instr [stmt] in
        stmt.sid,newstmt
     in
      template (new template08Visitor) one_ele
   ****************************************************************)
  let retval1 =
    if (isVoidTFun fd) && (fun_exists get_fun_by_name "memset") then
      visitFnGetList (new template08Pattern01) fd
    else [] in
  if (llen retval1) > 0 then
    let one_ele  (addr,t,stmt,loc) =
      let fun_lval = Lval(Var(get_fun_by_name "memset"),NoOffset) in
      let args = [ addr; zero;SizeOf(t)] in
      let instr = mkStmt (Instr([Call(None,fun_lval,args,loc)])) in
      stmt.sid, append_after_stmt instr [stmt]
    in
    pre_template retval1 one_ele
  else
    let retval2 =
      if  (llen fd.sformals) > 2 && (fun_exists get_fun_by_name "strlen") then
        visitFnGetList (new template08Pattern02 fd.sformals) fd else []
    in
    let one_ele (stmt,exp,usedVarPtr,usedVarIndex,blk1,blk2,loc) =
      (* collect a particular expression from an If conditional expression. *)
      (* let usedVarPtr = ref [] in
         let usedVarIndex = ref [] in
         let _ = ignore(visitCilExpr (new newLvalExprVisitor (integer 512) usedVarPtr usedVarIndex) exp) in
         debug "%s\n" (exp_str exp); *)
      (* create a function call and assign the returned value to a temporary variable. *)
      let fun_lval = mk_lval (get_fun_by_name "strlen") in
      let args = [ (lhead usedVarPtr) ] in
      let lval_tmpVar = Var (makeTempVar fd intType), NoOffset in
      let call_stmt = mkStmt (Instr([Call(Some lval_tmpVar,fun_lval,args,lu)])) in
      (* create an If statement by using an used variable in a function call. *)
      let boExp = BinOp(Gt, (Lval lval_tmpVar), (integer 511), intType) in
      let instrSet = mkStmt (Instr([Set((lhead usedVarIndex),zero,lu)])) in
      let if_stmt = mkStmt (If(boExp,mkBlock([instrSet]),mkBlock([]),lu)) in
      (* update a current If block with the created statements. *)
      let ifBlk = mkBlock(if_stmt::blk1.bstmts) in
      (* create a wrapping If statement *)
      let boExp_wrp = BinOp(Gt, (Lval lval_tmpVar), (integer 0), intType) in
      let ifStmt_wrp = mkStmt (If (boExp_wrp, ifBlk, mkBlock([]), lu)) in
      stmt.sid, mkStmt (If (exp, mkBlock ([call_stmt;ifStmt_wrp]), blk2, loc))
    in
    pre_template retval2 one_ele
(*
 * Myoungkyu Song     <mksong1117@utexas.edu>
 *
 * Template 09: null and size checker
 * -----------
 *
 *    #1 match
 *   -----------
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
 *    #2 match
 *   -----------
 *
 *  1. We find an If statement, whose conditional expression includes an variable
 *     that used in arguments of the preceding Call.
 *
 *     We fix defects by taking a look at each argument of the Call, choosing
 *     a variable from it, which is not used in the If conditional expression
 *     that we found above, and replacing the chosen variable with the existing
 *     one.
 *
 *     Regarding to the If statement, we find the preceding If statement that
 *     includes a Call and an inner If statement. We take a look at the relation
 *     between them: a variable that the Call returns and the same variable that
 *     is used in the If conditional expression.
 *
 *  2. In terms of changes by this template, we go through the If statement and
 *     take a look at its expression. If this expression uses the variable that
 *     we found above, we replace it with another variable that we found above
 *     as well.

  @Example

    if (tmp___1 == 0) {
    tmp___0 = zend_parse_parameters_ex((void * )0, (void * )0, (void * )0);
    if (tmp___0 == 0) {
      tmp = zend_parse_parameters_ex((void * )0, (void * )(& isostr), (void * )(& isostr_len));
      if (tmp == 0) {
        php_error_docref((void * )0, (char * )"This constructor accepts either (DateTime, DateInterval, int) OR (DateTime, DateInterval, DateTime) OR (string) as arguments.");
        zend_restore_error_handling((void * )0);
        return;
      }
    }
  }
  dpobj = zend_object_store_get_object((void * )0);
  dpobj->current = (timelib_time * )0;
- if (isostr != 0) {
+ if (isostr_len != 0) {
    if ((unsigned int )dpobj->end == (unsigned int )((timelib_time * )0)) {
      zend_restore_error_handling((void * )0);
    }
  }
  return;

 *
 *    #3 match
 *   -----------
 *
 *  1. We go through an If statement to find a variable that has something wrong
 *     when converting a string to an integer.
 *
 *     We fix defects by adding an If statement, within whose body we insert a
 *     function call as memory deallocation and a Return statement along with a
 *     value zero.
 *
 *     Regarding to the If statement that we want to find, we check whether an If
 *     statement has multiple inner If statements, where we check to collect the
 *     one whose expression includes binary operation, especially greater than.
 *
 *     To add an If statement, we use an expression extracted from an If
 *     statement holding unary operation. We check the extracted expression if
 *     it has pointer type.
 *
 *  2. In terms of changes by this template, we create an If statement by adding
 *     unary and binary operation expressions that we found above, within whose
 *     body we insert a function call as memory deallocation using a pointer
 *     type variable. Lastly we insert a Return statement with a return value
 *     zero.

  @Example

  tmp = ecalloc(1, (int )sizeof(php_url ));
  ret = (php_url * )tmp;
  if ((unsigned int )p >= (unsigned int )s) {
    if ((int const )*p == 58) {
      if (! ret->port) {
        p ++;
        if (e - p > 5) {
          STR_FREE((void * )ret->scheme);
          STR_FREE((void * )ret->user);
          STR_FREE((void * )ret->pass);
          efree((void * )ret);
          return (0);
        } else
        if (e - p > 0) {
          memcpy(port_buf, p, e - p);
          port_buf[e - p] = (char )'\000';
          tmp___0 = atoi(port_buf);
          ret->port = (unsigned short )tmp___0;
+         if (! ret->port && e - p) {
+           free(ret);
+           return (0);
+         }
        }
        p --;
      }
    } else {
      p = e;
    }
  } else {
    p = e;
  }

 *
 *    #4 match
 *   -----------
 *
 *  1. We go through a Call statement where one argument includes binary
 *     operation, especially subtract of a variable by an Integer value.
 *
 *     We fix defects by adding, before the Call, an If statement as an
 *     arithmetic checker. We check if the variable is less than the value in the
 *     expression of the If statement, we don't execute it any more by adding a
 *     Return in the body of the If statement.
 *
 *     We go through a Call instruction and we iterate its each argument, taking
 *     a look at the expression, in which we check if it is Integer type. Once we
 *     find an Integer type variable, we store it in a temporary place.
 *
 *  2. In terms of changes by this template, we create an If statement using an
 *     variable that we found above. We make a binary operation with an Integer
 *     value that is less than the variable in the If conditional expression. In
 *     its body, we insert a Return statement.


    @ Example

    void case_accesslog_append_escaped(buffer *dest , buffer *str )
    {
+     if (str->used > 1) {
+       return;
+     }
      buffer_prepare_append(dest, str->used - 1);
      return;
    }

 *
 *    #5 match
 *   -----------
 *
 *
 *  1. We go through an If statement that there are multiple Call statements that
 *     have arguments, one of which has an arithmetic expression.
 *
 *     We fix defects by adding an If statement that has a binary operation
 *     expression which can check if an element of an array is invalid.
 *
 *     Regarding to the If statement we visit, we check if there are more than one Call
 *     instructions and check if the first Call has a returned variable, which we
 *     check if it is used in the subsequent Calls.
 *
 *  2. In terms of changes by this template, we add an If statement using an
 *     expression that is used in one of argument of a Call in the body of the If
 *     statement that we found above. We check if the expression is valid
 *     comparing with a Null value.


  @ Example:

      void add_rr_to_tree(int type , char *name )
      {
        char **srv_rr_info ;

        if (type == 12) {
          srv_rr_info = g_strsplit(name, 3);
          proto_tree_add_string(1, *(srv_rr_info + 0) + 1);
+         if ( *(srv_rr_info + 1) != 0) {
            proto_tree_add_string(2, *(srv_rr_info + 1) + 1);
+         }
+         if ( *(srv_rr_info + 1) != 0 && *(srv_rr_info + 2) != 0) {
            proto_tree_add_string(3, *(srv_rr_info + 2));
+         }
        }
        return;

 *
 *)

let under_token = Str.regexp "_"

class chkLvalBlockVisitor vname fname s1 s2 fields = object
  inherit nopCilVisitor

  method vlval = function
    | Mem(Lval(Var(vi),_)),Field(fi,_) when
        (* first phase of (easy) filtering *)
        (isIntegralType (unrollType fi.ftype))
        && vname = vi.vname
        && fname != fi.fname -> begin
        match (Str.split under_token fi.fname) with
        (* matches when field 1 looks like foo_bar and this field (field 2) looks like foo_nbar *)
          [one;two] when one = s1 && two = s2 -> fields := (vi,fi) :: !fields
        | _ -> ()
      end; DoChildren
    | _ -> DoChildren
end

(* visit a statement to collect a particular statement information -- If statements. *)
class chkStmtThenBlockVisitor retval1 retval2 = object
  inherit nopCilVisitor

  val preceding_call_retVar = ref []
  val preceding_call_fun_exp = ref []
  val preceding_call_args = ref []

  method vstmt s =
    match s.skind with
    | Instr ([Call(Some (Var vi,_),fun_exp,arguments,loc)]) ->
      preceding_call_retVar := [vi];
      preceding_call_args := arguments;
      preceding_call_fun_exp := fun_exp::!preceding_call_fun_exp;
      DoChildren
    | If (exp, bl1, bl2, loc) when (llen !preceding_call_retVar) > 0 ->
      let usedVarsExp = visitExprGetList (new usedVarVisitor) exp in
      (* check if preceding function call's returned variable is used in the If expression. *)
      let vid_usedVarsExp = lfoldl (fun acc vi -> vi.vid :: acc)[] usedVarsExp in
      let rst_vars = lfilt (fun vi -> lmem vi.vid vid_usedVarsExp) !preceding_call_retVar in
      (* return all function expression if all patterns are satisfied. *)
      if (llen rst_vars) > 0 then begin
        let _ = retval1 := !preceding_call_fun_exp in
        retval2 := !preceding_call_args
      end;
      DoChildren
    | _ -> DoChildren
end


class chkCallsStmtVisitor preceding_retval retval = object
  inherit nopCilVisitor

  method vstmt s =
    let _ =
      match s.skind with
      | Instr([Call(Some (Var vi,_),fun_exp,args,loc)]) ->
        preceding_retval := vi::!preceding_retval;
      | _ -> ()
    in
    let _ =
      match s.skind with (* Claire to myoungkyu: this is going to match the same call twice, because you add to preceding retval above.  Is that what you mean to do? *)
      | Instr([Call(_,fun_exp,args,loc)]) when (llen !preceding_retval) > 0 ->
        (* inspect arguments if they include the preceding returned value. *)
        let usedVars = ref [] in
        let usedIndexVars = ref [] in

        let _ = liter(fun exp ->
            ignore(visitCilExpr(new usedVarVisitor usedVars) exp)
          ) args in
        let filtered = List.exists(fun v ->
            let pr = lhead !preceding_retval in
            pr.vid == v.vid
          ) !usedVars in
        if filtered then begin
          (* inspect each of argument if it operates the index of the array. *)
          let _ = liter(fun exp ->
              ignore(visitCilExpr(chkBoIndexPIExprVisitor usedIndexVars) exp)
            ) args in begin
            if (llen !usedIndexVars) > 0 then begin
              retval := (s,(lhead !usedIndexVars))::!retval
            end
          end
        end
      | _ -> ()
    in
    DoChildren
end

class chkCallsThenBlockVisitor retval = object
  inherit nopCilVisitor

  method vblock b =
    let preceding_retval = ref [] in
    let _ = liter(fun s ->
        ignore(visitCilStmt(new chkCallsStmtVisitor preceding_retval retval) s);
      ) b.bstmts in
    DoChildren
end

let get_var_from_expr exps =
  lfoldl(fun acc exp ->
      let usedVars = visitExprGetList (new usedVarVisitor) exp in
      if (llen usedVars) > 0 then begin
        lapnd usedVars acc
      end else acc
    ) [] exps

let contains_filter v1 v2 =
  let vid_v2 = lfoldl (fun acc vi ->
      vi.vid :: acc
    ) [] v2 in
  lfilt (fun vi ->
      (lmem vi.vid vid_v2)
    ) v1

let contains_vars v1 v2 = begin
  let filtered_v1 = contains_filter v1 v2 in
  (llen filtered_v1) == (llen v1)
end

let get_contained_vars v1 v2 = contains_filter v1 v2

class template09Pattern01 retval1 = object
  inherit nopCilVisitor

  method vstmt s =
    let _ = match s.skind with
      | If(exp,bl1,bl2,loc)->
        (match exp with
           (Lval(Mem(Lval(Var vi,_)),Field(fi,NoOffset)))
           when (any_match under_token fi.fname)
           ->
           (* only counts when the field name has a single underscore in it *)
           (match Str.split under_token fi.fname with
              [one;two] ->
              let then_lvals = ref [] in
              let _ = visitCilBlock (new chkLvalBlockVisitor vi.vname fi.fname one ("n"^two) then_lvals) bl1 in
              if (llen !then_lvals) > 0 then
                retval1 := (s,exp,!then_lvals,bl1,bl2,!currentLoc) :: !retval1
            | _ -> ())
         | _ -> ())
      | _ -> ()
    in
    DoChildren
end

class template09Pattern02 fd retval2 = object
  inherit nopCilVisitor

  val preceding_call_retVar = ref []
  val preceding_call_fun_exp = ref []
  val preceding_vars_innerIfExp = ref []

  method vstmt s =
    match s.skind with
    | Instr([Call(Some (Var vi,_),fun_exp,arguments,loc)]) ->
      preceding_call_retVar := [vi];
      preceding_call_fun_exp := [fun_exp]; DoChildren
    | If (exp, bl1, bl2, loc) when (llen !preceding_call_retVar) > 0 ->
      (* walk through the If-block to see if there is a particular pattern --
         two more If conditional expressions that have same function calls. *)
      let retval_funExp = ref [] in
      let retval_args_innerIfExp = ref [] in
      let _ = ignore (visitCilBlock(new chkStmtThenBlockVisitor retval_funExp retval_args_innerIfExp) bl1) in
      (* check if we got multiple same patterns, comprising a call, a returned variable,
         and an If expression with the call's returned variable *)
      let _ = retval_funExp := lapnd !retval_funExp !preceding_call_fun_exp in
      let check_IfExpr = (llen !retval_funExp) > 2 && (llen (uniq (!retval_funExp))) == 1  in
      (* check if arguments of a function call are defined locally,
         by collecting all variables used in the If conditioinal expression. *)
      let varsFromExp = get_var_from_expr !retval_args_innerIfExp in
      let check_innerIfExpr =
        if check_IfExpr then
          let decVars = visitFnGetList (new declVarVisitor) fd in
          (* check all variables in a varialbe list are contained in the ther list. *)
          contains_vars varsFromExp decVars
        else false
      in
      (* store variables used in an inner If conditional expression. *)
      if check_innerIfExpr then
        preceding_vars_innerIfExp := varsFromExp;
      DoChildren
    | If (exp, bl1, bl2, loc) ->
      let usedVars = ref [] in
      let _ = ignore (visitCilExpr (new usedVarVisitor usedVars) exp) in begin
        match (get_contained_vars !usedVars !preceding_vars_innerIfExp) with
          usedVar :: rest ->
          let unUsedVars = lfilt (fun vi ->
              let bool_check1 = vi.vid <> usedVar.vid in
              let bool_check2 = (contains vi.vname usedVar.vname) || (contains usedVar.vname vi.vname) in
              (bool_check1 && bool_check2)
            ) !preceding_vars_innerIfExp in
          (* check if the expression contains offset, such as pointer or index type. *)
          let has_offset = ref false in
          ignore(visitCilExpr(chkOffsetFieldIndexVisitor has_offset) exp);
          (* let's check *)
          if not !has_offset && (llen unUsedVars) > 0 then begin
            let unUsedVar = lhead unUsedVars in
            retval2 := (s,bl1,bl2,unUsedVar,loc)::!retval2
          end; DoChildren
        | _ -> DoChildren
      end
    | _ -> DoChildren

end

class template09Pattern03 retval3 = object
  inherit nopCilVisitor

  val preceding_has_multiple_if = ref false
  val preceding_has_binop = ref false
  val preceding_retval_binopGt = ref []
  val preceding_retval_unop = ref []
  val preceding_has_unop = ref false
  val preceding_ptrExp = ref []
  val preceding_has_ptr = ref false

  method vstmt s =
    (* find patterns by using two combined matches. *)
    (match s.skind with
     | If (exp,bl1,bl2,loc) ->
       (* check whether the current statement has multiple If statements. (particularly more than three) *)
       let retval_multi_if_exprs = visitBlkGetList chkIfExprBlockVisitor bl1 in
       let _ = preceding_has_multiple_if := (llen retval_multi_if_exprs) > 3 in

       (* check if inner If statements have binary operation. *)
       if !preceding_has_multiple_if then begin
         (* collect all bin-op expression from the If expression. *)
         let list_binop_exprs = lfoldl (fun acc exp ->
             let retv_binopGt = visitExprGetList chkBinopExprVisitor exp in
             lapnd retv_binopGt acc
           ) [] retval_multi_if_exprs
         in
         (* check if there are similar expressions of binary operation in multiple location. *)
         if (llen list_binop_exprs) > 1 && (llen (uniq list_binop_exprs) == 1) then begin
           preceding_has_binop := true ;
           let tmp_list_wt_stmt = lfoldl(fun acc exp -> (s,exp)::acc)[] list_binop_exprs in
           preceding_retval_binopGt := lapnd tmp_list_wt_stmt !preceding_retval_binopGt;
         end
       end
     | _ -> ());
    (match s.skind with
     | If (exp,bl1,bl2,loc) ->
       (* check if there is any unary operation. *)
       let retv_unop = visitExprGetList chkUoLNotExprVisitor exp in
       let _ = if (llen retv_unop) > 0 then begin
           (* save the return values into a global variable. *)
           let tmp_list_wt_stmt = lfoldl(fun acc exp -> (s,exp)::acc)[] retv_unop in
           preceding_retval_unop := lapnd tmp_list_wt_stmt !preceding_retval_unop;
           (* CLG to Myoungkyu: do you mean to set this to true every time? *)
           let _ = preceding_has_unop := true in
           (* check if there is any pointer type value in the unary operation's expression. *)
           let _,prec_unop = lhead !preceding_retval_unop in
           let _ = ignore(visitCilExpr (new usedPointerVisitor preceding_ptrExp) prec_unop) in
           if (llen !preceding_ptrExp) > 0 then
             preceding_has_ptr := true
         end in

       (* let's check all parameters that I checked above. *)
       if !preceding_has_multiple_if && !preceding_has_binop && !preceding_has_unop && !preceding_has_ptr then begin
         let stmt_prec_unop, prec_unop = lhead !preceding_retval_unop in
         let unopexp = UnOp(LNot,prec_unop,intType) in
         let stmt_prec_binopexp, binopexp = lhead !preceding_retval_binopGt in
         let varfree = lhead !preceding_ptrExp in

         (* collect all If statements in the Then block. *)
         let retvalIf = visitBlkGetList chkIfStmtVisitor bl1 in
         if (llen retvalIf) > 0 then begin
           (* check containment relationship. *)
           let contain_stmt par chd =
             match par.skind with
             | If (_,bl1,_,_) ->
               visitBlkGetBool (cmpSidVisitor chd.sid) bl1
             | _ -> false
           in
           (* we check if the last If statement includes memory deallocation. *)
           let stmt_last_if = lhead retvalIf in
           match stmt_last_if.skind with
           | If(exp,bl1,bl2,loc) when (contain_stmt stmt_prec_unop stmt_last_if) &&
                                      (contain_stmt stmt_prec_binopexp stmt_last_if) ->
             retval3 := (stmt_last_if,exp,bl1,bl2,loc,unopexp,binopexp,varfree)::!retval3
           | _ -> ()
         end
       end
     | _ -> ());
    DoChildren

end

class template09Pattern04 fd retval4 = object
  inherit nopCilVisitor

  method vstmt s =
    match s.skind with
    | Instr ([Call(None,fun_exp,args,loc)]) when ((llen args) > 0) && (isVoidTFun fd) ->
      (* binary operation with minus, while holding integer 1. *)
      let retval =
        lfoldl (fun retval exp ->
            retval @ (visitExprGetList (new chkBinopMinExprVisitor) exp)) [] args
      in
      (* check if an expression contains a function call. *)
      let has_sizeof_stmt exp = visitExprGetBool collectSizeOfVisitor exp in
      (* check if an expression list contains sizeof. *)
      let chk_sizeof_stmt exp_list =
        List.exists (fun exp ->
            let ret_exp_lst = visitExprGetList (new chkBinopMinExprVisitor) exp in
            List.exists (fun (e,n) -> has_sizeof_stmt e) ret_exp_lst)
          exp_list
      in
      if (llen retval) > 0 && not (chk_sizeof_stmt args) then
        retval4 := (s,args,loc)::!retval4;
      DoChildren
    | _ -> DoChildren

end

class template09Pattern05 retval5 = object
  inherit nopCilVisitor

  method vstmt s =
    let _ =
      match s.skind with
      | If (exp,bl1,bl2,loc) -> begin
          (* inspect the Then block to see if there are multiple function calls. *)
          let retval = visitBlkGetList (new chkCallsThenBlockVisitor) bl1 in
          match retval with
            (s1,exp1) :: (s2,exp2) :: rest ->
            (* check if two expressions are identical. *)
            if not (comp_str (exp_str exp1) (exp_str exp2)) then begin
              let lval_exp1 = Lval(Mem exp1,NoOffset) in
              let lval_exp2 = Lval(Mem exp2,NoOffset) in

              let binop_exp1 = BinOp(Ne,lval_exp1,zero,intType) in
              let binop_exp2 = BinOp(Ne,lval_exp2,zero,intType) in

              (* combine to include the first and second conditional expression. *)
              let comb_exp1_exp2 = BinOp(LAnd,binop_exp2,binop_exp1,intType) in
              retval5 := (s1,comb_exp1_exp2)::!retval5;
              retval5 := (s2,binop_exp2)::!retval5;
            end
          | (s,exp) :: rest -> retval5 := (s,exp)::!retval5;
          | _ -> ()
        end
      | _ -> ()
    in
    DoChildren
end

let template09 get_fun_by_name fd =
  (***********************************************************
     let one_ele (stmt,exp,lvals,bl1,bl2,loc) =
      let binop =
        List.fold_left
          (fun binop (vi,fi) ->
            let new_lval = Lval (Mem (Lval(Var vi,NoOffset)), Field(fi, NoOffset)) in
            let new_gaurd = BinOp(Gt,new_lval,zero,intType) in
              BinOp(LAnd,binop,new_gaurd,intType)
          ) exp lvals
      in
        stmt.sid, { stmt with skind = If(binop,bl1,bl2,loc) }
     in
      template (new template09Visitor) one_ele
   ***********************************************************)

  let retval1 = visitFnGetList (new template09Pattern01) fd in
  if (llen retval1) > 0 then
    let one_ele (stmt,exp,lvals,bl1,bl2,loc) =
      let binop =
        List.fold_left
          (fun binop (vi,fi) ->
             let new_lval = Lval (Mem (Lval(Var vi,NoOffset)), Field(fi, NoOffset)) in
             let new_gaurd = BinOp(Gt,new_lval,zero,intType) in
             BinOp(LAnd,binop,new_gaurd,intType)
          ) exp lvals
      in
      stmt.sid, {stmt with skind = If (binop, bl1, bl2, loc)}
    in
    pre_template retval1 one_ele
  else
    let retval2 =  visitFnGetList (new template09Pattern02 fd) fd in
    if (llen retval2) > 0 then
      let one_ele (stmt,bl1,bl2,unUsedVar,loc) =
        let binop = BinOp(Ne, Lval(Var unUsedVar, NoOffset), zero, intType) in
        stmt.sid, {stmt with skind = If (binop, bl1, bl2, loc)}
      in
      pre_template retval2 one_ele
    else
      let retval3 =
        if fun_exists get_fun_by_name "free" then visitFnGetList (new template09Pattern03) fd else []
      in
      if (llen retval3) > 0 then
        let one_ele (stmt,exp,bl1,bl2,loc,unopexp,binopexp,varfree) =
          (* create a function call, free. *)
          let fun_lval = mk_lval (get_fun_by_name "free") in
          let args = [Lval(Var varfree, NoOffset )] in
          let instr = mkStmt (Instr([Call(None,fun_lval,args,lu)])) in
          (* create Return statement. *)
          let ret_stmt = mkStmt (Return(Some zero,lu)) in
          (* create If statement.  *)
          let a_bop = BinOp (LAnd,unopexp,binopexp,intType) in
          let a_if = mkStmt (If(a_bop,mkBlock([instr;ret_stmt]),mkBlock([]),lu)) in
          let a_blk = mkBlock(lapnd bl1.bstmts [a_if]) in
          stmt.sid, {stmt with skind = If(exp,a_blk,bl2,loc)}
        in
        pre_template retval3 one_ele
      else
        let retval4 =  visitFnGetList (new template09Pattern04 fd) fd in
        if (llen retval4) > 0 then
          let one_ele (stmt,args,loc) =
            let retval = ref [] in
            let _ = liter(fun exp -> ignore(visitCilExpr(new chkBinopMinExprVisitor retval) exp)) args in
            let stmts =
              lfoldl(fun acc (e, n) ->
                  let binop = BinOp(Lt,e,integer n,intType) in
                  let retStmt = mkStmt(Return(None,lu)) in
                  let ifStmt = mkStmt(If(binop,mkBlock([retStmt]),mkBlock([]),lu)) in
                  ifStmt::acc
                )[] !retval
            in
            let fixedStmts = mkBlock((lapnd stmts [stmt])) in
            stmt.sid, {stmt with skind = Block fixedStmts}
          in
          pre_template retval4 one_ele
        else
          template (new template09Pattern05)
            (fun (stmt,exp) ->
               stmt.sid, mkStmt(If(exp,mkBlock([stmt]),mkBlock([]),lu)))
            fd

(*
 * Myoungkyu Song     <mksong1117@utexas.edu>
 *
 * Template 10: missed function call checker
 * -----------
 *
 * 1. We find an If conditional expression checking a particular type and a
 *    known macro value we specified, to add a missed function call.
 *    Additionally, if possible, we look into its Else conditional expression
 *    if it has a same checking pattern. Once we find them, we modify each body
 *    the If statements by adding a function call that is required.
 *
 *    For example, suppose there appears an If statement like,
 *
 *    if (student->common.register & MACRO_NUM) { .. }
 *    else if (senior_student->common.register & MACRO_NUM) { .. }
 *
 *    These If conditional expressions include the same type of struct
 *    variables "student" and "senior_student", which uses the same field
 *    "common.register", with the same value of a macro definition "MACRO_NUM".
 *    Such patterns that we specify as a template is required to add a call that
 *    passes arguments using variables formerly used in the If conditional
 *    expression.
 *
 * 2. We change the body of the each If statement we found above by adding a
 *    call, respectively. When adding a call, we use same variables as arguments
 *    with the different order switching each other.
 *

   Example.

       if (other_trait_fn->common.fn_flags & 2U) {
+        do_inheritance_check_on_method(other_trait_fn, fn);
         ..
       } else {
         if (fn->common.fn_flags & 2U) {
+          do_inheritance_check_on_method(fn, other_trait_fn);
           ..

 *    The above example that we observed in the data set edits the body by
 *    adding a call which passes two arguments yet uses the different order,
 *    when passing them.
 *
 *)

(* visitor *)

let overwrapped_stmts stmt1 stmt2 = begin
  let has_stmt larger_st1 st2 =
    let collected_stmts = ref [] in
    ignore(visitCilStmt(stmtVisitor collected_stmts) larger_st1);
    List.exists (fun s -> s.sid == st2.sid) !collected_stmts
  in
  (has_stmt stmt1 stmt2) || (has_stmt stmt2 stmt1)
end

class template10Visitor retval = object
  inherit nopCilVisitor

  val mutable preceding = None

  method vstmt (s:Cil.stmt) =
    (* CLG FIXME *)
    let match_expr vtype ftype =
      let typstr = trim_str (typ_str vtype) in
      let ftypstr = trim_str (typ_str ftype) in
      (contains ftypstr "struct __anonstruct_common_" ) &&
      (typstr = "zend_function *")
    in
    (* checking. *)
    let _ =
      begin
        match s.skind with
        | If(BinOp(BAnd,Lval(Mem (Lval ((Var vi),NoOffset)),Field(fi,o)),Const(CInt64(macro_value,_,_)),_), bl1,bl2,loc)
          when match_expr vi.vtype fi.ftype && (llen bl1.bstmts) > 0 ->
          (*Const(CInt64 (n,_,_)),_) when (i64_to_int n) == 2 -> *)
          (* CLG made a unilateral decision about this because she thinks it's too
             specific; can change back if someone wants. *)
          begin
            match preceding with
            | Some(parstmt,stmt1,arg1,macro_value1,loc1) ->
              let arg2 = mk_lval vi in
              (* check if these are a pair of argument expression. *)
              if (exp_str arg1) <> (exp_str arg2) then begin
                let ret1 = parstmt,stmt1,[arg1;arg2],(i64_to_int macro_value1),loc1 in
                let ret2 = s,(lhead bl1.bstmts),[arg2;arg1],(i64_to_int macro_value),loc in
                retval := [ret1;ret2] @ !retval;
                preceding <- Some(s,(lhead bl1.bstmts),arg2,macro_value1,loc);
              end;
            | None -> (* no preceding if *)
              preceding <- Some(s,(lhead bl1.bstmts),mk_lval vi,macro_value,loc);
          end
        | _ -> ()
      end
    in
    DoChildren
end

let template10 get_fun_by_name fd =
  (* *********************************************************************************************
     let one_ele (s1,args,loc) =
      let fun_to_insert = Lval(Var(get_fun_by_name "do_inheritance_check_on_method"),NoOffset) in
      let call = mkStmt (Instr([Call(None,fun_to_insert,args,loc)])) in
        s1.sid, prepend_before_stmt s1 [call]
     in
      template (new template10Visitor) one_ele
    ********************************************************************************************* *)
  let retval1 = visitFnGetList (new template10Visitor) fd in

  if (llen retval1) > 0 then begin
    (* let fun_name = "do_inheritance_check_on_method" in  *)
    (* let fun_decl = makeGlobalVar fun_name voidType in *)

    (* get all keys from a hashtable. *)
    let get_all_keys htbl =
      let retval = ref [] in
      hiter(fun k v -> retval := k::!retval) htbl;
      !retval
    in

    (* create a hash table. *)
    let htbl_retval1 = hcreate 255 in
    liter(fun (parstmt,s,args,macro_value,loc) ->
        hadd htbl_retval1 macro_value (parstmt,s,args,macro_value,loc);
      ) retval1;

    let all_keys = uniq(get_all_keys htbl_retval1) in

    (* filtering. *)
    let retval1 = lfoldl (fun acc k ->
        let all_found_values = hfinda htbl_retval1 k in
        if (llen all_found_values) == 2 then begin
          (* check each macro value. *)
          let m_value_holder = ref [] in
          let _ =
            liter(fun (parstmt,s,args,macro_value,loc) ->
                (match parstmt.skind with
                 | If(BinOp(BAnd,Lval(Mem (Lval ((Var vi),NoOffset)),Field(fi,o)),Const(CInt64(m_value,_,_)),_), _,_,_) ->
                   m_value_holder := (i64_to_int m_value) :: !m_value_holder;
                 | _ -> ());
              ) all_found_values
          in
          m_value_holder := uniq(!m_value_holder);
          (* check if there is a shared macro value. *)
          if (llen !m_value_holder) == 1 then begin
            lapnd all_found_values acc
          end else acc
        end else acc
      ) [] all_keys in

    if (llen retval1) > 1 then begin
      let (_,s1,_,_,_) :: (_,s2,_,_,_) :: _ = retval1 in
      (* check if the two statements are overwrapped. *)
      if overwrapped_stmts s1 s2 then IntMap.empty
      else begin
        lfoldl(fun map(parstmt,s1,args,macro_value,loc)->
            (* checking if the function definition exists. *)
            try
              let fun_varinfo = get_fun_by_name "do_inheritance_check_on_method" in
              let fun_to_insert = mk_lval fun_varinfo (* Lval(Var(fun_decl),NoOffset) *) in
              let call = mkStmt (Instr([Call(None,fun_to_insert,args,loc)])) in
              IntMap.add s1.sid (prepend_before_stmt s1 [call]) map
            with
            | Not_found -> IntMap.empty
          ) (IntMap.empty) retval1
      end
    end else IntMap.empty
  end else IntMap.empty

let templates
(*    (Cil.fundec -> Cil.stmt -> (string -> Cil.varinfo) -> Cil.stmt) StringMap.t*)
  =
  List.fold_left (fun m (n,f) -> StringMap.add n f m) StringMap.empty [
    ("template01", template01);
    ("template02", template02);
    ("template03", template03);
    ("template04", template04);
    ("template05", template05);
    ("template06", template06);
    ("template07", template07);
    ("template08", template08);
    ("template09", template09);
    ("template10", template10);
  ]
