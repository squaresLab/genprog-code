open Printf
open Global
open Cil
open Cilprinter
open Cilrep

let exp_str exp = Pretty.sprint ~width:80 (printExp defaultCilPrinter () exp) 

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

let template03 stmt =
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
        let strncpy_varinfo,_,_ = Hashtbl.find va_table "__builtin_strncpy" in
        let strncpy_lval = Lval(Var(strncpy_varinfo),NoOffset) in

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
  let the_xform stmt =
    if IntMap.mem stmt.sid newstmts then
      IntMap.find stmt.sid newstmts
    else stmt
  in
    Cil.lineDirectiveStyle := old_directive_style;
    visitCilStmt (my_xform the_xform nop_bxform) stmt
      
