(* Transform a C program to print out all of the statements it visits
 * to stderr. Basically, this computes statement coverage a la standard
 * testing. 
 *
 * This is used as a first step in Weimer's Genetic Programming approach to
 * evolving a program variant that adheres to some testcases. 
 *
 * You apply 'coverage' to a single C file (use CIL to combine a big
 * project into a single C file) -- say, foo.c. This produces:
 *
 * foo.ast  -- a serialized CIL AST of the *original* program
 *
 * foo.ht   -- a hashtable mapping from numbers to
 *             'statements' in the *original* foo.c
 *
 * 'stdout' -- an *instrumented* version of foo.c that prints out
 *             each 'statement' reached at run-time into
 *             the file foo.path
 *
 * Typical usage: 
 *
 *   ./coverage foo_comb.c > foo_coverage.c 
 *) 
open Printf
open Cil

let fprintf_va = makeVarinfo true "fprintf" (TVoid [])
let fopen_va = makeVarinfo true "fopen" (TVoid [])
let fflush_va = makeVarinfo true "fflush" (TVoid [])
let memset_va = makeVarinfo true "memset" (TVoid [])
let stderr_va = makeVarinfo true "_coverage_fout" (TPtr(TVoid [], []))
let fprintf = Lval((Var fprintf_va), NoOffset)
let fopen = Lval((Var fopen_va), NoOffset)
let fflush = Lval((Var fflush_va), NoOffset)
let stderr = Lval((Var stderr_va), NoOffset)
let memset = Lval((Var memset_va), NoOffset)
let counter = ref 1 

let massive_hash_table = Hashtbl.create 4096  

(* 
 * Here is the list of CIL statementkinds that we consider as
 * possible-to-be-modified
 * (i.e., nodes in the AST that we may mutate/crossover via GP later). 
 *)
let can_trace sk = match sk with
  | Instr _ 
  | Return _  
  | If _ 
  | Loop _ 
  -> true

  | Goto _ 
  | Break _ 
  | Continue _ 
  | Switch _ 
  | Block _ 
  | TryFinally _ 
  | TryExcept _ 
  -> false 

let get_next_count () = 
  let count = !counter in 
  incr counter ;
  count 

(* This makes a deep copy of an arbitrary Ocaml data structure *) 
let copy (x : 'a) = 
  let str = Marshal.to_string x [] in
  (Marshal.from_string str 0 : 'a) 
  (* Cil.copyFunction does not preserve stmt ids! Don't use it! *) 

class numToZeroVisitor = object
  inherit nopCilVisitor
  method vstmt s = s.sid <- 0 ; DoChildren
end 
let my_zero = new numToZeroVisitor
let old_coverage_bug = ref false 

(* 
 * This visitor changes empty statement lists (e.g., the else branch in if
 * (foo) { bar(); } ) into dummy statements that we can modify later. 
 *)
class emptyVisitor = object
  inherit nopCilVisitor
  method vblock b = 
    ChangeDoChildrenPost(b,(fun b ->
      if b.bstmts = [] then 
        mkBlock [ mkEmptyStmt () ] 
      else b 
    ))
end 

(* This visitor makes every instruction into its own statement. *)
class everyVisitor = object
  inherit nopCilVisitor
  method vblock b = 
    ChangeDoChildrenPost(b,(fun b ->
      let stmts = List.map (fun stmt ->
        match stmt.skind with
        | Instr([]) -> [stmt] 
        | Instr(first :: rest) -> 
            ({stmt with skind = Instr([first])}) ::
            List.map (fun instr -> mkStmtOneInstr instr ) rest 
        | other -> [ stmt ] 
      ) b.bstmts in
      let stmts = List.flatten stmts in
      { b with bstmts = stmts } 
    ))
end 

(* This visitor walks over the C program AST and builds the hashtable that
 * maps integers to statements. *) 
class numVisitor = object
  inherit nopCilVisitor
  method vblock b = 
    ChangeDoChildrenPost(b,(fun b ->
      List.iter (fun b -> 
        if can_trace b.skind then begin
          let count = get_next_count () in 
          b.sid <- count ;
          let rhs = 
            if not !old_coverage_bug then begin 
              let bcopy = copy b in
              let bcopy = visitCilStmt my_zero bcopy in 
              bcopy.skind
            end else b.skind
          in 
          Hashtbl.add massive_hash_table count rhs
          (* the copy is because we go through and update the statements
           * to add coverage information later *) 
        end else begin
          b.sid <- 0; 
        end ;
      ) b.bstmts ; 
      b
    ) )
end 


(* This visitor walks over the C program AST and modifies it so that each
 * statment is preceeded by a 'printf' that writes that statement's number
 * to the .path file at run-time. *) 
class covVisitor = object
  inherit nopCilVisitor
  method vblock b = 
    ChangeDoChildrenPost(b,(fun b ->
      let result = List.map (fun stmt -> 
        if stmt.sid > 0 then begin
          let str = Printf.sprintf "%d\n" stmt.sid in
          let str_exp = Const(CStr(str)) in 
          let instr = Call(None,fprintf,[stderr; str_exp],!currentLoc) in 
          let instr2 = Call(None,fflush,[stderr],!currentLoc) in 
          let skind = Instr([instr;instr2]) in
          let newstmt = mkStmt skind in 
          [ newstmt ; stmt ] 
        end else [stmt] 
      ) b.bstmts in 
      { b with bstmts = List.flatten result } 
    ) )
end 

let uniq_array_va = ref 
  (makeGlobalVar "___coverage_array" (TArray(charType,None,[])))

(* This visitor walks over the C program AST and modifies it so that each
 * statment is printed _at most once_ when visited. *) 
class uniqCovVisitor = object
  inherit nopCilVisitor
  method vblock b = 
    ChangeDoChildrenPost(b,(fun b ->
      let result = List.map (fun stmt -> 
        if stmt.sid > 0 then begin
          let str = Printf.sprintf "%d\n" stmt.sid in
          (* asi = array_sub_i = array[i] *) 
          let iexp = Const(CInt64(Int64.of_int stmt.sid,IInt,None)) in 
          let asi_lval = (Var(!uniq_array_va)), (Index(iexp,NoOffset)) in
          let asi_exp = Lval(asi_lval) in 
          let bexp = BinOp(Eq,asi_exp,zero,ulongType) in

          let str_exp = Const(CStr(str)) in 
          let instr = Call(None,fprintf,[stderr; str_exp],!currentLoc) in 
          let instr2 = Call(None,fflush,[stderr],!currentLoc) in 
          let instr3 = Set(asi_lval,one,!currentLoc) in 
          let skind = Instr([instr;instr2;instr3]) in
          let newstmt = mkStmt skind in 
          let the_if = If(bexp,mkBlock [newstmt],mkBlock [],!currentLoc) in 
          let if_stmt = mkStmt the_if in 
          [ if_stmt ; stmt ] 
        end else [stmt] 
      ) b.bstmts in 
      { b with bstmts = List.flatten result } 
    ) )
end 

let my_cv = new covVisitor
let my_uniq_cv = new uniqCovVisitor
let my_num = new numVisitor
let my_empty = new emptyVisitor
let my_every = new everyVisitor

let main () = begin
  let usageMsg = "Prototype No-Specification Bug-Fixer\n" in 
  let do_cfg = ref false in 
  let do_empty = ref false in 
  let do_every = ref false in 
  let do_uniq = ref false in 
  let filenames = ref [] in 

  let argDescr = [
    "--calls", Arg.Set do_cfg, " convert calls to end basic blocks";
    "--empty", Arg.Set do_empty, " allow changes to empty blocks";
    "--uniq", Arg.Set do_uniq, " print each visited stmt only once";
    "--every-instr", Arg.Set do_every, " allow changes between every statement";
    "--old_bug", Arg.Set old_coverage_bug, " compatibility with old hideous bug";
  ] in 
  let handleArg str = filenames := str :: !filenames in 
  Arg.parse (Arg.align argDescr) handleArg usageMsg ; 

  Cil.initCIL () ; 
  List.iter (fun arg -> 
    begin
      let file = Frontc.parse arg () in 
      if !do_every then begin
        visitCilFileSameGlobals my_every file ; 
      end ; 
      if !do_cfg then begin
        Partial.calls_end_basic_blocks file 
      end ; 
      if !do_empty then begin
        visitCilFileSameGlobals my_empty file ; 
      end; 

      visitCilFileSameGlobals my_num file ; 
      let ast = arg ^ ".ast" in 
      let fout = open_out_bin ast in 
      Marshal.to_channel fout (file) [] ;
      close_out fout ; 

      if !do_uniq then begin
        let size = 1 + !counter in 
        let size_exp = Const(CInt64(Int64.of_int size,IInt,None)) in 
        uniq_array_va := (makeGlobalVar "___coverage_array"
          (TArray(charType, Some(size_exp), []))) ;
        let new_global = GVarDecl(!uniq_array_va,!currentLoc) in 
        file.globals <- new_global :: file.globals ; 
        visitCilFileSameGlobals my_uniq_cv file ; 

        let uniq_array_exp = Lval(Var(!uniq_array_va),NoOffset) in 
        let sizeof_uniq_array = SizeOfE(uniq_array_exp) in 
        let fd = Cil.getGlobInit file in 
        let instr = Call(None,memset,
          [uniq_array_exp;zero;sizeof_uniq_array],!currentLoc) in 
        let new_stmt = Cil.mkStmt (Instr[instr]) in 
        fd.sbody.bstmts <- new_stmt :: fd.sbody.bstmts ; 

      end else begin
        visitCilFileSameGlobals my_cv file ; 
      end ; 

      let new_global = GVarDecl(stderr_va,!currentLoc) in 
      file.globals <- new_global :: file.globals ; 

      let fd = Cil.getGlobInit file in 
      let lhs = Var(stderr_va),NoOffset in 
      let data_str = arg ^ ".path" in 
      let str_exp = Const(CStr(data_str)) in 
      let str_exp2 = Const(CStr("wb")) in 
      let instr = Call((Some(lhs)),fopen,[str_exp;str_exp2],!currentLoc) in 
      let new_stmt = Cil.mkStmt (Instr[instr]) in 
      fd.sbody.bstmts <- new_stmt :: fd.sbody.bstmts ; 
      iterGlobals file (fun glob ->
        dumpGlobal defaultCilPrinter stdout glob ;
      ) ; 
      let ht = arg ^ ".ht" in 
      let fout = open_out_bin ht in 
      Marshal.to_channel fout (!counter,massive_hash_table) [] ;
      close_out fout ; 
    end 
  ) !filenames ; 

end ;;

main () ;;
