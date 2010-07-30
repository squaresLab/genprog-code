(* 
 * Program Repair Prototype (v2) 
 *
 * Program Representation -- CIL C AST 
 *
 * This is the main implementation of the "Rep" interface. 
 *
 * Notably, this includes code and support for: 
 *  -> compiling C programs
 *  -> running test cases
 *  -> computing "coverage" fault localization information automatically
 *     => or allowing you to specify it from a file
 *  -> deleting/appending/swaping statements in C programs
 *)

open Printf
open Global
open Cil
open Rep

(*************************************************************************
 *************************************************************************
                      CIL Representation - C Programs
 *************************************************************************
 *************************************************************************)

(* 
 * Here is the list of CIL statementkinds that we consider as
 * possible atomic mutate-able statements. 
 *
 * Different handling is required for expression-level mutation.
 *)
let can_repair_statement sk = match sk with
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

(*let can_repair_expression ek = match ek with
  true*)

let probability prob =
  if (Random.float 1.0) < prob then true else false

let contains ht obj =
  begin
  let flag = ref false in
  Hashtbl.iter(fun _ x ->
    if x=obj then flag:=true
  ) ht;
  !flag; end

let lstcontains lst obj =
  begin
  let flag = ref false in
  List.iter(fun x ->
    if x=obj then flag:=true
  ) lst;
  !flag; end

(*************************************************************************
 * Coverage and Weighted Path Information
 *************************************************************************)

(* These are CIL variables describing C standard library functions like
 * 'fprintf'. We use them when we are instrumenting the file to
 * print out statement coverage information for fault localization. *)  
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

(* This helper visitor resets all stmt ids to zero. *) 
class numToZeroVisitor = object
  inherit nopCilVisitor
  method vstmt s = s.sid <- 0 ; DoChildren
end 
let my_zero = new numToZeroVisitor


(* This visitor walks over the C program AST and builds the hashtable that
 * maps integers to statements. *)
class numStmtVisitor count ht = object
  inherit nopCilVisitor
  method vblock b = 
    ChangeDoChildrenPost(b,(fun b ->
      List.iter (fun b -> 
        if can_repair_statement b.skind then begin
          b.sid <- !count ;
          let rhs = 
              let bcopy = copy b in
              let bcopy = visitCilStmt my_zero bcopy in 
              bcopy.skind
          in 
          Hashtbl.add ht !count rhs;
          incr count ; 
        end else begin
          b.sid <- 0; 
        end ;
      ) b.bstmts ; 
      b
    ) )
end 
let my_num_stmt = new numStmtVisitor

let fname = ref ""
let top = ref false
let parentstmt = ref 0
(* This visitor walks over the C program AST and builds the hashtable that
 * maps integers to expressions and the hashtable that maps expressions to 
 * expression data. *)
class numExpVisitor eht new_exp_map= object
  inherit nopCilVisitor 
 

  method vfunc f =
    let finfo = f.svar in
    fname := finfo.vname;
    (*visitCilFunction (my_num_stmt count ht eht fname new_exp_map) f ;*)
    DoChildren
  
  method vstmt s = 
    top := true;
    parentstmt := s.sid;
    ignore(Pretty.printf "\nSTMT %d=%a\n" !parentstmt d_stmt s);
    DoChildren

  method vexpr e =     
    let parents = Hashtbl.create 255 in
    let functions = Hashtbl.create 255 in
    let uses = ref 0 in
    let topcount = Hashtbl.create 255 in
    if Hashtbl.mem new_exp_map e then 
    begin 
      let (parents,functions,u,topcount) = Hashtbl.find new_exp_map e in
      uses:=!u;
    end else begin
      Hashtbl.add eht (Hashtbl.length eht + 1) (e,1.0) ;
      ignore(Pretty.printf "  EXP %d=%a\n" (Hashtbl.length eht) d_exp e);
    end;
    Hashtbl.add parents (Hashtbl.length parents + 1) !parentstmt;
    Hashtbl.add functions (Hashtbl.length functions + 1) !fname;
    incr uses;
    Hashtbl.add topcount (Hashtbl.length topcount + 1) !top;
    Hashtbl.replace new_exp_map e (parents,functions,uses,topcount);
    ignore(Pretty.printf "  EXP    %a, uses=%d, fname=%s, parentstmt=%d, top=%b\n" d_exp e !uses !fname !parentstmt !top); 
    top := false;
    DoChildren    

end
let my_num_exp = new numExpVisitor


(* This visitor walks over the C program AST and builds the list that
 * holds the expressions in the statement being considered for mutation *)
class findEEFaultVisitor top_dest exp_list count fname= object
  inherit nopCilVisitor
  method vexpr e = 
    ignore(Pretty.printf "  FindEFault e=%a count=%d\n" d_exp e !count); 
    if !top || not (probability top_dest) then 
      exp_list := (!count,!fname) :: !exp_list;
    top := false;
    incr count;
    DoChildren 
end
let my_find_eefault = new findEEFaultVisitor

let count = ref 0
class findEFaultVisitor stmt_id top_dest exp_list= object
  inherit nopCilVisitor 

  method vfunc f =
    let finfo = f.svar in
    fname := finfo.vname;
    (*visitCilFunction (my_find_efault_stmt stmt_id top_dest exp_list fname) f ;*)
    DoChildren

  method vstmt s = 
    if s.sid = stmt_id then begin
      top := true;
      count := 1 ;
      visitCilStmt (my_find_eefault top_dest exp_list count fname) s; 
      ()
    end else ();
    DoChildren 
end
let my_find_efault = new findEFaultVisitor

(* 
 * Visitor for computing statement coverage (for a "weighted path").
 *
 * This visitor walks over the C program AST and modifies it so that each
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

let my_empty = new emptyVisitor
let my_every = new everyVisitor
let my_cv = new covVisitor

let coverage_filename = "coverage.c" 
let coverage_exename = "./coverage" 
let coverage_outname = "coverage.path" 
let cilRep_version = "4" 
let sanity_filename = "repair.sanity.c" 
let sanity_exename = "./repair.sanity" 
let test_counter = ref 0 
let label_counter = ref 0 

(*************************************************************************
 * Atomic Mutations (e.g., delete on CIL statement) 
 *************************************************************************)

(* For debugging, it is sometimes handy to add an in-source label
 * indicating what we have changed. *) 
let possibly_label s str id =
  if !label_repair then
    let text = sprintf "__repair_%s_%d__%x" str id !label_counter in
    incr label_counter ;
    let new_label = Label(text,!currentLoc,false) in
    new_label :: s.labels 
  else s.labels 

(* Delete a single statement (atom) *)
class delVisitor (to_del : atom_id) = object
  inherit nopCilVisitor
  method vstmt s = ChangeDoChildrenPost(s, fun s ->
      if to_del = s.sid then begin 
        let block = {
          battrs = [] ;
          bstmts = [] ; 
        } in

        { s with skind = Block(block) ;
                 labels = possibly_label s "del" to_del; } 
      end else begin 
        s 
      end 
    ) 
end 
let my_del = new delVisitor 

(* Append a single statement (atom) after a given statement (atom) *)
class appVisitor (append_after : atom_id) 
                 (what_to_append : Cil.stmtkind) = object
  inherit nopCilVisitor
  method vstmt s = ChangeDoChildrenPost(s, fun s ->
      if append_after = s.sid then begin 
        let copy = copy what_to_append in 
        let block = {
          battrs = [] ;
          bstmts = [s ; { s with skind = copy } ] ; 
        } in
        { s with skind = Block(block) ; 
                 labels = possibly_label s "app" append_after ; 
        } 
      end else begin 
        s 
      end 
    ) 
end 
let my_app = new appVisitor 

(* Swap to statements (atoms) *)  
class swapVisitor 
    (sid1 : atom_id) 
    (skind1 : Cil.stmtkind) 
    (sid2 : atom_id) 
    (skind2 : Cil.stmtkind) 
                  = object
  inherit nopCilVisitor
  method vstmt s = ChangeDoChildrenPost(s, fun s ->
      if s.sid = sid1 then begin 
        { s with skind = copy skind2 ;
                 labels = possibly_label s "swap1" sid1 ;
        } 
      end else if s.sid = sid2 then begin 
        { s with skind = copy skind1  ;
                 labels = possibly_label s "swap2" sid2 ;
        }
      end else s 
    ) 
end 
let my_swap = new swapVisitor 

(* Swap two expressions *)
class swapEExpVisitor 
    (eid1 : atom_id) 
    (ekind2 : exp) 
    (stmt_id : int)
    (expcount : int ref)
              = object
  inherit nopCilVisitor 
  method vexpr e = ChangeDoChildrenPost(e, fun e ->
    incr expcount;
    ignore(Pretty.printf "    SWAP VISITOR eid=%d, e=%a\n" !expcount d_exp e );
    if !expcount = eid1 then 
      ekind2
    else 
      e
  )
end
let my_swap_eexp = new swapEExpVisitor

class swapExpVisitor 
    (eid1 : atom_id) 
    (ekind2 : exp) 
    (stmt_id : int)
              = object
  inherit nopCilVisitor 

  method vstmt s=
    if s.sid=stmt_id then begin
      let expcount = ref 0 in
      visitCilStmt (my_swap_eexp eid1 ekind2 s.sid expcount) s; 
      ()
    end else ();
    DoChildren
end
let my_swap_exp = new swapExpVisitor

class putVisitor 
    (sid1 : atom_id) 
    (skind1 : Cil.stmtkind) 
                  = object
  inherit nopCilVisitor
  method vstmt s = ChangeDoChildrenPost(s, fun s ->
      if s.sid = sid1 then begin 
        { s with skind = skind1 ;
                 labels = possibly_label s "put" sid1 ;
        } 
      end else s 
    )  
end
let my_put = new putVisitor



let gotten_code = ref (mkEmptyStmt ()).skind

class getVisitor 
    (sid1 : atom_id) 
                  = object
  inherit nopCilVisitor
  method vstmt s = 
    (if s.sid = sid1 then 
      gotten_code := s.skind
    ) ;
    DoChildren
end

let my_get = new getVisitor

(*************************************************************************
 * The CIL Representation
 *************************************************************************)
let use_path_files = ref false 
let use_weight_file = ref false 

let repeat_src = ref 1.0
let zero_always = ref 0.0
let one_always = ref 0.0

let debug_put = ref false 

let allow_coverage_fail = ref false 

let _ =
  options := !options @
  [
    "--use-path-files", Arg.Set use_path_files, " use existing coverage.path.{pos,neg}" ;

    "--use-weight-file", Arg.Set use_weight_file, " use existing coverage.path (stmtid,weight) list" ;

    "--repeat_src", Arg.Set_float repeat_src, "Use X as probability that expression will be allowed into src table more than once";
    "--zero_always", Arg.Set_float zero_always, "Use X as probalitiy that zero will be included as an additional expression in src table (ignores repeat_src)";
    "--one_always", Arg.Set_float one_always, "Use X as probability that one will be included as an additional expression in src table (ignores repeat_src)";

    "--use-weight-file", Arg.Set use_weight_file, " use existing coverage.path (stmtid,weight) list" ;
    "--allow-coverage-fail", Arg.Set allow_coverage_fail, " allow coverage to fail its test cases" ;
    "--debug-put", Arg.Set debug_put, " note each #put in a variant's name" 


  ] 

class cilRep : representation = object (self) 
  inherit nullRep

  (***********************************
   * State Variables
   ***********************************)
  val base = ref Cil.dummyFile
  val stmt_map = ref (Hashtbl.create 255)
  val stmt_count = ref 1 
  val weighted_path = ref ([] : (atom_id* float) list) 
  val weights = ref (Hashtbl.create 255)  
  val already_sourced = ref None 
  val already_compiled = ref None 
  val history = ref [] 

  val exp_map = ref (Hashtbl.create 255)
  val master_exp_map = ref (Hashtbl.create 255)
  (***********************************
   * Methods
   ***********************************)

  (* indicate that cached information based on our AST structure
   * is no longer valid *) 
  method private updated () = 
    already_compiled := None ;
    already_sourced := None ; 
    () 

  (* make a fresh copy of this variant *) 
  method copy () = 
    ({< base = ref (Global.copy !base) ;
        history = ref !history ; 
        already_sourced = ref !already_sourced ; 
        already_compiled = ref !already_compiled ; 
      >} :> representation)

  (* serialize the state *) 
  method save_binary (filename : string) = begin
    let fout = open_out_bin filename in 
    Marshal.to_channel fout (cilRep_version) [] ; 
    Marshal.to_channel fout (!base) [] ;
    Marshal.to_channel fout (!stmt_map) [] ;
    Marshal.to_channel fout (!stmt_count) [] ;
    Marshal.to_channel fout (!weighted_path) [] ;
    Marshal.to_channel fout (!weights) [] ;

    Marshal.to_channel fout (!exp_map) [] ; (*?*)
     Marshal.to_channel fout (!master_exp_map) [] ;

    debug "cilRep: %s: saved\n" filename ; 
    close_out fout 
  end 

  (* load in serialized state *) 
  method load_binary (filename : string) = begin
    if !use_path_files then begin
      debug "cilRep: --use-path-files: not loading %s" filename ;
      failwith "--use-path-files" 
    end ;
    if !use_weight_file then begin
      debug "cilRep: --use-weight-files: not loading %s" filename ;
      failwith "--use-weight-files" 
    end ;
    let fout = open_in_bin filename in 
    let version = Marshal.from_channel fout in
    if version <> cilRep_version then begin
      debug "cilRep: %s has old version\n" filename ;
      failwith "version mismatch" 
    end ;
    base := Marshal.from_channel fout ; 
    stmt_map := Marshal.from_channel fout ;
    stmt_count := Marshal.from_channel fout ;
    weighted_path := Marshal.from_channel fout ; 
    weights := Marshal.from_channel fout ; 

    exp_map := Marshal.from_channel fout; 
    master_exp_map := Marshal.from_channel fout ;

    debug "cilRep: %s: loaded\n" filename ; 
    close_in fout 
  end 

  (* print debugging information *)  
  method debug_info () = begin
    debug "cilRep: stmt_count = %d\n" !stmt_count ;
    debug "cilRep: stmts in weighted_path = %d\n" 
      (List.length !weighted_path) ; 
    debug "cilRep: stmts in weighted_path with weight >= 1.0 = %d\n" 
      (List.length (List.filter (fun (a,b) -> b >= 1.0) !weighted_path)) ; 

  end 

  (* load in a CIL AST from a C source file *) 
  method from_source (filename : string) = begin 
    debug "cilRep: %s: parsing\n" filename ; 
    let file = Frontc.parse filename () in 
    debug "cilRep: %s: parsed\n" filename ; 
    visitCilFileSameGlobals my_every file ; 
    visitCilFileSameGlobals my_empty file ; 
    visitCilFileSameGlobals (my_num_stmt stmt_count !stmt_map) file;
    visitCilFileSameGlobals (my_num_exp !exp_map !master_exp_map) file ; 
    if probability !zero_always then begin
      let parents = Hashtbl.create 255 in
      let functions = Hashtbl.create 255 in
      let uses = ref 0 in
      let topcount = Hashtbl.create 255 in
      if Hashtbl.mem !master_exp_map zero then 
      begin 
        let (parents,functions,u,topcount) = Hashtbl.find !master_exp_map zero in
        uses:=!u;
      end else begin
        Hashtbl.add !exp_map (Hashtbl.length !exp_map + 1) (zero,1.0) ;
        ignore(Pretty.printf "  EXP %d=%a\n" (Hashtbl.length !exp_map) d_exp zero);
      end;
      Hashtbl.add parents (Hashtbl.length parents + 1) 0;
      Hashtbl.add functions (Hashtbl.length functions + 1) "";
      incr uses;
      Hashtbl.add topcount (Hashtbl.length topcount + 1) true;
      Hashtbl.replace !master_exp_map zero (parents,functions,uses,topcount);
    end;
    if probability !one_always then begin
      let parents = Hashtbl.create 255 in
      let functions = Hashtbl.create 255 in
      let uses = ref 0 in
      let topcount = Hashtbl.create 255 in
      if Hashtbl.mem !master_exp_map one then 
      begin 
        let (parents,functions,u,topcount) = Hashtbl.find !master_exp_map one in
        uses:=!u;
      end else begin
        Hashtbl.add !exp_map (Hashtbl.length !exp_map + 1) (one,1.0) ;
        ignore(Pretty.printf "  EXP %d=%a\n" (Hashtbl.length !exp_map) d_exp one);
      end;
      Hashtbl.add parents (Hashtbl.length parents + 1) 0;
      Hashtbl.add functions (Hashtbl.length functions + 1) "";
      incr uses;
      Hashtbl.add topcount (Hashtbl.length topcount + 1) true;
      Hashtbl.replace !master_exp_map one (parents,functions,uses,topcount);
    end;
    (*visitCilFileSameGlobals (my_num_exp exp_count !exp_map) file ;*)
    (* we increment after setting, so we're one too high: *) 
    stmt_count := pred !stmt_count ; 
    base := file ; 
  end 

  (* Perform various sanity checks. Currently we check to
   * ensure that that original program passes all positive
   * tests and fails all negative tests. *) 
  method sanity_check () = begin
    debug "cilRep: sanity checking begins\n" ; 
    self#output_source sanity_filename ; 
    let c = self#compile ~keep_source:true sanity_filename sanity_exename in
    if not c then begin
      debug "cilRep: %s: does not compile\n" sanity_filename ;
      exit 1 
    end ; 
    for i = 1 to !pos_tests do
      let r = self#internal_test_case sanity_exename (Positive i) in
      debug "\tp%d: %b\n" i r ;
      assert(r) ; 
    done ;
    for i = 1 to !neg_tests do
      let r = self#internal_test_case sanity_exename (Negative i) in
      debug "\tn%d: %b\n" i r ;
      assert(not r) ; 
    done ;
    debug "cilRep: sanity checking passed\n" ; 
  end 

  (* Compile this variant to an executable on disk. *)
  method compile ?(keep_source=false) source_name exe_name = begin
    let cmd = Printf.sprintf "%s -o %s %s %s >& /dev/null" 
      !compiler_name exe_name source_name !compiler_options in 
    let result = (match Stats2.time "compile" Unix.system cmd with
    | Unix.WEXITED(0) -> 
        already_compiled := Some(exe_name) ; 
        true
    | _ -> 
        already_compiled := Some("") ; 
        debug "\t%s fails to compile\n" (self#name ()) ; 
        incr compile_failures ;
        false 
    ) in
    if not keep_source then begin
      Unix.unlink source_name ; 
    end ;
    result
  end 

  (* An intenral method for the raw running of a test case.
   * This does the bare bones work: execute the program
   * on the test case. No caching at this level. *)
  method private internal_test_case exe_name test = begin
    (*debug "        cilrep: entering internal_test_case\n";*)
    let port_arg = Printf.sprintf "%d" !port in
    change_port () ; 
    let cmd = Printf.sprintf "%s %s %s %s >& /dev/null" 
      !test_command exe_name (test_name test) port_arg in 
    (*debug "        cilrep: starting match Stats2.time - cmd = %s\n" cmd;*)
    match Stats2.time "test" Unix.system cmd with
    | Unix.WEXITED(0) -> true
    | _ -> false  
  end 

  (* Pretty-print this CIL AST to a C file *) 
  method output_source source_name = begin
    Stats2.time "output_source" (fun () -> 
    let fout = open_out source_name in
    begin try 
      (* if you've done truly evil mutation -- for example, changing
       * ( *fptr )(args) into (1)(args) -- CIL will die here with
       * internal sanity checking. Basically, this means you have
       * a C file that can't compile, so this exception handling
       * causes us to print out an empty file. This problem was
       * noticed by Briana around Fri Apr 16 10:25:11 EDT 2010.
       *)
      iterGlobals !base (fun glob ->
        dumpGlobal defaultCilPrinter fout glob ;
      ) ; 
    with _ -> () end ;
    close_out fout ;
    let digest = Digest.file source_name in  
    already_sourced := Some(digest) ; 
    ) () 
  end 

  (* This is our public interface for running a single test case.
   * It checks in the cache, compiles this AST to an EXE if  
   * needed, and runs the EXE on the test case. *) 
  method test_case test = try begin

  (*debug "    cilrep: test_case %s %s (digest=%S)\n" 
      (self#name ()) (test_name test) 
      (match !already_sourced with | None -> "" | Some(x) -> x) ; *)

    let try_cache () = 
      (* first, maybe we'll get lucky with the persistent cache *) 
      (match !already_sourced with
      | None -> ()
      | Some(digest) -> begin 
        match test_cache_query digest test with
        | Some(x) -> raise (Test_Result x)
        | _ -> ()
        end  
      )  
    in 
    try_cache () ; 
    (*debug "     cilrep: test_case - check if already compiled\n";*)
    (* second, maybe we've already compiled it *) 
    let exe_name, worked = match !already_compiled with
    | None -> (* never compiled before, so compile it now *) 
      (*debug "     cilrep: test_case - never compiled\n";*)
      let source_name = sprintf "%05d.c" !test_counter in  
      let exe_name = sprintf "./%05d" !test_counter in  
      incr test_counter ; 
      self#output_source source_name ; 
      try_cache () ; 
      if not (self#compile source_name exe_name) then 
        exe_name,false
      else
        exe_name,true

    | Some("") -> 
      (*debug "     cilrep: test_case - failed to compile before\n";*)
      "", false (* it failed to compile before *) 
    | Some(exe) -> 
       (*debug "     cilrep: test_case - compiled before\n";*)
       exe, true (* it compiled successfully before *) 
    in
    (*debug "     cilrep: test_case - before computing result\n";*)
    let result = 
      if worked then begin 
        (* actually run the program on the test input *) 
        (*debug "     cilrep: test_case - run program on test input\n";*)
        self#internal_test_case exe_name test ;
      end else false 
    in 
    (*debug "     cilrep: test_case - result computed\n";*)
    (* record result for posterity in the cache *) 
    (match !already_sourced with
    | None -> ()
    | Some(digest) -> test_cache_add digest test result
    ) ; 
    raise (Test_Result(result))
  (*debug "     cilrep: test_case - additional bookeeping info\n";*)
  end with Test_Result(x) -> (* additional bookkeeping information *) 
    (match !already_sourced with
    | None -> ()
    | Some(digest) -> Hashtbl.replace tested (digest,test) () 
    ) ;
   (*debug "cilrep: exiting test_case";*)
    x

  (* Compute the fault localization information. For now, this is 
   * weighted path localization based on statement coverage. *) 
  method compute_fault_localization () = begin
    assert(!base <> Cil.dummyFile) ; 
    debug "cilRep: computing fault localization information\n" ; 
    if !use_path_files || !use_weight_file then
      (* do nothing, we'll just read the user-provided files below *) 
      ()
    else begin 
      (* instrument the program with statement printfs *)
      let file = copy !base in 
      visitCilFileSameGlobals my_cv file ; 
      let new_global = GVarDecl(stderr_va,!currentLoc) in 
      file.globals <- new_global :: file.globals ; 
      let fd = Cil.getGlobInit file in 
      let lhs = (Var(stderr_va),NoOffset) in 
      let data_str = coverage_outname in 
      let str_exp = Const(CStr(data_str)) in 
      let str_exp2 = Const(CStr("wb")) in 
      let instr = Call((Some(lhs)),fopen,[str_exp;str_exp2],!currentLoc) in 
      let new_stmt = Cil.mkStmt (Instr[instr]) in 
      fd.sbody.bstmts <- new_stmt :: fd.sbody.bstmts ; 
      (* print out the instrumented source code *) 
      let fout = open_out coverage_filename in
      iterGlobals file (fun glob ->
        dumpGlobal defaultCilPrinter fout glob ;
      ) ; 
      close_out fout ;
      (* compile the instrumented program *) 
      if not (self#compile ~keep_source:true coverage_filename coverage_exename) then begin
        debug "ERROR: cannot compile %s\n" coverage_filename ;
        exit 1 
      end ;
      (* run the instrumented program *) 
      if not (self#internal_test_case coverage_exename (Positive 1)) then begin 
        debug "ERROR: coverage FAILS test Positive 1\n" ;
        if not !allow_coverage_fail then exit 1 
      end ;
      Unix.rename coverage_outname (coverage_outname ^ ".pos") ;
      if (self#internal_test_case coverage_exename (Negative 1)) then begin 
        debug "ERROR: coverage PASSES test Negative 1\n" ;
        if not !allow_coverage_fail then exit 1 
      end ;
      Unix.rename coverage_outname (coverage_outname ^ ".neg") ;
      (* now we have a positive path and a negative path *) 
    end ;

    weighted_path := [] ; 

    for i = 1 to !stmt_count do
      Hashtbl.replace !weights i 0.1 ;
    done ;

    if !use_weight_file then begin
      (* Give a list of "line,weight" pairs. You can separate with
         commas and/or whitespace. If you leave off the weight,
         we assume 1.0. *) 
      let fin = open_in (coverage_outname) in 
      let regexp = Str.regexp "[ ,\t]" in 
      (try while true do
        let line = input_line fin in
        let words = Str.split regexp line in
        let s, w = 
          match words with
          | [stmt] -> (int_of_string stmt), 1.0 
          | [stmt ; weight] -> (int_of_string stmt), (float_of_string weight)
          | _ -> debug "ERROR: %s: malformed line:\n%s\n" coverage_outname line;
                 failwith "malformed input" 
        in 
        Hashtbl.replace !weights s 0.5 ;
        weighted_path := (s,w) :: !weighted_path 

      done with _ -> close_in fin) ;
      weighted_path := List.rev !weighted_path ; 

    end else begin 
      (* This is the normal case. The user is not overriding our
       * positive and negative path files, so we'll read them both
       * in and combine them to get the weighted path. *) 

      let neg_ht = Hashtbl.create 255 in 
      let pos_ht = Hashtbl.create 255 in 
      let fin = open_in (coverage_outname ^ ".pos") in 
      (try while true do (* read in positive path *) 
        let line = input_line fin in
        Hashtbl.replace pos_ht line () ;
        Hashtbl.replace !weights (int_of_string line) 0.5 ;
      done with _ -> close_in fin) ;

      let fin = open_in (coverage_outname ^ ".neg") in 
      (try while true do (* read in negative path *) 
        let line = input_line fin in
        if Hashtbl.mem neg_ht line then
          ()
        else begin 
          (* a statement only on the negative path gets weight 1.0 ;
           * if it is also on the positive path, its weight is 0.1 *) 
          let weight = if Hashtbl.mem pos_ht line then 0.1 else 1.0 in 
          weighted_path := (int_of_string line, weight) :: !weighted_path ;
          Hashtbl.replace neg_ht line () ; 
          Hashtbl.replace !weights (int_of_string line) 0.5 ; 
        end 
      done with _ -> close_in fin) ;

      weighted_path := List.rev !weighted_path ; 
    end 
  end 


  (* Compute the exp fix localization information. For now, this is 
   * a test run for a test.c *) 
  method compute_exp_fix_localization () =
    Hashtbl.iter(fun num (exp,weight) ->
      if not (exp = zero) then 
        Hashtbl.replace !exp_map num (exp,0.0);
    ) !exp_map;
    

  (* return the total number of statements, for search strategies that
   * want to iterate over all statements or consider them uniformly 
   * at random *) 
  method max_atom () = !stmt_count  


  method get_quark_src_lst top_src local_src fault_fname=
    let src_list = ref [] in
    Hashtbl.iter(fun num (exp,weight) ->
      begin
        let (parents, functions, uses, topcount) = Hashtbl.find !master_exp_map exp in
        Hashtbl.iter(fun n parentnum ->
          let fname = Hashtbl.find functions n in
          let top = Hashtbl.find topcount n in
          if (probability weight) && (n=1 || probability !repeat_src) && (fault_fname=fname || not (probability !local_src)) && (top=true || not (probability !top_src)) then
            src_list:=num::!src_list; 
        )parents;
      end
    ) !exp_map;
    !src_list

  method get_quark_fault_lst top_dest stmtnum =
    let fault_list = ref [] in 
    visitCilFileSameGlobals (my_find_efault stmtnum !top_dest fault_list) !base;
    !fault_list 
    

  method get_fault_localization () = !weighted_path 

  method get_fix_localization () = 
    let res = ref [] in 
    Hashtbl.iter (fun  stmt_id weight  ->
      res := (stmt_id,weight) :: !res 
    ) !weights ;
    !res

  (* give a "descriptive" name for this variant. For CIL, the name is
   * based on the atomic mutations applied in order. Those are stored
   * in the "history" list. *) 
  method name () = 
    if !history = [] then "original"
    else begin 
      let b = Buffer.create 40 in
      ignore (List.rev_map (fun s ->
        Buffer.add_string b s ; () 
      ) !history) ;
      Buffer.contents b 
    end 

  (* Atomic Delete of a single statement (atom) *) 
  method delete stmt_id = 
    self#updated () ; 
    history := (sprintf "d(%d)" stmt_id) :: !history ;
    visitCilFileSameGlobals (my_del stmt_id) !base  

  (* Atomic Append of a single statement (atom) after another statement *) 
  method append append_after what_to_append = 
    self#updated () ; 
    history := (sprintf "a(%d,%d)" append_after what_to_append) :: !history ;
    let what = 
      try Hashtbl.find !stmt_map what_to_append 
      with _ -> debug "append: %d not found\n" what_to_append ; exit 1
    in 
    visitCilFileSameGlobals (my_app append_after what) !base  

  (* Atomic Swap of two statements (atoms) *)
  method swap stmt_id1 stmt_id2 = 
    self#updated () ; 
    history := (sprintf "s(%d,%d)" stmt_id1 stmt_id2) :: !history ;
    let skind1 = 
      try Hashtbl.find !stmt_map stmt_id1 
      with _ -> debug "swap: %d not found\n" stmt_id1 ; exit 1
    in 
    let skind2 = 
      try Hashtbl.find !stmt_map stmt_id2 
      with _ -> debug "swap: %d not found\n" stmt_id2 ; exit 1
    in 
    visitCilFileSameGlobals (my_swap stmt_id1 skind1 
                                    stmt_id2 skind2) !base

  (* Atomic Swap of two expressions *)
  method swap_exp exp_id1 exp_id2 stmt_id=
    self #updated () ;
    history := (sprintf "e(%d-%d,%d)" stmt_id exp_id1 exp_id2 ) :: !history ;
    let ekind2 = 
      try 
        let (ek2,_) = Hashtbl.find !exp_map exp_id2 in 
        ek2
      with _ -> debug "swap_exp: %d not found\n" exp_id2 ; exit 1
    in
    ignore(Pretty.printf "\n  SWAP e(%d-%d,%d=%a)\n" stmt_id exp_id1 exp_id2 d_exp ekind2);
    visitCilFileSameGlobals (my_swap_exp exp_id1 ekind2 stmt_id) !base
  
  method put stmt_id stmt = 
    self#updated () ; 
    (if !debug_put then 
      history := (sprintf "p(%d)" (stmt_id)) :: !history ;
    ) ;
    visitCilFileSameGlobals (my_put stmt_id stmt) !base

  method add_name_note str =
    history := str :: !history 

  method get stmt_id =
    let output = 
      try Hashtbl.find !stmt_map stmt_id
      with _ -> debug "get: %d not found\n" stmt_id ; exit 1
    in 
      output	

  method get_fault_weight stmt_id =
    let weight = ref 0.0 in
    List.iter (fun (s,w) ->
      if stmt_id = s then
        weight := w;
    )!weighted_path;
    !weight
    (*let temp_tbl = Hashtbl.create 255 in
    List.iter(fun (s,w) ->
      Hashtbl.add temp_tbl s w
    ) !weighted_path;
    Hashtbl.find temp_tbl stmt_id*)

end 
