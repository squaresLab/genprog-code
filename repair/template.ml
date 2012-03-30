open Cil
open Global


(* Ref: referenced in the hole referenced by the integer *)
type hole_type = Stmt_hole | Exp_hole | Lval_hole
type constraints =  Fault_path | Fix_path | Ref of string | InScope of string

module OrderedConstraint = 
struct
  type t = constraints
  let compare c1 c2 = 
	if c1 = c2 then 0 else 
	  match c1,c2 with
	  | Fault_path,_ -> -1
	  | _,Fault_path -> 1
	  | Fix_path,_ -> -1
	  | _,Fix_path -> 1
	  | Ref(i1),Ref(i2)
	  | Ref(i1),InScope(i2)
	  | InScope(i1),Ref(i2)
	  | InScope(i1),InScope(i2) -> compare i1 i2
end

module ConstraintSet = Set.Make(OrderedConstraint)

type hole = hole_type * ConstraintSet.t

module OrderedHole =
struct 
  type t = hole
  let compare h1 h2 =
	match h1, h2 with
	  (ht1,cons1),(ht2,cons2) ->
		  match ht1,ht2 with
			Stmt_hole, Stmt_hole
		  | Exp_hole, Exp_hole
		  | Lval_hole, Lval_hole -> compare cons1 cons2
		  | Stmt_hole, Exp_hole
		  | Stmt_hole, Lval_hole
		  | Exp_hole, Lval_hole -> 1
		  | Exp_hole, Stmt_hole
		  | Lval_hole,Stmt_hole
		  | Lval_hole, Exp_hole -> -1
end

module HoleSet = Set.Make(OrderedHole)

type filled = hole_type * int * int option

type hole_info =
	{
	  hole_id : string;
	  htyp : hole_type;
	  constraints : ConstraintSet.t
	}

type 'a template = 
	{
	  template_name : string;
	  hole_constraints : hole_info StringMap.t;
	  hole_code_ht : (string, 'a) Hashtbl.t
	}


(*** CilRep-specific template stuff *)


let hole_regexp = Str.regexp "__hole[0-9]+__"

exception FoundIt of string

class collectConstraints template_constraints_ht template_code_ht template_name = object
  inherit nopCilVisitor

  method vfunc fundec =
	let hole_ht = hcreate 10 in
	let holes = 
	  lfilt (fun varinfo -> Str.string_match hole_regexp varinfo.vname 0) 
		fundec.slocals in
	  liter
		(fun varinfo ->
		  let htyp =
			let [Attr(_,[AStr(typ)])] = 
			  filterAttributes "holetype" varinfo.vattr in
			  match typ with
				"stmt" -> Stmt_hole
			  | "lval" -> Lval_hole
			  | "exp" -> Exp_hole
			  | _ -> failwith "Unexpected value in htype value"
		  in
		  let constraints = 
			lfoldl
			  (fun constraints attr ->
				match attr with
				  Attr("constraint", [AStr("fault_path")]) -> 
					ConstraintSet.add Fault_path constraints
				| Attr("constraint", [AStr("fix_path")]) -> 
				  ConstraintSet.add Fix_path constraints
				| Attr("inscope", [AStr(v)]) -> 
				  ConstraintSet.add (InScope(v)) constraints
				| Attr("reference", [AStr(v)]) -> 
				  ConstraintSet.add (Ref(v)) constraints
				| _ -> constraints
			  ) ConstraintSet.empty varinfo.vattr
		  in
			hrep hole_ht varinfo.vname 
			  { hole_id=varinfo.vname; htyp=htyp; constraints=constraints})
		holes;
	  template_name := fundec.svar.vname;
	  hadd template_constraints_ht !template_name hole_ht;
	  DoChildren
		
  method vblock block =
	match block.battrs with
	  [] -> DoChildren
	| lst ->
	  let hole_ht = hfind template_constraints_ht !template_name in
	  let holes = 
		hfold (fun k -> fun v -> fun lst -> k :: lst) hole_ht [] in
		try
		  liter
			(fun attr ->
			  match attr with
				Attr(name,_) ->
					liter (fun hole -> 
					  if ("__"^name^"__") = hole then 
						raise (FoundIt(hole))
					) holes
			) block.battrs;
		  DoChildren
		with FoundIt(holename) ->
		  begin
			let newattrs = dropAttribute ("__"^holename^"__") block.battrs in
			let code_ht = ht_find template_code_ht !template_name 
			  (fun _ -> hcreate 10) in
			  hadd code_ht holename 
				({ block with battrs=newattrs });
			  hrep template_code_ht !template_name code_ht;
			  DoChildren
		  end
end

