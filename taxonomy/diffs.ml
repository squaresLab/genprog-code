open Batteries 
open Utils
open Ref
open Unix
open IO
open Enum
open Str
open String
open List
open Globals

module type DataPoint = 
sig

  type t

  val to_string : t -> string
  val compare : t -> t -> int
  val cost : t -> t -> float
  val distance : t -> t -> float
end

module type Diffs =
sig
  type t
	
  (* Takes the url of an svn repository and a start and end revision
   * number, returns an enumeration of diffs that have to do with
   * "fixes", as specified in the log message *)

  val get_diffs : string -> int -> int -> t Enum.t

  (* cost should be normalized, and should involve caching. *)
  val cost : t -> t -> float

  val compare : t -> t -> int
end

module XYPoint =
struct 
  
  type t =
	  { x : int ;
		y : int ; }

  let to_string p = Printf.sprintf "%d,%d\n" p.x p.y
	
  let create x y = { x=x;y=y;}

  let compare p1 p2 = ((p1.x - p2.x) * (p1.x - p2.x)) + ((p1.y - p2.y) * (p1.y - p2.y))
  let cost p1 p2 = float_of_int ((abs (p1.x - p2.x)) + (abs (p1.y - p2.y)))
  let distance p1 p2 =  sqrt (float_of_int (compare p1 p2))
end

module Diffs =
struct

  (* this record type is "private" *)
  type rev = {
	revnum : int;
	logmsg : string;
	files : (string * string list) Enum.t;
  }

  (* diff type and initialization *)
  let diffid = ref 0

  type diff = {
	id : int;
	rev_num : int;
	fname : string;
	msg : string;
	syntactic : string list;
  }

  let new_diff revnum fname msg syntactic = 
	{id=(post_incr diffid);rev_num=revnum;fname=fname;msg=msg;syntactic=syntactic}


  (* collect diffs is a helper function for get_diffs *)

  let collect_diffs (rev : rev) (url : string) : diff Enum.t =
	let diffcmd = "svn diff -r"^(of_int (rev.revnum-1))^":"^(of_int rev.revnum)^" "^url in
	let innerInput = open_process_in ?autoclose:(Some(true)) ?cleanup:(Some(true)) diffcmd in
	let enumInput =  IO.lines_of innerInput in
	let finfos,(lastname,strs) =
	  efold
		(fun (finfos,(fname,strs)) ->
		   fun str ->
			 if (string_match index_regexp str 0) then 
			   begin
				 let split = Str.split space_regexp str in
				 let fname' = hd (tl split) in
				 let ext = 
				   try
					 let base = Filename.chop_extension fname' in
					   String.sub fname' ((String.length base)+1)
						 ((String.length fname') - ((String.length base)+1))
				   with _ -> "" in
				 let fname' =
				   match String.lowercase ext with
				   | "c" | "i" | ".h" | ".y" -> fname'
				   | _ -> "" in
				   ((fname,strs)::finfos),(fname',[])
			   end 
			 else 
			   if (String.is_empty fname) ||
				 (string_match junk str 0) then
				   (finfos,(fname,strs))
			   else (finfos,(fname,str::strs))
		) ([],("",[])) enumInput
	in
	let finfos = List.enum ((lastname,strs)::finfos) in
	let files = efilt (fun (str,_) -> not (String.is_empty str)) finfos in
	  ignore(close_process_in innerInput);
	  emap 
		(fun (str,diff) -> new_diff rev.revnum str rev.logmsg (List.rev diff)
		) files 

  let get_diffs url startrev endrev =
	let logcmd = "svn log "^url^" -r"^(of_int startrev)^":"^(of_int endrev) in
	let proc = open_process_in ?autoclose:(Some(true)) ?cleanup:(Some(true)) logcmd in
	let log =  IO.lines_of proc in
	let grouped = egroup (fun str -> string_match dashes_regexp str 0) log in
	let filtered =
	  efilt
		(fun enum ->
		   (not (eexists
				   (fun str -> (string_match dashes_regexp str 0)) enum))
		) grouped in
	let all_revs = 
	  emap 
		(fun one_enum ->
		   let first = Option.get (eget one_enum) in
		   let rev_num = int_of_string (string_after (hd (Str.split space_regexp first)) 1) in
			 ejunk one_enum;
			 let logmsg = efold (fun msg -> fun str -> msg^str) "" one_enum in
			   {revnum=rev_num;logmsg=logmsg;files=(Enum.empty())}
		) filtered in
	let only_fixes = 
	  efilt
		(fun rev ->
		   try
			 ignore(search_forward fix_regexp rev.logmsg 0); true
		   with Not_found -> false) all_revs
	in
	let with_files = 
	  eflat (emap (fun rev -> collect_diffs rev url) only_fixes) 
	in
	  efilt (fun diff -> not (String.is_empty diff.fname)) with_files (*in*)
		
  (* this takes integers as diff keys instead of proper diffs, which I
	 think should be faster/take less memory? Maybe not *)

  let distance d1 d2 = failwith "Not implemented" (* REMEMBER TO CACHE! *)
	

end
