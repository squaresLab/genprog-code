open Batteries
open Set
open Map
open Random
open Utils
open Globals
open Datapoint
open Diffs
open Distance
open Ttypes
open Tprint
open Template

let cluster = ref false 
let k = ref 2

module TemplateDP =
struct
  type t = int
  let default = -1
  let is_default def = def == (-1)
  let outfile = ref ""

  let cache_ht = hcreate 10 

  let to_string it = 
	let (actual,_) = hfind init_template_tbl it in
	  itemplate_to_str actual (* this is just one change, not sets of changes! Remember that!*)

  let count = ref 0 
  let set_save saveto = outfile := saveto
  let load_from loadfrom = 
	let fin = open_in_bin loadfrom in
	let res1 = Marshal.input fin in 
	  hiter (fun k -> fun v -> hadd cache_ht k v) res1; 
	  close_in fin

  let distance it1 it2 = 
	let it1, it2 = if it1 < it2 then it1,it2 else it2,it2 in 
(*	  pprintf "DEBUG, distance between %d and %d\n" it1 it2;
	flush stdout;*)
	ht_find cache_ht (it1,it2) 
		(fun _ ->
		  pprintf "%d: distance between %d, %d\n" !count it1 it2; flush stdout; incr count;
		  if it1 == it2 then 0.0 else 
			let template1,info1 = hfind init_template_tbl it1 in
			let template2,info2 = hfind init_template_tbl it2 in
			let synth = unify_itemplate template1 template2 in
			let synth_info = measure_info synth in
			  pprintf "template1: %s\n template2: %s\n synth: %s\n" (to_string it1) (to_string it2) (template_to_str synth); 
			let maxinfo = 2.0 /. ((1.0 /. float_of_int(info1)) +. (1.0 /. (float_of_int(info2)))) in
			let retval = (maxinfo -. float_of_int(synth_info)) /. maxinfo in
			let retval = if retval < 0.0 then 0.0 else retval in
			  pprintf "Info1: %d, info2: %d, maxinfo: %g synth_info: %d	distance: %g\n" info1 info2 maxinfo synth_info retval; 
			  if !outfile <> "" &&  !count / 20 == 0 then begin
				let fout = open_out_bin !outfile in 
				  Marshal.output fout cache_ht;
				  close_out fout
			  end;
			  retval)

  let precompute array =
	Array.iter
	  (fun key1 ->
		Array.iter
		  (fun key2 ->
			let key1,key2 = if key1 < key2 then key1,key2
			else key2,key1 in
	  pprintf "PRECOMPUTE, distance between %d and %d\n" key1 key2;
	flush stdout;

			  ignore(distance key1 key2)
		  ) array) array


end

module type KClusters =
sig
  type configuration

  type pointSet
  type pointMap
  type cluster
  type clusters 

  val print_configuration : configuration -> unit

  val cost : configuration -> float
  val random_config : int -> pointSet -> configuration
  val compute_clusters : configuration -> pointSet -> clusters * float
  val kmedoid : int -> pointSet -> configuration
end

module KClusters =
  functor (DP : DataPoint ) ->
struct

  type pointSet = DP.t Set.t
  type pointMap = (DP.t, pointSet) Map.t

  type cluster = pointSet
  type clusters = pointMap 

  (*
   * 1. Initialize: randomly select k of the n data points as the
   * medoids
   * 2. Associate each data point to the closest medoid. ("closest"
   * here is defined using any valid distance metric, most commonly
   * Euclidean distance, Manhattan distance or Minkowski distance) 
   * 3. For each medoid m
   *      1. For each non-medoid data point o
   *          1. Swap m and o and compute the total cost of the configuration
   * 4. Select the configuration with the lowest cost.
   * 5. repeat steps 2 to 5 until there is no change in the medoid.
   *)
  (* distance metrics on trees? *)

  type configuration = pointSet


  (* debug printout functions *)
  let print_configuration config =
	let num = ref 0 in
	Set.iter (fun p -> pprintf "Medoid %d: " !num; incr num; let str = DP.to_string p in pprintf "%s\n" str) config

  let print_cluster cluster medoid = 
	Set.iter (fun point -> 
				let str = DP.to_string point in
				let distance = DP.distance medoid point in 
				  pprintf "Distance from medoid: %g\n" distance;
				  pprintf "Point: %s\n\n" str) cluster

  let print_clusters clusters =
	let num = ref 0 in
	Map.iter
	  (fun medoid ->
		 fun cluster ->
		   pprintf "Cluster %d:\n" !num; incr num;
		   let medoidstr = DP.to_string medoid in 
			 pprintf "medoid: %s" medoidstr;
			 pprintf "  Cluster: ";
			 print_cluster cluster medoid;
			 pprintf "\n"; flush stdout) clusters

(* lots and lots and lots and lots and lots of caching *)

(*  let clusters_cache : (pointSet, (clusters * float)) Hashtbl.t = hcreate 100*)

  let random_config (k : int) (data : pointSet) : configuration =
	let data_enum = Set.enum data in
	let firstk = Enum.take k data_enum in
	  Set.of_enum firstk

(* takes a configuration (a set of medoids) and a set of data and
  computes a list of k clusters, where k is the length of the medoid
  set/configuraton.  A data point is in a cluster if its distance from
  the cluster's medoid is less than its distance from any other
  medoid. *)

  let compute_clusters (medoids : configuration) (data : pointSet) : clusters * float =
	Set.fold
	  (fun point -> 
		fun (clusters,cost) ->
		  let (distance,medoid) =
			Set.fold
			  (fun medoid -> 
				fun (bestdistance,bestmedoid) ->
				  let distance = DP.distance point medoid in
					if distance > bestdistance || DP.is_default bestmedoid 
					then (distance,medoid) 
					else (bestdistance,bestmedoid)
			  ) medoids (0.0,DP.default)
		  in
		  let cluster = try Map.find medoid clusters with Not_found -> Set.empty in
		  let cluster' = Set.add point cluster in
			(Map.add medoid cluster' clusters),((DP.distance medoid point) +. cost)
	  ) data ((Map.empty),0.0) 

  let new_config (config : configuration) (medoid : DP.t) (point : DP.t) : configuration =
	Set.add point (Set.remove medoid config) 

  let kmedoid ?(savestate=(false,"")) (k : int) (data : pointSet) : configuration = 
    pprintf "In kmedoid, k: %d\n" k; flush stdout;
    
	let init_config : configuration = random_config k data in
	let clusters,cost = compute_clusters init_config data in
	let configEnum =
	  Enum.seq
		(init_config,clusters,cost,clusters)
		(fun (config,clusters,cost,candidate_swaps) ->
		   (* first, pick a medoid *)
		   let possible_medoids = 
			 Set.filter (fun medoid -> Map.mem medoid candidate_swaps) config in
		   let medoid : DP.t = Set.choose possible_medoids in
			 (* pick a point in that medoid's cluster.  This is
				complicated by the fact that we don't want to try any
				swap more than once, so we keep a map of candidate
				swaps that maps medoids to a set of points in its
				cluster that we haven't tried yet *)

		   let candidates : pointSet = Map.find medoid candidate_swaps in
		   let point : DP.t = Set.choose candidates in 
			 (* since we're trying it, remove it from the list of
				candidate swaps *)

		   let candidates' : pointSet = Set.remove point candidates in
		   let candidate_swaps' : pointMap = 
			 if not (Set.is_empty candidates') then begin
			   Map.add medoid candidates' candidate_swaps
			 end
			 else Map.remove medoid candidate_swaps
		   in
			 (* now, swap the point and the medoid to get a new configuration *)
		   let config' : configuration = new_config config medoid point in
			 (* cluster based on that new configuration *)
		   let clusters',cost' = compute_clusters config' data in
			 if cost' < cost then (* FIXME: I CHANGED THIS so that larger
									 "costs" are rewarded *)
			   (* start over with this new configuration.  If this
				  point has been a medoid before, then we need to
				  remove the swap we just did from its candidate
				  swaps. Otherwise, it can be swapped with anything in
				  its cluster besides the swap we just did. *)
			   begin 
				 let candidate_swaps' : pointMap = Map.remove medoid candidate_swaps' in
				 let candidates : pointSet = 
				   if Map.mem point candidate_swaps' then
					 Map.find point candidate_swaps'
				   else Map.find point clusters'
				 in
				 let candidates' : pointSet = Set.remove medoid candidates in
				 let candidate_swaps'' : pointMap = 
				   if not (Set.is_empty candidates') 
				   then Map.add point candidates' candidate_swaps' 
				   else Map.remove point candidate_swaps'
				 in
				   (config',clusters',cost',candidate_swaps'')
			   end
			 else
			   begin
				 (config,clusters,cost,candidate_swaps')
			   end
		)
		(fun (config,clusters,cost,candidate_swaps) -> not (Map.is_empty candidate_swaps))
	in
	let (config,clusters,cost,candidate_swaps) = 
	  Enum.reduce
		(fun accum ->
		   fun next -> next) configEnum
	in 
	  pprintf "Best config is: ";
	  print_configuration config;
	  pprintf "  Clusters: \n";
	  print_clusters clusters;
	  pprintf "cost is: %g\n" cost; flush stdout;
	  config
end

module TestCluster = KClusters(XYPoint)
module TemplateCluster = KClusters(TemplateDP)
