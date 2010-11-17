open Batteries
open Set
open Map
open Random
open Utils
open Diffs

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
	Set.iter (fun p -> let str = DP.to_string p in pprintf "%s" str) config

  let print_cluster cluster = 
	Set.iter (fun point -> 
				let str = DP.to_string point in
				  pprintf "  %s" str) cluster

  let print_clusters clusters =
	Map.iter
	  (fun medoid ->
		 fun cluster ->
		   let medoid = DP.to_string medoid in 
			 pprintf "medoid: %s" medoid;
			 pprintf "  Cluster: ";
			 print_cluster cluster;
			 pprintf "\n"; flush stdout) clusters

(* lots and lots and lots and lots and lots of caching *)

  let clusters_cache : (pointSet, (clusters * float)) Hashtbl.t = hcreate 100

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
	ht_find clusters_cache medoids
	  (fun x -> 
		 let clusters,cost =
		   Set.fold
			 (fun point -> 
				fun (clusters,cost) ->
				  let all_distances =
					Set.fold
					  (fun medoid -> 
						 fun distance_map ->
						   let distance = DP.distance point medoid in
							 Map.add distance (medoid,distance) distance_map
					  ) medoids Map.empty
				  in
				  let _,(medoid,distance) = Map.min_binding all_distances in
				  let cluster = try Map.find medoid clusters with Not_found -> Set.empty in
				  let cluster' = Set.add point cluster in
					(Map.add medoid cluster' clusters),((DP.cost medoid point) +. cost)
			 ) data ((Map.empty),0.0) 
		 in
		   hadd clusters_cache medoids (clusters,cost);
		   (clusters, cost))

  let new_config (config : configuration) (medoid : DP.t) (point : DP.t) : configuration =
	Set.add point (Set.remove medoid config) 

  let kmedoid (k : int) (data : pointSet) : configuration = 
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
			 if cost' < cost then
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
module DiffCluster = KClusters(Diffs)
