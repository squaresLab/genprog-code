open Globals

(*module type BelProp =
sig
  type graphT

  val initialize : graphT -> ranking -> unit 
  val doBP : graphT -> unit
    
end

module FaultProp =
  functor (G : BPGraph) ->
    functor (S : BPState with type t = G.stateT) -> 
struct 
  
  type graphT = G.t
  type stateT = S.t

  let initialize graph r = failwith "Not implemented"

  let doBP bpgraph = 
    

end

module FixProp =
  functor (G : Graph) ->
    functor (S : State with type t = G.stateT) -> 
struct 

end

module ProgRepair = 
  functor (G : Graph) ->
    functor (S : State with type t = G.stateT) -> 
struct 

end*)
