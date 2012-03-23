type population : [representation]

(* methods on populations *)
val select : population -> representation

val search : population -> mutation_operation list -> population
