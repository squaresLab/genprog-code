(** The class used to represent software objects which will be read
    from disk modified, evaluated, and written back out to disk. *)
class virtual ('software_object) representation = object
  val software_object : 'software_object;
  val history : list of edits;
  (* - fitness? (should this live elsewhere?) *)
end

(* methods on representations *)
(** apply OPERATOR to REPRESENTATION returning the resulting representation *)
val mutate : represenation -> operator -> representation
(** delete the portion of representation identified by ID *)
val delete : representation -> id -> representation
(** insert the portion of representation identify by ID_from into ID_TO *)
val insert : representation -> id -> id -> representation
(** swap two elements of a representation *)
val swap : representation -> -> id -> representation
(** single point crossover between two representations *)
val crossover : representation -> representation -> id -> representation

val fitness : representation -> integer
val hash : representation -> integer

(** write a software object to disk *)
val to_disk : software_object -> string -> unit
(** read a software object from disk *)
val from_disk : string -> software_object

(** Used to identify a portion of a software object
    (e.g., index to array, subtree of a tree) *)
class virtual ('software_object) id = object
  (* basically empty, this is for use by specific software objects *)
end

(** where our abstractions meet actual software, these may be trees
    of statements, vectors of instructions or bytes, whatever... *)
class virtual software_object = object
  
end

(** check if a software object is sane *)
val sanity_check : software_object -> boolean
