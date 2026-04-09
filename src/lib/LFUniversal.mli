

type 'a t

(** [create num_threads] creates a new universal construction instance for num_threads threads. *)
val create : int -> 'a t

(** [apply lfu_obj invoc new_obj tid] is called by the thread with id [tid] and 
    applies the operation represented by [invoc] to the object [new_obj] in the universal construction instance [lfu_obj] *)
(*new_obj is a newly constructed object*)
(*invoc is a partially applied function. Its only missing argument is the object. 
It returns a new object of the same type after applying the function *)
val apply : 'a t -> ('a -> 'a) -> 'a -> int -> 'a