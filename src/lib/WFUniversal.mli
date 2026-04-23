type ('a,'b) t

(** [create num_threads] creates a new universal construction instance for num_threads threads. *)
val create : int -> ('a,'b) t

(** [apply lfu_obj new_obj invoc] applies the operation represented by [invoc]
    to the object [new_obj] in the universal construction instance [lfu_obj]. *)
(*invoc is a partially applied function. Its only missing argument is the object. 
It returns a new object of the same type along with the result after applying the function.
new_obj is a newly constructed object of type 'a *)
val apply : ('a,'b) t -> 'a -> ('a -> 'a * 'b) -> 'a * 'b