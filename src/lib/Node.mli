
(** 'a is the type of the object on which the invoc function operates. 
'a is also the type of the object invoc function returns *)
type 'a t

(** Creates a new node with the given value and sequence number *)
val create : ('a -> 'a) -> int -> 'a t

(** Returns the node with the maximum sequence number in the array *)
val max : 'a t array -> 'a t

(** Updates the sequence number of the given node *)
val set_seq : 'a t -> int -> unit

(** Returns the sequence number of the given node *)
val get_seq : 'a t -> int

(** Returns the CAS consensus instance associated with the given node *)
val get_decide_next : 'a t -> 'a t CASConsensus.t

(** sets the next node for the given node *)
val set_next : 'a t -> 'a t -> unit

(** returns the next node for the given node *)
val get_next : 'a t -> 'a t option

(** returns the function stored in the given node *)
val get_invoc : 'a t -> ('a -> 'a)