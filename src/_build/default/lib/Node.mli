

type 'a t

(** Creates a new node with the given invocation *)
val create : 'a -> 'b

(** Returns the node with the maximum sequence number in the array *)
val max : 'a t Array.t -> 'a t