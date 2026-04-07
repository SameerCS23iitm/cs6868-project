

type t

(** Creates a new node with the given value and sequence number *)
val create : 'a -> int -> t

(** Returns the node with the maximum sequence number in the array *)
val max : t Array.t -> t