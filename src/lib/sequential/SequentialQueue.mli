

(** The type representing the state of the queue *)
type 'a state = 'a list

(** The type representing operations on the queue *)
type 'a op =
  | Enqueue of 'a
  | Dequeue


(** [apply state op] applies the operation [op] to the queue [state], returning
    the new state and the result of the operation. *)
val apply : 'a state -> 'a op -> 'a state * 'a option


val empty : 'a state