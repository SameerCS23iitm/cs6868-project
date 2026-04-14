(** Sequential stack interface *)

(** The type representing the state of the stack *)
type 'a state = 'a list

(** The type representing operations on the stack *)
type 'a op =
  | Push of 'a
  | Pop

(** [apply state op] applies the operation [op] to the stack [state], returning
    the new state and the result of the operation. *)
val apply : 'a state -> 'a op -> 'a state * 'a option

val empty : 'a state