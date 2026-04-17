(** Simple lock-based stack.

    Every operation acquires a mutex for the full duration of the operation. *)

type 'a t
exception Empty

val create : unit -> 'a t
(** [create ()] creates an empty stack. *)

val push : 'a t -> 'a -> unit
(** [push s x] pushes [x] onto [s]. *)

val try_pop : 'a t -> 'a option
(** [try_pop s] pops and returns [Some v], or [None] if empty. *)

val pop : 'a t -> 'a
(** [pop s] pops and returns the top value.
    @raise Empty if the stack is empty. *)
