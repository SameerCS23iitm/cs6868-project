(** Simple lock-based FIFO queue.

    Every operation acquires a mutex for the full duration of the operation. *)

type 'a t
exception Empty

val create : unit -> 'a t
(** [create ()] creates an empty queue. *)

val enq : 'a t -> 'a -> unit
(** [enq q x] enqueues [x] at the tail of [q]. *)

val try_deq : 'a t -> 'a option
(** [try_deq q] dequeues and returns [Some v], or [None] if empty. *)

val deq : 'a t -> 'a
(** [deq q] dequeues and returns a value.
    @raise Empty if the queue is empty. *)
