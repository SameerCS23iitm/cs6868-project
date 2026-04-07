
type 'a t

(** [create n] creates a new consensus instance for n threads. *)
val create : int -> 'a t

(** [propose c v i] proposes value [v] for thread [i] in consensus instance [c]. *)
val propose : 'a t -> 'a -> int -> unit

(** [decide c v i] returns the decided value for thread [i] in consensus instance [c]. *)
val decide : 'a t -> 'a -> int -> 'a