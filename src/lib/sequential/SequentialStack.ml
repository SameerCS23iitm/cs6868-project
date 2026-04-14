(** Sequential stack implementation *)

(** The type representing the state of the stack *)
type 'a state = 'a list

(** The type representing operations on the stack *)
type 'a op =
  | Push of 'a
  | Pop


let empty = []

(** [apply state op] applies the operation [op] to the stack [state], returning
    the new state and the result of the operation. *)
let apply state op =
  match op with
  | Push x -> (x :: state, None)
  | Pop -> (
      match state with
      | [] -> ([], None)
      | x :: xs -> (xs, Some x)
    )