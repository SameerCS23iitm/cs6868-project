(** Simple lock-based stack.

    Every operation takes a mutex, performs the operation, and releases
    the mutex before returning. *)

exception Empty

type 'a t = {
  lock : Mutex.t;
  mutable data : 'a list;
}

let create () =
  { lock = Mutex.create (); data = [] }

let push s x =
  Mutex.lock s.lock;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock s.lock)
    (fun () -> s.data <- x :: s.data)

let try_pop s =
  Mutex.lock s.lock;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock s.lock)
    (fun () ->
      match s.data with
      | [] -> None
      | x :: xs ->
        s.data <- xs;
        Some x)

let pop s =
  match try_pop s with
  | Some x -> x
  | None -> raise Empty
