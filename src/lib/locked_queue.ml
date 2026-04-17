(** Simple lock-based FIFO queue.

    Every operation takes a mutex, performs the operation, and releases
    the mutex before returning. *)

exception Empty

type 'a t = {
  lock : Mutex.t;
  q : 'a Queue.t;
}

let create () =
  { lock = Mutex.create (); q = Queue.create () }

let enq q x =
  Mutex.lock q.lock;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock q.lock)
    (fun () -> Queue.push x q.q)

let try_deq q =
  Mutex.lock q.lock;
  Fun.protect
    ~finally:(fun () -> Mutex.unlock q.lock)
    (fun () ->
      if Queue.is_empty q.q then None
      else Some (Queue.pop q.q))

let deq q =
  match try_deq q with
  | Some x -> x
  | None -> raise Empty
