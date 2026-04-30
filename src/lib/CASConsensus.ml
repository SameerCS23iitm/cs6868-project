
type 'a t = {
  num_threads : int;
  proposed : 'a option Atomic.t array; 
  winner : int Atomic.t;
}

let create num_threads = {
  num_threads;
  proposed = Array.init num_threads (fun _ -> Atomic.make None);
  winner = Atomic.make (-1);
}

(* Propose a value for the calling thread; does not resolve consensus. *)
let propose consensus value tid = 
  Atomic.set (consensus.proposed.(tid)) (Some value)

(* Decide the consensus value: first thread to claim winner wins, others read it. *)
let decide consensus value tid =
  propose consensus value tid;
  if Atomic.compare_and_set consensus.winner (-1) tid then begin
    match Atomic.get consensus.proposed.(tid) with
    | Some v -> v
    | None -> failwith "This should never happen 1"
  end
  else begin
    let winner_tid = Atomic.get consensus.winner in
    match Atomic.get consensus.proposed.(winner_tid) with
    | Some v -> v
    | None -> failwith "This should never happen 2"
  end
    
