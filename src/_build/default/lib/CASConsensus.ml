
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

let propose consensus value tid = 
  Atomic.set (consensus.proposed.(tid)) (Some value)

let decide consensus value tid =
  propose consensus value tid;
  if Atomic.compare_and_set consensus.winner (-1) tid then begin
    match Atomic.get consensus.proposed.(tid) with
    | Some v -> v
    | None -> failwith "This should never happen"
  end
  else begin
    let winner_tid = Atomic.get consensus.winner in
    match Atomic.get consensus.proposed.(winner_tid) with
    | Some v -> v
    | None -> failwith "This should never happen"
  end
    
