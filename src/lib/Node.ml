
type 'a t =
  {
    decide_next : 'a t CASConsensus.t;
    next : 'a t option Atomic.t;
    seq : int Atomic.t;
    invoc : 'a -> 'a (*invoc is a function*)
  }

let create invoc num_threads = {
    decide_next = CASConsensus.create num_threads; 
    next = Atomic.make None;
    seq = Atomic.make 0;
    invoc;
  }

let max arr = 
  let rec aux acc i =
    if i >= Array.length arr then acc
    else
      let cur = arr.(i) in
      if Atomic.get cur.seq > Atomic.get acc.seq then aux cur (i+1)
      else aux acc (i+1)
  in
  aux arr.(0) 1

let set_seq r v =
  Atomic.set r.seq v

let get_seq r = Atomic.get r.seq

let get_next r = Atomic.get r.next

let set_next r next_node =
  Atomic.set r.next (Some next_node)

let get_decide_next r = r.decide_next

let get_invoc r = r.invoc