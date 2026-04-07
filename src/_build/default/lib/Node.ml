
type t =
  Node :
  {
    decide_next : t CASConsensus.t;
    next : t option;
    seq : int;
    invoc : 'f;
  }
  -> t

let create invoc num_threads = Node {
    decide_next = CASConsensus.create num_threads; 
    next = None; 
    seq = 0;
    invoc;
  }

let max arr = 
  let rec aux acc i = 
    if i >= Array.length arr then acc
    else 
      if arr.(i).seq > acc.seq then aux arr.(i) (i + 1)
      else aux acc (i + 1)
  in
  aux arr.(0) 1