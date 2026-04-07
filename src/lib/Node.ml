
type t =
  Node :
  {
    decide_next : t CASConsensus.t;
    next : t option;
    seq : int;
    invoc : 'f; (*invoc is a function*)
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
    else begin match arr.(i) with
    | Node r -> begin match acc with
                | Node acc_r -> if r.seq > acc_r.seq then aux arr.(i) (i + 1) 
                                else aux acc (i + 1)
                end
    end
  in
  aux arr.(0) 1