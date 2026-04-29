type ('a, 'b) t = {
  announce : ('a, 'b) Node.t Atomic.t array;
  head : ('a, 'b) Node.t Atomic.t array;
  tail : ('a, 'b) Node.t;
  num_threads : int;
  next_tid : int Atomic.t;
}

type stats = {
  helped : int;
  own : int;
}

let create num_threads =
  let tail = Node.create None (num_threads+1) in
  Node.set_seq tail 1;
  {
    head = Array.init (num_threads+1) (fun _ -> Atomic.make tail);
    announce = Array.init (num_threads+1) (fun _ -> Atomic.make tail);
    tail;
    num_threads=num_threads+1;
    next_tid = Atomic.make 0;
  }

let apply_with_stats wfu_obj new_obj invoc =
  let tid = Domain.self_index () in
  let anc = Node.create (Some invoc) wfu_obj.num_threads in
  Atomic.set wfu_obj.announce.(tid) anc;
  Atomic.set wfu_obj.head.(tid) (Node.max (Array.map Atomic.get wfu_obj.head));
  let rec aux () =
    let ann_i = Atomic.get wfu_obj.announce.(tid) in
    if Node.get_seq ann_i <> 0 then ()
    else begin
      let before = Atomic.get wfu_obj.head.(tid) in
      let idx = (Node.get_seq before + 1) mod wfu_obj.num_threads in
      let help = Atomic.get wfu_obj.announce.(idx) in
      let prefer =
        if Node.get_seq help = 0 then help
        else Atomic.get wfu_obj.announce.(tid)
      in
      let after = CASConsensus.decide (Node.get_decide_next before) prefer tid in
      Node.set_next before after;
      Node.set_seq after (Node.get_seq before + 1);
      Atomic.set wfu_obj.head.(tid) after;
      aux ()
    end
  in
  aux ();
  let ann_i = Atomic.get wfu_obj.announce.(tid) in
  Atomic.set wfu_obj.head.(tid) ann_i;
  let rec app current acc helped =
    if
      match current with
      | Some node -> node == ann_i
      | None -> false
    then
      match Node.get_invoc ann_i with
      | Some invoc ->
          let (next_acc, result) = invoc acc in
          (next_acc, result, { helped; own = 1 })
      | None -> failwith "This should never happen"
    else
      match current with
      | Some node ->
          let next_acc =
            match Node.get_invoc node with
            | Some invoc -> fst (invoc acc)
            | None -> failwith "This should never happen"
          in
          app (Node.get_next node) next_acc (helped + 1)
      | None -> failwith "This should never happen"
  in
  app (Node.get_next wfu_obj.tail) new_obj 0

let apply wfu_obj new_obj invoc =
  let (next_obj, result, _stats) = apply_with_stats wfu_obj new_obj invoc in
  (next_obj, result)
