type ('a,'b) t = {
  announce : ('a,'b) Node.t Atomic.t array;
  head : ('a,'b) Node.t Atomic.t array;
  tail : ('a,'b) Node.t;
  num_threads : int;
  tid_of_domain : (int, int) Hashtbl.t;
  tid_lock : Mutex.t;
  next_tid : int Atomic.t;
}

let create num_threads = 
  let tail = Node.create None num_threads in 
  Node.set_seq tail 1;
  {
    head = Array.init num_threads (fun _ -> Atomic.make tail);
    announce = Array.init num_threads (fun _ -> Atomic.make tail);
    tail;
    num_threads;
    tid_of_domain = Hashtbl.create num_threads;
    tid_lock = Mutex.create ();
    next_tid = Atomic.make 0;
  }

let get_tid wfu_obj =
  let did = (Domain.self () :> int) in
  Mutex.lock wfu_obj.tid_lock;
  let tid =
    match Hashtbl.find_opt wfu_obj.tid_of_domain did with
    | Some tid -> tid
    | None ->
        let fresh = Atomic.fetch_and_add wfu_obj.next_tid 1 in
        if fresh >= wfu_obj.num_threads then begin
          Mutex.unlock wfu_obj.tid_lock;
          invalid_arg "WFUniversal.apply: more active domains than num_threads"
        end;
        Hashtbl.add wfu_obj.tid_of_domain did fresh;
        fresh
  in
  Mutex.unlock wfu_obj.tid_lock;
  tid

let apply wfu_obj new_obj invoc =
  let tid = get_tid wfu_obj in
  let anc = Node.create (Some invoc) wfu_obj.num_threads in
  Atomic.set wfu_obj.announce.(tid) anc;
  Atomic.set wfu_obj.head.(tid) (Node.max (Array.map Atomic.get wfu_obj.head));
  let rec aux () = 
    let ann_i = Atomic.get wfu_obj.announce.(tid) in
    if Node.get_seq ann_i <> 0 then ()
    else begin
      let before = Atomic.get wfu_obj.head.(tid) in
      let idx = ((Node.get_seq before) + 1) mod wfu_obj.num_threads in
      let help =  Atomic.get wfu_obj.announce.(idx) in
      let prefer = begin
        if Node.get_seq help = 0 then help
        else Atomic.get wfu_obj.announce.(tid)
      end in
      let after = CASConsensus.decide (Node.get_decide_next before) prefer tid in
      Node.set_next before after;
      Node.set_seq after (Node.get_seq before + 1);
      Atomic.set wfu_obj.head.(tid) after;
      aux ()
    end
  in aux ();
  let ann_i = Atomic.get wfu_obj.announce.(tid) in
  Atomic.set wfu_obj.head.(tid) ann_i;
  let rec app current acc = 
    if match current with
      | Some node -> node == ann_i
      | None -> false
    then begin
      match (Node.get_invoc ann_i) with
      | Some invoc -> invoc acc
      | None -> failwith "This should never happen 6"
    end
    else begin match current with
    | Some node -> begin
      match (Node.get_invoc node) with
      | Some invoc -> let (next, _) = invoc acc in app (Node.get_next node) next
      | None -> failwith "This should never happen 7"
      end
    | None -> failwith "This should never happen 8"
    end
  in
  app (Node.get_next wfu_obj.tail) new_obj