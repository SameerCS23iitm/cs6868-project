type ('a,'b) t = {
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
    tail;
    num_threads;
    tid_of_domain = Hashtbl.create num_threads;
    tid_lock = Mutex.create ();
    next_tid = Atomic.make 0;
  }

let get_tid lfu_obj =
  let did = (Domain.self () :> int) in
  match Hashtbl.find_opt lfu_obj.tid_of_domain did with
  | Some tid -> tid
  | None ->
      Mutex.lock lfu_obj.tid_lock;
      let tid =
        match Hashtbl.find_opt lfu_obj.tid_of_domain did with
        | Some tid -> tid
        | None ->
            let fresh = Atomic.fetch_and_add lfu_obj.next_tid 1 in
            if fresh >= lfu_obj.num_threads then begin
              Mutex.unlock lfu_obj.tid_lock;
              invalid_arg "LFUniversal.apply: more active domains than num_threads"
            end;
            Hashtbl.add lfu_obj.tid_of_domain did fresh;
            fresh
      in
      Mutex.unlock lfu_obj.tid_lock;
      tid

let apply lfu_obj new_obj invoc =
  let tid = get_tid lfu_obj in
  let prefer = Node.create (Some invoc) lfu_obj.num_threads in
  let rec aux () =
    if Node.get_seq prefer <> 0 then ()
    else begin let before = Node.max (Array.map Atomic.get lfu_obj.head) in
    let after = CASConsensus.decide (Node.get_decide_next before) prefer tid in
    Node.set_next before after;
    Node.set_seq after (Node.get_seq before + 1);
    Atomic.set lfu_obj.head.(tid) after;
    aux ()
    end
  in
  aux ();
  let rec app current acc = 
    if match current with
      | Some node -> node == prefer
      | None -> false
    then begin 
      match Node.get_invoc prefer with
        | Some invoc -> invoc acc
        | None -> failwith "This should never happen 3"
      end
    else begin 
      match current with
      | Some node -> let new_acc = match Node.get_invoc node with
                                   | Some invoc -> fst (invoc acc)
                                   | None -> failwith "This should never happen 4"
                     in
                     app (Node.get_next node) new_acc
      | None -> failwith "This should never happen 5"
    end
  in
  app (Node.get_next lfu_obj.tail) new_obj
