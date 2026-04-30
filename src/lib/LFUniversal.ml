type ('a,'b) t = {
  head : ('a,'b) Node.t Atomic.t array;
  tail : ('a,'b) Node.t;
  num_threads : int;
}

(* Initialize the universal construction with a shared tail node. *)
let create num_threads = 
  let tail = Node.create None (num_threads+1) in 
  Node.set_seq tail 1;
  {
    head = Array.init (num_threads+1) (fun _ -> Atomic.make tail);
    tail;
    num_threads=num_threads+1
  }

(* Apply an invocation by linearizing it into the global log via consensus. *)
let apply lfu_obj new_obj invoc =
  let tid = Domain.self_index () in
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
