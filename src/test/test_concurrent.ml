open Src
open Sequential

module MakeLF (Seq : Sequential.SeqObject.S) = struct
  type 'a t = ('a Seq.state * 'a option, 'a option) LFUniversal.t

  let create = LFUniversal.create

  let apply obj op tid =
    let invoc (state, _) =
      let (next_state, result) = Seq.apply state op in
      ((next_state, result), result)
    in
    let initial_obj = (Seq.empty, None) in
    let (_new_obj, result) = LFUniversal.apply obj initial_obj invoc tid in
    result
end

module MakeWF (Seq : Sequential.SeqObject.S) = struct
  type 'a t = ('a Seq.state * 'a option, 'a option) WFUniversal.t

  let create = WFUniversal.create

  let apply obj op tid =
    let invoc (state, _) =
      let (next_state, result) = Seq.apply state op in
      ((next_state, result), result)
    in
    let initial_obj = (Seq.empty, None) in
    let (_new_obj, result) = WFUniversal.apply obj initial_obj invoc tid in
    result
end

module LFStack = MakeLF(SequentialStack)
module LFQueue = MakeLF(SequentialQueue)

module WFStack = MakeWF(SequentialStack)
module WFQueue = MakeWF(SequentialQueue)

module LFList = MakeLF(SequentialSortedList)
module WFList = MakeWF(SequentialSortedList)

let num_threads = 8


module MakeTests (U : sig
  module Stack : sig
    type 'a t
    val create : int -> 'a t
    val apply : 'a t -> 'a SequentialStack.op -> int -> 'a option
  end

  module Queue : sig
    type 'a t
    val create : int -> 'a t
    val apply : 'a t -> 'a SequentialQueue.op -> int -> 'a option
  end

  module SortedList : sig
    type 'a t
    val create : int -> 'a t
    val apply : 'a t -> 'a SequentialSortedList.op -> int -> 'a option
  end
  
end) = struct



    (* ===== STACK TESTS ===== *)

  let test_stack_sequential () =
    Printf.printf "Stack: testing sequential operations...\n%!";
    let s = U.Stack.create num_threads in
    assert (U.Stack.apply s SequentialStack.Pop 0 = None);
    ignore (U.Stack.apply s (SequentialStack.Push 1) 0);
    ignore (U.Stack.apply s (SequentialStack.Push 2) 0);
    ignore (U.Stack.apply s (SequentialStack.Push 3) 0);
    assert (U.Stack.apply s SequentialStack.Pop 0 = Some 3);
    assert (U.Stack.apply s SequentialStack.Pop 0 = Some 2);
    assert (U.Stack.apply s SequentialStack.Pop 0 = Some 1);
    assert (U.Stack.apply s SequentialStack.Pop 0 = None);
    Printf.printf "Stack: sequential OK\n%!"

  let test_stack_interleaved () =
    Printf.printf "Stack: testing interleaved push/pop...\n%!";
    let s = U.Stack.create num_threads in
    ignore (U.Stack.apply s (SequentialStack.Push 10) 0);
    assert (U.Stack.apply s SequentialStack.Pop 0 = Some 10);
    ignore (U.Stack.apply s (SequentialStack.Push 20) 0);
    ignore (U.Stack.apply s (SequentialStack.Push 30) 0);
    assert (U.Stack.apply s SequentialStack.Pop 0 = Some 30);
    ignore (U.Stack.apply s (SequentialStack.Push 40) 0);
    assert (U.Stack.apply s SequentialStack.Pop 0 = Some 40);
    assert (U.Stack.apply s SequentialStack.Pop 0 = Some 20);
    assert (U.Stack.apply s SequentialStack.Pop 0 = None);
    Printf.printf "Stack: interleaved OK\n%!"

  let test_stack_fill_and_drain () =
    Printf.printf "Stack: testing fill and drain...\n%!";
    let n = 1000 in
    let s = U.Stack.create num_threads in
    for i = 0 to n - 1 do
      ignore (U.Stack.apply s (SequentialStack.Push i) 0)
    done;
    for i = n - 1 downto 0 do
      assert (U.Stack.apply s SequentialStack.Pop 0 = Some i)
    done;
    assert (U.Stack.apply s SequentialStack.Pop 0 = None);
    Printf.printf "Stack: fill and drain OK\n%!"

  let test_stack_concurrent () =
    Printf.printf "Stack: testing concurrent operations...\n%!";
    let num_producers = 4 in
    let num_consumers = 4 in
    let items_per_producer = 250 in
    let total_items = num_producers * items_per_producer in
    let s = U.Stack.create num_threads in
    let seen = Array.make total_items false in
    let seen_lock = Mutex.create () in
    let consumed = Atomic.make 0 in

    let producer id =
      let start = id * items_per_producer in
      for i = start to start + items_per_producer - 1 do
        ignore (U.Stack.apply s (SequentialStack.Push i) id)
      done
    in

    let consumer id =
      while Atomic.get consumed < total_items do
        match U.Stack.apply s SequentialStack.Pop (num_producers + id) with
        | Some v ->
            Mutex.lock seen_lock;
            seen.(v) <- true;
            Mutex.unlock seen_lock;
            ignore (Atomic.fetch_and_add consumed 1)
        | None ->
            Domain.cpu_relax ()
      done
    in

    let producers =
      Array.init num_producers (fun id ->
        Domain.spawn (fun () -> producer id))
    in
    let consumers =
      Array.init num_consumers (fun id ->
        Domain.spawn (fun () -> consumer id))
    in

    Array.iter Domain.join producers;
    Array.iter Domain.join consumers;

    for i = 0 to total_items - 1 do
      if not seen.(i) then Printf.printf "MISSING item %d\n%!" i;
      assert seen.(i)
    done;

    Printf.printf "Stack: concurrent OK\n%!"



(* ===== QUEUE TESTS ===== *)

  let test_queue_sequential () =
    Printf.printf "Queue: testing sequential operations...\n%!";
    let q = U.Queue.create num_threads in
    assert (U.Queue.apply q SequentialQueue.Dequeue 0 = None);
    ignore (U.Queue.apply q (SequentialQueue.Enqueue 1) 0);
    ignore (U.Queue.apply q (SequentialQueue.Enqueue 2) 0);
    ignore (U.Queue.apply q (SequentialQueue.Enqueue 3) 0);
    assert (U.Queue.apply q SequentialQueue.Dequeue 0 = Some 1);
    assert (U.Queue.apply q SequentialQueue.Dequeue 0 = Some 2);
    assert (U.Queue.apply q SequentialQueue.Dequeue 0 = Some 3);
    assert (U.Queue.apply q SequentialQueue.Dequeue 0 = None);
    Printf.printf "Queue: sequential OK\n%!"

  let test_queue_interleaved () =
    Printf.printf "Queue: testing interleaved enq/deq...\n%!";
    let q = U.Queue.create num_threads in
    ignore (U.Queue.apply q (SequentialQueue.Enqueue 10) 0);
    assert (U.Queue.apply q SequentialQueue.Dequeue 0 = Some 10);
    ignore (U.Queue.apply q (SequentialQueue.Enqueue 20) 0);
    ignore (U.Queue.apply q (SequentialQueue.Enqueue 30) 0);
    assert (U.Queue.apply q SequentialQueue.Dequeue 0 = Some 20);
    ignore (U.Queue.apply q (SequentialQueue.Enqueue 40) 0);
    assert (U.Queue.apply q SequentialQueue.Dequeue 0 = Some 30);
    assert (U.Queue.apply q SequentialQueue.Dequeue 0 = Some 40);
    assert (U.Queue.apply q SequentialQueue.Dequeue 0 = None);
    Printf.printf "Queue: interleaved OK\n%!"

  let test_queue_fill_and_drain () =
    Printf.printf "Queue: testing fill and drain...\n%!";
    let n = 1000 in
    let q = U.Queue.create num_threads in
    for i = 0 to n - 1 do
      ignore (U.Queue.apply q (SequentialQueue.Enqueue i) 0)
    done;
    for i = 0 to n - 1 do
      assert (U.Queue.apply q SequentialQueue.Dequeue 0 = Some i)
    done;
    assert (U.Queue.apply q SequentialQueue.Dequeue 0 = None);
    Printf.printf "Queue: fill and drain OK\n%!"

  let test_queue_concurrent () =
    Printf.printf "Queue: testing concurrent operations...\n%!";
    let num_producers = 4 in
    let num_consumers = 4 in
    let items_per_producer = 250 in
    let total_items = num_producers * items_per_producer in
    let q = U.Queue.create num_threads in
    let seen = Array.make total_items false in
    let seen_lock = Mutex.create () in
    let consumed = Atomic.make 0 in

    let producer id =
      let start = id * items_per_producer in
      for i = start to start + items_per_producer - 1 do
        ignore (U.Queue.apply q (SequentialQueue.Enqueue i) id)
      done
    in

    let consumer id =
      while Atomic.get consumed < total_items do
        match U.Queue.apply q SequentialQueue.Dequeue (num_producers + id) with
        | Some v ->
            Mutex.lock seen_lock;
            seen.(v) <- true;
            Mutex.unlock seen_lock;
            ignore (Atomic.fetch_and_add consumed 1)
        | None ->
            Domain.cpu_relax ()
      done
    in

    let producers =
      Array.init num_producers (fun id ->
        Domain.spawn (fun () -> producer id))
    in
    let consumers =
      Array.init num_consumers (fun id ->
        Domain.spawn (fun () -> consumer id))
    in

    Array.iter Domain.join producers;
    Array.iter Domain.join consumers;

    for i = 0 to total_items - 1 do
      if not seen.(i) then Printf.printf "MISSING item %d\n%!" i;
      assert seen.(i)
    done;

    Printf.printf "Queue: concurrent OK\n%!"


(* ===== SORTED LIST TESTS ===== *)


let test_list_sequential () =
  Printf.printf "List: testing sequential operations...\n%!";
  let l = U.SortedList.create num_threads in

  ignore (U.SortedList.apply l (SequentialSortedList.Insert 5) 0);
  ignore (U.SortedList.apply l (SequentialSortedList.Insert 10) 0);
  ignore (U.SortedList.apply l (SequentialSortedList.Insert 3) 0);

  (* duplicate insert *)
  ignore (U.SortedList.apply l (SequentialSortedList.Insert 5) 0);

  assert (U.SortedList.apply l (SequentialSortedList.Contains 5) 0 = Some 5);
  assert (U.SortedList.apply l (SequentialSortedList.Contains 10) 0 = Some 10);
  assert (U.SortedList.apply l (SequentialSortedList.Contains 3) 0 = Some 3);
  assert (U.SortedList.apply l (SequentialSortedList.Contains 7) 0 = None);

  ignore (U.SortedList.apply l (SequentialSortedList.Remove 5) 0);
  assert (U.SortedList.apply l (SequentialSortedList.Contains 5) 0 = None);

  Printf.printf "List: sequential OK\n%!"




let test_list_concurrent () =
  Printf.printf "List: testing concurrent operations...\n%!";
  let l = U.SortedList.create num_threads in

  let num_domains = 4 in
  let ops_per_domain = 250 in

  let worker id =
    let start = id * ops_per_domain in
    for i = 0 to ops_per_domain - 1 do
      let value = start + i in
      ignore (U.SortedList.apply l (SequentialSortedList.Insert value) id);
      if i mod 2 = 0 then
        ignore (U.SortedList.apply l (SequentialSortedList.Remove value) id);
    done
  in

  let domains =
    Array.init num_domains (fun id ->
      Domain.spawn (fun () -> worker id))
  in
  Array.iter Domain.join domains;

  (* verification *)
  for id = 0 to num_domains - 1 do
    for i = 0 to ops_per_domain - 1 do
      let value = id * ops_per_domain + i in
      let expected = i mod 2 <> 0 in

      let res = U.SortedList.apply l (SequentialSortedList.Contains value) 0 in

      match (expected, res) with
      | true, Some _ -> ()
      | false, None -> ()
      | _ -> assert false
    done
  done;

  Printf.printf "List: concurrent OK\n%!"



let test_list_high_contention () =
  Printf.printf "List: testing high contention...\n%!";
  let l = U.SortedList.create num_threads in

  let n_domains = 8 in
  let n_ops = 1000 in

  let worker id =
    for _ = 1 to n_ops do
      let key = Random.int 10 in
      match Random.int 3 with
      | 0 -> ignore (U.SortedList.apply l (SequentialSortedList.Insert key) id)
      | 1 -> ignore (U.SortedList.apply l (SequentialSortedList.Remove key) id)
      | _ -> ignore (U.SortedList.apply l (SequentialSortedList.Contains key) id)
    done
  in

  let domains =
    Array.init n_domains (fun id ->
      Domain.spawn (fun () -> worker id))
  in
  Array.iter Domain.join domains;

  Printf.printf "List: high contention OK\n%!"




end




module LF = struct
  module Stack = LFStack
  module Queue = LFQueue
  module SortedList = LFList
end

module WF = struct
  module Stack = WFStack
  module Queue = WFQueue
  module SortedList = WFList
end


module LFTests = MakeTests(LF)
module WFTests = MakeTests(WF)





let () =
  Printf.printf "=== LF Tests ===\n\n%!";
  LFTests.test_stack_sequential ();
  LFTests.test_stack_interleaved ();
  LFTests.test_stack_fill_and_drain ();
  LFTests.test_stack_concurrent ();
  LFTests.test_queue_sequential ();
  LFTests.test_queue_interleaved ();
  LFTests.test_queue_fill_and_drain ();
  LFTests.test_queue_concurrent ();
  LFTests.test_list_sequential ();
  LFTests.test_list_concurrent ();
  LFTests.test_list_high_contention ();

  Printf.printf "\n=== WF Tests ===\n\n%!";
  WFTests.test_stack_sequential ();
  WFTests.test_stack_interleaved ();
  WFTests.test_stack_fill_and_drain ();
  WFTests.test_stack_concurrent ();
  WFTests.test_queue_sequential ();
  WFTests.test_queue_interleaved ();
  WFTests.test_queue_fill_and_drain ();
  WFTests.test_queue_concurrent ();
  WFTests.test_list_sequential ();
  WFTests.test_list_concurrent ();
  WFTests.test_list_high_contention ();

  Printf.printf "\nAll tests passed for both LF and WF!\n%!"
