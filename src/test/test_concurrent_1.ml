open Src
open Sequential


module LFStack = Universal.Make(LFUniversal)(SequentialStack)
module LFQueue = Universal.Make(LFUniversal)(SequentialQueue)

module WFStack = Universal.Make(WFUniversal)(SequentialStack)
module WFQueue = Universal.Make(WFUniversal)(SequentialQueue)

let num_threads = 8

(* let stack_create () = LFStack.create num_threads
let stack_push s x tid = ignore (LFStack.apply s (SequentialStack.Push x) tid)
let Impl.stack_pop s tid = LFStack.apply s SequentialStack.Pop tid

let queue_create () = LFQueue.create num_threads
let queue_enq q x tid = ignore (LFQueue.apply q (SequentialQueue.Enqueue x) tid)
let queue_deq q tid = LFQueue.apply q SequentialQueue.Dequeue tid

let wf_stack_create () = WFStack.create num_threads
let wf_stack_push s x tid = ignore (WFStack.apply s (SequentialStack.Push x) tid)
let wf_Impl.stack_pop s tid = WFStack.apply s SequentialStack.Pop tid

let wf_queue_create () = WFQueue.create num_threads
let wf_queue_enq q x tid = ignore (WFQueue.apply q (SequentialQueue.Enqueue x) tid)
let wf_queue_deq q tid = WFQueue.apply q SequentialQueue.Dequeue tid *)


module type STACK_QUEUE = sig
  type stack
  type queue

  val stack_create : unit -> stack
  val stack_push : stack -> int -> int -> unit
  val Impl.stack_pop : stack -> int -> int option

  val queue_create : unit -> queue
  val queue_enq : queue -> int -> int -> unit
  val queue_deq : queue -> int -> int option
end


module LFImpl : STACK_QUEUE = struct
  type stack = int LFStack.t
  type queue = int LFQueue.t

  let stack_create () = LFStack.create num_threads
  let stack_push s x tid = ignore (LFStack.apply s (SequentialStack.Push x) tid)
  let Impl.stack_pop s tid = LFStack.apply s SequentialStack.Pop tid

  let queue_create () = LFQueue.create num_threads
  let queue_enq q x tid = ignore (LFQueue.apply q (SequentialQueue.Enqueue x) tid)
  let queue_deq q tid = LFQueue.apply q SequentialQueue.Dequeue tid
end



module WFImpl : STACK_QUEUE = struct
  type stack = int WFStack.t
  type queue = int WFQueue.t

  let stack_create () = WFStack.create num_threads
  let stack_push s x tid = ignore (WFStack.apply s (SequentialStack.Push x) tid)
  let Impl.stack_pop s tid = WFStack.apply s SequentialStack.Pop tid

  let queue_create () = WFQueue.create num_threads
  let queue_enq q x tid = ignore (WFQueue.apply q (SequentialQueue.Enqueue x) tid)
  let queue_deq q tid = WFQueue.apply q SequentialQueue.Dequeue tid
end


module MakeTests (Impl : STACK_QUEUE) = struct

let test_stack_sequential () =
  Printf.printf "Stack: testing sequential operations...\n%!";
  let s = stack_create () in
  assert (Impl.stack_pop s 0 = None);
  stack_push s 1 0;
  stack_push s 2 0;
  stack_push s 3 0;
  assert (Impl.stack_pop s 0 = Some 3);
  assert (Impl.Impl.stack_pop s 0 = Some 2);
  assert (Impl.stack_pop s 0 = Some 1);
  assert (Impl.stack_pop s 0 = None);
  Printf.printf "Stack: sequential OK\n%!"


let test_stack_interleaved () =
  Printf.printf "Stack: testing interleaved push/pop...\n%!";
  let s = stack_create () in
  stack_push s 10 0;
  assert (Impl.stack_pop s 0 = Some 10);
  stack_push s 20 0;
  stack_push s 30 0;
  assert (Impl.stack_pop s 0 = Some 30);
  stack_push s 40 0;
  assert (Impl.stack_pop s 0 = Some 40);
  assert (Impl.stack_pop s 0 = Some 20);
  assert (Impl.stack_pop s 0 = None);
  Printf.printf "Stack: interleaved OK\n%!"

let test_stack_fill_and_drain () =
  Printf.printf "Stack: testing fill and drain...\n%!";
  let n = 1000 in
  let s = stack_create () in
  for i = 0 to n - 1 do
    stack_push s i 0
  done;
  for i = n - 1 downto 0 do
    assert (Impl.stack_pop s 0 = Some i)
  done;
  assert (Impl.stack_pop s 0 = None);
  Printf.printf "Stack: fill and drain OK\n%!"



let test_stack_concurrent () =
  Printf.printf "Stack: testing concurrent operations...\n%!";
  let num_producers = 4 in
  let num_consumers = 4 in
  let items_per_producer = 250 in
  let total_items = num_producers * items_per_producer in
  let s = stack_create () in
  let seen = Array.make total_items false in
  let seen_lock = Mutex.create () in
  let consumed = Atomic.make 0 in
  let producer id =
    let start = id * items_per_producer in
    for i = start to start + items_per_producer - 1 do
      stack_push s i id
    done
  in
  let consumer id =
    while Atomic.get consumed < total_items do
      match Impl.stack_pop s (num_producers + id) with
      | Some v ->
        Mutex.lock seen_lock;
        seen.(v) <- true;
        Mutex.unlock seen_lock;
        ignore (Atomic.fetch_and_add consumed 1)
      | None ->
        Domain.cpu_relax ()
    done
  in
  let producers = Array.init num_producers (fun id ->
    Domain.spawn (fun () -> producer id)) in
  let consumers = Array.init num_consumers (fun id ->
    Domain.spawn (fun () -> consumer id)) in
  Array.iter Domain.join producers;
  Array.iter Domain.join consumers;
  for i = 0 to total_items - 1 do
    if not seen.(i) then Printf.printf "MISSING item %d\n%!" i;
    assert seen.(i)
  done;
  Printf.printf "Stack: concurrent OK\n%!"





let test_queue_sequential () =
  Printf.printf "Queue: testing sequential operations...\n%!";
  let q = queue_create () in
  assert (queue_deq q 0 = None);
  queue_enq q 1 0;
  queue_enq q 2 0;
  queue_enq q 3 0;
  assert (queue_deq q 0 = Some 1);
  assert (queue_deq q 0 = Some 2);
  assert (queue_deq q 0 = Some 3);
  assert (queue_deq q 0 = None);
  Printf.printf "Queue: sequential OK\n%!"

let test_queue_interleaved () =
  Printf.printf "Queue: testing interleaved enq/deq...\n%!";
  let q = queue_create () in
  queue_enq q 10 0;
  assert (queue_deq q 0 = Some 10);
  queue_enq q 20 0;
  queue_enq q 30 0;
  assert (queue_deq q 0 = Some 20);
  queue_enq q 40 0;
  assert (queue_deq q 0 = Some 30);
  assert (queue_deq q 0 = Some 40);
  assert (queue_deq q 0 = None);
  Printf.printf "Queue: interleaved OK\n%!"



let test_queue_fill_and_drain () =
  Printf.printf "Queue: testing fill and drain...\n%!";
  let n = 1000 in
  let q = queue_create () in
  for i = 0 to n - 1 do
    queue_enq q i 0
  done;
  for i = 0 to n - 1 do
    assert (queue_deq q 0 = Some i)
  done;
  assert (queue_deq q 0 = None);
  Printf.printf "Queue: fill and drain OK\n%!"



let test_queue_concurrent () =
  Printf.printf "Queue: testing concurrent operations...\n%!";
  let num_producers = 4 in
  let num_consumers = 4 in
  let items_per_producer = 250 in
  let total_items = num_producers * items_per_producer in
  let q = queue_create () in
  let seen = Array.make total_items false in
  let seen_lock = Mutex.create () in
  let consumed = Atomic.make 0 in
  let producer id =
    let start = id * items_per_producer in
    for i = start to start + items_per_producer - 1 do
      queue_enq q i id
    done
  in
  let consumer id =
    while Atomic.get consumed < total_items do
      match queue_deq q (num_producers + id) with
      | Some v ->
        Mutex.lock seen_lock;
        seen.(v) <- true;
        Mutex.unlock seen_lock;
        ignore (Atomic.fetch_and_add consumed 1)
      | None ->
        Domain.cpu_relax ()
    done
  in
  let producers = Array.init num_producers (fun id ->
    Domain.spawn (fun () -> producer id)) in
  let consumers = Array.init num_consumers (fun id ->
    Domain.spawn (fun () -> consumer id)) in
  Array.iter Domain.join producers;
  Array.iter Domain.join consumers;
  for i = 0 to total_items - 1 do
    if not seen.(i) then Printf.printf "MISSING item %d\n%!" i;
    assert seen.(i)
  done;
  Printf.printf "Queue: concurrent OK\n%!"


end



module LFTests = MakeTests(LFImpl)
module WFTests = MakeTests(WFImpl)

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

  Printf.printf "\n=== WF Tests ===\n\n%!";
  WFTests.test_stack_sequential ();
  WFTests.test_stack_interleaved ();
  WFTests.test_stack_fill_and_drain ();
  WFTests.test_stack_concurrent ();
  WFTests.test_queue_sequential ();
  WFTests.test_queue_interleaved ();
  WFTests.test_queue_fill_and_drain ();
  WFTests.test_queue_concurrent ();

  Printf.printf "\nAll tests passed for both LF and WF!\n%!"