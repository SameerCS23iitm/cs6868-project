open Src
open Sequential


module LFStack = Universal.Make(LFUniversal)(SequentialStack)
module LFQueue = Universal.Make(LFUniversal)(SequentialQueue)

let num_threads = 8

let stack_create () = LFStack.create num_threads
let stack_push s x tid = ignore (LFStack.apply s (SequentialStack.Push x) tid)
let stack_pop s tid = LFStack.apply s SequentialStack.Pop tid

let queue_create () = LFQueue.create num_threads
let queue_enq q x tid = ignore (LFQueue.apply q (SequentialQueue.Enqueue x) tid)
let queue_deq q tid = LFQueue.apply q SequentialQueue.Dequeue tid


let test_stack_sequential () =
  Printf.printf "Stack: testing sequential operations...\n%!";
  let s = stack_create () in
  assert (stack_pop s 0 = None);
  stack_push s 1 0;
  stack_push s 2 0;
  stack_push s 3 0;
  assert (stack_pop s 0 = Some 3);
  assert (stack_pop s 0 = Some 2);
  assert (stack_pop s 0 = Some 1);
  assert (stack_pop s 0 = None);
  Printf.printf "Stack: sequential OK\n%!"


let test_stack_interleaved () =
  Printf.printf "Stack: testing interleaved push/pop...\n%!";
  let s = stack_create () in
  stack_push s 10 0;
  assert (stack_pop s 0 = Some 10);
  stack_push s 20 0;
  stack_push s 30 0;
  assert (stack_pop s 0 = Some 30);
  stack_push s 40 0;
  assert (stack_pop s 0 = Some 40);
  assert (stack_pop s 0 = Some 20);
  assert (stack_pop s 0 = None);
  Printf.printf "Stack: interleaved OK\n%!"

let test_stack_fill_and_drain () =
  Printf.printf "Stack: testing fill and drain...\n%!";
  let n = 1000 in
  let s = stack_create () in
  for i = 0 to n - 1 do
    stack_push s i 0
  done;
  for i = n - 1 downto 0 do
    assert (stack_pop s 0 = Some i)
  done;
  assert (stack_pop s 0 = None);
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
      match stack_pop s (num_producers + id) with
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


