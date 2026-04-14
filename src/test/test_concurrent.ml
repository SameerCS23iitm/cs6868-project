open Src
open Sequential

module LFStack = Universal.Make(LFUniversal)(SequentialStack)
module LFQueue = Universal.Make(LFUniversal)(SequentialQueue)

module WFStack = Universal.Make(WFUniversal)(SequentialStack)
module WFQueue = Universal.Make(WFUniversal)(SequentialQueue)

let num_threads = 8


module MakeTests (U : sig
  module Stack : sig
    type t
    val create : int -> t
    val apply : t -> int SequentialStack.op -> int -> int option
  end

  module Queue : sig
    type t
    val create : int -> t
    val apply : t -> int SequentialQueue.op -> int -> int option
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






end