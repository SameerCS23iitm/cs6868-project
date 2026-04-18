open QCheck
open Universal_instances

let int_small = nat_small

(* IMPORTANT: must match create *)
let num_threads = 2

let norm_tid tid =
  let m = tid mod num_threads in
  if m < 0 then m + num_threads else m


(* ================= STACK ================= *)

module StackSpec (S : sig
  type 'a t
  val create : int -> 'a t
  val apply : 'a t -> 'a Sequential.SequentialStack.op -> int -> 'a option
end) = struct
  type t = int S.t

  let init () = S.create num_threads     
  let cleanup _ = ()

  let api =
    let open Lin in
    [
      val_ "push"
        (fun s x tid ->
          S.apply s (Sequential.SequentialStack.Push x) (norm_tid tid))
        (t @-> int_small @-> int @-> returning (option int));

      val_ "pop"
        (fun s tid ->
          S.apply s Sequential.SequentialStack.Pop (norm_tid tid))
        (t @-> int @-> returning (option int));
    ]
end

module LFStackTest = Lin_domain.Make(StackSpec(LFStack))
module WFStackTest = Lin_domain.Make(StackSpec(WFStack))


(* ================= QUEUE ================= *)

module QueueSpec (Q : sig
  type 'a t
  val create : int -> 'a t
  val apply : 'a t -> 'a Sequential.SequentialQueue.op -> int -> 'a option
end) = struct
  type t = int Q.t

  let init () = Q.create num_threads
  let cleanup _ = ()

  let api =
    let open Lin in
    [
      val_ "enq"
        (fun q x tid ->
          Q.apply q (Sequential.SequentialQueue.Enqueue x) (norm_tid tid))
        (t @-> int_small @-> int @-> returning (option int));

      val_ "deq"
        (fun q tid ->
          Q.apply q Sequential.SequentialQueue.Dequeue (norm_tid tid))
        (t @-> int @-> returning (option int));
    ]
end

module LFQueueTest = Lin_domain.Make(QueueSpec(LFQueue))
module WFQueueTest = Lin_domain.Make(QueueSpec(WFQueue))


(* ================= SORTED LIST ================= *)

module ListSpec (L : sig
  type 'a t
  val create : int -> 'a t
  val apply : 'a t -> 'a Sequential.SequentialSortedList.op -> int -> 'a option
end) = struct
  type t = int L.t

  let init () = L.create num_threads
  let cleanup _ = ()

  let api =
    let open Lin in
    [
      val_ "insert"
        (fun l x tid ->
          L.apply l (Sequential.SequentialSortedList.Insert x) (norm_tid tid))
        (t @-> int_small @-> int @-> returning (option int));

      val_ "remove"
        (fun l x tid ->
          L.apply l (Sequential.SequentialSortedList.Remove x) (norm_tid tid))
        (t @-> int_small @-> int @-> returning (option int));

      val_ "contains"
        (fun l x tid ->
          L.apply l (Sequential.SequentialSortedList.Contains x) (norm_tid tid))
        (t @-> int_small @-> int @-> returning (option int));
    ]
end

module LFListTest = Lin_domain.Make(ListSpec(LFList))
module WFListTest = Lin_domain.Make(ListSpec(WFList))


(* ================= SKIP LIST ================= *)

module SkipListSpec (L : sig
  type 'a t
  val create : int -> 'a t
  val apply :
    'a t ->
    'a Universal_instances.SeqSkipListAdapter.op ->
    int -> 'a option
end) = struct
  type t = int L.t

  let init () = L.create num_threads
  let cleanup _ = ()

  let api =
    let open Lin in
    [
      val_ "insert"
        (fun l x tid ->
          L.apply l (Universal_instances.SeqSkipListAdapter.Insert x)
            (norm_tid tid))
        (t @-> int_small @-> int @-> returning (option int));

      val_ "remove"
        (fun l x tid ->
          L.apply l (Universal_instances.SeqSkipListAdapter.Remove x)
            (norm_tid tid))
        (t @-> int_small @-> int @-> returning (option int));

      val_ "contains"
        (fun l x tid ->
          L.apply l (Universal_instances.SeqSkipListAdapter.Contains x)
            (norm_tid tid))
        (t @-> int_small @-> int @-> returning (option int));
    ]
end

module LFSkipListTest = Lin_domain.Make(SkipListSpec(LFSkipList))
module WFSkipListTest = Lin_domain.Make(SkipListSpec(WFSkipList))


(* ================= RUN ================= *)

let () =
  QCheck_base_runner.run_tests_main [

    LFStackTest.lin_test ~count:3 ~name:"LF Stack Lin";
    WFStackTest.lin_test ~count:3 ~name:"WF Stack Lin";

    LFQueueTest.lin_test ~count:3 ~name:"LF Queue Lin";
    WFQueueTest.lin_test ~count:3 ~name:"WF Queue Lin";  

    LFListTest.lin_test ~count:3 ~name:"LF List Lin";
    WFListTest.lin_test ~count:3 ~name:"WF List Lin";

    LFSkipListTest.lin_test ~count:3 ~name:"LF SkipList Lin";
    WFSkipListTest.lin_test ~count:3 ~name:"WF SkipList Lin";
  ]