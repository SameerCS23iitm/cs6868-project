open QCheck
open Universal_instances

let int_small = nat_small

(* IMPORTANT: must match create *)
let num_threads = 10

(* ================= STACK ================= *)

module StackSpec (S : sig
  type 'a t
  val create : int -> 'a t
  val apply : 'a t -> 'a Sequential.SequentialStack.op -> 'a option
end) = struct
  type t = int S.t

  let init () = S.create num_threads     
  let cleanup _ = ()

  let api =
    let open Lin in
    [
      val_ "push"
        (fun s x ->
          S.apply s (Sequential.SequentialStack.Push x))
        (t @-> int_small @-> returning (option int));

      val_ "pop"
        (fun s ->
          S.apply s Sequential.SequentialStack.Pop)
        (t @-> returning (option int));
    ]
end

module LFStackTest = Lin_domain.Make(StackSpec(LFStack))
module WFStackTest = Lin_domain.Make(StackSpec(WFStack))


(* ================= QUEUE ================= *)

module QueueSpec (Q : sig
  type 'a t
  val create : int -> 'a t
  val apply : 'a t -> 'a Sequential.SequentialQueue.op -> 'a option
end) = struct
  type t = int Q.t

  let init () = Q.create num_threads
  let cleanup _ = ()

  let api =
    let open Lin in
    [
      val_ "enq"
        (fun q x ->
          Q.apply q (Sequential.SequentialQueue.Enqueue x))
        (t @-> int_small @-> returning (option int));

      val_ "deq"
        (fun q->
          Q.apply q Sequential.SequentialQueue.Dequeue)
        (t @-> returning (option int));
    ]
end

module LFQueueTest = Lin_domain.Make(QueueSpec(LFQueue))
module WFQueueTest = Lin_domain.Make(QueueSpec(WFQueue))


(* ================= SORTED LIST ================= *)

module ListSpec (L : sig
  type 'a t
  val create : int -> 'a t
  val apply : 'a t -> 'a Sequential.SequentialSortedList.op -> 'a option
end) = struct
  type t = int L.t

  let init () = L.create num_threads
  let cleanup _ = ()

  let api =
    let open Lin in
    [
      val_ "insert"
        (fun l x ->
          L.apply l (Sequential.SequentialSortedList.Insert x) )
        (t @-> int_small @-> returning (option int));

      val_ "remove"
        (fun l x ->
          L.apply l (Sequential.SequentialSortedList.Remove x))
        (t @-> int_small @-> returning (option int));

      val_ "contains"
        (fun l x ->
          L.apply l (Sequential.SequentialSortedList.Contains x))
        (t @-> int_small @-> returning (option int));
    ]
end

module LFListTest = Lin_domain.Make(ListSpec(LFList))
module WFListTest = Lin_domain.Make(ListSpec(WFList))


(* ================= BST ================= *)

module BstSpec (B : sig
  type 'a t
  val create : int -> 'a t
  val apply : 'a t -> 'a Sequential.SequentialBst.op -> 'a option
end) = struct
  type t = int B.t

  let init () = B.create num_threads
  let cleanup _ = ()

  let api =
    let open Lin in
    [
      val_ "insert"
        (fun b x ->
          B.apply b (Sequential.SequentialBst.Insert x))
        (t @-> int_small @-> returning (option int));

      val_ "remove"
        (fun b x ->
          B.apply b (Sequential.SequentialBst.Remove x))
        (t @-> int_small @-> returning (option int));

      val_ "contains"
        (fun b x ->
          B.apply b (Sequential.SequentialBst.Contains x))
        (t @-> int_small @-> returning (option int));
    ]
end

module LFBstTest = Lin_domain.Make(BstSpec(LFBst))
module WFBstTest = Lin_domain.Make(BstSpec(WFBst))


(* ================= SKIP LIST ================= *)

module SkipListSpec (L : sig
  type 'a t
  val create : int -> 'a t
  val apply :
    'a t ->
    'a Universal_instances.SeqSkipListAdapter.op ->
    'a option
end) = struct
  type t = int L.t

  let init () = L.create num_threads
  let cleanup _ = ()

  let api =
    let open Lin in
    [
      val_ "insert"
        (fun l x ->
          L.apply l (Universal_instances.SeqSkipListAdapter.Insert x))
        (t @-> int_small @-> returning (option int));

      val_ "remove"
        (fun l x ->
          L.apply l (Universal_instances.SeqSkipListAdapter.Remove x))
        (t @-> int_small @-> returning (option int));

      val_ "contains"
        (fun l x ->
          L.apply l (Universal_instances.SeqSkipListAdapter.Contains x))
        (t @-> int_small @-> returning (option int));
    ]
end

module LFSkipListTest = Lin_domain.Make(SkipListSpec(LFSkipList))
module WFSkipListTest = Lin_domain.Make(SkipListSpec(WFSkipList))


(* ================= RUN ================= *)

let () =
  QCheck_base_runner.run_tests_main [

    (* LFStackTest.lin_test ~count:10 ~name:"LF Stack Lin";
    WFStackTest.lin_test ~count:10 ~name:"WF Stack Lin";

    LFQueueTest.lin_test ~count:10 ~name:"LF Queue Lin";
    WFQueueTest.lin_test ~count:10 ~name:"WF Queue Lin";   

    LFListTest.lin_test ~count:100 ~name:"LF List Lin";
    WFListTest.lin_test ~count:100 ~name:"WF List Lin"; *)

    LFBstTest.lin_test ~count:100 ~name:"LF BST Lin";
    WFBstTest.lin_test ~count:100 ~name:"WF BST Lin";  

    (* LFSkipListTest.lin_test ~count:10 ~name:"LF SkipList Lin";
    WFSkipListTest.lin_test ~count:10 ~name:"WF SkipList Lin";  *)
  ]