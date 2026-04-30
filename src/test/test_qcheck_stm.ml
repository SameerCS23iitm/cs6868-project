open Src
open Sequential
open QCheck
open STM

(* QCheck-STM model-based tests for LF/WF universal wrappers. *)
let num_threads = 8
module type SeqLike = sig
  type 'a state
  type 'a op
  val apply : 'a op -> 'a state -> 'a state * 'a option
  val empty : unit -> 'a state
end

module MakeLF (Seq : SeqLike) = struct
  type 'a t = ('a Seq.state * 'a option, 'a option) LFUniversal.t

  let create = LFUniversal.create

  let apply obj op =
    let invoc (state, _) =
      let (next_state, result) = Seq.apply op state in
      ((next_state, result), result)
    in
    let initial_obj = (Seq.empty (), None) in
    let (_new_obj, result) = LFUniversal.apply obj initial_obj invoc in
    result
end

module MakeWF (Seq : SeqLike) = struct
  type 'a t = ('a Seq.state * 'a option, 'a option) WFUniversal.t

  let create = WFUniversal.create

  let apply obj op =
    let invoc (state, _) =
      let (next_state, result) = Seq.apply op state in
      ((next_state, result), result)
    in
    let initial_obj = (Seq.empty (), None) in
    let (_new_obj, result) = WFUniversal.apply obj initial_obj invoc in
    result
end

module StackSeq = struct
  type 'a state = 'a SequentialStack.state
  type 'a op = 'a SequentialStack.op
  let apply = SequentialStack.apply
  let empty () = SequentialStack.empty
end

module QueueSeq = struct
  type 'a state = 'a SequentialQueue.state
  type 'a op = 'a SequentialQueue.op
  let apply = SequentialQueue.apply
  let empty () = SequentialQueue.empty
end

module SortedListSeq = struct
  type 'a state = 'a SequentialSortedList.state
  type 'a op = 'a SequentialSortedList.op
  let apply op state = SequentialSortedList.apply state op
  let empty () = SequentialSortedList.empty
end

module SkipListSeq = struct
  type 'a state = 'a SequentialSkipList.state
  type 'a op = 'a SequentialSkipList.op
  let empty () = SequentialSkipList.empty ()
  let apply op state = SequentialSkipList.apply op state
end

module BstSeq = struct
  type 'a state = 'a SequentialBst.state
  type 'a op = 'a SequentialBst.op
  let empty () = SequentialBst.empty
  let apply op state = SequentialBst.apply state op
end

module LFStack = MakeLF(StackSeq)
module LFQueue = MakeLF(QueueSeq)
module WFStack = MakeWF(StackSeq)
module WFQueue = MakeWF(QueueSeq)
module LFList = MakeLF(SortedListSeq)
module WFList = MakeWF(SortedListSeq)
module LFSkipList = MakeLF(SkipListSeq)
module WFSkipList = MakeWF(SkipListSeq)
module LFBst = MakeLF(BstSeq)
module WFBst = MakeWF(BstSeq)

(** QCheck-STM tests for Universal Construction wrappers.
    Sequential model: the Sequential* modules themselves.
    SUT: LF/WF variants produced by MakeLF / MakeWF. *)


(* ================================================================== *)
(* Shared runner: sequential agree_test + parallel agree_prop_par      *)
(* ================================================================== *)

(* Run both sequential and parallel STM agreement tests for a spec. *)
let run_tests (type s) (module Spec : STM.Spec with type state = s) label =
  Printf.printf "\nRunning %s...\n%!" label;
  let module Seq = STM_sequential.Make(Spec) in
  let module Dom = STM_domain.Make(Spec) in
  let seq_test =
    Seq.agree_test ~count:1000 ~name:(label ^ " sequential")
  in
  let arb_cmds_par =
    Dom.arb_triple 15 10 Spec.arb_cmd Spec.arb_cmd Spec.arb_cmd
  in
  let par_test =
    QCheck.Test.make ~retries:10 ~count:200 ~name:(label ^ " concurrent")
      arb_cmds_par
    @@ fun triple -> Dom.agree_prop_par triple
  in
  ignore (QCheck_base_runner.run_tests ~verbose:true [seq_test; par_test])

(* ================================================================== *)
(* STACK                                                               *)
(* ================================================================== *)

let show_stack_cmd = function
  | SequentialStack.Push x -> Printf.sprintf "Push(%d)" x
  | SequentialStack.Pop    -> "Pop"
let arb_stack_cmd _state =
   QCheck.make ~print:show_stack_cmd
    (Gen.oneof [
       Gen.map (fun x -> SequentialStack.Push x) (Gen.int_range 0 100);
       Gen.return SequentialStack.Pop;
     ])

(* Shared fields; only sut type, run, init_sut differ between LF/WF *)
module StackBase = struct
  type state        = int SequentialStack.state
  type nonrec cmd   = int SequentialStack.op
  let arb_cmd       = arb_stack_cmd
  let show_cmd = show_stack_cmd
  let init_state    = []
  let next_state cmd state =
    let (next, _) = SequentialStack.apply cmd state in
    next
  let postcond cmd state result =
    match result with
    | Res ((Option Int, _), v) ->  let expected : int option = snd (SequentialStack.apply cmd state) in v = expected
   
    | _ -> false
  let precond _c _s = true
  let cleanup _     = ()
end

module LFStackSpec = struct
  include StackBase
  type sut          = int LFStack.t
  let init_sut ()   = LFStack.create num_threads
  let run cmd sut =
  match cmd with
  | SequentialStack.Push x -> Res (option int, LFStack.apply sut (SequentialStack.Push x))
  | SequentialStack.Pop    -> Res (option int, LFStack.apply sut  SequentialStack.Pop)
end

module WFStackSpec = struct
  include StackBase
  type sut          = int WFStack.t
  let init_sut ()   = WFStack.create num_threads
  let run cmd sut =
   match cmd with
  | SequentialStack.Push x -> Res (option int, WFStack.apply sut (SequentialStack.Push x))
  | SequentialStack.Pop    -> Res (option int, WFStack.apply sut  SequentialStack.Pop)
end

(* ================================================================== *)
(* QUEUE                                                               *)
(* ================================================================== *)

let show_queue_cmd = function
  | SequentialQueue.Enqueue x -> Printf.sprintf "Enqueue(%d)" x
  | SequentialQueue.Dequeue   -> "Dequeue"
let arb_queue_cmd _state =
   QCheck.make ~print:show_queue_cmd
    (Gen.oneof [
       Gen.map (fun x -> SequentialQueue.Enqueue x) (Gen.int_range 0 100);
       Gen.return SequentialQueue.Dequeue;
     ])

module QueueBase = struct
  type state        = int SequentialQueue.state
  type nonrec cmd   = int SequentialQueue.op
  let arb_cmd       = arb_queue_cmd
  let show_cmd = show_queue_cmd
  let init_state    = []
  let next_state cmd state =
    let (next, _) = SequentialQueue.apply cmd state in
    next
  let precond _c _s = true
  let postcond cmd state result =
    match result with
    | Res ((Option Int, _), v) ->   let expected : int option = snd (SequentialQueue.apply cmd state) in v = expected
    | _ -> false
  let cleanup _     = ()
end

module LFQueueSpec = struct
  include QueueBase
  type sut          = int LFQueue.t
  let init_sut ()   = LFQueue.create num_threads
  let run cmd sut   =
    match cmd with
    | SequentialQueue.Enqueue x -> Res (option int, LFQueue.apply sut (SequentialQueue.Enqueue x))
    | SequentialQueue.Dequeue   -> Res (option int, LFQueue.apply sut  SequentialQueue.Dequeue)
end

module WFQueueSpec = struct
  include QueueBase
  type sut          = int WFQueue.t
  let init_sut ()   = WFQueue.create num_threads
  let run cmd sut   =
    match cmd with
    | SequentialQueue.Enqueue x -> Res (option int, WFQueue.apply sut (SequentialQueue.Enqueue x))
    | SequentialQueue.Dequeue   -> Res (option int, WFQueue.apply sut  SequentialQueue.Dequeue)
end


(* ================================================================== *)
(* Sorted List                                                        *)
(* ================================================================== *)

let show_sortdlst_cmd = function
  | SequentialSortedList.Insert x -> Printf.sprintf "Insert(%d)" x
  | SequentialSortedList.Remove x -> Printf.sprintf "Remove(%d)" x
  | SequentialSortedList.Contains x -> Printf.sprintf "Contains(%d)" x
let arb_sortdlst_cmd _state =
   QCheck.make ~print:show_sortdlst_cmd
    (Gen.oneof [
       Gen.map (fun x -> SequentialSortedList.Insert x) (Gen.int_range 0 100);
       Gen.map (fun x -> SequentialSortedList.Remove x) (Gen.int_range 0 100);
       Gen.map (fun x -> SequentialSortedList.Contains x) (Gen.int_range 0 100);
     ])

module SortedListBase = struct
  type state        = int SequentialSortedList.state
  type nonrec cmd   = int SequentialSortedList.op
  let arb_cmd       = arb_sortdlst_cmd
  let show_cmd = show_sortdlst_cmd
  let init_state    = []
  let next_state cmd state =
    let (next, _) = SequentialSortedList.apply state cmd in
    next
  let precond _c _s = true
  let postcond cmd state result =
    match result with
    | Res ((Option Int, _), v) ->   let expected : int option = snd (SequentialSortedList.apply state cmd) in v = expected
    | _ -> false
  let cleanup _     = ()
end

module LFListSpec = struct
  include SortedListBase
  type sut          = int LFList.t
  let init_sut ()   = LFList.create num_threads
  let run cmd sut   =
    match cmd with
    | SequentialSortedList.Insert x -> Res (option int, LFList.apply sut (SequentialSortedList.Insert x))
    | SequentialSortedList.Remove x -> Res (option int, LFList.apply sut (SequentialSortedList.Remove x))
    | SequentialSortedList.Contains x -> Res (option int, LFList.apply sut (SequentialSortedList.Contains x))
end

module WFListSpec = struct
  include SortedListBase
  type sut          = int WFList.t
  let init_sut ()   = WFList.create num_threads
  let run cmd sut   =
    match cmd with
    | SequentialSortedList.Insert x -> Res (option int, WFList.apply sut (SequentialSortedList.Insert x))
    | SequentialSortedList.Remove x -> Res (option int, WFList.apply sut (SequentialSortedList.Remove x))
    | SequentialSortedList.Contains x -> Res (option int, WFList.apply sut (SequentialSortedList.Contains x))
end

(* ================================================================== *)
(* Skip List                                                          *)
(* ================================================================== *)

let show_skiplst_cmd = function
  | SequentialSkipList.Insert x -> Printf.sprintf "Insert(%d)" x
  | SequentialSkipList.Remove x -> Printf.sprintf "Remove(%d)" x
  | SequentialSkipList.Contains x -> Printf.sprintf "Contains(%d)" x
let arb_skiplst_cmd _state =
   QCheck.make ~print:show_skiplst_cmd
    (Gen.oneof [
       Gen.map (fun x -> SequentialSkipList.Insert x) (Gen.int_range 0 100);
       Gen.map (fun x -> SequentialSkipList.Remove x) (Gen.int_range 0 100);
       Gen.map (fun x -> SequentialSkipList.Contains x) (Gen.int_range 0 100);
     ])

module SkipListBase = struct
  type state        = int list
  type nonrec cmd   = int SequentialSkipList.op
  let arb_cmd       = arb_skiplst_cmd
  let show_cmd = show_skiplst_cmd
  let init_state    = []

  let insert_sorted x xs =
    let rec loop acc = function
      | [] -> List.rev (x :: acc)
      | y :: ys as rest ->
        if x = y then List.rev_append acc rest
        else if x < y then List.rev_append acc (x :: rest)
        else loop (y :: acc) ys
    in
    loop [] xs

  let next_state cmd state =
    match cmd with
    | SequentialSkipList.Insert x -> insert_sorted x state
    | SequentialSkipList.Remove x -> List.filter (fun y -> y <> x) state
    | SequentialSkipList.Contains _ -> state

  let precond _c _s = true
  let postcond cmd state result =
    match result with
    | Res ((Option Int, _), v) ->
      let expected : int option =
        match cmd with
        | SequentialSkipList.Contains x -> if List.mem x state then Some x else None
        | SequentialSkipList.Insert _ -> None
        | SequentialSkipList.Remove _ -> None
      in
      v = expected
    | _ -> false
  let cleanup _     = ()
end

module LFSkipListSpec = struct
  include SkipListBase
  type sut          = int LFSkipList.t
  let init_sut ()   = LFSkipList.create num_threads
  let run cmd sut   =
    match cmd with
    | SequentialSkipList.Remove x -> Res (option int, LFSkipList.apply sut (SequentialSkipList.Remove x))
    | SequentialSkipList.Insert x -> Res (option int, LFSkipList.apply sut (SequentialSkipList.Insert x))
    | SequentialSkipList.Contains x -> Res (option int, LFSkipList.apply sut (SequentialSkipList.Contains x))
end

module WFSkipListSpec = struct
  include SkipListBase
  type sut          = int WFSkipList.t
  let init_sut ()   = WFSkipList.create num_threads
  let run cmd sut   =
    match cmd with
    | SequentialSkipList.Insert x -> Res (option int, WFSkipList.apply sut (SequentialSkipList.Insert x))
    | SequentialSkipList.Remove x -> Res (option int, WFSkipList.apply sut (SequentialSkipList.Remove x))
    | SequentialSkipList.Contains x -> Res (option int, WFSkipList.apply sut (SequentialSkipList.Contains x))
end


(* ================================================================== *)
(* BST                                                                *)
(* ================================================================== *)

let show_bst_cmd = function
  | SequentialBst.Insert x -> Printf.sprintf "Insert(%d)" x
  | SequentialBst.Remove x -> Printf.sprintf "Remove(%d)" x
  | SequentialBst.Contains x -> Printf.sprintf "Contains(%d)" x
let arb_bst_cmd _state =
   QCheck.make ~print:show_bst_cmd
    (Gen.oneof [
       Gen.map (fun x -> SequentialBst.Insert x) (Gen.int_range 0 100);
       Gen.map (fun x -> SequentialBst.Remove x) (Gen.int_range 0 100);
       Gen.map (fun x -> SequentialBst.Contains x) (Gen.int_range 0 100);
     ])

module BstBase = struct
  type state        = int SequentialBst.state
  type nonrec cmd   = int SequentialBst.op
  let arb_cmd       = arb_bst_cmd
  let show_cmd = show_bst_cmd
  let init_state    = SequentialBst.empty 
  let next_state cmd state =
    let (next, _) = SequentialBst.apply state cmd in
    next
  let precond _c _s = true
  let postcond cmd state result =
    match result with
    | Res ((Option Int, _), v) ->   let expected : int option = snd (SequentialBst.apply state cmd) in v = expected
    | _ -> false
  let cleanup _     = ()
end

module LFSBstSpec = struct
  include BstBase
  type sut          = int LFBst.t
  let init_sut ()   = LFBst.create num_threads
  let run cmd sut   =
    match cmd with
    | SequentialBst.Insert x -> Res (option int, LFBst.apply sut (SequentialBst.Insert x))
    | SequentialBst.Remove x -> Res (option int, LFBst.apply sut (SequentialBst.Remove x))
    | SequentialBst.Contains x -> Res (option int, LFBst.apply sut (SequentialBst.Contains x))
end

module WFBstSpec = struct
  include BstBase
  type sut          = int WFBst.t
  let init_sut ()   = WFBst.create num_threads
  let run cmd sut   =
    match cmd with
    | SequentialBst.Insert x -> Res (option int, WFBst.apply sut (SequentialBst.Insert x))
    | SequentialBst.Remove x -> Res (option int, WFBst.apply sut (SequentialBst.Remove x))
    | SequentialBst.Contains x -> Res (option int, WFBst.apply sut (SequentialBst.Contains x))
end

(* ================================================================== *)
(* Add more data structures here following the same pattern:           *)
(*   1. type ds_cmd = ...                                              *)
(*   2. show / arb / next_state / postcond functions                   *)
(*   3. module DSBase = struct ... end                                 *)
(*   4. module LFDSSpec = struct include DSBase; type sut ...; ... end *)
(*   5. module WFDSSpec = struct include DSBase; type sut ...; ... end *)
(*   6. add run_tests calls in () below                                *)
(* ================================================================== *)

let () =
  run_tests (module LFStackSpec) "LF Stack";
  run_tests (module WFStackSpec) "WF Stack";
  run_tests (module LFQueueSpec) "LF Queue";
  run_tests (module WFQueueSpec) "WF Queue";
  run_tests (module LFListSpec) "LF Sorted List";
  run_tests (module WFListSpec) "WF Sorted List";
  run_tests (module LFSkipListSpec) "LF Skip List";
  run_tests (module WFSkipListSpec) "WF Skip List";
  run_tests (module LFSBstSpec) "LF BST";
  run_tests (module WFBstSpec) "WF BST";