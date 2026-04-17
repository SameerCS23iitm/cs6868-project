open Src
open Sequential

module type SeqLike = sig
  type 'a state
  type 'a op
  val apply : 'a op -> 'a state -> 'a state * 'a option
  val empty : unit -> 'a state
end

module MakeLF (Seq : SeqLike) = struct
  type 'a t = ('a Seq.state * 'a option, 'a option) LFUniversal.t

  let create = LFUniversal.create

  let apply obj op tid =
    let invoc (state, _) =
      let (next_state, result) = Seq.apply op state in
      ((next_state, result), result)
    in
    let initial_obj = (Seq.empty (), None) in
    let (_new_obj, result) = LFUniversal.apply obj initial_obj invoc tid in
    result
end

module MakeWF (Seq : SeqLike) = struct
  type 'a t = ('a Seq.state * 'a option, 'a option) WFUniversal.t

  let create = WFUniversal.create

  let apply obj op tid =
    let invoc (state, _) =
      let (next_state, result) = Seq.apply op state in
      ((next_state, result), result)
    in
    let initial_obj = (Seq.empty (), None) in
    let (_new_obj, result) = WFUniversal.apply obj initial_obj invoc tid in
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
  let apply = SequentialSortedList.apply
  let empty () = SequentialSortedList.empty
end

module SkipListSeq = struct
  type 'a state = 'a SequentialSkipList.state
  type 'a op = 'a SequentialSkipList.op

  let empty () = SequentialSkipList.empty ()

  let apply op state =
    match op with
    | SequentialSkipList.Insert x ->
        let next_state, _ = SequentialSkipList.apply (SequentialSkipList.Insert x) state in
        (next_state, None)
    | SequentialSkipList.Remove x ->
        let next_state, _ = SequentialSkipList.apply (SequentialSkipList.Remove x) state in
        (next_state, None)
    | SequentialSkipList.Contains x ->
        let next_state, present = SequentialSkipList.apply (SequentialSkipList.Contains x) state in
        (next_state, if present = Some true then Some x else None)
end

module BstSeq = struct
  type 'a state = 'a SequentialBst.state
  type 'a op = 'a SequentialBst.op

  let empty () = SequentialBst.empty Stdlib.compare

  let apply op state =
    match op with
    | SequentialBst.Insert x ->
        let next_state, _ = SequentialBst.apply (SequentialBst.Insert x) state in
        (next_state, None)
    | SequentialBst.Remove x ->
        let next_state, _ = SequentialBst.apply (SequentialBst.Remove x) state in
        (next_state, None)
    | SequentialBst.Contains x ->
        let next_state, present = SequentialBst.apply (SequentialBst.Contains x) state in
        (next_state, if present = Some true then Some x else None)
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