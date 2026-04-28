open Src
open Sequential


module SeqStackAdapter = struct
  type 'a state = 'a SequentialStack.state
  type 'a op = 'a SequentialStack.op

  let empty = SequentialStack.empty
  let apply state op = SequentialStack.apply op state
end

module SeqQueueAdapter = struct
  type 'a state = 'a SequentialQueue.state
  type 'a op = 'a SequentialQueue.op

  let empty = SequentialQueue.empty
  let apply state op = SequentialQueue.apply op state
end

module SeqListAdapter = struct
  type 'a state = 'a SequentialSortedList.state
  type 'a op = 'a SequentialSortedList.op

  let empty = SequentialSortedList.empty
  let apply state op = SequentialSortedList.apply state op
end

module SeqBstAdapter = struct
  type 'a state = 'a SequentialBst.state
  type 'a op = 'a SequentialBst.op

  let empty = SequentialBst.empty
  let apply state op = SequentialBst.apply state op
end

module SeqSkipListAdapter = struct
  type 'a state = 'a SequentialSkipList.skip_list option

  type 'a op =
    | Insert of 'a
    | Remove of 'a
    | Contains of 'a

  let empty = None

  let ensure = function
    | Some sl -> sl
    | None -> SequentialSkipList.create 16 0.5

  let apply state op =
    let sl = ensure state in
    let next_state = Some sl in
    match op with
    | Insert x ->
        SequentialSkipList.insert sl x;
        (next_state, None)
    | Remove x ->
        SequentialSkipList.erase sl x;
        (next_state, None)
    | Contains x ->
        (next_state, if SequentialSkipList.search sl x then Some x else None)
end




module LFStack = Universal.Make (LFUniversal) (SeqStackAdapter)
module WFStack = Universal.Make (WFUniversal) (SeqStackAdapter)

module LFQueue = Universal.Make (LFUniversal) (SeqQueueAdapter)
module WFQueue = Universal.Make (WFUniversal) (SeqQueueAdapter)

module LFList = Universal.Make (LFUniversal) (SeqListAdapter)
module WFList = Universal.Make (WFUniversal) (SeqListAdapter)

module LFBst = Universal.Make (LFUniversal) (SeqBstAdapter)
module WFBst = Universal.Make (WFUniversal) (SeqBstAdapter)

module LFSkipList = Universal.Make (LFUniversal) (SeqSkipListAdapter)
module WFSkipList = Universal.Make (WFUniversal) (SeqSkipListAdapter)

