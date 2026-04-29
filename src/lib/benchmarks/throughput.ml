module SeqStackAdapter = struct
  type 'a state = 'a Sequential.SequentialStack.state
  type 'a op = 'a Sequential.SequentialStack.op

  let empty = Sequential.SequentialStack.empty
  let apply state op = Sequential.SequentialStack.apply op state
end

module SeqQueueAdapter = struct
  type 'a state = 'a Sequential.SequentialQueue.state
  type 'a op = 'a Sequential.SequentialQueue.op

  let empty = Sequential.SequentialQueue.empty
  let apply state op = Sequential.SequentialQueue.apply op state
end

module SeqBstAdapter = struct
  type 'a state = 'a Sequential.SequentialBst.state
  type 'a op = 'a Sequential.SequentialBst.op

  let empty = Sequential.SequentialBst.empty
  let apply state op = Sequential.SequentialBst.apply state op
end

module SeqSortedListAdapter = struct
  type 'a state = 'a Sequential.SequentialSortedList.state
  type 'a op = 'a Sequential.SequentialSortedList.op

  let empty = Sequential.SequentialSortedList.empty
  let apply state op = Sequential.SequentialSortedList.apply state op
end

module SeqSkipListAdapter = struct
  type 'a state = 'a Sequential.SequentialSkipList.state
  type 'a op = 'a Sequential.SequentialSkipList.op

  let empty : 'a state = (Obj.magic ([] : 'a list) : 'a state)
  let apply state op = Sequential.SequentialSkipList.apply op state
end

module LFUniversalStack = Src.Universal.Make (Src.LFUniversal) (SeqStackAdapter)
module WFUniversalStack = Src.Universal.Make (Src.WFUniversal) (SeqStackAdapter)
module LFUniversalQueue = Src.Universal.Make (Src.LFUniversal) (SeqQueueAdapter)
module WFUniversalQueue = Src.Universal.Make (Src.WFUniversal) (SeqQueueAdapter)
module LFUniversalBst = Src.Universal.Make (Src.LFUniversal) (SeqBstAdapter)
module WFUniversalBst = Src.Universal.Make (Src.WFUniversal) (SeqBstAdapter)
module LFUniversalSortedList = Src.Universal.Make (Src.LFUniversal) (SeqSortedListAdapter)
module WFUniversalSortedList = Src.Universal.Make (Src.WFUniversal) (SeqSortedListAdapter)
module LFUniversalSkipList = struct
  type 'a t = { u : ('a Sequential.SequentialSkipList.state, 'a option) Src.LFUniversal.t; num_threads : int }

  let create num_threads = { u = Src.LFUniversal.create num_threads; num_threads }

  let apply obj op =
    let invoc state = Sequential.SequentialSkipList.apply op state in
    let initial_obj = Sequential.SequentialSkipList.create 16 0.5 in
    let (_new_state, result) = Src.LFUniversal.apply obj.u initial_obj invoc in
    result
end

module WFUniversalSkipList = struct
  type 'a t = { u : ('a Sequential.SequentialSkipList.state, 'a option) Src.WFUniversal.t; num_threads : int }

  let create num_threads = { u = Src.WFUniversal.create num_threads; num_threads }

  let apply obj op =
    let invoc state = Sequential.SequentialSkipList.apply op state in
    let initial_obj = Sequential.SequentialSkipList.create 16 0.5 in
    let (_new_state, result) = Src.WFUniversal.apply obj.u initial_obj invoc in
    result
end

let chunks total parts =
  let base = total / parts in
  let rem = total mod parts in
  Array.init parts (fun i -> if i < rem then base + 1 else base)

let run_parallel num_threads f =
  let workers =
    Array.init (max 0 (num_threads - 1)) (fun i -> Domain.spawn (fun () -> f (i + 1)))
  in
  f 0;
  Array.iter Domain.join workers

let timed_seconds f =
  let t0 = Unix.gettimeofday () in
  f ();
  Unix.gettimeofday () -. t0

type bench_stats = {
  seconds : float;
  throughput : float;
}

let throughput total_ops seconds =
  if seconds <= 0.0 then infinity else float_of_int total_ops /. seconds

let benchmark_stack ~num_threads ~total_ops ~make_universal ~push ~pop =
  let per_thread = chunks total_ops num_threads in
  let obj = make_universal num_threads in
  let seconds =
    timed_seconds (fun () ->
        run_parallel num_threads (fun tid ->
            let pairs = per_thread.(tid) in
            for i = 0 to pairs - 1 do
              ignore (push obj (tid lsl 20 + i));
              ignore (pop obj)
            done))
  in
  {
    seconds;
    throughput = throughput (total_ops * 2) seconds;
  }

let benchmark_queue ~num_threads ~total_ops ~make_universal ~enq ~deq =
  let per_thread = chunks total_ops num_threads in
  let obj = make_universal num_threads in
  let seconds =
    timed_seconds (fun () ->
        run_parallel num_threads (fun tid ->
            let pairs = per_thread.(tid) in
            for i = 0 to pairs - 1 do
              ignore (enq obj (tid lsl 20 + i));
              ignore (deq obj)
            done))
  in
  {
    seconds;
    throughput = throughput (total_ops * 2) seconds;
  }

let benchmark_bst ~num_threads ~total_ops ~make_universal ~insert ~remove =
  let per_thread = chunks total_ops num_threads in
  let obj = make_universal num_threads in
  let seconds =
    timed_seconds (fun () ->
        run_parallel num_threads (fun tid ->
            let pairs = per_thread.(tid) in
            for i = 0 to pairs - 1 do
              ignore (insert obj (tid lsl 20 + i));
              ignore (remove obj)
            done))
  in
  {
    seconds;
    throughput = throughput (total_ops * 2) seconds;
  }

let benchmark_sortedlist ~num_threads ~total_ops ~make_universal ~insert ~remove =
  let per_thread = chunks total_ops num_threads in
  let obj = make_universal num_threads in
  let seconds =
    timed_seconds (fun () ->
        run_parallel num_threads (fun tid ->
            let pairs = per_thread.(tid) in
            for i = 0 to pairs - 1 do
              ignore (insert obj (tid lsl 20 + i));
              ignore (remove obj)
            done))
  in
  {
    seconds;
    throughput = throughput (total_ops * 2) seconds;
  }



let append_csv_row oc ~num_threads ~object_name ~implementation ~ops_per_sec =
  Printf.fprintf oc "%d,%s,%s,%.3f\n" num_threads object_name implementation ops_per_sec;
  flush oc

let benchmark ~num_ops ~repeats =
  if num_ops <= 0 then invalid_arg "benchmark: num_ops must be > 0";
  if num_ops mod 2 <> 0 then invalid_arg "benchmark: num_ops must be even";
  let thread_counts = List.init 15 (fun i -> i + 1) in
  let output_file = "throughput.csv" in
  let oc = open_out_gen [ Open_creat; Open_text; Open_trunc; Open_wronly ] 0o644 output_file in
  Printf.fprintf oc "threads,object,implementation,ops_per_sec\n";
  let avg f =
    let sum = ref 0.0 in
    for _ = 1 to repeats do sum := !sum +. f () done;
    !sum /. float_of_int repeats
  in

  List.iter (fun num_threads ->
    let total_ops_per_run = num_ops / 2 in

    let stack_lf_avg = avg (fun () -> (benchmark_stack ~num_threads ~total_ops:total_ops_per_run ~make_universal:LFUniversalStack.create ~push:(fun obj value -> LFUniversalStack.apply obj (Sequential.SequentialStack.Push value)) ~pop:(fun obj -> LFUniversalStack.apply obj Sequential.SequentialStack.Pop)).throughput) in
    append_csv_row oc ~num_threads ~object_name:"stack" ~implementation:"lock-free" ~ops_per_sec:stack_lf_avg;

    let stack_wf_avg = avg (fun () -> (benchmark_stack ~num_threads ~total_ops:total_ops_per_run ~make_universal:WFUniversalStack.create ~push:(fun obj value -> WFUniversalStack.apply obj (Sequential.SequentialStack.Push value)) ~pop:(fun obj -> WFUniversalStack.apply obj Sequential.SequentialStack.Pop)).throughput) in
    append_csv_row oc ~num_threads ~object_name:"stack" ~implementation:"wait-free" ~ops_per_sec:stack_wf_avg;

    let queue_lf_avg = avg (fun () -> (benchmark_queue ~num_threads ~total_ops:total_ops_per_run ~make_universal:LFUniversalQueue.create ~enq:(fun obj value -> LFUniversalQueue.apply obj (Sequential.SequentialQueue.Enqueue value)) ~deq:(fun obj -> LFUniversalQueue.apply obj Sequential.SequentialQueue.Dequeue)).throughput) in
    append_csv_row oc ~num_threads ~object_name:"queue" ~implementation:"lock-free" ~ops_per_sec:queue_lf_avg;

    let queue_wf_avg = avg (fun () -> (benchmark_queue ~num_threads ~total_ops:total_ops_per_run ~make_universal:WFUniversalQueue.create ~enq:(fun obj value -> WFUniversalQueue.apply obj (Sequential.SequentialQueue.Enqueue value)) ~deq:(fun obj -> WFUniversalQueue.apply obj Sequential.SequentialQueue.Dequeue)).throughput) in
    append_csv_row oc ~num_threads ~object_name:"queue" ~implementation:"wait-free" ~ops_per_sec:queue_wf_avg;

    let bst_lf_avg = avg (fun () -> (benchmark_bst ~num_threads ~total_ops:total_ops_per_run ~make_universal:LFUniversalBst.create ~insert:(fun obj value -> LFUniversalBst.apply obj (Sequential.SequentialBst.Insert value)) ~remove:(fun obj -> LFUniversalBst.apply obj (Sequential.SequentialBst.Remove 0))).throughput) in
    append_csv_row oc ~num_threads ~object_name:"bst" ~implementation:"lock-free" ~ops_per_sec:bst_lf_avg;

    let bst_wf_avg = avg (fun () -> (benchmark_bst ~num_threads ~total_ops:total_ops_per_run ~make_universal:WFUniversalBst.create ~insert:(fun obj value -> WFUniversalBst.apply obj (Sequential.SequentialBst.Insert value)) ~remove:(fun obj -> WFUniversalBst.apply obj (Sequential.SequentialBst.Remove 0))).throughput) in
    append_csv_row oc ~num_threads ~object_name:"bst" ~implementation:"wait-free" ~ops_per_sec:bst_wf_avg;

    let sortedlist_lf_avg = avg (fun () -> (benchmark_sortedlist ~num_threads ~total_ops:total_ops_per_run ~make_universal:LFUniversalSortedList.create ~insert:(fun obj value -> LFUniversalSortedList.apply obj (Sequential.SequentialSortedList.Insert value)) ~remove:(fun obj -> LFUniversalSortedList.apply obj (Sequential.SequentialSortedList.Remove 0))).throughput) in
    append_csv_row oc ~num_threads ~object_name:"sortedlist" ~implementation:"lock-free" ~ops_per_sec:sortedlist_lf_avg;

    let sortedlist_wf_avg = avg (fun () -> (benchmark_sortedlist ~num_threads ~total_ops:total_ops_per_run ~make_universal:WFUniversalSortedList.create ~insert:(fun obj value -> WFUniversalSortedList.apply obj (Sequential.SequentialSortedList.Insert value)) ~remove:(fun obj -> WFUniversalSortedList.apply obj (Sequential.SequentialSortedList.Remove 0))).throughput) in
    append_csv_row oc ~num_threads ~object_name:"sortedlist" ~implementation:"wait-free" ~ops_per_sec:sortedlist_wf_avg;

    let skiplist_lf_avg = avg (fun () -> (benchmark_sortedlist ~num_threads ~total_ops:total_ops_per_run ~make_universal:LFUniversalSkipList.create ~insert:(fun obj value -> LFUniversalSkipList.apply obj (Sequential.SequentialSkipList.Insert value)) ~remove:(fun obj -> LFUniversalSkipList.apply obj (Sequential.SequentialSkipList.Remove 0))).throughput) in
    append_csv_row oc ~num_threads ~object_name:"skiplist" ~implementation:"lock-free" ~ops_per_sec:skiplist_lf_avg;

    let skiplist_wf_avg = avg (fun () -> (benchmark_sortedlist ~num_threads ~total_ops:total_ops_per_run ~make_universal:WFUniversalSkipList.create ~insert:(fun obj value -> WFUniversalSkipList.apply obj (Sequential.SequentialSkipList.Insert value)) ~remove:(fun obj -> WFUniversalSkipList.apply obj (Sequential.SequentialSkipList.Remove 0))).throughput) in
    append_csv_row oc ~num_threads ~object_name:"skiplist" ~implementation:"wait-free" ~ops_per_sec:skiplist_wf_avg;

  ) thread_counts;

  close_out oc;
  output_file

let () =
  if Array.length Sys.argv <> 2 then invalid_arg "Usage: throughput <num_runs>";
  let repeats = int_of_string Sys.argv.(1) in
  if repeats <= 0 then invalid_arg "num_runs must be > 0";
  let num_ops = 2_000 in
  Printf.printf "Running benchmark with %d total operations, %d repeats...\n" num_ops repeats;
  let output_file = benchmark ~num_ops ~repeats in
  Printf.printf "Benchmark complete. Results written to %s\n" output_file