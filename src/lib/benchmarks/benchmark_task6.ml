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

module LFUniversalStack = Src.Universal.Make (Src.LFUniversal) (SeqStackAdapter)
module WFUniversalStack = Src.Universal.Make (Src.WFUniversal) (SeqStackAdapter)
module LFUniversalQueue = Src.Universal.Make (Src.LFUniversal) (SeqQueueAdapter)
module WFUniversalQueue = Src.Universal.Make (Src.WFUniversal) (SeqQueueAdapter)

let chunks total parts =
  let base = total / parts in
  let rem = total mod parts in
  Array.init parts (fun i -> if i < rem then base + 1 else base)

let run_parallel num_threads f =
  let workers =
    Array.init (max 0 (num_threads - 1)) (fun i ->
        Domain.spawn (fun () -> f (i + 1)))
  in
  f 0;
  Array.iter Domain.join workers

let timed_seconds f =
  let t0 = Unix.gettimeofday () in
  f ();
  Unix.gettimeofday () -. t0

let benchmark_stack_lockfree_builtin ~num_threads ~total_ops =
  let stack = Src.Lockfree_stack_builtin_list.create () in
  let per_thread = chunks total_ops num_threads in
  timed_seconds (fun () ->
      run_parallel num_threads (fun tid ->
          let pairs = per_thread.(tid) in
          for i = 0 to pairs - 1 do
            Src.Lockfree_stack_builtin_list.push stack (tid lsl 20 + i);
            ignore (Src.Lockfree_stack_builtin_list.try_pop stack)
          done))

let benchmark_stack_locked ~num_threads ~total_ops =
  let stack = Src.Locked_stack.create () in
  let per_thread = chunks total_ops num_threads in
  timed_seconds (fun () ->
      run_parallel num_threads (fun tid ->
          let pairs = per_thread.(tid) in
          for i = 0 to pairs - 1 do
            Src.Locked_stack.push stack (tid lsl 20 + i);
            ignore (Src.Locked_stack.try_pop stack)
          done))

let benchmark_stack_lf_universal ~num_threads ~total_ops =
  let stack = LFUniversalStack.create num_threads in
  let per_thread = chunks total_ops num_threads in
  timed_seconds (fun () ->
      run_parallel num_threads (fun tid ->
          let pairs = per_thread.(tid) in
          for i = 0 to pairs - 1 do
            ignore
              (LFUniversalStack.apply stack
                 (Sequential.SequentialStack.Push (tid lsl 20 + i))
                 tid);
            ignore (LFUniversalStack.apply stack Sequential.SequentialStack.Pop tid)
          done))

let benchmark_stack_wf_universal ~num_threads ~total_ops =
  let stack = WFUniversalStack.create num_threads in
  let per_thread = chunks total_ops num_threads in
  timed_seconds (fun () ->
      run_parallel num_threads (fun tid ->
          let pairs = per_thread.(tid) in
          for i = 0 to pairs - 1 do
            ignore
              (WFUniversalStack.apply stack
                 (Sequential.SequentialStack.Push (tid lsl 20 + i))
                 tid);
            ignore (WFUniversalStack.apply stack Sequential.SequentialStack.Pop tid)
          done))

let benchmark_queue_lockfree ~num_threads ~total_ops =
  let q = Src.Lockfree_queue.create () in
  let per_thread = chunks total_ops num_threads in
  timed_seconds (fun () ->
      run_parallel num_threads (fun tid ->
          let pairs = per_thread.(tid) in
          for i = 0 to pairs - 1 do
            Src.Lockfree_queue.enq q (tid lsl 20 + i);
            ignore (Src.Lockfree_queue.try_deq q)
          done))

let benchmark_queue_locked ~num_threads ~total_ops =
  let q = Src.Locked_queue.create () in
  let per_thread = chunks total_ops num_threads in
  timed_seconds (fun () ->
      run_parallel num_threads (fun tid ->
          let pairs = per_thread.(tid) in
          for i = 0 to pairs - 1 do
            Src.Locked_queue.enq q (tid lsl 20 + i);
            ignore (Src.Locked_queue.try_deq q)
          done))

let benchmark_queue_lf_universal ~num_threads ~total_ops =
  let q = LFUniversalQueue.create num_threads in
  let per_thread = chunks total_ops num_threads in
  timed_seconds (fun () ->
      run_parallel num_threads (fun tid ->
          let pairs = per_thread.(tid) in
          for i = 0 to pairs - 1 do
            ignore
              (LFUniversalQueue.apply q
                 (Sequential.SequentialQueue.Enqueue (tid lsl 20 + i))
                 tid);
            ignore (LFUniversalQueue.apply q Sequential.SequentialQueue.Dequeue tid)
          done))

let benchmark_queue_wf_universal ~num_threads ~total_ops =
  let q = WFUniversalQueue.create num_threads in
  let per_thread = chunks total_ops num_threads in
  timed_seconds (fun () ->
      run_parallel num_threads (fun tid ->
          let pairs = per_thread.(tid) in
          for i = 0 to pairs - 1 do
            ignore
              (WFUniversalQueue.apply q
                 (Sequential.SequentialQueue.Enqueue (tid lsl 20 + i))
                 tid);
            ignore (WFUniversalQueue.apply q Sequential.SequentialQueue.Dequeue tid)
          done))

let append_csv_row oc ~threads ~kind ~impl ~total_ops ~secs =
  let throughput = (float_of_int total_ops) /. secs in
  Printf.fprintf oc "%d,%s,%s,%d,%.9f,%.3f\n"
    threads kind impl total_ops secs throughput;
  flush oc

let benchmark ~max_threads =
  if max_threads <= 0 then invalid_arg "benchmark: max_threads must be > 0";
  (* Universal constructions replay history on each apply, so a modest number
     of operation pairs keeps runtime practical while preserving comparisons. *)
  let total_pairs = 2_000 in
  let total_ops = total_pairs * 2 in
  let output_file = Printf.sprintf "benchmark_vs_threads_1_to_%d.csv" max_threads in
  let oc = open_out_gen [ Open_creat; Open_text; Open_trunc; Open_wronly ] 0o644 output_file in
  Printf.fprintf oc "threads,object,implementation,total_ops,seconds,ops_per_sec\n";
  for num_threads = 1 to max_threads do
    let stack_locked = benchmark_stack_locked ~num_threads ~total_ops in
    append_csv_row oc ~threads:num_threads ~kind:"stack"
      ~impl:"locked_stack" ~total_ops ~secs:stack_locked;

    let stack_lf = benchmark_stack_lockfree_builtin ~num_threads ~total_ops in
    append_csv_row oc ~threads:num_threads ~kind:"stack"
      ~impl:"lockfree_stack_builtin_list" ~total_ops ~secs:stack_lf;

    let stack_uni_lf = benchmark_stack_lf_universal ~num_threads ~total_ops in
    append_csv_row oc ~threads:num_threads ~kind:"stack"
      ~impl:"lf_universal_stack" ~total_ops ~secs:stack_uni_lf;

    let stack_uni_wf = benchmark_stack_wf_universal ~num_threads ~total_ops in
    append_csv_row oc ~threads:num_threads ~kind:"stack"
      ~impl:"wf_universal_stack" ~total_ops ~secs:stack_uni_wf;

    let queue_locked = benchmark_queue_locked ~num_threads ~total_ops in
    append_csv_row oc ~threads:num_threads ~kind:"queue"
      ~impl:"locked_queue" ~total_ops ~secs:queue_locked;

    let queue_lf = benchmark_queue_lockfree ~num_threads ~total_ops in
    append_csv_row oc ~threads:num_threads ~kind:"queue"
      ~impl:"lockfree_queue" ~total_ops ~secs:queue_lf;

    let queue_uni_lf = benchmark_queue_lf_universal ~num_threads ~total_ops in
    append_csv_row oc ~threads:num_threads ~kind:"queue"
      ~impl:"lf_universal_queue" ~total_ops ~secs:queue_uni_lf;

    let queue_uni_wf = benchmark_queue_wf_universal ~num_threads ~total_ops in
    append_csv_row oc ~threads:num_threads ~kind:"queue"
      ~impl:"wf_universal_queue" ~total_ops ~secs:queue_uni_wf;
  done;
  close_out oc;
  output_file

let () =
  let max_threads =
    if Array.length Sys.argv >= 2 then int_of_string Sys.argv.(1) else 4
  in
  let output_file = benchmark ~max_threads in
  Printf.printf "Benchmark complete. Results written to %s\n" output_file
