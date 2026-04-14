


let test_stack_push_pop () =
  let s = SequentialStack.empty in
  let s, _ = SequentialStack.apply s (Push 1) in
  let s, _ = SequentialStack.apply s (Push 2) in
  let s, r = SequentialStack.apply s Pop in
  assert (r = Some 2);
  let _, r = SequentialStack.apply s Pop in
  assert (r = Some 1);
  print_endline "stack push/pop: OK"



let test_stack_empty_pop () =
  let s = SequentialStack.empty in
  let _, r = SequentialStack.apply s Pop in
  assert (r = None);
  print_endline "stack empty pop: OK"



let test_stack_lifo () =
  let ops = [1; 2; 3; 4; 5] in
  let s = List.fold_left (fun acc x ->
    fst (SequentialStack.apply acc (Push x))
  ) SequentialStack.empty ops in
  let results = List.fold_left (fun (acc, results) _ ->
    let acc, r = SequentialStack.apply acc Pop in
    (acc, r :: results)
  ) (s, []) ops in
  let results = List.rev (snd results) in
  assert (results = [Some 5; Some 4; Some 3; Some 2; Some 1]);
  print_endline "stack LIFO order: OK"





let test_queue_enqueue_dequeue () =
  let q = SequentialQueue.empty in
  let q, _ = SequentialQueue.apply q (Enqueue 1) in
  let q, _ = SequentialQueue.apply q (Enqueue 2) in
  let q, r = SequentialQueue.apply q Dequeue in
  assert (r = Some 1);
  let _, r = SequentialQueue.apply q Dequeue in
  assert (r = Some 2);
  print_endline "queue enqueue/dequeue: OK"



let test_queue_empty_dequeue () =
  let q = SequentialQueue.empty in
  let _, r = SequentialQueue.apply q Dequeue in
  assert (r = None);
  print_endline "queue empty dequeue: OK"



let test_queue_fifo () =
  let ops = [1; 2; 3; 4; 5] in
  let q = List.fold_left (fun acc x ->
    fst (SequentialQueue.apply acc (Enqueue x))
  ) SequentialQueue.empty ops in
  let results = List.fold_left (fun (acc, results) _ ->
    let acc, r = SequentialQueue.apply acc Dequeue in
    (acc, r :: results)
  ) (q, []) ops in
  let results = List.rev (snd results) in
  assert (results = [Some 1; Some 2; Some 3; Some 4; Some 5]);
  print_endline "queue FIFO order: OK"



let test_set_insert_contains () =
  let s = SequentialSortedSet.empty in
  let s, _ = SequentialSortedSet.apply s (Insert 3) in
  let s, _ = SequentialSortedSet.apply s (Insert 1) in
  let s, _ = SequentialSortedSet.apply s (Insert 2) in
  let _, r = SequentialSortedSet.apply s (Contains 2) in
  assert (r = Some 2);
  let _, r = SequentialSortedSet.apply s (Contains 99) in
  assert (r = None);
  print_endline "set insert/contains: OK"


let test_set_remove () =
  let s = SequentialSortedSet.empty in
  let s, _ = SequentialSortedSet.apply s (Insert 1) in
  let s, _ = SequentialSortedSet.apply s (Insert 2) in
  let s, _ = SequentialSortedSet.apply s (Remove 1) in
  let _, r = SequentialSortedSet.apply s (Contains 1) in
  assert (r = None);
  let _, r = SequentialSortedSet.apply s (Contains 2) in
  assert (r = Some 2);
  print_endline "set remove: OK"

let test_set_sorted_order () =
  (* insert out of order, check internal list is sorted *)
  let s = SequentialSortedSet.empty in
  let s, _ = SequentialSortedSet.apply s (Insert 5) in
  let s, _ = SequentialSortedSet.apply s (Insert 2) in
  let s, _ = SequentialSortedSet.apply s (Insert 8) in
  let s, _ = SequentialSortedSet.apply s (Insert 1) in
  assert (s = [1; 2; 5; 8]);
  print_endline "set sorted order: OK"


let test_set_duplicate_insert () =
  let s = SequentialSortedSet.empty in
  let s, _ = SequentialSortedSet.apply s (Insert 3) in
  let s, _ = SequentialSortedSet.apply s (Insert 3) in
  assert (s = [3]);
  print_endline "set duplicate insert: OK"

let test_set_remove_nonexistent () =
  let s = SequentialSortedSet.empty in
  let s, _ = SequentialSortedSet.apply s (Insert 1) in
  let s, _ = SequentialSortedSet.apply s (Remove 99) in
  assert (s = [1]);
  print_endline "set remove nonexistent: OK"



