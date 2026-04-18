
open Src
open Sequential

let test_stack_push_pop () =
  let s = SequentialStack.empty in
  let s, _ = SequentialStack.apply (SequentialStack.Push 1) s in
  let s, _ = SequentialStack.apply (SequentialStack.Push 2) s in
  let s, r = SequentialStack.apply SequentialStack.Pop s in
  assert (r = Some 2);
  let _, r = SequentialStack.apply SequentialStack.Pop s in
  assert (r = Some 1);
  print_endline "stack push/pop: OK"



let test_stack_empty_pop () =
  let s = SequentialStack.empty in
  let _, r = SequentialStack.apply SequentialStack.Pop s in
  assert (r = None);
  print_endline "stack empty pop: OK"



let test_stack_lifo () =
  let ops = [1; 2; 3; 4; 5] in
  let s = List.fold_left (fun acc x ->
    fst (SequentialStack.apply (SequentialStack.Push x) acc)
  ) SequentialStack.empty ops in
  let results = List.fold_left (fun (acc, results) _ ->
    let acc, r = SequentialStack.apply SequentialStack.Pop acc in
    (acc, r :: results)
  ) (s, []) ops in
  let results = List.rev (snd results) in
  assert (results = [Some 5; Some 4; Some 3; Some 2; Some 1]);
  print_endline "stack LIFO order: OK"





let test_queue_enqueue_dequeue () =
  let q = SequentialQueue.empty in
  let q, _ = SequentialQueue.apply (SequentialQueue.Enqueue 1) q in
  let q, _ = SequentialQueue.apply (SequentialQueue.Enqueue 2) q in
  let q, r = SequentialQueue.apply SequentialQueue.Dequeue q in
  assert (r = Some 1);
  let _, r = SequentialQueue.apply SequentialQueue.Dequeue q in
  assert (r = Some 2);
  print_endline "queue enqueue/dequeue: OK"



let test_queue_empty_dequeue () =
  let q = SequentialQueue.empty in
  let _, r = SequentialQueue.apply SequentialQueue.Dequeue q in
  assert (r = None);
  print_endline "queue empty dequeue: OK"



let test_queue_fifo () =
  let ops = [1; 2; 3; 4; 5] in
  let q = List.fold_left (fun acc x ->
    fst (SequentialQueue.apply (SequentialQueue.Enqueue x) acc)
  ) SequentialQueue.empty ops in
  let results = List.fold_left (fun (acc, results) _ ->
    let acc, r = SequentialQueue.apply SequentialQueue.Dequeue acc in
    (acc, r :: results)
  ) (q, []) ops in
  let results = List.rev (snd results) in
  assert (results = [Some 1; Some 2; Some 3; Some 4; Some 5]);
  print_endline "queue FIFO order: OK"



let test_set_insert_contains () =
  let s = SequentialSortedList.empty in
  let s, _ = SequentialSortedList.apply (SequentialSortedList.Insert 3) s in
  let s, _ = SequentialSortedList.apply (SequentialSortedList.Insert 1) s in
  let s, _ = SequentialSortedList.apply (SequentialSortedList.Insert 2) s in
  let _, r = SequentialSortedList.apply (SequentialSortedList.Contains 2) s in
  assert (r = Some 2);
  let _, r = SequentialSortedList.apply (SequentialSortedList.Contains 99) s in
  assert (r = None);
  print_endline "set insert/contains: OK"


let test_set_remove () =
  let s = SequentialSortedList.empty in
  let s, _ = SequentialSortedList.apply (SequentialSortedList.Insert 1) s in
  let s, _ = SequentialSortedList.apply (SequentialSortedList.Insert 2) s in
  let s, _ = SequentialSortedList.apply (SequentialSortedList.Remove 1) s in
  let _, r = SequentialSortedList.apply (SequentialSortedList.Contains 1) s in
  assert (r = None);
  let _, r = SequentialSortedList.apply (SequentialSortedList.Contains 2) s in
  assert (r = Some 2);
  print_endline "set remove: OK"

let test_set_sorted_order () =
  (* insert out of order, check internal list is sorted *)
  let s = SequentialSortedList.empty in
  let s, _ = SequentialSortedList.apply (SequentialSortedList.Insert 5) s in
  let s, _ = SequentialSortedList.apply (SequentialSortedList.Insert 2) s in
  let s, _ = SequentialSortedList.apply (SequentialSortedList.Insert 8) s in
  let s, _ = SequentialSortedList.apply (SequentialSortedList.Insert 1) s in
  assert (s = [1; 2; 5; 8]);
  print_endline "set sorted order: OK"


let test_set_duplicate_insert () =
  let s = SequentialSortedList.empty in
  let s, _ = SequentialSortedList.apply (SequentialSortedList.Insert 3) s in
  let s, _ = SequentialSortedList.apply (SequentialSortedList.Insert 3) s in
  assert (s = [3]);
  print_endline "set duplicate insert: OK"

let test_set_remove_nonexistent () =
  let s = SequentialSortedList.empty in
  let s, _ = SequentialSortedList.apply (SequentialSortedList.Insert 1) s in
  let s, _ = SequentialSortedList.apply (SequentialSortedList.Remove 99) s in
  assert (s = [1]);
  print_endline "set remove nonexistent: OK"

let test_skiplist_insert_search () =
  let sl = SequentialSkipList.create 16 0.5 in
  List.iter (SequentialSkipList.insert sl) [3; 6; 7; 9; 12; 19];
  assert (SequentialSkipList.search sl 6);
  assert (not (SequentialSkipList.search sl 15));
  print_endline "skiplist insert/search: OK"

let test_skiplist_erase () =
  let sl = SequentialSkipList.create 16 0.5 in
  List.iter (SequentialSkipList.insert sl) [3; 6; 7; 9; 12; 19];
  SequentialSkipList.erase sl 6;
  assert (not (SequentialSkipList.search sl 6));
  assert (SequentialSkipList.search sl 7);
  print_endline "skiplist erase: OK"

let test_skiplist_duplicate_insert () =
  let sl = SequentialSkipList.create 16 0.5 in
  SequentialSkipList.insert sl 3;
  SequentialSkipList.insert sl 3;
  let level_zero = List.assoc 0 (SequentialSkipList.to_lists sl) in
  assert (level_zero = [3]);
  print_endline "skiplist duplicate insert: OK"

let test_skiplist_erase_nonexistent () =
  let sl = SequentialSkipList.create 16 0.5 in
  List.iter (SequentialSkipList.insert sl) [1; 2; 3];
  SequentialSkipList.erase sl 99;
  let level_zero = List.assoc 0 (SequentialSkipList.to_lists sl) in
  assert (level_zero = [1; 2; 3]);
  print_endline "skiplist erase nonexistent: OK"

let test_bst_insert_remove () =
  let t = SequentialBst.empty compare in
  let t = SequentialBst.insert t 5 in
  let t = SequentialBst.insert t 2 in
  let t = SequentialBst.insert t 8 in
  let t = SequentialBst.insert t 1 in
  let t = SequentialBst.insert t 3 in
  let t = SequentialBst.insert t 7 in
  assert (SequentialBst.to_list t = [1; 2; 3; 5; 7; 8]);
  let t = SequentialBst.remove t 2 in
  assert (SequentialBst.to_list t = [1; 3; 5; 7; 8]);
  print_endline "bst insert/remove: OK"



let () =
  print_endline "\n── Sequential Stack ──";
  test_stack_push_pop ();
  test_stack_empty_pop ();
  test_stack_lifo ();

  print_endline "\n── Sequential Queue ──";
  test_queue_enqueue_dequeue ();
  test_queue_empty_dequeue ();
  test_queue_fifo ();

  print_endline "\n── Sequential SortedSet ──";
  test_set_insert_contains ();
  test_set_remove ();
  test_set_sorted_order ();
  test_set_duplicate_insert ();
  test_set_remove_nonexistent ();

  print_endline "\n── Sequential SequentialSkipList ──";
  test_skiplist_insert_search ();
  test_skiplist_erase ();
  test_skiplist_duplicate_insert ();
  test_skiplist_erase_nonexistent ();

  print_endline "\n── Sequential SequentialBst ──";
  test_bst_insert_remove ();

  print_endline "\nAll sequential tests passed."


