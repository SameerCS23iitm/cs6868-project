type 'a node_val =
  | Start
  | End
  | Val of 'a

let compare_node_val a b =
  match (a, b) with
  | Start, Start -> 0
  | Start, _ -> -1
  | _, Start -> 1
  | End, End -> 0
  | End, _ -> 1
  | _, End -> -1
  | Val x, Val y -> Stdlib.compare x y

type 'a node = {
  value : 'a node_val;
  forward : 'a node option array;
}

type 'a skip_list = {
  mutable level : int;
  max_level : int;
  prob : float;
  head : 'a node;
  tail : 'a node;
}

let make max_level prob =
  if max_level < 1 then invalid_arg "SkipList.empty: max_level must be >= 1";
  if prob <= 0.0 || prob >= 1.0 then
    invalid_arg "SkipList.empty: prob must be in (0.0, 1.0)";
  Random.self_init ();
  let tail = { value = End; forward = Array.make (max_level + 1) None } in
  let head = { value = Start; forward = Array.make (max_level + 1) (Some tail) } in
  { level = 0; max_level; prob; head; tail }

let create max_level prob = make max_level prob

let random_level sl =
  let rec aux lvl =
    if lvl < sl.max_level && Random.float 1.0 < sl.prob then aux (lvl + 1)
    else lvl
  in
  aux 0

let search sl target =
  let curr = ref sl.head in
  for i = sl.level downto 0 do
    let continue = ref true in
    while !continue do
      match (!curr).forward.(i) with
      | Some node when compare_node_val node.value (Val target) < 0 -> curr := node
      | _ -> continue := false
    done
  done;
  match (!curr).forward.(0) with
  | Some node when compare_node_val node.value (Val target) = 0 -> true
  | _ -> false

let insert sl value =
  let update = Array.make (sl.max_level + 1) sl.head in
  let curr = ref sl.head in

  for i = sl.level downto 0 do
    let continue = ref true in
    while !continue do
      match (!curr).forward.(i) with
      | Some node when compare_node_val node.value (Val value) < 0 -> curr := node
      | _ -> continue := false
    done;
    update.(i) <- !curr
  done;

  let next0 = (!curr).forward.(0) in
  match next0 with
  | Some node when compare_node_val node.value (Val value) = 0 -> ()
  | _ ->
    let new_level = random_level sl in

    if new_level > sl.level then begin
      for i = sl.level + 1 to new_level do
        update.(i) <- sl.head
      done;
      sl.level <- new_level
    end;

    let new_node =
      { value = Val value; forward = Array.make (new_level + 1) None }
    in

    for i = 0 to new_level do
      new_node.forward.(i) <- update.(i).forward.(i);
      update.(i).forward.(i) <- Some new_node
    done

let erase sl value =
  let update = Array.make (sl.max_level + 1) sl.head in
  let curr = ref sl.head in

  for i = sl.level downto 0 do
    let continue = ref true in
    while !continue do
      match (!curr).forward.(i) with
      | Some node when compare_node_val node.value (Val value) < 0 -> curr := node
      | _ -> continue := false
    done;
    update.(i) <- !curr
  done;

  match (!curr).forward.(0) with
  | Some node when compare_node_val node.value (Val value) = 0 ->
    for i = 0 to sl.level do
      match update.(i).forward.(i) with
      | Some n when n == node -> update.(i).forward.(i) <- node.forward.(i)
      | _ -> ()
    done;

    while sl.level > 0 && update.(0) == sl.head && sl.head.forward.(sl.level) == Some sl.tail do
      sl.level <- sl.level - 1
    done
  | _ -> ()

let to_lists sl =
  let collect_level i =
    let rec loop acc node_opt =
      match node_opt with
      | None -> List.rev acc
      | Some node -> (
        match node.value with
        | End -> List.rev acc
        | Start -> loop acc node.forward.(i)
        | Val v -> loop (v :: acc) node.forward.(i)
      )
    in
    loop [] sl.head.forward.(i)
  in
  let rec all_levels i acc =
    if i < 0 then acc else all_levels (i - 1) ((i, collect_level i) :: acc)
  in
  all_levels sl.level []

let print pp_value sl =
  for i = sl.level downto 0 do
    Printf.printf "Level %d: " i;
    let rec loop node_opt =
      match node_opt with
      | None -> ()
      | Some node -> (
        match node.value with
        | End -> ()
        | Start -> loop node.forward.(i)
        | Val v ->
          pp_value v;
          Printf.printf " ";
          loop node.forward.(i)
      )
    in
    loop sl.head.forward.(i);
    Printf.printf "\n"
  done

type 'a state = 'a skip_list

let empty () : 'a state = create 16 0.5

type 'a op =
  | Insert of 'a
  | Remove of 'a
  | Contains of 'a

let apply op state =
  match op with
  | Insert x -> insert state x; (state, None)
  | Remove x -> erase state x; (state, None)
  | Contains x -> (state, if search state x then Some x else None)

