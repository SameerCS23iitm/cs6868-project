type 'a tree = 
  Empty
  | Node of {l : 'a tree ; v : 'a ; r : 'a tree}

type 'a rnode = {
  compare : 'a -> 'a -> int;
  root : 'a tree;
}

let create ~compare () =
  { compare; root = Empty; }
  
let rec to_list root =  
  match root with
  | Empty -> []
  | Node {l; v; r} -> (
    to_list l @
    [v] @
    to_list r;
  )

let to_list rnode = 
  match rnode with
  {compare = _; root = rt} -> to_list rt

let rec is_present root cmp node = 
  match root with
  | Empty -> false
  | Node{l;v;r} -> (
      if (cmp node v = 0) then true
      else if (cmp node v < 0) then is_present l cmp node
      else is_present r cmp node
  )

let is_present rnode elt =
  match rnode with
  {compare = cmp; root = rt} -> is_present rt cmp elt

let rec insert tree cmp elt =
  match tree with  
  | Empty -> Node{l = Empty; v = elt; r = Empty}
  | Node {l; v; r} -> (
    if (cmp elt v <= 0) then Node{l = (insert l cmp elt); v; r}
    else Node{l; v; r = (insert r cmp elt)}
  )

let insert rnode elt =
  if is_present rnode elt then rnode
  else begin
    match rnode with
    {compare = cmp; root = rt} -> {compare = cmp; root = insert rt cmp elt} 
  end 

let rec find_min root = 
  match root with
  | Empty -> failwith "Empty tree"
  | Node{l;v;_} -> (
      match l with
      | Empty -> v
      | left -> find_min left 
  )

let rec remove root cmp node = 
  match root with
    | Empty -> Empty (* just return normally, someone else must've deleted it*)
    | Node{l;v;r} -> (
        if (cmp node v < 0) then Node{l = remove l cmp node; v; r}
        else if (cmp node v > 0) then Node{l; v; r = remove r cmp node}
        else (
          match (l, r) with
          | (Empty, Empty) -> Empty
          | (node, Empty) -> node
          | (Empty, node) -> node
          | (left, right) -> (
              let min_val = find_min right in
              Node {l = left; v = min_val; r = remove right cmp min_val}
          )
        ) 
    )

  let remove rnode elt = 
    match rnode with
    {compare = cmp; root = rt} -> {compare = cmp; root = remove rt cmp elt} 

  let empty cmp = create ~compare:cmp ()

  type 'a state = 'a rnode

  type 'a op = 
    | Insert of 'a
    | Remove of 'a
    | Contains of 'a
    
  let apply op state =
    match op with
    | Insert x -> (insert state x, None)
    | Remove x -> (remove state x, None)
    | Contains x -> (state, if is_present state x then Some x else None)