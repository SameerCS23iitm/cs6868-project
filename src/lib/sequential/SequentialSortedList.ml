type 'a state = 'a list
type 'a op =
  | Insert of 'a
  | Remove of 'a
  | Contains of 'a

let empty = []

let rec insert x = function
  | [] -> [x]
  | h :: t ->
    let c = Stdlib.compare x h in
    if c < 0 then x :: h :: t
    else if c = 0 then h :: t
    else h :: insert x t

let rec remove x = function
  | [] -> []
  | h :: t ->
    let c = Stdlib.compare x h in
    if c = 0 then t
    else h :: remove x t

let apply state op =
  match op with
  | Insert x  -> (insert x state, None)
  | Remove x  -> (remove x state, None)
  | Contains x -> (state, if List.mem x state then Some x else None)