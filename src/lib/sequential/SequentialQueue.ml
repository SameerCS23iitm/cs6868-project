

type 'a state = 'a list

type 'a op = 
  | Enqueue of 'a
  | Dequeue


let empty = []


let apply state op =
  match op with 
  | Enqueue x -> (state @ [x],None)
  | Dequeue -> (
      match state with 
      | [] -> ([], None)
      | x::xs -> (xs, Some x)
  )