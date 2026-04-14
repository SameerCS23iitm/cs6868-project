module type S = sig
  
  type 'a state
  type 'a op
  val apply : 'a state -> 'a op -> 'a state * 'a option
  val empty : 'a state
  
end