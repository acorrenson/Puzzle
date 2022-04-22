(** DSL for problem specification *)
module type SPEC = sig
  (** The type of states *)
  type state
  
  (** The type of actions *)
  type action
  
  (** The initial state *)
  val init : state

  (** Goal state detection *)
  val goal : state -> bool
  
  (** Cost of an action *)
  val cost : action -> int

  (** Compute the actions available in a state *)
  val actions : state -> action list

  (** Apply an action to a state *)
  val apply : state -> action -> state
end