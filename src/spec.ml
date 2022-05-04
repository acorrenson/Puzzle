(** 
  {1 DSL(s) for generic problem specification}
*)

(** {!UNIT_SPEC} is a language to describe search problems
    where no notion of "cost" is involved.

    Such problems are described by a state-space graph
    with unlabelled edges.
    The graphs are not explicitely represented because
    they can have an infinite number of nodes.
    Graphs are instead represented by the following
    elements :

    {ul {- a value [init] representing the initial state
        }
        {- a function [actions] listing the possible actions
          in a given state
        }
        {- a function [apply] to compute a new state
           from a previous state and an action 
        }
        {- a function [goal] describing the goal states }
    }
*)
module type UNIT_SPEC = sig
  (** Type of the states *)
  type state

  (** Type of the actions *)
  type action

  (** Initial states *)
  val init : state

  (** Test if a state is a goal state *)
  val goal : state -> bool

  (** Compute the actions available in a state *)
  val actions : state -> action list

  (** Apply an action to a state *)
  val apply : state -> action -> state
end

(** {!SPEC} is similar to {!UNIT_SPEC} but
    allows to assign a cost to the actions.
    Cost informations can be used by solvers
    to search for an optimal solution (a solution
    with the lowest possible cost).
*)
module type SPEC = sig
  include UNIT_SPEC

  (** Cost of an action *)
  val cost : action -> int
end

(** {!HSPEC} is similar to {!SPEC} but
    allows to additionally specify a heuristic
    function.
    The solver {!Search.HSolver} can take advantage of this
    heuristic to speed up the search.
*)
module type HSPEC = sig
  include SPEC
  
  (** Heuristic function *)
  val heuristic : state -> int
end