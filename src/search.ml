open Spec

(** Type of solvers *)
module type SOLVER = functor (X : SPEC) -> sig
  val solve : unit -> X.action list option
end


(** A module to ease the incremental construction
    of paths in a state space graph.
    The state space is supposed to be characterised by
    a type [state] of states and a type [action] of actions
    to move from a state to another.
*)
module Paths : sig
  (** Type of paths tables *)
  type ('state, 'action) t
  
  (** Create a new table with a given initial state *)
  val init : 'state -> ('state, 'action) t

  (** Update the table.
      [set tbl x act y] updates the table [tbl]
      such that [x] is declared as the new predecessor of [y]
      and [act] is the action to perform to go from [x] to [y]
  *)
  val set : ('state, 'action) t -> 'state -> 'action -> 'state -> unit
  
  (** Try to extract a path (a list of actions) from
      the initial state to a given state.
      This function might not terminate if there's a cycle in the
      table.
      This function might throw an exception if the table is ill-formed
      (if there is a state in the table whose predecessor is not
      in the table)
  *)
  val build : ('state, 'action) t -> 'state -> 'action list
end = struct
  type ('a, 'b) t = ('a, ('a * 'b) option) Hashtbl.t

  let init x =
    let h = Hashtbl.create 10 in
    Hashtbl.add h x None;
    h

  let set h x act y =
    Hashtbl.add h y (Some (x, act))

  let build preds x =
    let rec step path = function
      | None -> path
      | Some (x, act) ->
        step (act::path) (Hashtbl.find preds x)
    in
    step [] (Hashtbl.find preds x)
end

(** A module to keep track of the costs associated
    to nodes in a search algorithm
*)
module Costs : sig
  (** Type of costs tables *)
  type 'a t

  (** Create a costs table with an initial node.
      The cost associated to the node is always [0]
  *)
  val init : 'a -> 'a t

  (** [improve tbl x cx] checks if the cost [cx]
      is better than the current cost of [x] in [tbl].
      If so, the cost is updated in place and [true] is
      returned. Otherwise, [tbl] is left unchanged and
      [false] is returned.
  *)
  val improve : 'a t -> 'a -> int -> bool
end = struct
  type 'a t = ('a, int) Hashtbl.t

  let init x =
    let h = Hashtbl.create 10 in
    Hashtbl.add h x 0;
    h

  let improve h x cx =
    match Hashtbl.find_opt h x with
    | None ->
      Hashtbl.add h x cx;
      true
    | Some cx' when cx < cx' ->
      Hashtbl.add h x cx;
      true
    | _ -> false
end

(**
  A Simple solver based on Dijsktra algorithm.
  If the problem specified by [X] has a solution,
  [Solver(X).solve ()] returns an optimal sequence
  of actions leading to a goal state.

  If the cost of all actions is the same, it is recommended
  to use [BfsSolver] instead.
*)
module Solver : SOLVER = functor (X: SPEC) -> struct
  module Marks = Set.Make(struct
    type t = X.state
    let compare = compare
  end)

  exception Found of X.state

  let transitions x =
    List.map (fun act -> act, X.apply x act) (X.actions x)

  let solve () =
    (* Cost of going to a state *)
    let costs = Costs.init X.init in
    (* Table of predecessors *)
    let preds = Paths.init X.init in
    (* Marked states *)
    let marks = ref Marks.empty in
    (* Work list *)
    let queue = Heap.of_array [| 0, X.init |] in
    let marked x = Marks.mem x !marks in
    let mark x = marks := Marks.add x !marks in
    try
      while not (Heap.is_empty queue) do begin
        let (xcost, x) = Heap.extract queue in
        if X.goal x then begin
          raise (Found x)
        end else if not (marked x) then begin
          List.iter (fun (act, y) ->
            let new_ycost = xcost + X.cost act in
            if Costs.improve costs y new_ycost then begin
              Heap.insert queue y new_ycost;
              Paths.set preds x act y
            end
          ) (transitions x);
          mark x
        end
      end done; None
    with Found x -> Some (Paths.build preds x)
end

(**
  A solver Breadth First Search resolution algorithm.
  If the problem specified by [X] has a solution,
  [BfsSolver(X).solve ()] returns the shortest sequence
  of actions leading to a goal state.

  Note that this solver is usually much faster
  than [Solver] on problems where the cost of actions
  is constant.
*)
module BfsSolver : SOLVER = functor (X : UNIT_SPEC) -> struct

  module Marks = Set.Make(struct
    type t = X.state
    let compare = compare
  end)

  type node = {
    state : X.state;
    path  : X.action list;
  }

  let expand queue { state; path } =
    List.iter (fun act ->
      Queue.add
        { state = X.apply state act; path = act::path }
        queue
    ) (X.actions state)

  exception Found of (X.action list)

  let solve () =
    (* Table of predecessors *)
    (* Marked states *)
    let marks = ref Marks.empty in
    (* Work list *)
    let queue = Queue.create () in
    let marked x = Marks.mem x !marks in
    let mark x = marks := Marks.add x !marks in
    Queue.add { state = X.init; path = [] } queue;
    try
      while not (Queue.is_empty queue) do
        let x = Queue.take queue in
        if X.goal x.state then
          raise (Found (List.rev x.path))
        else if not (marked x.state) then begin
          let (a, b) : int * int = Obj.magic x.state in
          Printf.printf "expanding (%d, %d)\n" a b;
          mark x.state;
          expand queue x
        end
    done; None
    with Found sol -> Some sol
end