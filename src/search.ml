open Spec

(** Type of solvers *)
module type SOLVER = functor (X : SPEC) -> sig
  val solve : unit -> X.action list option
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
  module States = Set.Make(struct
    type t = X.state
    let compare = compare
  end)

  let transitions x =
    List.map (fun act -> act, X.apply x act) (X.actions x)

  let solve () =
    (* Cost of going to a state *)
    let costs = Hashtbl.create 10 in
    (* Work list *)
    let queue = Heap.of_array [| 0, X.init |] in
    (* Table of predecessors *)
    let preds : (X.state, (X.state * X.action) option) Hashtbl.t = Hashtbl.create 10 in
    (* Marked states *)
    let marks = ref States.empty in
    (* Test if a state is marked *)
    let marked x = States.mem x !marks in
    (* Mark a state *)
    let mark x = marks := States.add x !marks in
    (* Build full path from [x] to [X.init] *)
    let build_path x =
      let rec step path = function
        | None -> path
        | Some (x, act) ->
          step (act::path) (Hashtbl.find preds x)
      in
      step [] (Hashtbl.find preds x)
    in
    Hashtbl.add costs X.init 0;
    Hashtbl.add preds X.init None;
    while not (Heap.is_empty queue) do
      let (xcost, x) = Heap.extract queue in
      if not (marked x) then begin
        List.iter (fun (act, y) ->
          let new_ycost = xcost + X.cost act in
          match Hashtbl.find_opt costs y with
          | Some ycost when new_ycost < ycost ->
            Hashtbl.add costs y new_ycost;
            Hashtbl.add preds y (Some (x, act));
            Heap.insert queue y new_ycost
          | None ->
            Hashtbl.add costs y new_ycost;
            Hashtbl.add preds y (Some (x, act));
            Heap.insert queue y new_ycost
          | _ -> ()
        ) (transitions x);
        mark x
      end
    done;
    let reach = List.of_seq (Hashtbl.to_seq_keys preds) in
    let goal = List.find_opt X.goal reach in
    Option.map build_path goal
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
module BfsSolver : SOLVER = functor (X : SPEC) -> struct
  let solve () = failwith "not implemented yet"
end