open Spec

(** Generic solver *)
module Solver (X : SPEC) = struct
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
    let preds = Hashtbl.create 10 in
    (* Marked states *)
    let marks = ref States.empty in
    (* Test if a state is marked *)
    let marked x = States.mem x !marks in
    (* Mark a state *)
    let mark x = marks := States.add x !marks in
    (* Build full path from [x] to [X.init] *)
    let build_path (x, _) =
      let rec step path (x, act) =
        if x = X.init then
          act::path
        else
          step (act::path) (Hashtbl.find preds x)
      in
      step [] (Hashtbl.find preds x)
    in
    Hashtbl.add costs X.init 0;
    while not (Heap.is_empty queue) do
      let (xcost, x) = Heap.extract queue in
      if not (marked x) then begin
        List.iter (fun (act, y) ->
          let new_ycost = xcost + X.cost act in
          match Hashtbl.find_opt costs y with
          | Some ycost when new_ycost > ycost ->
            Hashtbl.add costs y new_ycost;
            Hashtbl.add preds y (x, act);
            Heap.insert queue y new_ycost
          | None ->
            Hashtbl.add costs y new_ycost;
            Hashtbl.add preds y (x, act);
            Heap.insert queue y new_ycost
          | _ -> ()

        ) (transitions x);
        mark x
      end
    done;
    let reach = List.of_seq (Hashtbl.to_seq_values preds) in
    let goal = List.find_opt (fun (x, _) -> X.goal x) reach in
    Option.map build_path goal
end