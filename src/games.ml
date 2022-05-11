open Spec

let cache f =
  let h = Hashtbl.create 10 in
  fun x ->
    match Hashtbl.find_opt h x with
    | Some fx -> fx
    | None ->
      let fx = f x in
      Hashtbl.add h x fx;
      fx

(** Minimax for 2-players games.
    Solve the game for player [P1].
*)
module MinMax(X : GAME2) : sig
  val play : X.state -> X.action option
end = struct

  let arg_max' ((_, vx) as x) ((_, vy) as y) =
    if vx > vy then x else y

  let arg_max = function
    | [] -> None
    | x::xs -> Some (List.fold_left arg_max' x xs |> fst)

  let rec expand_first s =
    let next act = act, min_val (X.apply s act)
    in List.map next (X.actions s)

  and expand s f =
    let next act = f (X.apply s act)
    in List.map next (X.actions s)
  
  and min_val s =
    List.fold_left min (X.utility s) (expand s max_val)

  and max_val s =
    List.fold_left max (X.utility s) (expand s min_val)

  let minmax s =
    arg_max (expand_first s)
  
  let play = cache minmax
end

(** Minimax with Alpha Beta pruning.
    Solve the game for player [P1].
*)
module MinMaxAB(X : GAME2) : sig
  val solve : unit -> X.action list option
end = struct
  let solve () = assert false
end

(** Minimax generalized for N players.
    Solve the game for player [1].
*)
module MinMaxN(X : GAME) : sig
  val solve : unit -> X.action list option
end = struct
  let solve () = assert false
end