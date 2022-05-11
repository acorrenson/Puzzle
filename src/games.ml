open Spec

(** Minimax for 2-players games.
    Solve the game for player [P1].
*)
module MinMax(X : GAME2) : sig
  val solve : unit -> X.action list option
end = struct
  let solve () = assert false
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