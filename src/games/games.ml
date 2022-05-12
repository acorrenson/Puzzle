open Spec

module Utils : sig
  val cache : ('a -> 'b) -> ('a -> 'b)
  val arg_max : ('a * int) list -> 'a option
end = struct
  let cache f =
    let h = Hashtbl.create 10 in
    fun x ->
      match Hashtbl.find_opt h x with
      | Some fx -> fx
      | None ->
        let fx = f x in
        Hashtbl.add h x fx;
        fx

  let arg_max' ((_, vx) as x) ((_, vy) as y) =
    if vx > vy then x else y

  let arg_max = function
    | [] -> None
    | x::xs -> Some (List.fold_left arg_max' x xs |> fst)
end



(** Minimax for 2-players games.
    Solve the game for player [P1].
*)
module MinMax(X : GAME2) : sig
  val play : X.state -> X.action option
end = struct

  let rec expand_first s =
    let next act = act, min_val (X.apply s act)
    in List.map next (X.actions s)

  and expand s f =
    let next act = f (X.apply s act)
    in List.map next (X.actions s)
  
  and min_val s =
    List.fold_left min (X.utility s P2) (expand s max_val)

  and max_val s =
    List.fold_left max (X.utility s P1) (expand s min_val)

  let minmax s =
    Utils.arg_max (expand_first s)
  
  let play = Utils.cache minmax
end

(** Minimax with Alpha Beta pruning.
    Solve the game for player [P1].
*)
module MinMaxAB(X : GAME2) : sig
  val play : X.state -> X.action option
end = struct

  type update =
    | Upd of int * int * int
    | Prn of int

  let rec bounded_expansion p s a b =
    let upd = match p with P1 -> max_upd | P2 -> min_upd in
    let rec step v a b = function
      | [] -> v
      | act::next ->
        match upd s act a b with
        | Upd (v, a, b) -> step v a b next
        | Prn v -> v
    in
    step (X.utility s p) a b (X.actions s)

  and min_upd s act a b =
    let v = max_val (X.apply s act) a b in
    if v <= a then Prn v
    else Upd (v, a, min b v)

  and max_upd s act a b =
    let v = min_val (X.apply s act) a b in
    if v >= b then Prn v
    else Upd (v, max a v, b)
  
  and min_val s a b =
    bounded_expansion P2 s a b

  and max_val s a b =
    bounded_expansion P1 s a b

  and expand_first s =
    let next act = act, min_val (X.apply s act) min_int max_int
    in List.map next (X.actions s)

  let minmax s =
    Utils.arg_max (expand_first s)
  
  let play = Utils.cache minmax
end

(** Minimax generalized for N players.
    Solve the game for player [1].
*)
module MinMaxN(X : GAME) : sig
  val solve : unit -> X.action list option
end = struct
  let solve () = assert false
end