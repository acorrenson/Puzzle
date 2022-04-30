open Puzzle
open Spec
open Search

type increment = PLUS1 | PLUS2

module MySpec : SPEC with type action = increment = struct
  type state = int
  
  type action = increment

  let apply s = function
    | PLUS1 -> (s + 1) mod 10
    | PLUS2 -> (s + 2) mod 10
  
  let actions _ = [PLUS1; PLUS2]
  
  let goal s = s = 4

  let init = 0
  
  let cost = function
    | PLUS1 -> 2
    | PLUS2 -> 1
end

type direction = RIGHT | DOWN

module MySpec2 : SPEC with type action = direction = struct
  
  type state = int * int

  type action = direction

  let init = (0, 0)

  let grid = [|
    [| 0; 0; 0; 0 |];
    [| 0; 0; 0; 1 |];
    [| 0; 0; 0; 0 |];
    [| 0; 0; 1; 0 |];
  |]

  let actions (x, y) =
    (if x + 1 < 4 && grid.(y).(x + 1) = 0 then [RIGHT] else [])
    @
    (if y + 1 < 4 && grid.(y + 1).(x) = 0 then [DOWN] else [])

  let apply (x, y) = function
    | RIGHT -> (x + 1, y)
    | DOWN -> (x, y + 1)

  let goal (x, y) = (x = 3 && y = 3)
  let cost _ = assert false
end

module Problem1 = Solver(MySpec)
module Problem2 = BfsSolver(MySpec2)

let () =
  match Problem1.solve () with
  | None    ->
    Printf.printf "No solution!\n"
  | Some l  ->
    List.iter (function PLUS1 -> print_endline "+1" | PLUS2 -> print_endline "+2") l

let () =
  match Problem2.solve () with
  | None    ->
    Printf.printf "No solution!\n"
  | Some l  ->
    List.iter (function RIGHT -> print_endline "left" | DOWN -> print_endline "down") l