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

module Problem = Solver(MySpec)

let () =
  match Problem.solve () with
  | None    ->
    Printf.printf "No solution!\n"
  | Some l  ->
    List.iter (function PLUS1 -> print_endline "+1" | PLUS2 -> print_endline "+2") l