open Puzzle
open Spec
open Search

module MySpec : SPEC with type action = int * int = struct
  type state = int
  type action = int * int
  let apply s _ = (s + 1) mod 10
  let actions x = [(x, x + 1)]
  let goal s = s = 4
  let init = 0
  let cost _ = 0
end

module Problem = Solver(MySpec)

let () =
  match Problem.solve () with
  | None -> Printf.printf "No solution!\n"
  | Some l ->
    List.iter (fun (x, y) -> Printf.printf "%d -> %d\n" x y) l