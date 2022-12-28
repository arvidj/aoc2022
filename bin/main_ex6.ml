open Aoc2022.Base
open Aoc2022.Day6

let () =
  match Sys.argv with
  | [| _; input_path |] ->
      iter_lines input_path @@ fun line -> pp_marker line (marker_pos line 4) 4
  | _ ->
      Printf.eprintf "Usage: %s [input_path.txt]\n" Sys.argv.(0);
      exit 1
