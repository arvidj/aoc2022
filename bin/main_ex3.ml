open Day3

let () =
  match Sys.argv with
  | [| _; input_path |] ->
      let prio_sum = ref 0 in
      ( iter_lines input_path @@ fun line ->
        prio_sum := !prio_sum + prio_of_line line );
      Printf.printf "Priority sum: %d\n" !prio_sum
  | _ ->
      Printf.eprintf "Usage: %s [input_path.txt]\n" Sys.argv.(0);
      exit 1
