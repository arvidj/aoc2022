open Day6

let () =
  match Sys.argv with
  | [| _; input_path |] ->
      iter_lines input_path @@ fun line ->
      Printf.printf "Start of packet:\n";
      pp_marker line (marker_pos line start_of_packet) start_of_packet;
      Printf.printf "Start of message:\n";
      pp_marker line (marker_pos line start_of_message) start_of_message
  | _ ->
      Printf.eprintf "Usage: %s [input_path.txt]\n" Sys.argv.(0);
      exit 1
