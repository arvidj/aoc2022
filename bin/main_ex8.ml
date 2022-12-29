open Day8

let () =
  match Sys.argv with
  | [| _; input_path |] -> (
      let lines = get_lines input_path |> List.map String.trim in
      match lines with
      | first :: others ->
          let width = String.length first in
          let () =
            if
              not
              @@ List.for_all (fun line -> String.length line = width) others
            then raise (Invalid_argument "lines have uneven length")
          in
          let f = Forest.create ~width ~height:(List.length lines) (-1) in
          List.iteri
            (fun y line ->
              String.iteri
                (fun x c -> Forest.set f.contents x y Char.(code c - code '0'))
                line)
            lines;
          let visible_outcome, visible_expected = Forest.get_visible_count f in
          if Sys.getenv_opt "SHOW" <> None then
            Printf.printf "%s\n\n" (Forest.show f);
          Printf.printf "Visible trees: %d, (expected: %d)\n" visible_outcome
            visible_expected
      | [] -> raise (Invalid_argument "no lines"))
  | _ ->
      Printf.eprintf "Usage: %s [input_path.txt]\n" Sys.argv.(0);
      exit 1
