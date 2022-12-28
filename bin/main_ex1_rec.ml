type elf = { index : int; cals : int }

let () =
  match Sys.argv with
  | [| _; input_path |] ->
      with_file_in input_path @@ fun channel ->
      let cals =
        match int_of_string_opt (input_line channel) with
        | None ->
            Printf.eprintf "First line unexpectedly non-integer\n";
            exit 1
        | Some cals -> cals
      in
      let rec loop max current =
        try
          match int_of_string_opt (input_line channel) with
          | None -> loop max { index = current.index + 1; cals = 0 }
          | Some cals ->
              let current = { current with cals = current.cals + cals } in
              if current.cals > max.cals then loop current current
              else loop max current
        with End_of_file -> max
      in
      let first = { index = 0; cals } in
      let max = loop first first in
      Printf.printf "Elf #%d carries most calories: %d\n" max.index max.cals
  | _ ->
      Printf.eprintf "Usage: %s [input_path.txt]\n" Sys.argv.(0);
      exit 1
