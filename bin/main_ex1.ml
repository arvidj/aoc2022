open Aoc2022.Base

type elf = { mutable index : int; mutable cals : int }

let () =
  match Sys.argv with
  | [| _; input_path |] -> (
      with_file_in input_path @@ fun channel ->
      let cals =
        match int_of_string_opt (input_line channel) with
        | None ->
            Printf.eprintf "First line unexpectedly non-integer\n";
            exit 1
        | Some cals -> cals
      in
      let current = { index = 0; cals } in
      let max = { index = 0; cals } in
      try
        while true do
          (match int_of_string_opt (input_line channel) with
          | None ->
              current.cals <- 0;
              current.index <- current.index + 1
          | Some cals -> current.cals <- current.cals + cals);
          if current.cals > max.cals then (
            max.cals <- current.cals;
            max.index <- current.index)
        done
      with End_of_file ->
        Printf.printf "Elf #%d carries most calories: %d\n" max.index max.cals)
  | _ ->
      Printf.eprintf "Usage: %s [input_path.txt]\n" Sys.argv.(0);
      exit 1
