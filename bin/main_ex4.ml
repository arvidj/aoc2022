open Day4

let () =
  match Sys.argv with
  | [| _; input_path |] ->
      let count_contained, count_overlapping =
        fold_lines input_path (0, 0)
        @@ fun (count_contained, count_overlapping) line ->
        let r1, r2 = read_elf_group line in
        ( (if contains r1 r2 then count_contained + 1 else count_contained),
          if overlaps r1 r2 then count_overlapping + 1 else count_overlapping )
      in
      Printf.printf "Contained regions: %d, overlapping: %d\n" count_contained
        count_overlapping
  | _ ->
      Printf.eprintf "Usage: %s [input_path.txt]\n" Sys.argv.(0);
      exit 1
