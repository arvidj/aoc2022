open Aoc2022.Base

let () =
  match Sys.argv with
  | [| _; input_path |] ->
      with_file_in input_path @@ fun ch ->
      let line = input_line ch in
      close_in ch;
      let buf = String.[ get line 0; get line 1; get line 2 ] in
      let rec aux i buf =
        if i < String.length line then
          let c = String.get line i in
          if List.sort_uniq compare (c :: buf) |> List.length < 4 then
            let buf = List.tl buf @ [ c ] in
            aux (i + 1) buf
          else 1 + i
        else raise (Failure "found no marker")
      in
      let marker = aux 3 buf in
      Printf.printf "Marker position: %d\n" marker
  | _ ->
      Printf.eprintf "Usage: %s [input_path.txt]\n" Sys.argv.(0);
      exit 1
