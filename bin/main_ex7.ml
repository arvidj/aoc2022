open Day7

type file = Dir of string | File of (string * int)
type command = Cd_dir of string | Cd_parent | Cd_root | Ls of file list

let rec commands_seq (input : string Seq.t) : command Seq.t =
  match input () with
  | Seq.Cons (line, lines) ->
      let cmd, lines =
        match String.split_on_char ' ' line with
        | [ "$"; "ls" ] ->
            let ls_output_lines, lines =
              let f line = line <> "" && String.get line 0 <> '$' in
              Seq.(take_while f lines, drop_while f lines)
            in
            let file_of_line line =
              match String.split_on_char ' ' line with
              | [ "dir"; x ] -> Dir x
              | [ size; name ] -> File (name, int_of_string size)
              | _ ->
                  raise
                    (Invalid_argument
                       ("[command] unrecognized ls ouput: '" ^ line ^ "'"))
            in
            (Ls (List.of_seq ls_output_lines |> List.map file_of_line), lines)
        | [ "$"; "cd"; ".." ] -> (Cd_parent, lines)
        | [ "$"; "cd"; "/" ] -> (Cd_root, lines)
        | [ "$"; "cd"; subdir ] -> (Cd_dir subdir, lines)
        | _ ->
            raise
              (Invalid_argument
                 ("[command] unrecognized command: '" ^ line ^ "'"))
      in
      Seq.cons cmd (commands_seq lines)
  | Seq.Nil -> Seq.empty

let () =
  match Sys.argv with
  | [| _; input_path |] ->
      let st = State.create 5 in
      lines_seq input_path @@ fun lines ->
      let commands = commands_seq lines in
      ignore
      @@ Seq.fold_left
           (fun (path : Path.t) (cmd : command) ->
             match cmd with
             | Cd_dir dir -> Path.cd path dir
             | Cd_parent -> Path.parent path
             | Cd_root -> Path.root
             | Ls files ->
                 List.iter
                   (function
                     | Dir _ -> ()
                     | File (name, size) ->
                         State.add st Path.(path // name) size)
                   files;
                 path)
           Path.root commands;
      State.pp st;
      let top_dirs_total =
        let top_dirs_threshold = 100_000 in
        State.fold
          (fun _path size total ->
            if size <= top_dirs_threshold then total + size else total)
          st 0
      in
      let to_free =
        let total_disk_space = 70_000_000 in
        let required_disk_space = 30_000_000 in
        let root_space = State.get st Path.root in
        let unused_space = total_disk_space - root_space in
        let must_free = required_disk_space - unused_space in
        match
          State.fold
            (fun _path size to_free ->
              if size >= must_free then
                match to_free with
                | None -> Some size
                | Some to_free -> Some (min to_free size)
              else to_free)
            st None
        with
        | None -> raise (Failure "Found no directory to free!")
        | Some to_free -> to_free
      in
      Printf.printf "Total size of top dirs: %d, to free: %d\n" top_dirs_total
        to_free
  | _ ->
      Printf.eprintf "Usage: %s [input_path.txt]\n" Sys.argv.(0);
      exit 1
