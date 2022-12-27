open Aoc2022.Base

type stacks = char Stack.t Array.t
type state = Initializing of string list | Rearranging of stacks

let stacks_of_lines (lines : string list) : stacks =
  match lines with
  | indices :: contents ->
      let sts =
        Array.init ((String.length indices / 4) + 1) (fun _ -> Stack.create ())
      in
      List.iteri
        (fun line_number content ->
          Array.iteri
            (fun i st ->
              let column_number = 1 + (i * 4) in
              match String.get content column_number with
              | ' ' -> ()
              | 'A' .. 'Z' as c -> Stack.push c st
              | c ->
                  raise
                    (Invalid_argument
                       (sf "Invalid stack content %c at column %d of line %d" c
                          column_number line_number)))
            sts)
        contents;
      sts
  | [] -> raise (Invalid_argument "Found no initialization lines")

let show_stacks (sts : stacks) : string =
  Array.mapi
    (fun i st ->
      let buf = Buffer.create (Stack.length st) in
      st |> Stack.to_seq |> List.of_seq |> List.rev
      |> List.iter (Buffer.add_char buf);
      (*       Stack.iter (Buffer.add_char buf) st; *)
      sf "%d: %s" (i + 1) (Buffer.contents buf))
    sts
  |> Array.to_list |> String.concat "\n"

let () =
  match Sys.argv with
  | [| _; input_path |] -> (
      match
        fold_lines input_path (Initializing []) @@ fun state line ->
        match (state, String.split_on_char ' ' line) with
        | Initializing lines, [ "" ] ->
            let sts = stacks_of_lines lines in
            Printf.printf "Stacks at initialization:\n%s\n\n" (show_stacks sts);
            Rearranging sts
        | Initializing lines, _ -> Initializing (line :: lines)
        | Rearranging sts, [ "move"; count; "from"; from; "to"; to_ ] ->
            let count, from, to_ =
              ( int_of_string count,
                int_of_string from - 1,
                int_of_string to_ - 1 )
            in
            let from = Array.get sts from in
            let to_ = Array.get sts to_ in
            (match Sys.getenv_opt "ALT" with
            | None -> repeat count @@ fun () -> Stack.push (Stack.pop from) to_
            | Some _ ->
                let q = Stack.create () in
                (repeat count @@ fun () -> Stack.push (Stack.pop from) q);
                repeat count @@ fun () -> Stack.push (Stack.pop q) to_);
            Rearranging sts
        | Rearranging _, _ ->
            raise (Invalid_argument (sf "Invalid rearrangement line %s" line))
      with
      | Initializing _ ->
          raise (Failure (sf "Invalid state at EOF, still initializing"))
      | Rearranging sts ->
          let buf = Buffer.create (Array.length sts) in
          Array.iter (fun st -> Stack.pop st |> Buffer.add_char buf) sts;
          Printf.printf "Final message: %s\n" (Buffer.contents buf))
  | _ ->
      Printf.eprintf "Usage: %s [input_path.txt]\n" Sys.argv.(0);
      exit 1
