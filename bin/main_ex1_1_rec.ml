type inventory = (int * int) list

let read_inventory (input_path : string) : inventory =
  with_file_in input_path @@ fun channel ->
  let rec loop inventory (index, backpack) =
    match int_of_string_opt (input_line channel) with
    | None -> loop ((index, backpack) :: inventory) (index + 1, 0)
    | Some cals -> loop inventory (index, cals + backpack)
    | exception End_of_file -> (index, backpack) :: inventory
  in
  List.rev (loop [] (0, 0))

let () =
  match Sys.argv with
  | [| _; input_path |] -> (
      let threshold_n = 3 in
      let inventory = read_inventory input_path in
      List.iter
        (fun (index, backpack) ->
          Printf.printf "Backpack #%d: %d\n" index backpack)
        inventory;
      let top_heavies =
        List.fold_left
          (fun top_heavies (index, backpack_sum) ->
            if index < threshold_n then insert_sorted backpack_sum top_heavies
            else
              match top_heavies with
              | [] ->
                  (* This case cannot happen as we always include at least [threshold_n]
                     backpacks in the list of heaviest *)
                  assert false
              | threshold_min :: top_heavies' ->
                  if backpack_sum > threshold_min then
                    insert_sorted backpack_sum top_heavies'
                  else top_heavies)
          [] inventory
      in
      match top_heavies with
      | top_heavies_min :: top_heavies' ->
          let sum_heavies = top_heavies_min + sum top_heavies' in
          let heaviest =
            match List.rev top_heavies with
            | [] -> top_heavies_min
            | top_heavy :: _ -> top_heavy
          in
          Printf.printf
            "Heaviest backpack: %d, sum of top three backpacks: %d\n" heaviest
            sum_heavies
      | [] ->
          Printf.eprintf "Input is empty\n";
          exit 1)
  | _ ->
      Printf.eprintf "Usage: %s [input_path.txt]\n" Sys.argv.(0);
      exit 1
