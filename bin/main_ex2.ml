open Aoc2022.Base

type shape = Rock | Paper | Scissors

let their_shape_of_string = function
  | "A" -> Rock
  | "B" -> Paper
  | "C" -> Scissors
  | s ->
      raise
        (Invalid_argument (sf "[their_shape_of_string] invalid input: %s" s))

let our_shape_of_string = function
  | "X" -> Rock
  | "Y" -> Paper
  | "Z" -> Scissors
  | s ->
      raise (Invalid_argument (sf "[our_shape_of_string] invalid input: %s" s))

type outcome = You_win | They_win | Draw

let score ~theirs ~ours =
  let shape_score = match ours with Rock -> 1 | Paper -> 2 | Scissors -> 3 in
  let your_outcome =
    match (ours, theirs) with
    | Rock, Scissors -> You_win
    | Rock, Paper -> They_win
    | Scissors, Rock -> They_win
    | Scissors, Paper -> You_win
    | Paper, Scissors -> They_win
    | Paper, Rock -> You_win
    | _, _ -> Draw
  in
  let outcome_score =
    match your_outcome with They_win -> 0 | Draw -> 3 | You_win -> 6
  in
  shape_score + outcome_score

let () =
  match Sys.argv with
  | [| _; input_path |] ->
      let score_total = ref 0 in
      ( iter_lines input_path @@ fun line ->
        match String.split_on_char ' ' line with
        | [ theirs_s; ours_s ] ->
            let ours, theirs =
              (our_shape_of_string ours_s, their_shape_of_string theirs_s)
            in
            score_total := !score_total + score ~theirs ~ours
        | _ -> raise (Failure (sf "Unexpected input: %S" line)) );
      Printf.printf "Your score: %d\n" !score_total
  | _ ->
      Printf.eprintf "Usage: %s [input_path.txt]\n" Sys.argv.(0);
      exit 1
