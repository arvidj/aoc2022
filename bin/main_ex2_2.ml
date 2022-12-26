open Aoc2022.Base

type shape = Rock | Paper | Scissors
type outcome = You_win | They_win | Draw

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

let desired_outcome_of_string = function
  | "X" -> They_win
  | "Y" -> Draw
  | "Z" -> You_win
  | s ->
      raise
        (Invalid_argument (sf "[desired_outcome_of_string] invalid input: %s" s))

(** [defeats x] is the shape that defeats [x] *)
let defeats = function Rock -> Paper | Paper -> Scissors | Scissors -> Rock

(** [loses_to x] is the shape that loses_to [x] *)
let loses_to = function Paper -> Rock | Scissors -> Paper | Rock -> Scissors

let get_outcome ~theirs ~ours =
  if ours = theirs then Draw
  else if ours = defeats theirs then You_win
  else They_win

let score ~theirs ~ours =
  let shape_score = match ours with Rock -> 1 | Paper -> 2 | Scissors -> 3 in
  let outcome_score =
    match get_outcome ~theirs ~ours with
    | They_win -> 0
    | Draw -> 3
    | You_win -> 6
  in
  shape_score + outcome_score

let shape_for_outcome theirs desired_outcome =
  match desired_outcome with
  | Draw -> theirs
  | You_win -> defeats theirs
  | They_win -> loses_to theirs

let () =
  match Sys.argv with
  | [| _; input_path |] ->
      let score_total = ref 0 in
      let score_total_alt = ref 0 in
      ( iter_lines input_path @@ fun line ->
        match String.split_on_char ' ' line with
        | [ theirs_s; snd_col_s ] ->
            let theirs, ours =
              (their_shape_of_string theirs_s, our_shape_of_string snd_col_s)
            in
            score_total := !score_total + score ~theirs ~ours;
            let desired_outcome = desired_outcome_of_string snd_col_s in
            let ours_alt = shape_for_outcome theirs desired_outcome in
            score_total_alt := !score_total_alt + score ~theirs ~ours:ours_alt
        | _ -> raise (Failure (sf "Unexpected input: %S" line)) );
      Printf.printf "Your score: %d, alternative score: %d\n" !score_total
        !score_total_alt
  | _ ->
      Printf.eprintf "Usage: %s [input_path.txt]\n" Sys.argv.(0);
      exit 1
