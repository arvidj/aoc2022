let sf = Printf.sprintf

let with_file_in path f =
  let channel = open_in path in
  try f channel
  with x ->
    close_in channel;
    raise x

let iter_lines path f =
  with_file_in path @@ fun channel ->
  try
    while true do
      f (input_line channel)
    done
  with End_of_file ->
    close_in channel;
    ()

let iter_lines3 path f =
  with_file_in path @@ fun channel ->
  try
    while true do
      let a = input_line channel in
      try
        let b = input_line channel in
        let c = input_line channel in
        f (a, b, c)
      with End_of_file ->
        close_in channel;
        raise
          (Invalid_argument
             (sf "[iter_lines3] lines in file %s is not tripled" path))
    done
  with End_of_file ->
    close_in channel;
    ()

let sum lst = List.fold_left ( + ) 0 lst
let min a b = if a < b then a else b
let max a b = if a > b then a else b

let insert_sorted (n : int) (lst : int list) : int list =
  let rec loop acc lst =
    match lst with
    | [] -> List.rev (n :: acc)
    | x :: xs ->
        if n < x then List.rev acc @ (n :: x :: xs) else loop (x :: acc) xs
  in
  loop [] lst

let%expect_test "test insert_sorted" =
  let pp_int_list ls =
    Printf.printf "[%s]\n" (String.concat ", " (List.map string_of_int ls))
  in
  let test n ls = pp_int_list (insert_sorted n ls) in
  test 1 [];
  [%expect {| [1] |}];
  test 1 [ 1; 2; 3 ];
  [%expect {| [1, 1, 2, 3] |}];
  test 4 [ 1; 2; 3 ];
  [%expect {| [1, 2, 3, 4] |}];
  test 0 [ 1; 2; 3 ];
  [%expect {| [0, 1, 2, 3] |}];
  test 2 [ 1; 2; 3 ];
  [%expect {| [1, 2, 2, 3] |}]
