let with_file_in path f =
  let channel = open_in path in
  try f channel
  with x ->
    close_in channel;
    raise x

let sum lst = List.fold_left ( + ) 0 lst
let min a b = if a < b then a else b
let max a b = if a > b then a else b
let sf = Printf.sprintf

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
  [%expect {| Ok (Var "FOO_BAR") |}];
  test 1 [ 1; 2; 3 ];
  [%expect {| Ok (Var "FOO_BAR") |}];
  test 4 [ 1; 2; 3 ];
  [%expect {| Ok (Var "FOO_BAR") |}];
  test 0 [ 1; 2; 3 ];
  [%expect {| Ok (Var "FOO_BAR") |}];
  test 2 [ 1; 2; 3 ];
  [%expect {| Ok (Var "FOO_BAR") |}]
