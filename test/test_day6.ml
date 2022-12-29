open Day6
open Common

let test_add_rem =
  QCheck.Test.make ~count:1000 ~long_factor:100 ~name:"test_state_add"
    QCheck.(printable_char)
  @@ fun c ->
  let max_len = 4 in
  let st = State.create max_len in
  check (State.distinct st = 0) "state is zero";
  State.add st c;
  check (State.distinct st = 1) "state is one";
  State.rem st;
  check (State.distinct st = 0) "state is zero again";
  true

let test_add =
  QCheck.Test.make ~count:1000 ~long_factor:100 ~name:"test_state_add"
    QCheck.(list printable_char)
  @@ fun cs ->
  let max_len = 4 in
  let st = State.create max_len in
  List.iter
    (fun c ->
      State.add st c;
      if State.distinct st > max_len then
        QCheck.Test.fail_reportf "State is too large")
    cs;
  true

let test_invariant =
  QCheck.Test.make ~count:1000 ~long_factor:100 ~name:"test_invariant"
    QCheck.(list printable_char)
  @@ fun cs ->
  let max_len = 4 in
  let st = State.create max_len in
  List.iter (State.add st) cs;
  let contents = st.contents |> Queue.to_seq |> List.of_seq in
  List.length (List.sort_uniq Char.compare contents) = st.distinct

let test_invariant2 =
  QCheck.Test.make ~count:1000 ~long_factor:100 ~name:"test_invariant2"
    QCheck.(list printable_char)
  @@ fun cs ->
  let max_len = 4 in
  let st = State.create max_len in
  List.iter (State.add st) cs;
  let contents = st.contents |> Queue.to_seq |> List.of_seq in
  List.iter
    (fun c ->
      let idx = Char.code c in
      let count = Array.get st.element_count idx in
      check (0 < Array.get st.element_count (Char.code c)) "count";
      Array.set st.element_count idx (count - 1))
    contents;
  true

let () =
  QCheck_runner.run_tests_main
    [ test_add_rem; test_add; test_invariant; test_invariant2 ]
