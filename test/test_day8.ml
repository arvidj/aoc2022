open Day8
open Common

let forest : Forest.t QCheck.arbitrary =
  let gen : Forest.t QCheck.Gen.t =
    let open QCheck.Gen in
    let size = 20 in
    let* width = int_range 1 size in
    let* height = int_range 1 size in
    let* contents = array_repeat width (array_repeat height (int_bound 9)) in
    return Forest.{ width; height; contents }
  in
  QCheck.make ~print:Forest.show gen

let test_visible =
  QCheck.Test.make ~count:1000 ~long_factor:100 ~name:"test_visible" forest
  @@ fun f ->
  let visible = Forest.compute_visible f in
  List.iter
    (fun y ->
      List.iter
        (fun x ->
          let height = Forest.get f.contents x y in
          let expected = Forest.is_visible f x y in
          let outcome = visible.(x).(y) in
          check (outcome = expected)
          @@ sf
               "Expected tree (%d) at x: %d, y: %d to be visible=%b, got \
                visible=%b"
               height x y expected outcome)
        (0 -- f.width))
    (0 -- f.height);
  true

let () = QCheck_runner.run_tests_main [ test_visible ]
