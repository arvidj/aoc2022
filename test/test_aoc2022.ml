let check b msg = if not b then QCheck.Test.fail_report msg

module Test_Day3 = struct
  open Day3

  let slot = QCheck.int_bound Bitset.slot_max

  let test_bitset_get_set =
    QCheck.Test.make ~count:1000 ~long_factor:100 ~name:"test_bitset_get_set"
      QCheck.(pair int slot)
    @@ fun (bs, slot) ->
    let bs = Bitset.of_int bs in
    let bs' = Bitset.(set bs slot) in
    List.for_all
      (fun slot' ->
        let is_set = Bitset.(get bs' slot') in
        if slot = slot' then
          if not is_set then
            QCheck.Test.fail_reportf "Expected slot %d to be set, was not" slot
          else true
        else if not (is_set = Bitset.(get bs slot')) then
          QCheck.Test.fail_reportf
            "Expected slot %d to be the original value %b, was %b" slot
            Bitset.(get bs slot')
            is_set
        else true)
      (List.init Bitset.slot_max Fun.id)

  let test_bitset_array_get_set =
    QCheck.Test.make ~count:1000 ~long_factor:100
      ~name:"test_bitset_array_get_set"
      QCheck.(pair int slot)
    @@ fun (seed, slot) ->
    let bs = BitsetArray.of_int seed in
    let bs' = BitsetArray.of_int seed in
    BitsetArray.(set bs' slot);
    List.for_all
      (fun slot' ->
        let is_set = BitsetArray.(get bs' slot') in
        if slot = slot' then
          if not is_set then
            QCheck.Test.fail_reportf "Expected slot %d to be set, was not" slot
          else true
        else if not (is_set = BitsetArray.(get bs slot')) then
          QCheck.Test.fail_reportf
            "Expected slot %d to be the original value %b, was %b" slot
            BitsetArray.(get bs slot')
            is_set
        else true)
      (List.init BitsetArray.slot_max Fun.id)

  let tests = [ test_bitset_get_set; test_bitset_array_get_set ]
end

module Test_Day6 = struct
  open Day6

  let check b msg = if not b then QCheck.Test.fail_report msg

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

  let tests = [ test_add_rem; test_add; test_invariant; test_invariant2 ]
end

module Test_Day8 = struct
  open Day8

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

  let tests = [ test_visible ]
end

let () =
  QCheck_runner.run_tests_main
    (Test_Day3.tests @ Test_Day6.tests @ Test_Day8.tests)
