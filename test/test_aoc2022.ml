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

let () = QCheck_runner.run_tests_main (Test_Day3.tests @ Test_Day6.tests)
