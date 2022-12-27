module Day3 = struct
  open Aoc2022.Day3

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

let () = QCheck_runner.run_tests_main Day3.tests
