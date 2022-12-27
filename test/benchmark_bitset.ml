open Benchmark
open Aoc2022.Day3

let benchmark_bitset n =
  let st = Random.State.make [||] in
  let set_slots = ref 0 in
  let rec aux bs n =
    if n = 0 then ()
    else
      let slot1 = Random.State.int st Bitset.slot_max in
      let slot2 = Random.State.int st Bitset.slot_max in
      let bs' = Bitset.set bs slot1 in
      if Bitset.get bs' slot2 then incr set_slots;
      aux bs' (n - 1)
  in
  aux Bitset.empty n

let benchmark_bitset_array n =
  let st = Random.State.make [||] in
  let set_slots = ref 0 in
  let bs = BitsetArray.empty () in
  let rec aux n =
    if n = 0 then ()
    else
      let slot1 = Random.State.int st Bitset.slot_max in
      let slot2 = Random.State.int st Bitset.slot_max in
      BitsetArray.set bs slot1;
      if BitsetArray.get bs slot2 then incr set_slots;
      aux (n - 1)
  in
  aux n

let () =
  let res =
    let n = 1000000 in
    throughputN ~repeat:3 8
      [
        ("bitset", benchmark_bitset, n);
        ("bitset_array", benchmark_bitset_array, n);
      ]
  in
  print_newline ();
  tabulate res
