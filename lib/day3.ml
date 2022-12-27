open Base

let prio (item : char) : int =
  match item with
  | 'a' .. 'z' -> 1 + Char.(code item - code 'a')
  | 'A' .. 'Z' -> 27 + Char.(code item - code 'A')
  | _ -> raise (Invalid_argument (sf "%c is not a valid rucksack item" item))

let%expect_test "test [prio]" =
  let test item = Printf.printf "Prio of %c: %d\n" item (prio item) in
  test 'a';
  [%expect {| Prio of a: 1 |}];
  test 'z';
  [%expect {| Prio of z: 26 |}];
  test 'A';
  [%expect {| Prio of A: 27 |}];
  test 'Z';
  [%expect {| Prio of Z: 52 |}]

module Bitset = struct
  type t = int

  let empty : t = 0
  let slot_max = Sys.int_size - 1

  let get (rs : t) (slot : int) : bool =
    if slot < 0 || slot > slot_max then
      raise (Invalid_argument (sf "[Bitset.get] invalid slot %d" slot))
    else rs land (1 lsl slot) <> 0

  let set (rs : t) (slot : int) : t =
    if slot < 0 || slot > slot_max then
      raise (Invalid_argument (sf "[Bitset.set] invalid slot %d" slot))
    else rs lor (1 lsl slot)

  let of_int n = n
end

module BitsetArray = struct
  type t = bool Array.t

  let slot_max = Sys.int_size - 1
  let empty () : t = Array.init (slot_max + 1) (Fun.const false)

  let get (rs : t) (slot : int) : bool =
    if slot < 0 || slot > slot_max then
      raise (Invalid_argument (sf "[Bitset.get] invalid slot %d" slot))
    else Array.get rs slot

  let set (rs : t) (slot : int) : unit =
    if slot < 0 || slot > slot_max then
      raise (Invalid_argument (sf "[Bitset.set] invalid slot %d" slot))
    else Array.set rs slot true

  let of_int n =
    let bs = Bitset.of_int n in
    let bsa = empty () in
    for slot = 0 to slot_max do
      bsa.(slot) <- Bitset.get bs slot
    done;
    bsa

  let intersection (rs1 : t) (rs2 : t) : t =
    let rs = empty () in
    for slot = 0 to slot_max do
      rs.(slot) <- get rs1 slot && get rs2 slot
    done;
    rs

  let contents (rs : t) : int list =
    let rec aux acc slot =
      if slot < 0 then acc
      else aux (if get rs slot then slot :: acc else acc) (slot - 1)
    in
    aux [] slot_max
end

let slot_of_item c = prio c - 1
let prio_of_slot slot = slot + 1

let rucksack_of_string (str : string) : BitsetArray.t =
  let rs = BitsetArray.empty () in
  String.iter (fun item -> BitsetArray.set rs (slot_of_item item)) str;
  rs

let prio_of_line (line : string) : int =
  let len = String.length line in
  if len mod 2 <> 0 then
    raise
      (Invalid_argument (sf "[prio_of_line] line %S has uneven length" line))
  else
    let len2 = len / 2 in
    let left, right = String.(sub line 0 len2, sub line len2 len2) in
    let rs_left = rucksack_of_string left in
    let rec loop idx =
      if idx >= len2 then
        raise
          (Invalid_argument
             (sf
                "[prio_of_line] RHS (%s) does not contain any character of LHS \
                 (%s)"
                left right))
      else
        let c = String.get right idx in
        if BitsetArray.get rs_left (slot_of_item c) then prio c
        else loop (idx + 1)
    in
    loop 0

let%expect_test "test [prio_of_line]" =
  let test line = Printf.printf "Prio of %s: %d\n" line (prio_of_line line) in
  test "vJrwpWtwJgWrhcsFMMfFFhFp";
  [%expect {| Prio of vJrwpWtwJgWrhcsFMMfFFhFp: 16 |}];
  test "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL";
  [%expect {| Prio of jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL: 38 |}];
  test "PmmdzqPrVvPwwTWBwg";
  [%expect {| Prio of PmmdzqPrVvPwwTWBwg: 42 |}];
  test "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn";
  [%expect {| Prio of wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn: 22 |}];
  test "ttgJtRGJQctTZtZT";
  [%expect {| Prio of ttgJtRGJQctTZtZT: 20 |}];
  test "CrZsJsPPZsGzwwsLwLmpwMDw";
  [%expect {| Prio of CrZsJsPPZsGzwwsLwLmpwMDw: 19 |}]

let prio_of_elf_group ((line1, line2, line3) : string * string * string) : int =
  let rs1 = rucksack_of_string line1 in
  let rs2 = rucksack_of_string line2 in
  let rs3 = rucksack_of_string line3 in
  let rs = BitsetArray.(intersection rs1 (intersection rs2 rs3)) in
  match BitsetArray.contents rs with
  | [ slot ] -> prio_of_slot slot
  | _ ->
      raise
        (Invalid_argument
           (sf "The lines\n%s\n%s\n%s\nhave no common item" line1 line2 line3))

let%expect_test "test [prio_of_elf_group]" =
  let test (l1, l2, l3) =
    Printf.printf "Prio of %s, %s, %s: %d\n" l1 l2 l3
      (prio_of_elf_group (l1, l2, l3))
  in
  test
    ( "vJrwpWtwJgWrhcsFMMfFFhFp",
      "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
      "PmmdzqPrVvPwwTWBwg" );
  [%expect {| Prio of vJrwpWtwJgWrhcsFMMfFFhFp, jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL, PmmdzqPrVvPwwTWBwg: 18 |}];
  test
    ( "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
      "ttgJtRGJQctTZtZT",
      "CrZsJsPPZsGzwwsLwLmpwMDw" );
  [%expect {| Prio of wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn, ttgJtRGJQctTZtZT, CrZsJsPPZsGzwwsLwLmpwMDw: 52 |}]
