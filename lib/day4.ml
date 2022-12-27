open Base

type region = int * int

let show_region (lo, hi) = sf "[%d-%d]" lo hi

let read_region (s : string) : region =
  match String.split_on_char '-' s with
  | [ low; high ] ->
      let low, high = (int_of_string low, int_of_string high) in
      if low > high then
        raise
          (Invalid_argument
             (sf "[read_region] invalid region: %s, [low] is higher than [high]"
                s))
      else (low, high)
  | _ -> raise (Invalid_argument (sf "[read_region] unreadable region: %s" s))

let%expect_test "test [read_region]" =
  let test s =
    Printf.printf "Region of %s: %s\n" s (read_region s |> show_region)
  in
  test "2-4";
  [%expect
    {| Region of 2-4: [2-4] |}];
  test "6-8";
  [%expect
    {| Region of 6-8: [6-8] |}]

let read_elf_group (line : string) : region * region =
  match String.split_on_char ',' line with
  | [ reg1_s; reg2_s ] -> (read_region reg1_s, read_region reg2_s)
  | _ -> raise (Invalid_argument (sf "invalid line: %s" line))

let%expect_test "test [read_elf_group]" =
  let test s =
    let r1, r2 = read_elf_group s in
    Printf.printf "Regions of %s: %s and %s\n" s (show_region r1)
      (show_region r2)
  in
  test "2-4,6-8";
  [%expect
    {| Regions of 2-4,6-8: [2-4] and [6-8] |}];
  test "2-3,4-5";
  [%expect
    {| Regions of 2-3,4-5: [2-3] and [4-5] |}];
  test "5-7,7-9";
  [%expect
    {| Regions of 5-7,7-9: [5-7] and [7-9] |}]

let contains (r1 : region) (r2 : region) : bool =
  let aux (lo1, hi1) (lo2, hi2) = lo1 <= lo2 && hi2 <= hi1 in
  aux r1 r2 || aux r2 r1

let%expect_test "test [contains]" =
  let test r1 r2 =
    Printf.printf "Region %s and %s overlaps: %b\n" (show_region r1)
      (show_region r2) (contains r1 r2)
  in
  test (0, 1) (2, 3);
  [%expect
    {| Region [0-1] and [2-3] overlaps: false |}];
  test (2, 8) (3, 7);
  [%expect
    {| Region [2-8] and [3-7] overlaps: true |}];
  test (6, 6) (4, 6);
  [%expect
    {| Region [6-6] and [4-6] overlaps: true |}]

let overlaps (r1 : region) (r2 : region) : bool =
  let aux (_lo1, hi1) (lo2, hi2) = hi1 >= lo2 && hi1 <= hi2 in
  aux r1 r2 || aux r2 r1

let%expect_test "test [overlaps]" =
  let test r1 r2 =
    Printf.printf "Region %s and %s overlaps: %b\n" (show_region r1)
      (show_region r2) (overlaps r1 r2)
  in
  test (2, 4) (6, 8);
  [%expect {| Region [2-4] and [6-8] overlaps: false |}];
  test (2, 3) (4, 5);
  [%expect {| Region [2-3] and [4-5] overlaps: false |}];
  test (5, 7) (7, 9);
  [%expect {| Region [5-7] and [7-9] overlaps: true |}];
  test (2, 8) (3, 7);
  [%expect {| Region [2-8] and [3-7] overlaps: true |}];
  test (6, 6) (4, 6);
  [%expect {| Region [6-6] and [4-6] overlaps: true |}];
  test (2, 6) (4, 8);
  [%expect {| Region [2-6] and [4-8] overlaps: true |}];
  test (1, 1) (2, 2);
  [%expect {| Region [1-1] and [2-2] overlaps: false |}];
  test (2, 2) (1, 1);
  [%expect {| Region [2-2] and [1-1] overlaps: false |}];
  test (1, 2) (2, 2);
  [%expect {| Region [1-2] and [2-2] overlaps: true |}];
  test (2, 2) (1, 2);
  [%expect {| Region [2-2] and [1-2] overlaps: true |}];
  test (2, 2) (2, 2);
  [%expect {| Region [2-2] and [2-2] overlaps: true |}]
