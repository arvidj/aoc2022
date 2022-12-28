module State = struct
  type t = {
    contents : char Queue.t;
    element_count : int Array.t;
    mutable distinct : int;
    max_len : int;
  }

  let create (n : int) =
    {
      contents = Queue.create ();
      element_count = Array.init 256 (Fun.const 0);
      distinct = 0;
      max_len = n;
    }

  let rem (t : t) =
    let c = Queue.pop t.contents in
    let idx = Char.code c in
    let count = Array.get t.element_count idx in
    Array.(set t.element_count idx (count - 1));
    if count = 1 then t.distinct <- t.distinct - 1

  let add (t : t) (c : char) =
    let idx = Char.code c in
    if Queue.length t.contents = t.max_len then rem t;
    let count = Array.get t.element_count idx in
    Queue.push c t.contents;
    Array.(set t.element_count idx (count + 1));
    if count = 0 then t.distinct <- t.distinct + 1

  let distinct t = t.distinct
end

let marker_pos line len =
  let buf = State.create len in
  let rec aux i =
    if i < String.length line then (
      State.add buf (String.get line i);
      if State.distinct buf < len then aux (i + 1) else 1 + i)
    else raise (Failure "found no marker")
  in
  aux 0

let pp_marker line marker len =
  let padding = "..." in
  let prefix = 2 in
  let marker', visible_line =
    if marker > 80 then
      let marker' = String.length padding + 1 + len + prefix in
      let visible_line =
        String.sub line (marker - prefix)
          (String.length line - (marker + prefix))
      in
      (marker', padding ^ " " ^ visible_line)
    else (marker, line)
  in
  let visible_line =
    if String.length visible_line > 80 then
      String.sub visible_line 0 80 ^ " " ^ padding
    else visible_line
  in
  Printf.printf "%s\n" visible_line;
  Printf.printf "%s%s%s (%d)\n"
    (String.make (marker' - len) ' ')
    (String.make (len - 1) '~')
    "^" marker

let start_of_packet = 4
let start_of_message = 14

let%expect_test "test [start_of_packet]" =
  let test line =
    pp_marker line (marker_pos line start_of_packet) start_of_packet
  in
  test "bvwbjplbgvbhsrlpgdmjqwftvncz";
  [%expect {|
    bvwbjplbgvbhsrlpgdmjqwftvncz
     ~~~^ (5) |}];
  test "nppdvjthqldpwncqszvftbrmjlhg";
  [%expect {|
    nppdvjthqldpwncqszvftbrmjlhg
      ~~~^ (6) |}];
  test "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg";
  [%expect {|
    nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg
          ~~~^ (10) |}];
  test "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw";
  [%expect {|
    zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw
           ~~~^ (11) |}]

let%expect_test "test [start_of_message]" =
  let test line =
    pp_marker line (marker_pos line start_of_message) start_of_message
  in
  test "mjqjpqmgbljsphdztnvjfqwrcgsmlb";
  [%expect
    {|
    mjqjpqmgbljsphdztnvjfqwrcgsmlb
         ~~~~~~~~~~~~~^ (19) |}];
  test "bvwbjplbgvbhsrlpgdmjqwftvncz";
  [%expect
    {|
    bvwbjplbgvbhsrlpgdmjqwftvncz
             ~~~~~~~~~~~~~^ (23) |}];
  test "nppdvjthqldpwncqszvftbrmjlhg";
  [%expect
    {|
    nppdvjthqldpwncqszvftbrmjlhg
             ~~~~~~~~~~~~~^ (23) |}];
  test "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg";
  [%expect
    {|
    nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg
                   ~~~~~~~~~~~~~^ (29) |}];
  test "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw";
  [%expect
    {|
    zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw
                ~~~~~~~~~~~~~^ (26) |}]
