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
  Printf.printf "%s\n" line;
  Printf.printf "%s%s%s (%d)\n"
    (String.make (marker - len) ' ')
    (String.make (len - 1) '~')
    "^" marker

let%expect_test "test [overlaps]" =
  let test line = pp_marker line (marker_pos line 4) 4 in
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
