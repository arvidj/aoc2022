open Base

module Forest = struct
  type t = { width : int; height : int; contents : int array array }

  let create ~width ~height (el : int) : t =
    let contents = Array.make_matrix width height el in
    { width; height; contents }

  let get m x y =
    try m.(x).(y)
    with Invalid_argument e ->
      raise
        (Invalid_argument
           (sf "[Forest.get] at x: %d, y: %d (w: %d, h: %d): %s" x y
              (Array.length m)
              (Array.length m.(0))
              e))

  let set m x y v =
    try m.(x).(y) <- v
    with Invalid_argument e ->
      raise
        (Invalid_argument
           (sf "[Forest.set] at x: %d, y: %d (w: %d, h: %d): %s" x y
              (Array.length m)
              (Array.length m.(0))
              e))

  let is_visible f x y : bool =
    let height = get f.contents x y in
    let smaller x y = get f.contents x y < height in
    (* Below *)
    List.for_all (fun y' -> smaller x y') (y + 1 -- f.height)
    (* To the right *)
    || List.for_all (fun x' -> smaller x' y) (x + 1 -- f.width)
    (* Above *)
    || List.for_all (fun y' -> smaller x y') (0 -- y)
    (* To the left *)
    || List.for_all (fun x' -> smaller x' y) (0 -- x)

  let compute_visible f : bool array array =
    let visible = Array.make_matrix f.width f.height false in
    let max_height_vert = Array.make f.width (-1) in
    (* Go from left to right, top to bottom *)
    for y = 0 to f.height - 1 do
      let max_height_hori = ref (-1) in
      for x = 0 to f.width - 1 do
        let height = get f.contents x y in
        if height > !max_height_hori then (
          visible.(x).(y) <- true;
          max_height_hori := height);
        if height > max_height_vert.(x) then (
          visible.(x).(y) <- true;
          max_height_vert.(x) <- height)
      done
    done;
    (* Go from right to left, bottom to top *)
    Array.fill max_height_vert 0 f.width (-1);
    for y = f.height - 1 downto 0 do
      let max_height_hori = ref (-1) in
      for x = f.width - 1 downto 0 do
        let height = get f.contents x y in
        if height > !max_height_hori then (
          visible.(x).(y) <- true;
          max_height_hori := height);
        if height > max_height_vert.(x) then (
          visible.(x).(y) <- true;
          max_height_vert.(x) <- height)
      done
    done;
    visible

  let get_visible_count f =
    let visible = compute_visible f in
    let visible_expected = ref 0 in
    let visible_outcome = ref 0 in
    for x = 0 to f.width - 1 do
      for y = 0 to f.height - 1 do
        if visible.(x).(y) then incr visible_outcome;
        if is_visible f x y then incr visible_expected
      done
    done;
    (!visible_outcome, !visible_expected)

  let show f =
    let visible = compute_visible f in
    let color_outcome x y h =
      Color.apply ~apply:(get visible x y) Color.FG.green h
    in
    let color_expected x y h =
      Color.apply ~apply:(is_visible f x y) Color.FG.green h
    in
    let show color =
      List.map
        (fun y ->
          List.map
            (fun x -> color x y @@ string_of_int (get f.contents x y))
            (0 -- f.width)
          |> String.concat "")
        (0 -- f.height)
      |> String.concat "\n"
    in
    let concat xs ys =
      List.map2
        (fun x y -> x ^ "   " ^ y)
        (String.split_on_char '\n' xs)
        (String.split_on_char '\n' ys)
      |> String.concat "\n"
    in
    "Outcome, expected\n\n" ^ concat (show color_outcome) (show color_expected)
end
