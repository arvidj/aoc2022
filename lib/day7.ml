open Base

module Path = struct
  type t = string list
  (** a path [/a/b/c] is represented as [c; b; a]. the path "/" is represented []. *)

  let of_string (str : string) : t =
    if str = "" || String.get str 0 != '/' then
      raise (Invalid_argument (sf "[of_string] not a valid path: %s" str));
    List.tl @@ String.split_on_char '/' str |> List.rev

  let to_string (path : t) : string = "/" ^ String.concat "/" (List.rev path)
  let root : t = []

  let parent (path : t) : t =
    match path with [] -> raise (Invalid_argument "[parent]") | _ :: tl -> tl

  let cd (path : t) (comp : string) = comp :: path
  let ( // ) = cd
end

module State = struct
  type t = (Path.t, int) Hashtbl.t

  let create n = Hashtbl.create n

  let add (st : t) (path : Path.t) (size : int) : unit =
    ignore
    @@ List.fold_left
         (fun path' comp ->
           hashtbl_update ~default:0 st path' (( + ) size);
           comp :: path')
         [] (List.rev path)

  let pp (st : t) : unit =
    let kvs =
      Hashtbl.fold
        (fun path size acc -> (Path.to_string path, size) :: acc)
        st []
    in
    List.iter
      (fun (path, size) -> Printf.printf "%s: %d\n" path size)
      (List.sort (fun (k1, _) (k2, _) -> String.compare k1 k2) kvs)

  let fold = Hashtbl.fold
  let get (st : t) (path : Path.t) : int = Hashtbl.find st path
end

let%expect_test "test State.add" =
  let st = State.create 5 in
  let test path size =
    State.(add st (Path.of_string path) size);
    State.pp st
  in
  test "/a/b/c.txt" 1;
  [%expect {|
    /: 1
    /a: 1
    /a/b: 1 |}];
  test "/a/b/d.txt" 2;
  [%expect {|
    /: 3
    /a: 3
    /a/b: 3 |}];
  test "/a/e.txt" 1;
  [%expect {|
    /: 4
    /a: 4
    /a/b: 3 |}]
