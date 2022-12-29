type t

val apply : ?apply:bool -> t -> string -> string
(** Apply a color to a string, and then reset colors. *)

val ( ++ ) : t -> t -> t
(** Combine two colors.

      Example: [Color.(bold ++ FG.red ++ BG.white)] *)

val reset : t
val bold : t

(** Foreground colors. *)
module FG : sig
  val black : t
  val red : t
  val green : t
  val yellow : t
  val blue : t
  val magenta : t
  val cyan : t
  val gray : t
  val bright_white : t
end

(** Background colors. *)
module BG : sig
  val black : t
  val red : t
  val green : t
  val yellow : t
  val blue : t
  val magenta : t
  val cyan : t
  val gray : t
  val bright_white : t
end
