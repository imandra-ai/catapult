
(** Address for the daemon *)

type t =
  | Unix of string
  | Tcp of string * int

val to_string : t -> string
val of_string : string -> t option

val default : t
(** Default address. *)
