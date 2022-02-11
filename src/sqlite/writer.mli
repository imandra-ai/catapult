
module P = Catapult
type t

val create :
  ?sync:[`OFF | `NORMAL] ->
  ?append:bool ->
  ?file:string ->
  trace_id:string ->
  dir:string ->
  unit -> t
(** Open writer into a database file.
    @param append if true, append to existing records (default false)
    @param file if provided, path to the file
    @param dir directory into which to place the file if [file] not provided
    @param trace_id basename for the file if [file] not provided
    @param sync controls "PRAGMA SYNCHRONOUS". Default is [`NORMAL] which is
    safe; [`OFF] is much faster but might corrupt DB if the machin crashes.
    See https://sqlite.org/pragma.html#pragma_synchronous
*)

val close : t -> unit
(** Close writer. *)

val write_string_l : t -> string list -> unit

val write_string : t -> string -> unit
(** Send a json encoded event.
    Thread safe. *)

