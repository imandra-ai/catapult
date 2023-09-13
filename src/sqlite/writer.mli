(** DB writer. *)

type t
(** DB writer. This holds a sqlite DB and writes new
    events to it. *)

val create :
  ?sync:[ `OFF | `NORMAL | `FULL ] ->
  ?append:bool ->
  ?file:string ->
  ?trace_id:string ->
  ?dir:string ->
  unit ->
  t
(** Open writer into a database file.
    @param append if true, append to existing records (default false)
    @param file if provided, path to the file
    @param dir directory into which to place the file if [file] not provided
    @param trace_id basename for the file if [file] not provided
    @param sync controls "PRAGMA SYNCHRONOUS". Default is [`NORMAL] which is
    safe; [`OFF] is much faster but might corrupt DB if the machin crashes.
    See https://sqlite.org/pragma.html#pragma_synchronous
*)

val cycle_stmt : t -> unit
(** Close and recreate internal state. *)

val close : t -> unit
(** Close writer. *)

val with_ :
  ?sync:[ `OFF | `NORMAL | `FULL ] ->
  ?append:bool ->
  ?file:string ->
  ?trace_id:string ->
  ?dir:string ->
  unit ->
  (t -> 'a) ->
  'a

val write_string_l : t -> string list -> unit

val write_string : t -> string -> unit
(** Send a json encoded event.
    Thread safe. *)
