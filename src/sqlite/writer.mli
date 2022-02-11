
module P = Catapult

(** A batch of events to insert into the DB *)
type batch = P.Ser.Event.t list

type t

val create :
  trace_id:string ->
  dir:string ->
  unit -> t

val close : t -> unit

val write_batch : t -> batch -> unit
(** Write a batch to the Sqlite database.
    Thread safe. *)

val write_event : t -> P.Ser.Event.t -> unit
(** Send an event, to be encoded to json.
    Thread safe. *)

val write_string_l : t -> string list -> unit

val write_string : t -> string -> unit
(** Send a json encoded event.
    Thread safe. *)

val with_buf : t -> (Buffer.t -> 'a) -> 'a
(** Use local buffer to produce a ['a].
    Thread safe. *)
