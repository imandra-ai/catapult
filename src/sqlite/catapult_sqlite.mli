
(** Backend that writes directly to a Sqlite database.

    The database path is either directly provided with "TRACE_DB"
    or {!set_file}, or it's a file named after the trace ID in the directory
    set by {!set_dir} (or the default directory otherwise).

    The trace ID is determined by "TRACE_ID" if present, otherwise auto-generated
    or set via {!set_trace_id}.
*)

include Catapult.IMPL

val set_sqlite_sync : [`OFF | `NORMAL | `FULL] -> unit
(** Set level of crash safety for sqlite.
    See {!Writer.create} for more details. *)

val enable : unit -> unit
val enabled : unit -> bool

val set_trace_id : string -> unit
val get_trace_id : unit -> string

val set_dir : string -> unit
(** Set directory in which to store the database by its trace ID. *)

val set_file : string -> unit
(** Set database path to use. If not specified, it will be picked
    from the trace ID, and the directory {!set_dir}. *)

module Writer = Writer
module Backend = Backend
module Ev_to_json = Ev_to_json
