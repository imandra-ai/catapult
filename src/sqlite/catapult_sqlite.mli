
(** Backend that writes directly to a Sqlite database.

    The trace ID is determined by "TRACE_ID" if present, otherwise auto-generated
    or set via {!set_trace_id}.
*)

include Catapult.IMPL

val enable : unit -> unit
val enabled : unit -> bool

val set_trace_id : string -> unit
val get_trace_id : unit -> string

val set_dir : string -> unit

module Writer = Writer
module Backend = Backend
module Ev_to_json = Ev_to_json
