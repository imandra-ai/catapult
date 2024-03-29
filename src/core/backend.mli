(** Tracing backend.

    The backend is responsible for turning a call to some function
    in {!Tracing} (e.g. {!Tracing.instant}), to some concrete event
    stored or sent somewhere. Each backend can have its own idea
    of how to represent and store the events.

    if no backend is installed, the tracing functions will do nothing.
*)

type arg = Trace_core.user_data

module type S = sig
  val emit :
    id:string option ->
    name:string ->
    ph:Event_type.t ->
    tid:int ->
    pid:int ->
    cat:string list option ->
    ts_us:float ->
    args:(string * [< arg ]) list option ->
    stack:string list option ->
    dur:float option ->
    ?extra:(string * string) list ->
    unit ->
    unit
  (** Emit an event. *)

  val tick : unit -> unit
  (** Function that can be called regularly to ensure background work
      is done. Potentially useful for long-running processes. *)

  val teardown : unit -> unit
  (** Tear down backend once the program is shutting down. *)
end
