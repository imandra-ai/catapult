(** Backend for Catapult, using a connection to the daemon.


    - The tracing is enabled/disabled via "TRACE=1".
    - The trace identifier is specified in "TRACE_ID" (as a unique string ID).
    - The daemon's address is either "TRACE_ENDPOINT=ipc://<path>"
      or "TRACE_ENDPOINT=tcp://host:port".

*)

include Catapult.IMPL
module Endpoint_address = Catapult.Endpoint_address

val enable : unit -> unit
val enabled : unit -> bool

val set_trace_id : string -> unit
(** Must be called before the setup. *)

val get_trace_id : unit -> string
val default_endpoint : Endpoint_address.t
val get_endpoint : unit -> Endpoint_address.t

val set_endpoint : Endpoint_address.t -> unit
(** Must be called before the setup. *)

val set_tcp_endpoint : string -> int -> unit
val set_ipc_endpoint : string -> unit
