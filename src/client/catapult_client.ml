(** Backend for Catapult, using a connection to the daemon.
*)

module Endpoint_address = Catapult_utils.Endpoint_address
module Backend = Backend
module Connection = Connection

let default_endpoint = Endpoint_address.default
let with_conn = Connection.with_
let backend_of_conn : Connection.t -> Catapult.backend = Backend.make

(** Parse a remote address. *)
let addr_of_string_exn : string -> Endpoint_address.t =
  Endpoint_address.of_string_exn

(** Obtain a trace collector from a network connection *)
let trace_collector_of_conn : Connection.t -> Trace_core.collector =
 fun conn -> backend_of_conn conn |> Catapult.trace_collector_of_backend

(** [with_ ~addr () f] runs [f()] in an environment where a connection
     to [addr] has been established and is used to forward
     tracing events to  the remote daemon. *)
let with_ ~addr ?trace_id () (f : unit -> 'a) : 'a =
  Connection.with_ ~addr ?trace_id () @@ fun conn ->
  Trace_core.setup_collector (trace_collector_of_conn conn);
  f ()
