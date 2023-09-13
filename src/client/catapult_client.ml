(** Backend for Catapult, using a connection to the daemon.
*)

module Endpoint_address = Catapult_utils.Endpoint_address
module Backend = Backend
module Connection = Connection

let default_endpoint = Endpoint_address.default
let with_conn = Connection.with_
let backend_of_conn : Connection.t -> Catapult.backend = Backend.make

(** Obtain a trace collector from a network connection *)
let trace_collector_of_conn : Connection.t -> Trace_core.collector =
 fun conn -> backend_of_conn conn |> Catapult.trace_collector_of_backend
