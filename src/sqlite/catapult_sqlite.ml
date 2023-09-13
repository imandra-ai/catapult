(** Backend that writes directly to a Sqlite database.

    Creating the database is done via {!Writer.create}
    or {!Writer.with_}. Then {!Backend} can be used to
    turn this {!Writer.t} into a Tracing collector. *)

module Writer = Writer
module Backend = Backend

let backend_of_writer : Writer.t -> Catapult.backend = Backend.make

(** Turn a writer into a Trace collector. *)
let trace_collector_of_writer : Writer.t -> Trace_core.collector =
 fun wr -> backend_of_writer wr |> Catapult.trace_collector_of_backend

(** [with_ () f] runs [f()] in a scope where a connection to
     a Sqlite DB has been established and is used to store
     tracing events emitted from within [f ()]. *)
let with_ ?sync ?append ?file ?trace_id ?dir () (f : unit -> 'a) : 'a =
  Writer.with_ ?sync ?append ?file ?trace_id ?dir () @@ fun wr ->
  Trace_core.setup_collector (trace_collector_of_writer wr);
  f ()
