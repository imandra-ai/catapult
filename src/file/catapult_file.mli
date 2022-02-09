
(** Logs to a file, if activated.

    Profiling is enabled if {!setup} is called, and if
    the environment variable "TRACE" is set to "1" or "true".
    The trace is emitted in the file "trace.json" in the directory
    where the program is launched.

    This uses mtime to obtain accurate timestamps of events.
*)


include Catapult.IMPL

val enable : unit -> unit
(** Enable manually *)
