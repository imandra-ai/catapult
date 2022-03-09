
(** Logs to a file, if activated.

    Profiling is enabled if {!setup} is called, and if
    the environment variable "TRACE" is set to "1" or "true",
    or {!enable} was called.
    The trace is emitted in the file "trace.json" in the directory
    where the program is launched.
*)


include Catapult.IMPL

val enable : unit -> unit
(** Enable manually *)

val set_file : string -> unit
(** Set output file. Call before {!setup}. *)
