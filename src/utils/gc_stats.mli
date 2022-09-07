val set_gc_interval_us : float -> unit
(** Set the interval, in microseconds, between 2 successive gathering
    of GC statistics, to be emitted as catapult counters. *)

val maybe_emit : now:float -> pid:int -> unit -> unit
(** This checks whether it has been long enough since we last
    emitted counters for the GC.
    If it has, then it emits the event and resets the clock.
    @param now latest timestamp in microseconds (See {!Catapult.Clock.now_us})
    @param pid PID of the current process
*)
