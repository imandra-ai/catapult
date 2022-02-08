
module type S = sig
  val get_ts : unit -> float

  val emit_duration_event :
    name : string ->
    start : float ->
    end_ : float ->
    unit ->
    unit

  val emit_instant_event :
    name : string ->
    ts : float ->
    unit ->
    unit

  val teardown : unit -> unit
end

