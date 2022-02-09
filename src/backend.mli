
module type S = sig
  val get_ts : unit -> float

  val emit :
    id:string option ->
    name:string ->
    ph:Event_type.t ->
    tid:int ->
    pid:int ->
    cat:string list option ->
    ts_sec:float ->
    args:(string*Arg.t) list option ->
    stack:string list option ->
    dur:float option ->
    ?extra:(string*string) list ->
    unit -> unit

  val teardown : unit -> unit
end

