module W = Catapult_wire

type t

val create :
  addr:W.Endpoint_address.t ->
  trace_id:string ->
  unit -> t

val send_msg : t -> pid:int -> now:float -> W.event -> unit

val close : t -> unit
