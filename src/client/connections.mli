module P = Catapult

type t

val create :
  addr:P.Endpoint_address.t ->
  trace_id:string ->
  unit -> t

val send_msg : t -> pid:int -> now:float -> P.Ser.Event.t -> unit

val close : t -> unit
