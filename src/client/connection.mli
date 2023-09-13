open Catapult_utils

type t

val default_addr : Endpoint_address.t
val create : addr:Endpoint_address.t -> ?trace_id:string -> unit -> t
val send_msg : t -> pid:int -> now:float -> Ser.Event.t -> unit
val close : t -> unit

val with_ :
  addr:Endpoint_address.t -> ?trace_id:string -> unit -> (t -> 'a) -> 'a
