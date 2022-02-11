module P = Catapult

val to_json : Buffer.t -> P.Ser.Event.t -> string
