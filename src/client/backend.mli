
module type ARG = sig
  val conn : Connections.t
end

module Make(_: ARG) : Catapult.BACKEND
