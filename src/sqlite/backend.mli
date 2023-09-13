module type ARG = sig
  val writer : Writer.t
end

module Make (Arg : ARG) : Catapult.BACKEND

val make : Writer.t -> Catapult.backend
