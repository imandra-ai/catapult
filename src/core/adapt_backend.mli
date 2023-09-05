module type BACKEND = Backend.S
module type COLLECTOR = Trace.Collector.S

val adapt : (module BACKEND) -> (module COLLECTOR)
(** Adapt a Catapult backend as a Trace collector *)
