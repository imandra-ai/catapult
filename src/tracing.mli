
(** Profiling probes *)

type backend = (module Backend.S)

type probe

val null_probe : probe

val enabled : unit -> bool
(** Is tracing enabled? *)

val instant : string -> unit

val begin_ : string -> probe

val exit : probe -> unit

val with_ : string -> (unit -> 'a) -> 'a
val with1 : string -> ('a -> 'b) -> 'a -> 'b
val with2 : string -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c

(** Controls the current backend. *)
module Control : sig
  val setup : backend option -> unit

  val teardown : unit -> unit
end
