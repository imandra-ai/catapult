(** Actual logger *)

(** Setting up and tearing down a concrete backend. *)
module type S = sig
  val setup : unit -> unit
  (** Install the catapult logger as a profiling backend. *)

  val teardown : unit -> unit
  val with_setup : (unit -> 'a) -> 'a
end
