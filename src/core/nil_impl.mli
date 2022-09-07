(** Nil implementation.

    This does not setup any backend. It can be useful for conditionally
    setting up an implementation, like using a "select" rule in dune. *)

include Impl.S
