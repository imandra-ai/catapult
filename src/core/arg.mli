(** Custom arguments.

    These arguments can be emitted as part of most events, and can be used
    to store custom data, debug messages, etc.
*)

type t = [`Int of int | `String of string | `Float of float | `Bool of bool | `Null]

