
(** Profiling probes.

    This is the main API. The user can insert probes into their code, and
    at runtime, these probes will use the {!Backend} (if present) to
    emit tracing events to be replayed later. If no backend is present,
    the probes will do nothing.
*)

type backend = (module Backend.S)

type arg = [`Int of int | `String of string]

type 'a emit_fun =
  ?cat:string list ->
  ?pid:int ->
  ?tid:int ->
  ?args:(string*arg) list ->
  string ->
  'a
(** An emitter function. The positional string argument is the name.

    @param cat list of categories for filtering the event
    @param pid the process ID
    @param tid the thread ID
    @param arguments list of arguments for the event, with a name for each

    @param name the name of this event
*)

type span_start
(** Represents the beginning of a span, to emit compact spans *)

val null_span : span_start

val enabled : unit -> bool
(** Is tracing enabled? *)

val emit : (Event_type.t -> unit) emit_fun
(** Emit a generic event. *)

val begin_ : unit -> span_start
val exit : (?stack:string list -> span_start -> unit)  emit_fun
val with_ : ((unit->'a) -> 'a) emit_fun
val with1 : (('a->'b) -> 'a->'b) emit_fun
val with2 : (('a->'b->'c) -> 'a->'b->'c) emit_fun
val with3 : (('a->'b->'c->'d) -> 'a->'b->'c->'d) emit_fun

val begin' : unit emit_fun
val exit': unit emit_fun

val obj_new : (id:string -> unit) emit_fun
val obj_snap : (snapshot:string -> id:string -> unit) emit_fun
val obj_delete : (id:string -> unit) emit_fun
val obj_with : (id:string -> (unit->'a) -> 'a) emit_fun
val obj_with1 : (id:string -> ('a->'b) -> 'a -> 'b) emit_fun

val a_begin : (id:string -> unit) emit_fun
val a_exit : (id:string -> unit) emit_fun
val a_snap : (id:string -> unit) emit_fun
val a_with : (id:string -> (unit->'a) -> 'a) emit_fun
val a_with1 : (id:string -> ('a->'b) -> 'a -> 'b) emit_fun

val f_begin : (id:string -> unit) emit_fun
val f_exit : (id:string -> unit) emit_fun
val f_step : (id:string -> unit) emit_fun

val instant : unit emit_fun
val instant_with_stack : (stack:string list -> unit) emit_fun
val counter : (cs:(string*int) list -> unit) emit_fun

val meta_thread_name : string -> unit
val meta_process_name : string -> unit

val tick : unit -> unit
(** Depending on the tracing backend, this needs to be
    called regularly to ensure background work is done. *)

module Syntax : sig
  val (let@) : ('a -> 'b) -> 'a -> 'b
end
include module type of Syntax

(** Controls the current backend. *)
module Control : sig
  val setup : backend option -> unit

  val teardown : unit -> unit
end
