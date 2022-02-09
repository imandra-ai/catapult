
(** Profiling probes *)

type backend = (module Backend.S)

type 'a emit_fun =
  ?cat:string list ->
  ?pid:int ->
  ?tid:int ->
  ?args:(string*Arg.t) list ->
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

(** Controls the current backend. *)
module Control : sig
  val setup : backend option -> unit

  val teardown : unit -> unit
end
