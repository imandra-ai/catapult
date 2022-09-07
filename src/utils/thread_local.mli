(** Basic thread-local storage.

    Values are indexed by the thread ID, obtained (for example)
    via [Thread.id (Thread.self())].

    This module assumes that [get_or_create m ~t_id] is only ever
    called from the thread whose ID is [t_id].

    The close function may be called from another thread.
*)

type 'a t

val create : init:(t_id:int -> 'a) -> close:('a -> unit) -> unit -> 'a t
val size : _ t -> int
val get_or_create : 'a t -> 'a
val remove : _ t -> t_id:int -> unit
val iter : f:('a -> unit) -> 'a t -> unit
val clear : _ t -> unit
