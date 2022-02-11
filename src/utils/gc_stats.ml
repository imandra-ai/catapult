
module P = Catapult
module Atomic = P.Atomic_shim_

(* store last time we emitted GC events *)
let last_gc: float Atomic.t = Atomic.make (P.Clock.now_us())
let emitting_gc = Atomic.make false

let gc_interval_us = ref 1e5
let set_gc_interval_us s = gc_interval_us := s

(* emit a GC counter event *)
let[@inline never] emit_gc_ ~pid () =
  let st = Gc.quick_stat() in
  P.Tracing.counter "gc" ~cs:[
    (Printf.sprintf "%d.major" pid), st.Gc.major_collections;
    (Printf.sprintf "%d.minor" pid), st.Gc.minor_collections;
    (Printf.sprintf "%d.compactions" pid), st.Gc.compactions;
    (Printf.sprintf "%d.heap_words" pid), st.Gc.heap_words;
    (Printf.sprintf "%d.heap_MB" pid), (st.Gc.heap_words * (Sys.word_size / 8) / 1024 / 1024);
    (Printf.sprintf "%d.minor_words" pid), (int_of_float st.Gc.minor_words);
  ]

let maybe_emit ~now ~pid () =
  let must_emit_gc_ =
    now -. Atomic.get last_gc > !gc_interval_us
    &&

    (* NOTE: this is evaluated only if the interval condition is true,
       and it has a side effect (setting "emitting_gc" to true).
       Once we reach this point we must cleanup afterwards. *)
    not (Atomic.exchange emitting_gc true)
  in
  if must_emit_gc_ then (
    Atomic.set last_gc now;
    emit_gc_ ~pid ();
    Atomic.set emitting_gc false;

  )

