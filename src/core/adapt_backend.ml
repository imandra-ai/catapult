module type BACKEND = Backend.S
module type COLLECTOR = Trace.Collector.S

type backend = (module BACKEND)

open Event_type

let pid = Unix.getpid ()
let now_ = Clock.now_us

type full_arg = [ `Float of float | Trace.user_data ]

let span_gen_ = Atomic_shim_.make 0

let k_span_info : (string * [ `Sync | `Async ]) Trace.Meta_map.Key.t =
  Trace.Meta_map.Key.create ()

module Mk_collector (B : BACKEND) : COLLECTOR = struct
  (** actually emit an event via the backend *)
  let[@inline never] emit_real_ ?ts_us ?cat ?(pid = pid)
      ?(tid = Thread.self () |> Thread.id) ?stack ?args ?id ?extra ?dur name
      (ev : Event_type.t) : unit =
    let ts_us =
      match ts_us with
      | Some x -> x
      | None -> now_ ()
    in
    B.emit ~id ~pid ~cat ~tid ~ts_us ~stack ~args ~name ~ph:ev ~dur ?extra ();
    ()

  let with_span ~__FUNCTION__ ~__FILE__ ~__LINE__ ~data name f =
    let start = now_ () in
    let sp = Trace.Collector.dummy_span in

    let finally () : unit =
      let now = now_ () in
      let dur = now -. start in
      let args =
        (data : (string * Trace.user_data) list :> (string * full_arg) list)
      in
      emit_real_ ~args name ~ts_us:start ~dur X
    in
    Fun.protect ~finally (fun () -> f sp)

  let enter_manual_span ~parent ~flavor ~__FUNCTION__ ~__FILE__ ~__LINE__ ~data
      name : Trace.explicit_span =
    let span = Int64.of_int (Atomic_shim_.fetch_and_add span_gen_ 1) in
    let flavor = Option.value ~default:`Sync flavor in
    let args =
      (data : (string * Trace.user_data) list :> (string * full_arg) list)
    in
    (match flavor with
    | `Sync ->
      emit_real_ ~cat:[ "async" ] ~args name ~id:(Int64.to_string span) B
    | `Async ->
      emit_real_ ~cat:[ "async" ] ~args name ~id:(Int64.to_string span) A_b);
    let meta = Trace.Meta_map.(empty |> add k_span_info (name, flavor)) in
    { Trace.span; meta }

  let exit_manual_span (es : Trace.explicit_span) : unit =
    let name, flavor = Trace.Meta_map.find_exn k_span_info es.meta in
    match flavor with
    | `Sync -> emit_real_ name E
    | `Async ->
      emit_real_ ~cat:[ "async" ] name ~id:(es.span |> Int64.to_string) A_e

  let counter_int name n : unit = emit_real_ "counter" C ~args:[ name, `Int n ]

  let counter_float name n : unit =
    emit_real_ "counter" C ~args:[ name, `Float n ]

  let message ?span:_ ~data msg : unit =
    let args =
      (data : (string * Trace.user_data) list :> (string * full_arg) list)
    in
    let args = ("msg", `String msg) :: args in
    emit_real_ ~args "msg" I

  let meta_ ~args name = emit_real_ ~args name M
  let name_thread name = meta_ "thread_name" ~args:[ "name", `String name ]
  let name_process name = meta_ "process_name" ~args:[ "name", `String name ]
  let shutdown = B.teardown
end

let[@inline] adapt (b : backend) : (module COLLECTOR) =
  let module M = Mk_collector ((val b)) in
  (module M)
