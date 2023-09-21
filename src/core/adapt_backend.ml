module Trace = Trace_core
module A = Atomic_shim_
module TLS = Thread_local

module type BACKEND = Backend.S
module type COLLECTOR = Trace.Collector.S

module Span_tbl = Hashtbl.Make (struct
  type t = int64

  let equal = Int64.equal
  let hash = Hashtbl.hash
end)

type backend = (module BACKEND)

open Event_type

let pid = Unix.getpid ()
let now_ = Clock.now_us

type full_arg = Trace.user_data

(** Counter used to allocate fresh spans *)
let span_gen_ = A.make 0

type span_info = { mutable data: (string * Trace.user_data) list }
(** Information for an in-flight span *)

(** Key for accessing span info in manual spans *)
let k_span_info : (string * [ `Sync | `Async ] * span_info) Trace.Meta_map.Key.t
    =
  Trace.Meta_map.Key.create ()

(** per-thread table to access span info from implicit spans *)
let span_info_tbl_ : span_info Span_tbl.t TLS.t =
  TLS.create ~init:(fun ~t_id:_ -> Span_tbl.create 8) ~close:ignore ()

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
    let span = A.fetch_and_add span_gen_ 1 |> Int64.of_int in

    let info_tbl = TLS.get_or_create span_info_tbl_ in
    let info = { data } in
    Span_tbl.add info_tbl span info;

    let finally () : unit =
      let now = now_ () in
      let dur = now -. start in
      let args =
        (info.data
          : (string * Trace.user_data) list
          :> (string * full_arg) list)
      in
      Span_tbl.remove info_tbl span;
      emit_real_ ~args name ~ts_us:start ~dur X
    in

    try
      let x = f span in
      finally ();
      x
    with e ->
      let bt = Printexc.get_raw_backtrace () in
      finally ();
      Printexc.raise_with_backtrace e bt

  let add_data_to_span span data =
    if data <> [] then (
      let info_tbl = TLS.get_or_create span_info_tbl_ in
      match Span_tbl.find_opt info_tbl span with
      | None -> ()
      | Some info -> info.data <- List.rev_append data info.data
    )

  let enter_manual_span ~parent ~flavor ~__FUNCTION__ ~__FILE__ ~__LINE__ ~data
      name : Trace.explicit_span =
    let span = Int64.of_int (A.fetch_and_add span_gen_ 1) in
    let flavor = Option.value ~default:`Sync flavor in
    let args =
      (data : (string * Trace.user_data) list :> (string * full_arg) list)
    in
    (match flavor with
    | `Sync ->
      emit_real_ ~cat:[ "async" ] ~args name ~id:(Int64.to_string span) B
    | `Async ->
      emit_real_ ~cat:[ "async" ] ~args name ~id:(Int64.to_string span) A_b);
    let meta =
      (* [data] has already been emitted on entry, so we only store additional
         meta data in this *)
      let info = { data = [] } in
      Trace.Meta_map.(empty |> add k_span_info (name, flavor, info))
    in
    { Trace.span; meta }

  let exit_manual_span (es : Trace.explicit_span) : unit =
    let name, flavor, info = Trace.Meta_map.find_exn k_span_info es.meta in
    (* emit data added after span creation *)
    let args =
      (info.data : (string * Trace.user_data) list :> (string * full_arg) list)
    in
    match flavor with
    | `Sync -> emit_real_ ~args name E
    | `Async ->
      emit_real_ ~cat:[ "async" ] ~args name
        ~id:(es.span |> Int64.to_string)
        A_e

  let add_data_to_manual_span (es : Trace.explicit_span) data =
    if data <> [] then (
      let _, _, info = Trace.Meta_map.find_exn k_span_info es.meta in
      info.data <- List.rev_append data info.data
    )

  let counter_int ~data:_ name n : unit =
    emit_real_ "counter" C ~args:[ name, `Int n ]

  let counter_float ~data:_ name n : unit =
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
