open Catapult_utils
module P = Catapult
module Tracing = P.Tracing
module Atomic = P.Atomic_shim_

type event = Ser.Event.t

module type ARG = sig
  val writer : Writer.t
end

module Make (A : ARG) : P.BACKEND = struct
  let writer = A.writer

  type local_buf = {
    t_id: int;
    buf: Buffer.t;
    mutable evs: string list; (* batch *)
    mutable n_evs: int;
  }

  let batch_size =
    try int_of_string @@ Sys.getenv "TRACE_BATCH_SIZE" with _ -> 100

  let max_batch_interval_us = 3. *. 1e6 (* max time between 2 flushes *)
  let last_batch_flush = Atomic.make (P.Clock.now_us ())

  (* send current batch to the writer *)
  let flush_batch (self : local_buf) : unit =
    if self.n_evs > 0 then (
      let b = List.rev self.evs in
      self.evs <- [];
      self.n_evs <- 0;
      Writer.write_string_l writer b
    )

  (* check if we need to flush the batch *)
  let check_batch (self : local_buf) ~now : unit =
    if
      self.n_evs > batch_size
      || self.n_evs > 0
         && now -. Atomic.get last_batch_flush > max_batch_interval_us
    then (
      Atomic.set last_batch_flush now;
      flush_batch self
    )

  (* per-thread buffer *)
  let buf : local_buf Thread_local.t =
    Thread_local.create
      ~init:(fun ~t_id ->
        { t_id; buf = Buffer.create 1024; n_evs = 0; evs = [] })
      ~close:flush_batch ()

  let teardown () =
    Thread_local.clear buf;
    Writer.close writer

  let tick () =
    let now = P.Clock.now_us () in
    Thread_local.iter buf ~f:(check_batch ~now)

  module Out = Catapult_utils.Json_out

  let[@inline] field_col oc = Out.char oc ':'
  let[@inline] field_sep oc = Out.char oc ','
  let any_val oc (j : string) = Out.raw_string oc j

  (* emit [k:v] using printer [f] for the value *)
  let field oc k f v : unit =
    Out.raw_string oc k;
    field_col oc;
    f oc v

  let[@inline] opt_iter o f =
    match o with
    | None -> ()
    | Some x -> f x

  let emit ~id ~name ~ph ~tid ~pid ~cat ~ts_us ~args ~stack ~dur ?extra () :
      unit =
    (* access local buffer to write and add to batch *)
    let lbuf = Thread_local.get_or_create buf in

    let j =
      let buf = lbuf.buf in
      Buffer.clear buf;

      Out.char buf '{';

      field buf {|"name"|} Out.str_val name;
      field_sep buf;

      field buf {|"ph"|} Out.char_val (P.Event_type.to_char ph);
      field_sep buf;

      field buf {|"tid"|} any_val (string_of_int tid);
      field_sep buf;

      field buf {|"ts"|} Out.float ts_us;
      field_sep buf;

      opt_iter dur (fun dur ->
          field buf {|"dur"|} Out.float dur;
          field_sep buf);

      opt_iter id (fun i ->
          field buf {|"id"|} Out.str_val i;
          field_sep buf);

      opt_iter stack (fun s ->
          Out.raw_string buf {|"stack"|};
          field_col buf;
          Out.char buf '[';
          List.iteri
            (fun i x ->
              if i > 0 then field_sep buf;
              any_val buf x)
            s;
          Out.char buf ']';
          field_sep buf);

      opt_iter cat (fun cs ->
          Out.raw_string buf {|"cat"|};
          field_col buf;
          Out.char buf '"';
          List.iteri
            (fun i x ->
              if i > 0 then field_sep buf;
              Out.raw_string buf x)
            cs;
          Out.char buf '"';
          field_sep buf);

      opt_iter args (fun args ->
          Out.raw_string buf {|"args"|};
          field_col buf;
          Out.char buf '{';
          List.iteri
            (fun i (k, v) ->
              if i > 0 then field_sep buf;
              Out.str_val buf k;
              field_col buf;
              Out.arg buf (v : P.Arg.t))
            args;
          Out.char buf '}';
          field_sep buf);

      opt_iter extra (fun l ->
          List.iter
            (fun (x, y) ->
              Out.str_val buf x;
              field_col buf;
              Out.str_val buf y;
              field_sep buf)
            l);

      field buf {|"pid"|} Out.int pid;
      Out.char buf '}';
      Buffer.contents buf
    in

    lbuf.evs <- j :: lbuf.evs;
    lbuf.n_evs <- 1 + lbuf.n_evs;

    (* see if we need to flush batch or emit GC counters *)
    check_batch lbuf ~now:ts_us;
    Gc_stats.maybe_emit ~now:ts_us ~pid ();

    ()
end
