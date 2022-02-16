
open Catapult_utils
module P = Catapult
module Tracing = P.Tracing
module Atomic = P.Atomic_shim_

let connect_endpoint ctx (addr: P.Endpoint_address.t) : [`Dealer] Zmq.Socket.t =
  let module E = P.Endpoint_address in
  let addr_str = E.to_string addr in
  let sock = Zmq.Socket.create ctx Zmq.Socket.dealer in
  Zmq.Socket.connect sock addr_str;
  Zmq.Socket.set_send_buffer_size sock (32 * 1024 * 1024);
  sock

(** Thread local logger. Each logger has a connection to the
    server/daemon. *)
module Logger = struct
  type t = {
    t_id: int; (* thread id *)
    trace_id: string; (* int, obtain from server *)
    buf: Buffer.t;
    out: P.Bare_encoding.Encode.t; (* outputs to buf *)
    sock: [`Dealer] Zmq.Socket.t;
    mutable closed: bool;
  }

  let send_msg (self:t) (msg:P.Ser.Client_message.t) : unit =
    Buffer.clear self.buf;
    P.Ser.Client_message.encode self.out msg;
    Zmq.Socket.send ~block:true self.sock (Buffer.contents self.buf);
    ()

  let close (self:t) =
    if not self.closed then (
      self.closed <- true;
      begin
        let msg = P.Ser.Client_message.Client_close_trace {trace_id=self.trace_id} in
        send_msg self msg
      end;
      Zmq.Socket.close self.sock;
    )

  (* add a new logger, connect to daemon, and return logger *)
  let create ~trace_id ~ctx ~addr ~t_id () : t =
    let buf = Buffer.create 512 in
    let out = P.Bare_encoding.Encode.of_buffer buf in
    let sock = connect_endpoint ctx addr in

    let logger = {
      t_id; sock;
      buf; out;
      trace_id;
      closed=false;
    } in

    Gc.finalise (fun _ -> close logger) (Thread.self()); (* close when thread dies *)

    (* send initial message *)
    send_msg logger (
      P.Ser.Client_message.Client_open_trace ({
          P.Ser.Client_open_trace.trace_id;
        })
    );
    logger
end

type t = {
  per_t: Logger.t Thread_local.t;
  addr: P.Endpoint_address.t;
  trace_id: string;
  ctx: Zmq.Context.t;
}

let close (self:t) =
  Thread_local.clear self.per_t;
  Zmq.Context.terminate self.ctx;
  ()

let create ~(addr: P.Endpoint_address.t) ~trace_id () : t =
  let ctx = Zmq.Context.create() in
  Zmq.Context.set_io_threads ctx 6;
  let per_t =
    Thread_local.create
      ~init:(fun ~t_id -> Logger.create ~ctx ~addr ~trace_id ~t_id ())
      ~close:Logger.close
      ()
  in
  let self = {
    per_t;
    ctx;
    addr;
    trace_id;
  } in
  Gc.finalise close self;
  self

(* send a message. *)
let send_msg (self:t) ~pid ~now (ev:P.Ser.Event.t): unit =
  let logger = Thread_local.get_or_create self.per_t in
  begin
    let msg = P.Ser.Client_message.Client_emit {
        P.Ser.Client_emit.
        trace_id=self.trace_id;
        ev;
      } in
    Logger.send_msg logger msg;
  end;

  (* maybe emit GC stats as well *)
  Gc_stats.maybe_emit ~now:ev.ts_us ~pid:(Int64.to_int ev.pid) ();
  ()
