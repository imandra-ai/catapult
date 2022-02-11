
open Catapult_utils
module P = Catapult
module Tracing = P.Tracing

module Atomic = P.Atomic_shim_

let connect_endpoint (addr: P.Endpoint_address.t) : out_channel =
  let module E = P.Endpoint_address in
  let dom = match addr with
    | E.Unix _ -> Unix.PF_UNIX
    | E.Tcp _ -> Unix.PF_INET in
  let sock = Unix.socket ~cloexec:true dom Unix.SOCK_STREAM 0 in
  let addr, tcp = match addr with
    | E.Unix s -> Unix.ADDR_UNIX s, false
    | E.Tcp (h,port) ->
      let ip = Unix.inet_addr_of_string h in
      Unix.ADDR_INET (ip, port), true
  in
  if tcp then Unix.setsockopt sock Unix.TCP_NODELAY true;
  Unix.connect sock addr;
  Unix.out_channel_of_descr sock

(** Thread local logger. Each logger has a connection to the
    server/daemon. *)
module Logger = struct
  type t = {
    t_id: int; (* thread id *)
    buf: Buffer.t;
    out: P.Bare_encoding.Encode.t; (* outputs to buf *)
    oc: out_channel;
    mutable closed: bool;
  }

  let send_msg (self:t) (msg:P.Ser.Client_message.t) : unit =
    Buffer.clear self.buf;
    P.Ser.Client_message.encode self.out msg;
    let len = Buffer.length self.buf in
    Printf.fprintf self.oc "b%d\n" len; (* framing *)
    Buffer.output_buffer self.oc self.buf;
    ()

  let close (self:t) =
    if not self.closed then (
      self.closed <- true;
      flush self.oc;
      close_out self.oc;
    )

  (* add a new logger, connect to daemon, and return logger *)
  let create ~trace_id ~addr ~t_id () : t =
    let buf = Buffer.create 512 in
    let out = P.Bare_encoding.Encode.of_buffer buf in
    let oc = connect_endpoint addr in
    let logger = {
      t_id; oc; buf; out; closed=false;
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
}

let close (self:t) = Thread_local.clear self.per_t

let create ~(addr: P.Endpoint_address.t) ~trace_id () : t =
  let per_t =
    Thread_local.create
      ~init:(fun ~t_id -> Logger.create ~addr ~trace_id ~t_id ())
      ~close:Logger.close
      ()
  in
  let self = {
    per_t;
    addr;
    trace_id;
  } in
  Gc.finalise close self;
  self

(* send a message. *)
let send_msg (self:t) ~pid ~now (ev:P.Ser.Event.t): unit =
  let logger =
    let t_id = Int64.to_int ev.tid in
    Thread_local.get_or_create self.per_t ~t_id
  in
  begin
    let msg = P.Ser.Client_message.Client_emit {P.Ser.Client_emit.ev} in
    Logger.send_msg logger msg;
  end;

  (* maybe emit GC stats as well *)
  Gc_stats.maybe_emit ~now:ev.ts_sec ~pid:(Int64.to_int ev.pid) ();
  ()
