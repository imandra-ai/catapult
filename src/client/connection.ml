open Catapult_utils
module Atomic = Catapult.Atomic_shim_
module TLS = Thread_local_storage
module Int_map = Map.Make (Int)

let ( let@ ) = ( @@ )
let default_addr = Endpoint_address.default

let connect_endpoint ctx (addr : Endpoint_address.t) : [ `Dealer ] Zmq.Socket.t
    =
  let module E = Endpoint_address in
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
    out: Bare_encoding.Encode.t; (* outputs to buf *)
    sock: [ `Dealer ] Zmq.Socket.t;
    mutable closed: bool;
  }

  let send_msg ~ignore_err (self : t) (msg : Ser.Client_message.t) : unit =
    if not self.closed then (
      try
        Buffer.clear self.buf;
        Ser.Client_message.encode self.out msg;
        Zmq.Socket.send ~block:true self.sock (Buffer.contents self.buf)
      with e ->
        if ignore_err then
          ()
        else
          raise e
    )

  let close (self : t) =
    if not self.closed then (
      (let msg =
         Ser.Client_message.Client_close_trace { trace_id = self.trace_id }
       in
       send_msg ~ignore_err:true self msg);
      self.closed <- true;
      Zmq.Socket.close self.sock
    )

  (* add a new logger, connect to daemon, and return logger *)
  let create ~trace_id ~ctx ~addr ~t_id () : t =
    let buf = Buffer.create 512 in
    let out = Bare_encoding.Encode.of_buffer buf in
    let sock = connect_endpoint ctx addr in

    let logger = { t_id; sock; buf; out; trace_id; closed = false } in

    Gc.finalise (fun _ -> close logger) logger;

    (* send initial message *)
    send_msg ~ignore_err:false logger
      (Ser.Client_message.Client_open_trace { Ser.Client_open_trace.trace_id });
    logger
end

let k_logger : Logger.t option ref TLS.key = TLS.new_key (fun () -> ref None)

type t = {
  per_t: Logger.t Int_map.t Atomic.t;  (** Keep an eye on the loggers *)
  addr: Endpoint_address.t;
  trace_id: string;
  ctx: Zmq.Context.t;
  mutable closed: bool;
}

let add_per_t (self : t) tid logger : unit =
  while
    let old = Atomic.get self.per_t in
    not (Atomic.compare_and_set self.per_t old (Int_map.add tid logger old))
  do
    ()
  done

let remove_per_t (self : t) tid =
  while
    let old = Atomic.get self.per_t in
    not (Atomic.compare_and_set self.per_t old (Int_map.remove tid old))
  do
    ()
  done

let[@inline never] new_logger (self : t) : Logger.t =
  let t_self = Thread.self () in
  let t_id = Thread.id t_self in
  let logger =
    Logger.create ~ctx:self.ctx ~addr:self.addr ~trace_id:self.trace_id ~t_id ()
  in
  add_per_t self t_id logger;
  Gc.finalise (fun _ -> remove_per_t self t_id) t_self;
  logger

let close (self : t) =
  if not self.closed then (
    self.closed <- true;
    try
      let loggers = Atomic.exchange self.per_t Int_map.empty in
      Int_map.iter (fun _ l -> Logger.close l) loggers;
      Zmq.Context.terminate self.ctx
    with e ->
      Printf.eprintf "catapult: error during exit: %s\n%!"
        (Printexc.to_string e)
  )

let create ~(addr : Endpoint_address.t) ?(trace_id = "trace") () : t =
  let ctx = Zmq.Context.create () in
  Zmq.Context.set_io_threads ctx 6;
  let per_t = Atomic.make Int_map.empty in
  let self = { per_t; ctx; addr; trace_id; closed = false } in
  Gc.finalise close self;
  self

let[@inline] get_logger (self : t) : Logger.t =
  let r = TLS.get k_logger in
  match !r with
  | Some l -> l
  | None ->
    let logger = new_logger self in
    logger

let send_msg (self : t) ~pid ~now (ev : Ser.Event.t) : unit =
  if not self.closed then (
    let logger = get_logger self in
    let msg =
      Ser.Client_message.Client_emit
        { Ser.Client_emit.trace_id = self.trace_id; ev }
    in
    Logger.send_msg ~ignore_err:false logger msg;

    (* maybe emit GC stats as well *)
    Gc_stats.maybe_emit ~now:ev.ts_us ~pid:(Int64.to_int ev.pid) ()
  )

let with_ ~addr ?trace_id () f =
  let conn = create ~addr ?trace_id () in
  let@ () = Fun.protect ~finally:(fun () -> close conn) in
  f conn
