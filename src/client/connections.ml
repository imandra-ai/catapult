
module P = Catapult
module Tracing = P.Tracing

module Atomic = P.Atomic_shim_

(* emulate thread local storage *)
module Int_map = Map.Make(struct
    type t = int
    let compare: int -> int -> int = compare
  end)

type t = {
  per_t: logger Int_map.t Atomic.t;
  addr: P.Endpoint_address.t;
  trace_id: string;
  mutable last_gc_stat: float;
}

(** Thread local logger *)
and logger = {
  main: t;
  t_id: int; (* thread id *)
  buf: Buffer.t;
  out: P.Bare_encoding.Encode.t; (* outputs to buf *)
  oc: out_channel;
  mutable closed: bool;
  mutable active: bool; (* emitting a message? *)
}

let[@inline] modify_map_ ~f (self:t) =
  while not (
    let cur = Atomic.get self.per_t in
    let new_ = f cur in
    Atomic.compare_and_set self.per_t cur new_
  )
  do () done

let send_msg_logger_ (self:logger) (msg:P.Ser.Client_message.t) : unit =
  Buffer.clear self.buf;
  P.Ser.Client_message.encode self.out msg;
  let len = Buffer.length self.buf in
  Printf.fprintf self.oc "b%d\n" len; (* framing *)
  Buffer.output_buffer self.oc self.buf;
  ()

let close_logger (self:logger) =
  if not self.closed then (
    self.closed <- true;
    flush self.oc;
    close_out self.oc;
    modify_map_ self.main ~f:(fun m -> Int_map.remove self.t_id m);
  )

let close (self:t) =
  Int_map.iter (fun _ per_t -> close_logger per_t) (Atomic.get self.per_t)

let create ~(addr: P.Endpoint_address.t) ~trace_id () : t =
  let self = {
    per_t=Atomic.make Int_map.empty;
    addr;
    trace_id;
    last_gc_stat=P.Clock.now_us();
  } in
  Gc.finalise close self;
  self

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

(* add a new logger, connect to daemon, and return logger *)
let[@inline never] add_logger_ self ~t_id : logger =
  let buf = Buffer.create 512 in
  let out = P.Bare_encoding.Encode.of_buffer buf in
  let oc = connect_endpoint self.addr in
  let logger = {
    t_id; oc; buf; out; main=self; closed=false; active=false;
  } in

  modify_map_ self ~f:(fun m -> Int_map.add t_id logger m);
  Gc.finalise (fun _ -> close_logger logger) (Thread.self()); (* close when thread dies *)

  (* send initial message *)
  send_msg_logger_ logger (
    P.Ser.Client_message.Client_open_trace ({
        P.Ser.Client_open_trace.trace_id=self.trace_id;
      })
  );
  logger

(* obtain logger for this thread *)
let[@inline] get_logger self ~t_id : logger =
  let m = Atomic.get self.per_t in
  match Int_map.find t_id m with
  | log -> log
  | exception Not_found ->
    add_logger_ self ~t_id (* slow path *)

(* emit a GC counter event *)
let emit_gc_ ~pid () =
  let st = Gc.quick_stat() in
  Tracing.counter "gc" ~cs:[
    (Printf.sprintf "%d.major" pid), st.Gc.major_collections;
    (Printf.sprintf "%d.minor" pid), st.Gc.minor_collections;
    (Printf.sprintf "%d.compactions" pid), st.Gc.compactions;
    (Printf.sprintf "%d.heap_words" pid), st.Gc.heap_words;
    (Printf.sprintf "%d.heap_MB" pid), (st.Gc.heap_words * (Sys.word_size / 8) / 1024 / 1024);
    (Printf.sprintf "%d.minor_words" pid), (int_of_float st.Gc.minor_words);
  ]

let gc_interval_us = 1e5

let send_msg (self:t) ~pid ~now (ev:P.Ser.Event.t): unit =
  let logger = get_logger self ~t_id:(Int64.to_int ev.tid) in
  begin
    let old_active = logger.active in
    (* gc stat after .2s *)
    let must_emit_gc_ = not old_active && now -. self.last_gc_stat > gc_interval_us in
    logger.active <- true; (* prevent recursive gc event *)

    (* time to emit some GC counters *)
    if must_emit_gc_ then (
      self.last_gc_stat <- now;
      emit_gc_ ~pid ();
    );

    let msg = P.Ser.Client_message.Client_emit {P.Ser.Client_emit.ev} in
    send_msg_logger_ logger msg;

    logger.active <- old_active;
  end
