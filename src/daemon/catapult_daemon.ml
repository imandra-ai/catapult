
module P = Catapult
module Tr = P.Tracing
module P_db = Catapult_sqlite
module Atomic = P.Atomic_shim_

module Log = (val Logs.src_log (Logs.Src.create "catapult.daemon"))
open Tr.Syntax

type event = P.Ser.Event.t
type batch = event list

let now_us = P.Clock.now_us

(** Handler writers for each trace *)
module Writer : sig
  type t
  val create :
    dir:string ->
    unit -> t

  val close : t -> unit

  val size : t -> int

  val incr_conn : t -> trace_id:string -> unit

  val decr_conn : t -> trace_id:string -> unit

  val write : t -> trace_id:string -> event -> unit

  (** Write a batch to the SQLITE database for this trace *)
  val write_batch : t -> trace_id:string -> batch -> unit

  val tick : t -> unit
end = struct
  module Str_tbl = Hashtbl.Make(struct
      include String  let hash=Hashtbl.hash
    end)

  (* close connection after that much time elapsed without writes. *)
  let close_after_us = 30. *. 1e6

  (* connection to a DB. Closed and removed if unused for more
     than {!close_after_us} *)
  type db_conn = {
    writer: P_db.Writer.t;
    last_write: float Atomic.t;
    rc: int Atomic.t;
    mutable batch_size: int;
    mutable batch: string list;
  }

  type t = {
    dir: string;
    dbs: db_conn Str_tbl.t;
    (* TODO: remove, unless we use domains for parallel encoding
       lock: Mutex.t;
       *)
    buf: Buffer.t;
  }

  (* NOTE: useless, a remnant of multithreaded version *)
  let[@inline] with_lock _self f = f ()

  let size self =
    let@ () = with_lock self in
    Str_tbl.length self.dbs

  let flush_batch_ (db:db_conn) : unit =
    if db.batch_size > 0 then (
      let b = List.rev db.batch in
      db.batch <- [];
      db.batch_size <- 0;
      P_db.Writer.write_string_l db.writer b;
    )

  let close_ (self:t) ~trace_id (db:db_conn) : unit =
    Tr.instant "db.close" ~args:["trace_id", `String trace_id];
    flush_batch_ db;
    P_db.Writer.close db.writer

  let[@inline never] open_ (self:t) ~trace_id ~rc : db_conn =
    Tr.instant "db.open" ~args:["trace_id", `String trace_id];
    let writer = P_db.Writer.create ~append:true ~dir:self.dir ~trace_id () in
    let db = {
      writer; rc=Atomic.make rc;
      last_write=Atomic.make @@ now_us();
      batch=[]; batch_size=0;
    } in
    Str_tbl.add self.dbs trace_id db;
    db

  (* obtain DB connection for this ID *)
  let get_db_ (self:t) ~trace_id : db_conn =
    with_lock self @@ fun () ->
    match Str_tbl.find_opt self.dbs trace_id with
    | Some db -> db
    | None -> open_ self ~trace_id ~rc:0

  let incr_conn (self:t) ~trace_id : unit =
    with_lock self @@ fun () ->
    match Str_tbl.find_opt self.dbs trace_id with
    | Some db -> Atomic.incr db.rc
    | None -> ignore (open_ self ~trace_id ~rc:1 : db_conn)

  let decr_conn (self:t) ~trace_id =
    with_lock self @@ fun () ->
    match Str_tbl.find_opt self.dbs trace_id with
    | Some db ->
      let n = Atomic.fetch_and_add db.rc (-1) in
      if n=1 then (
        close_ self ~trace_id db;
        Str_tbl.remove self.dbs trace_id
      )
    | None -> ()

  let[@inline] str_of_ev_ (self:t) (ev:event) : string =
    P_db.Ev_to_json.to_json self.buf ev

  let max_batch_size = 50

  let write (self:t) ~trace_id (ev:event) : unit =
    let@ () = with_lock self in
    let s = str_of_ev_ self ev in
    let db = get_db_ self ~trace_id in
    db.batch <- s :: db.batch;
    db.batch_size <- 1 + db.batch_size;
    if db.batch_size > 50 then flush_batch_ db;
    Atomic.set db.last_write (now_us ())

  let write_batch (self:t) ~trace_id (b:event list) : unit =
    let@ () = with_lock self in
    let l = List.map (str_of_ev_ self) b in
    let db = get_db_ self ~trace_id in
    db.batch <- List.rev_append l db.batch;
    db.batch_size <- List.length l + db.batch_size;
    if db.batch_size > 50 then flush_batch_ db;
    Atomic.set db.last_write (now_us ())

  let close self =
    let@ () = with_lock self in
    let close_conn_ trace_id db =
      close_ self ~trace_id db
    in
    Str_tbl.iter close_conn_ self.dbs;
    Str_tbl.clear self.dbs

  (* perform background work: cleaning up connections not used in
     a while *)
  let tick (self:t) : unit =
    let@ () = with_lock self in
    let now = now_us() in
    Str_tbl.filter_map_inplace
      (fun trace_id db ->
         let age = now -. Atomic.get db.last_write in
         if age > close_after_us then (
           Tr.instant "db.gc"
             ~args:["trace_id", `String trace_id;
                    "age", `Float (age *. 1e-6)];
           close_ self ~trace_id db; (* collect *)
           None
         ) else Some db)
      self.dbs

  let create ~dir () : t =
    let self = {
      dir;
      dbs=Str_tbl.create 32;
      buf=Buffer.create 1024;
    } in
    self
end

(* TODO: use multicore + domainslib to parallelize parsing
   of events and their serialization to JSON values. *)

module Server : sig
  type t

  val create :
    ?addr:P.Endpoint_address.t ->
    writer:Writer.t ->
    unit -> t

  val stop : t -> unit

  val run : t -> unit
end = struct
  module Addr = P.Endpoint_address

  type t = {
    writer: Writer.t;
    addr: Addr.t;
    ctx: Zmq.Context.t;
    sock: [`Dealer] Zmq.Socket.t;
    stop: bool Atomic.t;
  }

  let setup_sock ~ctx ~addr () : _ =
    (* TODO?
    let tcp, file = match addr with
      | Addr.Unix s -> false, s
      | Addr.Tcp (h,port) -> true, ""
    in
    (* unix socket: remove it if it exists *)
    (* FIXME: systemd socket activation *)
    if not tcp then (try Sys.remove file with _ -> ());
       *)
    let sock = Zmq.Socket.create ctx Zmq.Socket.dealer in
    Zmq.Socket.set_receive_buffer_size sock (64 * 1024 * 1024);
    let addr_str = Addr.to_string addr in
    Zmq.Socket.bind sock addr_str;
    sock

  let handle_client_msg (self:t) (msg:P.Ser.Client_message.t) : unit =
    Log.debug (fun k->k "client msg:@ %a" P.Ser.Client_message.pp msg);

    begin match msg with
      | P.Ser.Client_message.Client_open_trace {trace_id} ->
        Log.info (fun k->k "client opened trace %S" trace_id);
        Tr.instant "open.trace" ~args:["id", `String trace_id];
        Writer.incr_conn self.writer ~trace_id;

      | P.Ser.Client_message.Client_emit {trace_id; ev} ->
        Writer.write self.writer ~trace_id ev

      | P.Ser.Client_message.Client_close_trace {trace_id} ->
        Log.info (fun k->k "client closed trace %S" trace_id);
        Writer.decr_conn self.writer ~trace_id
    end

  let create ?(addr=Addr.default) ~writer () : t =
    let ctx = Zmq.Context.create() in
    let sock = setup_sock ~addr ~ctx () in
    let self = {
      writer;
      ctx;
      sock;
      addr;
      stop=Atomic.make false;
    } in
    self

  let stop self = Atomic.set self.stop true

  let run (self:t) : unit =
    let@ () = Tr.with_ "listen.loop" in
    Sys.catch_break true;

    let poll = Zmq.Poll.(mask_of [| self.sock, In |]) in
    let timeout = 200 in (* milliseconds *)

    while not (Atomic.get self.stop) do
      match Zmq.Socket.recv ~block:false self.sock with
      | msg ->
        let dec = P.Bare_encoding.Decode.of_string msg in
        let msg = P.Ser.Client_message.decode dec in
        handle_client_msg self msg

      | exception Unix.Unix_error (Unix.EAGAIN, _, _) ->
        (* just poll *)
        let@ () = Tr.with_ "poll" in
        ignore (Zmq.Poll.poll ~timeout poll : _ option array);

      | exception Sys.Break ->
        Tr.instant "sys.break";
        Atomic.set self.stop true
    done
end

(** Background thread *)
module Ticker_thread = struct
  open Catapult_utils

  let start ~server ~writer () =
    let run() =
      Tr.meta_thread_name "ticker";
      let pid = Unix.getpid() in
      while true do
        let@ () = Tr.with_ "tick" in
        Thread.delay 0.2;
        let now = P.Clock.now_us() in

        Tr.tick();
        Tr.counter "daemon" ~cs:[
          "writer", Writer.size writer;
        ];
        Gc_stats.maybe_emit ~now ~pid ();
        Writer.tick writer;
      done
    in

    ignore (Thread.create run () : Thread.t)

end

module Dir = Directories.Project_dirs(struct
    let qualifier = "ai"
    let organization = "imandra"
    let application = "catapult"
  end)

let setup_logs ~debug () =
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level ~all:true (Some (if debug then Logs.Debug else Logs.Info));
  let lock = Mutex.create() in
  Logs.set_reporter_mutex
    ~lock:(fun () -> Mutex.lock lock)
    ~unlock:(fun () -> Mutex.unlock lock);
  ()

let () =
  (* tracing for the daemon itself *)
  let@ () = Catapult_sqlite.with_setup in

  Tr.meta_process_name "catapult-daemon";
  Tr.meta_thread_name "main";

  let debug = ref false in
  let dir = ref @@ match Dir.data_dir with None -> "." | Some d -> d in
  let addr = ref P.Endpoint_address.default in
  let set_addr s = addr := P.Endpoint_address.of_string_exn s in
  let opts = [
    "--addr", Arg.String set_addr , " network address to listen on";
    "-d", Arg.Set debug, " enable debug";
    "--debug", Arg.Set debug, " enable debug";
    "--dir", Arg.Set_string dir, " set state directory";
  ] |> Arg.align in
  Arg.parse opts (fun _ ->raise (Arg.Help "no such option")) "catapult tracing daemon";

  setup_logs ~debug:!debug ();
  Log.info (fun k->k "directory: %s" !dir);

  let writer = Writer.create ~dir:!dir () in
  let server = Server.create ~writer ~addr:!addr () in
  Ticker_thread.start ~writer ~server ();
  Server.run server;
  ()

