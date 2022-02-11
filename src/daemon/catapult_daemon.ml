
module P = Catapult
module P_db = Catapult_sqlite
module Atomic = P.Atomic_shim_

module Log = (val Logs.src_log (Logs.Src.create "catapult.daemon"))

type batch = P_db.Writer.batch

(* TODO: move most of this into sqlite library *)
module Writer : sig
  type t
  val create :
    dir:string ->
    unit -> t

  (** Write a batch to the SQLITE database for this trace *)
  val send_batch : t -> trace_id:string -> batch -> unit

  val decr_trace : t -> trace_id:string -> unit

  val incr_trace : t -> trace_id:string -> unit
end = struct
  module Str_tbl = Hashtbl.Make(struct
      include String  let hash=Hashtbl.hash
    end)

  type db_conn = {
    refcount: int Atomic.t;
    writer: P_db.Writer.t;
  }

  type action =
    | Incr of string
    | Decr of string
    | Batch of {
        trace_id: string;
        b: batch;
      }

  type t = {
    dir: string;
    dbs: db_conn Str_tbl.t;
    lock: Mutex.t;
    q: action Queue.t;
    buf: Buffer.t;
    stop: bool Atomic.t;
  }

  let[@inline] with_lock self f =
    Mutex.lock self.lock;
    try
      let x=f() in
      Mutex.unlock self.lock;
      x
    with e ->
      Mutex.unlock self.lock;
      raise e

  (* open trace, or increment refcount *)
  let incr_trace_ (self:t) ~trace_id : unit =
    with_lock self @@ fun () ->
    match Str_tbl.find_opt self.dbs trace_id with
    | Some db -> Atomic.incr db.refcount
    | None ->
      let writer = P_db.Writer.create ~dir:self.dir ~trace_id () in
      Str_tbl.add self.dbs trace_id {writer; refcount=Atomic.make 1};
      ()

  (* decrement refcount and possibly close trace *)
  let decr_trace_ self ~trace_id : unit =
    match Str_tbl.find_opt self.dbs trace_id with
    | None -> Log.err (fun k->k "trace %S not opened" trace_id); assert false
    | Some db ->
      let n = Atomic.fetch_and_add db.refcount (-1) in
      if n=1 then (
        Str_tbl.remove self.dbs trace_id;
        assert (Atomic.get db.refcount = 0);
        Log.info (fun k->k "close writer to %s" trace_id);
        P_db.Writer.close db.writer;
      )

  (* obtain an already opened trace writer *)
  let get_writer_ self ~trace_id : P_db.Writer.t =
    match Str_tbl.find_opt self.dbs trace_id with
    | Some db -> db.writer
    | None ->
      Logs.err (fun k->k "DB for trace %S not opened" trace_id);
      assert false

  let write_loop_ self : unit =
    let delay = ref 0.001 in
    while not (Atomic.get self.stop) do
      match with_lock self (fun () -> Queue.take_opt self.q) with
      | None ->
        (* just spin *)
        Thread.delay !delay;
        delay := min 0.1 (!delay *. 0.2 );

      | Some (Incr trace_id) ->
        delay := 0.001;
        incr_trace_ self ~trace_id

      | Some (Decr trace_id) ->
        delay := 0.001;
        decr_trace_ self ~trace_id

      | Some (Batch {trace_id; b=batch}) ->
        delay := 0.001;

        (* encode events to json string *)
        let encoded_evs =
          batch
          |> List.map (P_db.Ev_to_json.to_json self.buf)
        in

        let writer = get_writer_ self ~trace_id in
        P_db.Writer.write_string_l writer encoded_evs;
    done

  let create ~dir () : t =
    let self = {
      dir;
      dbs=Str_tbl.create 32;
      lock=Mutex.create();
      q=Queue.create();
      buf=Buffer.create 1024;
      stop=Atomic.make false;
    } in
    ignore (Thread.create write_loop_ self : Thread.t);
    (* TODO: writer thread *)
    self

  let[@inline] send_batch (self:t) ~trace_id b : unit =
    with_lock self @@ fun () ->
    Queue.push (Batch {trace_id; b}) self.q

  let incr_trace self ~trace_id =
    with_lock self @@ fun () ->
    Queue.push (Incr trace_id) self.q

  let decr_trace self ~trace_id =
    with_lock self @@ fun () ->
    Queue.push (Decr trace_id) self.q
end

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
    sock: Unix.file_descr;
    stop: bool Atomic.t;
    (* TODO: sock *)
  }

  let setup_sock addr : Unix.file_descr =
    let dom = match addr with
      | Addr.Unix _ -> Unix.PF_UNIX
      | Addr.Tcp _ -> Unix.PF_INET in
    let sock = Unix.socket ~cloexec:true dom Unix.SOCK_STREAM 0 in
    let addr, tcp, file = match addr with
      | Addr.Unix s -> Unix.ADDR_UNIX s, false, s
      | Addr.Tcp (h,port) ->
        let ip = Unix.inet_addr_of_string h in
        Unix.ADDR_INET (ip, port), true, ""
    in
    if tcp then Unix.setsockopt sock Unix.TCP_NODELAY true;
    Unix.setsockopt_optint sock Unix.SO_LINGER None;
    (* unix socket: remove it if it exists *)
    (* FIXME: systemd socket activation *)
    if not tcp then (try Sys.remove file with _ -> ());
    Unix.bind sock addr;
    Unix.listen sock 32;
    sock

  let string_of_sockaddr (addr:Unix.sockaddr) : string =
    match addr with
    | Unix.ADDR_INET (addr,port) ->
      Addr.to_string (Addr.Tcp (Unix.string_of_inet_addr addr, port))
    | Unix.ADDR_UNIX s ->
      Addr.to_string (Addr.Unix s)

  let serve_client_ (self:t) (conn:Unix.file_descr) : unit =
    let ic = Unix.in_channel_of_descr conn in
    let oc = Unix.out_channel_of_descr conn in

    let trace_id = ref "" in
    let batch = ref [] in
    let batch_len = ref 0 in

    (* send batch of events to writer. *)
    let flush_batch () =
      if !batch_len > 0 then (
        let b = List.rev !batch in

        (* prepare a new batch *)
        batch_len := 0;
        batch := [];

        Writer.send_batch self.writer ~trace_id:!trace_id b
      );
    in

    let buf = ref (Bytes.create (16 * 1024)) in

    let continue = ref true in

    let handle_client_msg (msg:P.Ser.Client_message.t) : unit =
      Log.debug (fun k->k "client msg:@ %a" P.Ser.Client_message.pp msg);

      begin match msg with
        | P.Ser.Client_message.Client_open_trace {trace_id=id} ->
          Log.info (fun k->k "client opened trace %S" id);
          flush_batch();
          if !trace_id <> "" then Writer.decr_trace self.writer !trace_id;
          (* start new batch *)
          Writer.incr_trace self.writer id;
          trace_id := id;
          batch := [];

        | P.Ser.Client_message.Client_emit {ev} ->
          if !trace_id = "" then (
            Log.err (fun k->k "client: no trace ID opened");
            continue := false
          ) else (
            (* add to current batch *)
            batch := ev :: !batch;
            incr batch_len;
            if !batch_len >= 100 then flush_batch();
          )
      end;
      ()
    in

    let process_q header =
        if String.length header > 0 && header.[0] = 'b' then (
          match int_of_string_opt (String.sub header 1 (String.length header-1)) with
          | None ->
            Log.err(fun k->k "invalid header: %S" header);
            continue := false
          | Some n ->
            Log.debug (fun k->k "read client message (size %d)" n);
            if Bytes.length !buf < n then (
              buf := Bytes.create (max (Bytes.length !buf*2) n);
            );
            really_input ic !buf 0 n;
            let dec = P.Bare_encoding.Decode.of_bytes ~len:n !buf in
            let msg = P.Ser.Client_message.decode dec in
            handle_client_msg msg
        ) else (
          Log.err(fun k->k "unknown header: %S@.disconnecting client" header);
          continue := false
        )
    in


    let exit() =
      flush_batch();
      if !trace_id<>"" then Writer.decr_trace self.writer !trace_id;
    in

    try
      while !continue do
        let header = input_line ic in
        process_q header
      done;
      exit();
    with End_of_file ->
      exit();
      close_in_noerr ic;
      close_out_noerr oc;
      ()

  let create ?(addr=Addr.default) ~writer () : t =
    let sock = setup_sock addr in
    let self = {
      writer;
      addr; sock;
      stop=Atomic.make false;
    } in
    self

  let stop self = Atomic.set self.stop true

  let run (self:t) : unit =
    while not (Atomic.get self.stop) do
      let conn, client_addr = Unix.accept self.sock in
      Logs.info (fun k->k "new connection from %s" (string_of_sockaddr client_addr));
      ignore (Thread.create (serve_client_ self) conn : Thread.t);
      ()
    done
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
  Server.run server;
  ()

