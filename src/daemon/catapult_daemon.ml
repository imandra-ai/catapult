
module P = Catapult
module W = Catapult_wire
module Db = Sqlite3
module Atomic = P.Atomic_shim_

module Log = (val Logs.src_log (Logs.Src.create "catapult.daemon"))

type batch = {
  mutable evs: W.event list;
  trace_id: string;
}

(** Encode events to json *)
module Json_enc : sig
  val to_json : Buffer.t -> W.event -> string
end = struct
  type json = Yojson.Basic.json

  module Out = struct
    let char = Buffer.add_char
    let string = Buffer.add_string
    let int out i = string out (string_of_int i)
    let int64 out i = string out (Int64.to_string i)
    let float out f = string out (Printf.sprintf "%.1f" f)
  end

  let[@inline] field_col oc = Out.char oc ':'
  let[@inline] field_sep oc = Out.char oc ','

  let json oc (j:json) = Out.string oc (Yojson.Basic.to_string j)
  let any_val oc (j:string) = Out.string oc j

  let char_val oc (c:char) =
    Out.char oc '"';
    Out.char oc c;
    Out.char oc '"'

  let str_val oc (s:string) =
    Out.char oc '"';
    let s = if String.contains s '"' then String.escaped s else s in
    Out.string oc s;
    Out.char oc '"'

  (* emit [k:v] using printer [f] for the value *)
  let field oc k f v : unit =
    Out.string oc k;
    field_col oc;
    f oc v

  let[@inline] opt_iter o f = match o with
    | None -> ()
    | Some x -> f x

  let to_json buf (ev:W.event) : string =
    let
      { W.Ser.Event.
        id; name; ph; pid; tid; cat; ts_sec; args; stack; dur; extra } = ev
    in

    Buffer.clear buf;
    Out.char buf '{';

    field buf {|"name"|} str_val name;
    field_sep buf;

    field buf {|"ph"|} char_val (Char.chr ph);
    field_sep buf;

    field buf {|"tid"|} Out.int64 tid;
    field_sep buf;

    field buf {|"ts"|} Out.float ts_sec;
    field_sep buf;

    opt_iter dur (fun dur ->
        field buf {|"dur"|} Out.float dur;
        field_sep buf;
      );

    opt_iter id (fun i ->
        field buf {|"id"|} str_val i;
        field_sep buf;
      );

    opt_iter stack (fun s ->
        Out.string buf {|"stack"|};
        field_col buf;
        Out.char buf '[';
        Array.iteri (fun i x -> if i>0 then field_sep buf; any_val buf x) s;
        Out.char buf ']';
        field_sep buf;
      );

    opt_iter cat (fun cs ->
        Out.string buf {|"cat"|};
        field_col buf;
        Out.char buf '"';
        Array.iteri (fun i x -> if i>0 then field_sep buf; Out.string buf x) cs;
        Out.char buf '"';
        field_sep buf;
      );

    opt_iter args (fun args ->
        Out.string buf {|"args"|};
        field_col buf;
        Out.char buf '{';
        Array.iteri (fun i {W.Ser.Arg.key; value} ->
            if i>0 then field_sep buf;
            str_val buf key; field_col buf;
            match value with
              | W.Ser.Arg_value.Arg_value_0 i -> Out.int64 buf i
              | W.Ser.Arg_value.Arg_value_1 s -> str_val buf s)
          args;
        Out.char buf '}';
        field_sep buf;
      );

    opt_iter extra (fun l ->
        Array.iter (fun {W.Ser.Extra.key; value} ->
            str_val buf key; field_col buf; str_val buf value;
            field_sep buf)
          l);

    field buf {|"pid"|} Out.int64 pid;
    Out.char buf '}';
    Buffer.contents buf
end

(* TODO: move most of this into sqlite library *)
module Writer : sig
  type t
  val create :
    dir:string ->
    unit -> t

  (** Write a batch to the SQLITE database for this trace *)
  val send_batch : t -> batch -> unit

  val decr_trace : t -> trace_id:string -> unit

  val incr_trace : t -> trace_id:string -> unit
end = struct
  module Str_tbl = Hashtbl.Make(struct
      include String  let hash=Hashtbl.hash
    end)

  let check_ret_ e = match e with
    | Db.Rc.DONE | Db.Rc.OK -> ()
    | e ->
      failwith ("db error: " ^ Db.Rc.to_string e)

  type db_conn = {
    refcount: int Atomic.t;
    db: Db.db;
  }

  type action =
    | Incr of string
    | Decr of string
    | Batch of batch

  type t = {
    dir: string;
    dbs: db_conn Str_tbl.t;
    lock: Mutex.t;
    q: action Queue.t;
    stop: bool Atomic.t;
    buf: Buffer.t;
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

  let schema = {|
    create table if not exists events (ev TEXT NOT NULL);
  |}

  (* open DB or increment refcount *)
  let incr_trace_ (self:t) ~trace_id : unit =
    with_lock self @@ fun () ->
    match Str_tbl.find_opt self.dbs trace_id with
    | Some db -> Atomic.incr db.refcount
    | None ->
      (try Sys.mkdir self.dir 0o755 with _ ->());
      let file = Filename.concat self.dir (trace_id ^ ".db") in
      Log.debug (fun k->k "open DB file %S" file);
      let db = Db.db_open ~mutex:`FULL file in
      (* TODO: is this worth it?
      Db.exec db "PRAGMA journal_mode=WAL;" |> check_ret_;
         *)
      Db.exec db "PRAGMA journal_mode=WAL;" |> check_ret_;
      Db.exec db "PRAGMA synchronous=NORMAL;" |> check_ret_;
      Db.exec db schema |> check_ret_;
      Str_tbl.add self.dbs trace_id {db; refcount=Atomic.make 1};
      ()

  (* decrement refcount and possibly close DB *)
  let decr_trace_ self ~trace_id : unit =
    match Str_tbl.find_opt self.dbs trace_id with
    | None -> Logs.err (fun k->k "trace %S not opened" trace_id); assert false
    | Some db ->
      let n = Atomic.fetch_and_add db.refcount (-1) in
      if n=1 then (
        Str_tbl.remove self.dbs trace_id;
        assert (Atomic.get db.refcount = 0);
        while not (Db.db_close db.db) do () done
      )

  (* obtain an already opened DB *)
  let get_db_ self ~trace_id : Db.db =
    match Str_tbl.find_opt self.dbs trace_id with
    | Some db -> db.db
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

      | Some (Batch batch) ->
        delay := 0.001;

        (* encode events to json string *)
        let encoded_evs =
          batch.evs
          |> List.rev_map (Json_enc.to_json self.buf)
        in

        let db = get_db_ self ~trace_id:batch.trace_id in
        let stmt = Db.prepare db "insert into events values (?);" in
        Db.exec db "begin transaction;" |> check_ret_;

        let add_ev ev =
          Db.bind_blob stmt 1 ev |> check_ret_;
          Db.step stmt |> check_ret_;
          Db.reset stmt |> check_ret_;
        in
        List.iter add_ev encoded_evs;
        Db.finalize stmt |> check_ret_;

        Db.exec db "commit transaction;" |> check_ret_;
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

  let[@inline] send_batch (self:t) b : unit =
    with_lock self @@ fun () ->
    Queue.push (Batch b) self.q

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
    ?addr:W.Endpoint_address.t ->
    writer:Writer.t ->
    unit -> t

  val stop : t -> unit

  val run : t -> unit
end = struct
  module Addr = W.Endpoint_address

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
    let addr, tcp = match addr with
      | Addr.Unix s -> Unix.ADDR_UNIX s, false
      | Addr.Tcp (h,port) ->
        let ip = Unix.inet_addr_of_string h in
        Unix.ADDR_INET (ip, port), true
    in
    if tcp then Unix.setsockopt sock Unix.TCP_NODELAY true;
    Unix.setsockopt_optint sock Unix.SO_LINGER None;
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
    let batch = ref {trace_id=""; evs=[]} in
    let batch_len = ref 0 in

    (* send batch of events to writer. *)
    let flush_batch () =
      if !batch_len > 0 then (
        let b = !batch in

        (* prepare a new batch *)
        batch_len := 0;
        batch := { evs=[]; trace_id= !trace_id };

        Writer.send_batch self.writer b
      );
    in

    let buf = ref (Bytes.create (16 * 1024)) in

    let continue = ref true in

    let handle_client_msg (msg:W.Ser.Client_message.t) : unit =
      Log.debug (fun k->k "client msg:@ %a" W.Ser.Client_message.pp msg);

      begin match msg with
        | W.Ser.Client_message.Client_open_trace {trace_id=id} ->
          Log.info (fun k->k "client opened trace %S" id);
          flush_batch();
          if !trace_id <> "" then Writer.decr_trace self.writer !trace_id;
          (* start new batch *)
          Writer.incr_trace self.writer id;
          trace_id := id;
          batch := {trace_id=id; evs=[]}

        | W.Ser.Client_message.Client_emit {ev} ->
          if !trace_id = "" then (
            Log.err (fun k->k "client: no trace ID opened");
            continue := false
          ) else (
            (* add to current batch *)
            let b = !batch in
            b.evs <- ev :: b.evs;
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
            let dec = W.Bare_encoding.Decode.of_bytes ~len:n !buf in
            let msg = W.Ser.Client_message.decode dec in
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
    let application = "catapult-daemon"
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
  let addr = ref W.Endpoint_address.default in
  let set_addr s = addr := W.Endpoint_address.of_string_exn s in
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

