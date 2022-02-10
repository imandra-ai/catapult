
module P = Catapult
module W = Catapult_wire
module Db = Sqlite3_utils
module Atomic = P.Atomic_shim_

module Log = (val Logs.src_log (Logs.Src.create "catapult.daemon"))

module Server : sig
  type t

  val create :
    ?addr:W.Endpoint_address.t ->
    dir:string ->
    unit -> t

  val stop : t -> unit

  val run : t -> unit
end = struct
  module Addr = W.Endpoint_address

  type t = {
    dir: string;
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

    let db = ref None in
    let pending = Queue.create () in (* next batch *)
    let n_pending = ref 0 in

    let buf = ref (Bytes.create (16 * 1024)) in

    let continue = ref true in

    let handle_client_msg (msg:W.Ser.Client_message.t) : unit =
      Log.debug (fun k->k "client msg:@ %a" W.Ser.Client_message.pp msg);
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

    try
      while !continue do
        let header = input_line ic in
        process_q header
      done
    with End_of_file ->
      close_in_noerr ic;
      close_out_noerr oc;
      ()

  let create ?(addr=Addr.default) ~dir () : t =
    let sock = setup_sock addr in
    let self = {
      dir; addr; sock; stop=Atomic.make false;
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

  let server = Server.create ~addr:!addr ~dir:!dir () in
  Server.run server;
  ()

