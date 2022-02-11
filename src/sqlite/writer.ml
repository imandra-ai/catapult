
module P = Catapult
module Db = Sqlite3

type batch = P.Ser.Event.t list

let (let@) x f = x f
let check_ret_ e = match e with
  | Db.Rc.DONE | Db.Rc.OK -> ()
  | e ->
    failwith ("db error: " ^ Db.Rc.to_string e)

type t = {
  stmt_insert: Db.stmt; (* guard: db_lock *)
  db: Db.db; (* guard: db_lock *)
  db_lock: Mutex.t;
  buf: Buffer.t; (* guard: buf_lock *)
  buf_lock: Mutex.t;
  closed: bool Atomic.t;
}

let[@inline] with_lock lock f =
  Mutex.lock lock;
  try
    let x=f() in
    Mutex.unlock lock;
    x
  with e ->
    Mutex.unlock lock;
    raise e

(* main schema. *)
let schema = {|
  CREATE TABLE IF NOT EXISTS events (ev TEXT NOT NULL);
|}

let close self =
  if not (Atomic.exchange self.closed true) then (
    let@ () = with_lock self.db_lock in
    Db.finalize self.stmt_insert |> check_ret_;
    Db.exec self.db "PRAGMA journal=delete;" |> check_ret_; (* remove wal now *)
    while not (Db.db_close self.db) do () done
  )

let create ~trace_id ~dir () : t =
  (try Sys.mkdir dir 0o755 with _ ->());
  let file = Filename.concat dir (trace_id ^ ".db") in
  let db = Db.db_open ~mutex:`FULL file in
  (* TODO: is this worth it?
     Db.exec db "PRAGMA journal_mode=WAL;" |> check_ret_;
  *)
  Db.exec db "PRAGMA journal_mode=WAL;" |> check_ret_;
  Db.exec db "PRAGMA synchronous=NORMAL;" |> check_ret_;
  Db.exec db schema |> check_ret_;

  let stmt_insert = Db.prepare db "insert into events values (?);" in

  let self = {
    stmt_insert;
    db;
    db_lock=Mutex.create();
    buf=Buffer.create 512;
    buf_lock=Mutex.create();
    closed=Atomic.make false;
  } in
  Gc.finalise close self;
  self

let[@inline] write_str_ self s =
  Db.bind_blob self.stmt_insert 1 s |> check_ret_;
  Db.step self.stmt_insert |> check_ret_;
  Db.reset self.stmt_insert |> check_ret_

let[@inline] transactionally_ self f =
  Db.exec self.db "begin transaction;" |> check_ret_;
  f();
  Db.exec self.db "commit transaction;" |> check_ret_

let write_batch (self:t) (b:batch) : unit =
  (* encode events to json string *)
  let encoded_evs =
    let@ () = with_lock self.buf_lock in
    List.map (Ev_to_json.to_json self.buf) b
  in

  begin
    let@() = with_lock self.db_lock in
    let@() = transactionally_ self in
    List.iter (write_str_ self) encoded_evs;
  end

let[@inline] with_buf self f =
  let@() = with_lock self.buf_lock in
  Buffer.clear self.buf;
  f self.buf

let write_string (self:t) (j:string) =
  begin
    let@() = with_lock self.db_lock in
    write_str_ self j
  end

let write_string_l (self:t) (l:string list) =
  begin
    let@() = with_lock self.db_lock in
    let@() = transactionally_ self in
    List.iter (write_str_ self) l
  end

let write_event (self:t) (ev:P.Ser.Event.t) : unit =
  let j = with_buf self (fun buf -> Ev_to_json.to_json buf ev) in
  write_string self j
