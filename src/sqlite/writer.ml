module P = Catapult
module Db = Sqlite3
module Atomic = P.Atomic_shim_
open Catapult_utils

let ( let@ ) x f = x f

let check_ret_ e =
  match e with
  | Db.Rc.DONE | Db.Rc.OK -> ()
  | e -> failwith ("db error: " ^ Db.Rc.to_string e)

type t = {
  mutable stmt_insert: Db.stmt; (* guard: db_lock *)
  db: Db.db; (* guard: db_lock *)
  db_lock: Mutex.t;
  closed: bool Atomic.t;
}

let[@inline] with_lock lock f =
  Mutex.lock lock;
  try
    let x = f () in
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
    (* close DB itself *)
    let@ () = with_lock self.db_lock in
    Db.finalize self.stmt_insert |> check_ret_;
    Db.exec self.db "PRAGMA journal=delete;" |> check_ret_;
    (* remove wal now *)
    while not (Db.db_close self.db) do
      ()
    done
  )

let create ?(sync = `NORMAL) ?(append = false) ?file ?trace_id ?(dir = ".") () :
    t =
  let file =
    match file with
    | Some f -> f
    | None ->
      (try
         ignore
           (Sys.command (Printf.sprintf "mkdir -p %s" (Filename.quote dir))
             : int)
       with _ -> ());
      let trace_id =
        match trace_id with
        | Some id -> id
        | None -> "trace"
      in
      Filename.concat dir (trace_id ^ ".db")
  in
  let db = Db.db_open file in
  Db.busy_timeout db 3_000;
  (* TODO: is this worth it?
     Db.exec db "PRAGMA journal_mode=MEMORY;" |> check_ret_;
  *)
  Db.exec db "PRAGMA journal_mode=WAL;" |> check_ret_;
  (match sync with
  | `OFF -> Db.exec db "PRAGMA synchronous=OFF;" |> check_ret_
  | `FULL -> Db.exec db "PRAGMA synchronous=FULL;" |> check_ret_
  | `NORMAL -> Db.exec db "PRAGMA synchronous=NORMAL;" |> check_ret_);
  Db.exec db schema |> check_ret_;
  if not append then
    (* tabula rasa *)
    Db.exec db "DELETE FROM events; " |> check_ret_;

  let stmt_insert = Db.prepare db "insert into events values (?);" in

  let self =
    { stmt_insert; db; db_lock = Mutex.create (); closed = Atomic.make false }
  in
  Gc.finalise close self;
  self

let with_ ?sync ?append ?file ?trace_id ?dir () f =
  let wr = create ?sync ?append ?file ?trace_id ?dir () in
  let@ () = Fun.protect ~finally:(fun () -> close wr) in
  f wr

let cycle_stmt (self : t) =
  Db.finalize self.stmt_insert |> check_ret_;
  let stmt_insert = Db.prepare self.db "insert into events values (?);" in
  self.stmt_insert <- stmt_insert

let[@inline] write_str_ self s =
  Db.bind_blob self.stmt_insert 1 s |> check_ret_;
  Db.step self.stmt_insert |> check_ret_;
  Db.reset self.stmt_insert |> check_ret_

let[@inline] transactionally_ self f =
  Db.exec self.db "begin transaction;" |> check_ret_;
  f ();
  Db.exec self.db "commit transaction;" |> check_ret_

let write_string (self : t) (j : string) =
  let@ () = with_lock self.db_lock in
  write_str_ self j

let write_string_l (self : t) (l : string list) =
  let@ () = with_lock self.db_lock in
  let@ () = transactionally_ self in
  List.iter (write_str_ self) l
