
module Db = Sqlite3

let (let@) x f = x f
let debug_ = ref false

let[@inline] check_db_ e = match e with
  | Db.Rc.DONE | Db.Rc.OK -> ()
  | e ->
    failwith ("db error: " ^ Db.Rc.to_string e)

let write_json ~files ~out () =
  let oc = open_out out in
  let@ () = Fun.protect ~finally:(fun () -> close_out oc) in

  let first = ref true in
  let output_file file =
    if !debug_ then Printf.eprintf "reading file %S\n%!" file;
    let n_read = ref 0 in

    let db = Db.db_open ~mode:`READONLY file in
    let@ () = Fun.protect ~finally:(fun() -> while not (Db.db_close db) do () done) in

    let on_ev row =
      incr n_read;
      let j = row.(0) in

      if !first then (
        first := false;
        output_char oc '['
      ) else (
        output_string oc ",\n";
      );
      output_string oc j;
    in

    Db.exec_not_null_no_headers db "select ev from events; " ~cb:on_ev
    |> check_db_;

    if !debug_ then Printf.eprintf "read %d events\n%!" !n_read;
  in

  List.iter output_file files;
  output_char oc ']';
  flush oc

let conv ~files ~out () =
  if Filename.extension out = ".gz" then (
    let out = Filename.chop_extension out in
    write_json ~files ~out ();
    let cmd = Filename.quote_command "gzip" ["-f"; out] in
    let retcode = Sys.command cmd in
    if retcode<>0 then exit retcode
  ) else (
    write_json ~files ~out ();
  )

let () =
  let out = ref "" in
  let opts = [
    "-d", Arg.Set debug_, " enable debug";
    "--debug", Arg.Set debug_, " enable debug";
    "-o", Arg.Set_string out, " set output file (default 'trace.json.gz')";
  ] |> Arg.align in
  let files = ref [] in
  Arg.parse opts (fun f -> files := f :: !files) "catapult-conv file+ [opt]*";

  let out = if !out = "" then "trace.json.gz" else !out in
  let files = List.rev !files in

  if files<>[] then (
    conv ~files ~out ();
    if !debug_ then Printf.eprintf "done in %.3fs\n%!" (Sys.time());
  ) else (
    failwith "please provide at least one file"
  )

