module Db = Sqlite3

let ( let@ ) x f = x f
let spf = Printf.sprintf
let debug_ = ref false
let long = ref false

let[@inline] check_db_ e =
  match e with
  | Db.Rc.DONE | Db.Rc.OK -> ()
  | e -> failwith ("db error: " ^ Db.Rc.to_string e)

module Dir = Directories.Project_dirs (struct
  let qualifier = "ai"
  let organization = "imandra"
  let application = "catapult"
end)

(** List databases in this directory *)
let list_dir (d : string) : _ list =
  let files = ref [] in
  Array.iter
    (fun f -> if Filename.extension f = ".db" then files := f :: !files)
    (Sys.readdir d);
  files := List.sort String.compare !files;
  !files

let write_json ~files ~out () =
  let oc = open_out out in
  let@ () = Fun.protect ~finally:(fun () -> close_out oc) in

  let first = ref true in
  let output_file file =
    (* resolve file *)
    let file =
      let fail () = failwith (Printf.sprintf "file %S does not exist" file) in
      if Sys.file_exists file then
        file
      else (
        match Dir.data_dir with
        | Some d ->
          let file_in_d = Filename.concat d file in
          if Sys.file_exists file_in_d then
            file_in_d
          else if file = "last" then (
            let files = Array.of_list @@ list_dir d in
            if files = [||] then
              fail ()
            else
              Filename.concat d files.(Array.length files - 1)
            (* last entry *)
          ) else
            fail ()
        | None -> fail ()
      )
    in

    if !debug_ then Printf.eprintf "reading file %S\n%!" file;
    let n_read = ref 0 in

    let db = Db.db_open ~mode:`NO_CREATE file in
    let@ () =
      Fun.protect ~finally:(fun () ->
          while not (Db.db_close db) do
            ()
          done)
    in
    Db.exec db "PRAGMA journal=delete;" |> check_db_;

    let on_ev row =
      incr n_read;
      let j = row.(0) in

      if !first then (
        first := false;
        output_char oc '['
      ) else
        output_string oc ",\n";
      output_string oc j
    in

    Db.exec_not_null_no_headers db "select ev from events; " ~cb:on_ev
    |> check_db_;

    if !debug_ then Printf.eprintf "read %d events\n%!" !n_read
  in

  List.iter output_file files;
  output_char oc ']';
  flush oc

let conv ~files ~out () =
  if Filename.extension out = ".gz" then (
    let out = Filename.chop_extension out in
    write_json ~files ~out ();
    let cmd = Printf.sprintf "gzip -f %s" (Filename.quote out) in
    let retcode = Sys.command cmd in
    if retcode <> 0 then exit retcode
  ) else
    write_json ~files ~out ()

let help =
  {|catapult-conv file+ [opt]*

This converts a trace (normally, a sqlite file with .db extension) into
a JSON file. The default output is `trace.json.gz`. This JSON file
can be fed, for example, to chrome://tracing or https://ui.perfetto.dev/ .

If multiple files are given, their event streams are merged.

The special file "last" refers to the latest snapshot in the state
directory (typically, ~/.local/share/catapult).
|}

let human_size d : string =
  let d = float d in
  if d > 2_000_000. then
    spf "%.2fMB" (d /. 1e6)
  else if d > 2000. then
    spf "%.2fkB" (d /. 1e3)
  else
    spf "%fB" d

let () =
  let out = ref "" in
  let opts =
    [
      "-d", Arg.Set debug_, " enable debug";
      "--debug", Arg.Set debug_, " enable debug";
      "-o", Arg.Set_string out, " set output file (default 'trace.json.gz')";
      "-l", Arg.Set long, " information for each file";
    ]
    |> Arg.align
  in
  let files = ref [] in
  Arg.parse opts (fun f -> files := f :: !files) help;

  let out =
    if !out = "" then
      "trace.json.gz"
    else
      !out
  in
  let files = List.rev !files in

  if files <> [] then (
    conv ~files ~out ();
    if !debug_ then Printf.eprintf "done in %.3fs\n%!" (Sys.time ())
  ) else (
    match Dir.data_dir with
    | None -> failwith "please provide at least one file"
    | Some d ->
      let files = list_dir d in
      Printf.printf "no file provided.\n";
      if files <> [] then (
        Printf.printf "daemon files:\n";

        let print_file s =
          if !long then (
            let size = (Unix.stat (Filename.concat d s)).st_size in
            Printf.printf "%s (%s)\n" s (human_size size)
          ) else
            Printf.printf "%s\n" s
        in

        List.iter print_file files
      )
  )
