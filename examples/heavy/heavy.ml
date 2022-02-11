
module Tr = Catapult.Tracing
open Tr.Syntax
let spf = Printf.sprintf

let rec fib n =
  if n <= 1 then 1 else fib (n-1) + fib (n-2)

let do_work () =
  let@ () = Tr.with_ "dowork" in
  for j = 0 to 5_000 do
    let n = 15 + j mod 5 in
    let@ () = Tr.with_ ~args:["j", `Int j; "fib n", `Int n] "step" in
    ignore (Sys.opaque_identity (fib n) : int);
  done

let run n =
  Printf.printf "run %d iterations\n%!" n;

  for i = 1 to n do
    let@ () = Tr.with_ "main iter" in
    Printf.printf "iteration %d\n%!" i;
    for j=1 to 4 do
      do_work();
    done;
    if i mod 3 = 0 then Gc.major();
  done

type mode = Net | File | Db
let mode_of_str = function
  | "net" -> Net
  | "file" -> File
  | "db" -> Db
  | s -> failwith ("unknown mode: " ^ s)

let sync_of_str = function
  | "normal" -> `NORMAL
  | "off" -> `OFF
  | "full" -> `FULL
  | s -> failwith ("unknown sync level: " ^ s)

let () =
  let n = ref 10 in
  let mode = ref File in
  let file = ref "trace.json" in
  let addr = ref Catapult_client.default_endpoint in
  let db = ref "trace.db" in
  let sync = ref `NORMAL in
  let opts = [
    "-n", Arg.Set_int n, " number of iterations";
    "-o", Arg.Set_string file, " output file";
    "--mode", Arg.Symbol (["net"; "file"; "db"], (fun s->mode := mode_of_str s)), " serialization mode";
    "--db", Arg.String (fun s -> mode:= Db; db := s), " set trace database file";
    "--db-sync", Arg.Symbol (["normal";"off";"full"], fun s -> sync := sync_of_str s), " set level of sync for db";
    "--addr",
    Arg.String (fun s -> mode:=Net; addr := Catapult_client.Endpoint_address.of_string_exn s),
    " network address";
  ] |> Arg.align in
  Arg.parse opts (fun _ -> ()) "heavy";

  let run () = run !n in
  begin match !mode with
    | Net ->
      Printf.printf "use net client %s\n%!" (Catapult_client.Endpoint_address.to_string !addr);
      Catapult_client.set_endpoint !addr;
      Catapult_client.with_setup run
    | Db ->
      Printf.printf "use sqlite backend %s\n%!" !db;
      Catapult_sqlite.set_file !db;
      Catapult_sqlite.set_sqlite_sync !sync;
      Catapult_sqlite.with_setup run
    | File ->
      Printf.printf "write to file %S\n%!" !file;
      Catapult_file.set_file !file;
      Catapult_file.with_setup run
  end
