module Tr = Trace

let ( let@ ) = ( @@ )
let spf = Printf.sprintf

let rec fib n =
  if n <= 1 then
    1
  else
    fib (n - 1) + fib (n - 2)

let do_work () =
  let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "dowork" in
  for j = 0 to 5_000 do
    let n = 15 + (j mod 5) in
    let@ _sp =
      Trace.with_span ~__FILE__ ~__LINE__
        ~data:(fun () -> [ "j", `Int j; "fib n", `Int n ])
        "step"
    in
    ignore (Sys.opaque_identity (fib n) : int)
  done

let run n =
  Printf.printf "run %d iterations\n%!" n;

  for i = 1 to n do
    let@ _sp = Trace.with_span ~__FILE__ ~__LINE__ "main iter" in
    Printf.printf "iteration %d\n%!" i;
    for j = 1 to 4 do
      do_work ()
    done;
    if i mod 3 = 0 then Gc.major ()
  done

type mode = Net | Db

let mode_of_str = function
  | "net" -> Net
  | "db" -> Db
  | s -> failwith ("unknown mode: " ^ s)

let sync_of_str = function
  | "normal" -> `NORMAL
  | "off" -> `OFF
  | "full" -> `FULL
  | s -> failwith ("unknown sync level: " ^ s)

let () =
  let n = ref 10 in
  let mode = ref Db in
  let file = ref "trace.json" in
  let addr = ref Catapult_client.default_endpoint in
  let j = ref 1 in
  let db = ref "trace.db" in
  let trace_id = ref "" in
  let sync = ref `NORMAL in
  let worker = ref false in
  let opts =
    [
      "-j", Arg.Set_int j, " number of processes";
      "-n", Arg.Set_int n, " number of iterations";
      "-o", Arg.Set_string file, " output file";
      ( "--mode",
        Arg.Symbol ([ "net"; "db" ], fun s -> mode := mode_of_str s),
        " serialization mode" );
      "--worker", Arg.Set worker, " act as a worker";
      ( "--db",
        Arg.String
          (fun s ->
            mode := Db;
            db := s),
        " set trace database file" );
      ( "--db-sync",
        Arg.Symbol ([ "normal"; "off"; "full" ], fun s -> sync := sync_of_str s),
        " set level of sync for db" );
      "--trace-id", Arg.Set_string trace_id, " set trace ID";
      ( "--addr",
        Arg.String
          (fun s ->
            mode := Net;
            addr := Catapult_client.Endpoint_address.of_string_exn s),
        " network address" );
    ]
    |> Arg.align
  in
  Arg.parse opts (fun _ -> ()) "heavy";

  if (not !worker) && !j > 1 then (
    (match !mode with
    | Net -> ()
    | Db -> failwith "cannot use -j with a sqlite backend");

    let bin_name = Sys.executable_name in
    for _k = 2 to !j do
      let _p =
        Unix.open_process_args_out bin_name
          (Array.of_list
             (bin_name :: "--worker" :: "--trace-id" :: !trace_id
             :: Array.to_list (Array.sub Sys.argv 1 (Array.length Sys.argv - 1))
             ))
      in
      ignore (_p : out_channel)
    done
  );

  let run () = run !n in
  match !mode with
  | Net ->
    Printf.printf "use net client %s\n%!"
      (Catapult_client.Endpoint_address.to_string !addr);
    let trace_id =
      if !trace_id <> "" then
        Some !trace_id
      else
        None
    in
    let@ conn = Catapult_client.with_conn ?trace_id ~addr:!addr () in
    Trace_core.setup_collector (Catapult_client.trace_collector_of_conn conn);
    run ()
  | Db ->
    Printf.printf "use sqlite backend %s\n%!" !db;
    let@ writer = Catapult_sqlite.Writer.with_ ~file:!db ~sync:!sync () in
    Trace.setup_collector (Catapult_sqlite.trace_collector_of_writer writer);
    run ()
