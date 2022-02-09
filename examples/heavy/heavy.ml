
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


let () =
  let n = ref 10 in
  let net = ref false in
  let file = ref "trace.json" in
  let addr = ref Catapult_client.default_endpoint in
  let opts = [
    "-n", Arg.Set_int n, " number of iterations";
    "-o", Arg.Set_string file, " output file";
    "--net", Arg.Set net, " use network client";
    "--addr",
    Arg.String (fun s -> addr := Catapult_client.Endpoint_address.of_string_exn s),
    " network address";
  ] |> Arg.align in
  Arg.parse opts (fun _ -> ()) "heavy";

  let run () = run !n in
  if !net then (
    Printf.printf "use net client %s\n%!" (Catapult_client.Endpoint_address.to_string !addr);
    Catapult_client.set_endpoint !addr;
    Catapult_client.with_setup run
  ) else (
    Printf.printf "write to file %S\n%!" !file;
    Catapult_file.set_file !file;
    Catapult_file.with_setup run
  )
