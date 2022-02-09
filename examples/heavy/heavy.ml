
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

let () =
  Catapult_file.with_setup @@ fun () ->
  let n = try int_of_string (Sys.getenv "N") with _ -> 10 in
  Printf.printf "run %d iterations\n%!" n;

  for i = 1 to n do
    let@ () = Tr.with_ "main iter" in
    Printf.printf "iteration %d\n%!" i;
    for j=1 to 4 do
      do_work();
    done;
    if i mod 3 = 0 then Gc.major();
  done

