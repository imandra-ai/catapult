module Tr = Catapult.Tracing
let spf = Printf.sprintf

let rec fake_trace depth =
  if depth>=3 then ()
  else (
    Tr.with_ "step" @@ fun () ->
    Thread.delay 0.1;
    Printf.printf "fake (depth=%d)\n%!" depth;
    fake_trace (depth+1);
    Thread.delay 0.2;
    Tr.instant "iteration.done" ~args:["depth", `Int depth];
  )

let () =
  Catapult_file.with_setup @@ fun () ->
  let n = try int_of_string (Sys.getenv "N") with _ -> 10 in
  Printf.printf "run %d iterations\n%!" n;

  for _i = 1 to n do
    fake_trace 0;

    if _i mod 3 = 0 then Gc.major();
  done

