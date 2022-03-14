
let program_start =
  try Sys.getenv "TRACE_START" |> Int64.of_string |> Mtime.of_uint64_ns
  with _ ->
    let t = Mtime_clock.now() in
    (* children need to inherit that *)
    Unix.putenv "TRACE_START" (Int64.to_string @@ Mtime.to_uint64_ns t);
    t



(* main access to the clock *)
let[@inline] now_us () : float =
  let now = Mtime_clock.now() in
  Mtime.Span.to_us (Mtime.span program_start now)
